open Core_kernel[@@warning "-D"]
open Bap.Std
open Bap_c_data
open Bap_c_type

type real_size = [`r32 | `r64 | `r128]
type 'a unqualified = (no_qualifier, 'a) spec

type bits = Int.t

let next_multitude_of ~n x = (x + (n-1)) land (lnot (n-1))


let padding alignment offset =
  let align = Size.in_bits alignment in
  (align - offset mod align) mod align



class base (m : model) = object(self)
  method integer (t : integer) : size =
    match m,t with
    | _,(`bool|#char) -> `r8
    | _,#short -> `r16
    | `LP32,#cint -> `r16
    | (`ILP32|`LLP64|`LP64),#cint -> `r32
    | `ILP64,#cint -> `r64
    | (`LP32|`ILP32|`LLP64),#long -> `r32
    | (`ILP64|`LP64), #long -> `r64
    | _,#long_long -> `r64
    | _,`enum fields -> self#enum fields

  method pointer : addr_size = match m with
    | #model32 -> `r32
    | #model64 -> `r64

  method enum _ = self#integer `uint   (* approximation *)

  method real (v : real) : real_size = match v with
    | `float -> `r32
    | `double -> `r64
    | `long_double -> `r128

  method private double_size : real_size -> size = function
    | `r32 -> `r64
    | `r64 -> `r128
    | `r128 -> `r256

  method complex : complex -> size = function
    | `cfloat -> self#double_size (self#real `float)
    | `cdouble -> self#double_size (self#real `double)
    | `clong_double -> self#double_size (self#real `long_double)

  method floating : floating -> size = function
    | #real as t -> (self#real t :> size)
    | #complex as t -> (self#complex t :> size)

  method basic : basic -> size = function
    | #integer as t -> self#integer t
    | #floating as t -> self#floating t

  method scalar : scalar -> size = function
    | `Basic {Spec.t} -> self#basic t
    | `Pointer _ -> (self#pointer :> size)

  method padding t offset : size option =
    match Size.of_int @@ padding (self#alignment t) offset with
    | Error _ -> None
    | Ok s -> Some s

  method alignment (t : Bap_c_type.t) : size =
    let byte = `r8 in
    match t with
    | `Void -> byte
    | `Array {Spec.t={Array.element}} -> self#alignment element
    | `Structure {Spec.t={Compound.fields}}
    | `Union {Spec.t={Compound.fields}} ->
      List.fold fields ~init:byte ~f:(fun align (_,t) ->
          Size.max align (self#alignment t))
    | `Function _ -> (self#pointer :> size)
    | #scalar as t -> self#scalar t

  method bits : t -> Int.t option = fun t ->
    let size = match t with
      | `Void -> None
      | #scalar as t -> Some (Size.in_bits (self#scalar t))
      | `Function _ -> None
      | `Union s -> self#union s
      | `Array s -> self#array s
      | `Structure s -> self#structure s in
    Option.map size ~f:(fun size ->
        let alignment = self#alignment t in
        next_multitude_of ~n:(Size.in_bits alignment) size)

  method array : _  -> Int.t option =
    fun {Spec.t={Array.element=t; size}} -> match size with
      | None -> None
      | Some n -> match self#bits t with
        | None -> None
        | Some x -> Some (n * x)

  method union : compound unqualified -> Int.t option =
    fun {Spec.t={Compound.fields}} ->
    List.map fields ~f:(fun (_,t) -> self#bits t) |> Option.all |> function
    | None -> None
    | Some ss -> List.max_elt ~compare:Int.compare ss |> function
      | None -> None
      | Some s -> Some s

  method structure : compound unqualified -> Int.t option =
    fun {Spec.t={Compound.fields}} ->
    List.fold fields ~init:(Some 0) ~f:(fun sz (_,field) -> match sz with
        | None -> None
        | Some sz -> match self#bits field with
          | None -> None
          | Some sz' ->
            Some (sz + sz' + padding (self#alignment field) sz))
end

type location = {
  offset : bits;
  size : bits;
  t : t;
}

type value =
  | Vector of (string * value * location) list
  | Scalar of location

open Option.Monad_infix

let location ?(offset=0) t size = {
  t; size; offset
}

let scalar ?offset t size = Scalar (location ?offset t size)

let vector xs = Vector xs

let rec layout : #base -> t -> value option = fun model t ->
  model#bits t >>= fun size -> match t with
  | `Void -> None
  | `Basic _
  | `Pointer _
  | `Function _ -> Some (scalar t size)
  | `Array {t={size=None}} -> None
  | `Array {t={element; size=Some elts}} ->
    model#bits element >>= fun size ->
    layout model element >>| fun value ->
    vector @@ List.init elts ~f:(fun _ ->
        "",value,location element size)
  | `Structure t -> structure model t
  | `Union t -> union model t

and structure : #base -> compound unqualified -> _ =
  fun size {t={fields}} ->
  let open Option.Monad_infix in
  List.fold fields ~init:(Some (0,[])) ~f:(fun acc (name,field) ->
      acc >>= fun (off,fields) ->
      size#bits field >>= fun sz ->
      layout size field >>| fun value ->
      let offset = off + padding (size#alignment field) off in
      let field = name,value,location ~offset field sz in
      (offset+sz,field::fields)) |> function
  | None -> None
  | Some (_,fields) -> Some (vector (List.rev fields))

and union : #base -> compound unqualified -> _ =
  fun size {t={fields}} ->
  List.map fields ~f:(fun (name,field) ->
      size#bits field >>= fun sz ->
      layout size field >>| fun value ->
      name,value,location field sz) |>
  Option.all >>|
  vector
