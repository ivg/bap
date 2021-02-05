open Core_kernel
open Bap_core_theory
open Bap_primus.Std
open KB.Syntax
open KB.Let

let export = Primus.Lisp.Type.Spec.[
    "+", (all any @-> any),
    "(+ X Y ... Z) returns X + Y + ... + Z, performing any necessary \
     type conversions in the process. If no numbers are supplied, 0 is \
     returned.";

    "-", (all any @-> any),
    "(- X Y ... Z) returns X - Y - ... - Z, performing any necessary \
     type conversions in the process. If no numbers are supplied \
     returns 0. If one number is supplied returns its negation.";

    "neg", (one any @-> any),
    "(neg X) returns the negation of X. Same as (- X).";

    "*", (all any @-> any),
    "(*  X Y ... Z) returns X * Y * ... * Z, performing any necessary \
     type conversions in the process. If no numbers are supplied, 1 is \
     returned.";

    "/", (all any @-> any),
    "(/  X Y ... Z) returns X / Y / ... / Z, performing any necessary \
     type conversions in the process. If no numbers are supplied, 1 is \
     returned. If one number is provided returns its reciprocal.";


    "s/", (all any @-> any),
    "(s/ X Y ... Z) returns signed X / Y / ... / Z, performing any \
     necessary type conversions in the process. If no numbers are \
     supplied, 1 is returned. If one number is provided returns its \
     signed reciprocal.";

    "mod", (all any @-> any),
    "(/ X Y ... Z) returns mod(X,Y) Y mod ... mod Z, performing any \
     necessary type conversions in the process. Where `X mod Y` is the \
     remainder of [X / Y]. If no numbers are supplied, 1 is \
     returned. If one number is provided returns that number.";

    "signed-mod", (all any @-> any),
    "(/ X Y ... Z) returns signed mod(X,Y) Y mod ... mod Z, performing \
     any necessary type conversions in the process. Where `X mod Y` is \
     the remainder of [X / Y]. If no numbers are supplied, 1 is \
     returned. If one number is provided returns that number.";

    "lshift", (all any @-> any),
    "(lshift X Y ... Z) returns X << Y << ... << Z, performing \
     any necessary type conversions in the process. Where `X << Y` is \
     logical shift left of X by Y bits. If no numbers are supplied, 1 \
     is returned. If one number is provided returns that number";

    "rshift", (all any @-> any),
    "(rshift X Y ... Z) returns X >> Y >> ... >> Z, performing \
     any necessary type conversions in the process. Where `X >> Y` is \
     logical shift right of X by Y bits. If no numbers are supplied, 1 \
     is returned. If one number is provided returns that number";

    "arshift", (all any @-> any),
    "(arshift X Y ... Z) returns X ~>> Y ~>> ... ~>> Z, performing \
     any necessary type conversions in the process. Where `X ~>> Y` is \
     arithmetic shift right of X by Y bits. If no numbers are supplied, 1 \
     is returned. If one number is provided returns that number";

    "logand", (all any @-> any),
    "(logand X Y ... Z) returns X land Y land ... land Z, performing \
     any necessary type conversions in the process. Where `X land Y` is \
     bitwise (logical) /\ (AND) of X and Y. If no numbers are supplied, 1 \
     is returned. If one number is provided returns that number";

    "logor", (all any @-> any),
    "(logor X Y ... Z) returns X lor Y lor ... lor Z, performing \
     any necessary type conversions in the process. Where `X lor Y` is \
     bitwise (logical) \\/ (OR) of X and Y. If no numbers are supplied, 1 \
     is returned. If one number is provided returns that number";

    "logxor", (all any @-> any),
    "(loxgor X Y ... Z) returns X lxor Y lxor ... lxor Z, performing \
     any necessary type conversions in the process. Where `X lor Y` is \
     bitwise (logical) exclusive \\/ (XOR) of X and Y. If no numbers \
     are supplied, 1 is returned. If one number is provided returns \
     that number";

    "=", (all any @-> any),
    "(= X Y ... Z) returns one if all numbers are equal in value.";

    "/=", (all any @-> any),
    "(/= X Y ... Z) returns one if all numbers are distinct.";

    "<", (all any @-> any),
    "(< X Y ... Z) returns one if all numbers are in monotonically \
     increasing order.";

    ">", (all any @-> any),
    "(> X Y ... Z) returns one if all numbers are in monotonically \
     decreasing order.";

    "<=", (all any @-> any),
    "(<= X Y ... Z) returns one if all numbers are in monotonically \
     nondecreasing order.";

    ">=", (all any @-> any),
    "(> X Y ... Z) returns one if all numbers are in monotonically \
     nonincreasing order.";

    "is-zero", (all any @-> any),
    "(is-zero X Y ... Z) returns one if all numbers are zero.";

    "not", (all any @-> any),
    "(not X Y ... Z) returns one if all numbers are not \
     true. Equivalent to (is-zero X Y Z)";

    "is-positive", (all any @-> any),
    "(is-zero X Y ... Z) returns one if all numbers are positive.";

    "is-negative", (all any @-> any),
    "(is-zero X Y ... Z) returns one if all numbers are negative.";

    "word-width", (all any @-> any),
    "(word-width X Y ... Z) returns the maximum width of its \
     arguments. If no arguments provided returns the size of the \
     machine word.";

    "exec-addr", (one int @-> any),
    "(exec-addr ADDR) transfers control flow to ADDR.";

    "memory-read", (one int @-> byte),
    "(memory-read PTR) loads one byte from the address PTR";

    "memory-write", (tuple [int; byte] @-> int),
    "(memory-write PTR X) stores X at PTR."
  ]

type KB.conflict += Illformed of string

let illformed fmt =
  Format.kasprintf (fun msg ->
      KB.fail (Illformed msg)) fmt

let nothing = KB.Value.empty Theory.Semantics.cls


let size = Theory.Bitv.size
let forget x = x >>| Theory.Value.forget
let empty s = Theory.Value.(forget @@ empty s)
let fresh = KB.Object.create Theory.Program.cls
let bits x = size @@ Theory.Value.sort x

module Primitives(CT : Theory.Core) = struct

  let rec seq = function
    | [] -> CT.perform Theory.Effect.Sort.bot
    | [x] -> x
    | x :: xs -> CT.seq x @@ seq xs

  let undefined = seq []
  let pass = seq []
  let skip = seq []

  let negone s =
    CT.int s @@ Bitvec.(ones mod modulus (size s))


  let unary = function
    | [x] -> !!x
    | _ -> illformed "requires exactly one argument"

  let binary xs f = match xs with
    | [x; y] -> f x y
    | _ -> illformed "requires exactly two arguments"

  let const x = KB.Value.get Primus.Lisp.Semantics.static x
  let set_const v x =
    KB.Value.put Primus.Lisp.Semantics.static v (Some x)
  let const_int s x = CT.int s x >>| fun v -> set_const v x
  let int s x = const_int s @@ Bitvec.(int x mod modulus (size s))
  let true_ = CT.b1 >>| fun v -> set_const v Bitvec.one
  let false_ = CT.b0 >>| fun v -> set_const v Bitvec.zero
  let const_bool x = if x then true_ else false_

  let bitv x =
    match Theory.Value.resort Theory.Bitv.refine x with
    | Some x -> !!x
    | None -> match Theory.Value.resort Theory.Bool.refine x with
      | None -> illformed "defined for bits or bools"
      | Some b ->
        let s = Theory.Bitv.define 1 in
        let b1 = const_int s Bitvec.M1.one
        and b0 = const_int s Bitvec.M1.zero in
        match const b with
        | Some r ->
          if Bitvec.(equal r zero)
          then b0
          else b1
        | None -> CT.ite !!b b1 b0


  let nbitv = KB.List.map ~f:bitv

  type 'a bitv = 'a Theory.Bitv.t Theory.Value.sort

  let monoid s sf df init xs =
    nbitv xs >>= function
    | [] -> forget@@const_int s init
    | x :: xs ->
      KB.List.fold ~init:x xs ~f:(fun res x ->
          match const res, const x with
          | Some res, Some x ->
            const_int s@@sf res x
          | _ ->
            CT.cast s CT.b0 !!x >>= fun x ->
            df !!res !!x) |>
      forget

  let is_one x = CT.(inv@@is_zero x)

  let (&&&) x y = match const x, const y with
    | Some x, Some y ->
      if Bitvec.(equal x zero || equal y zero)
      then false_
      else true_
    | _ -> CT.and_ !!x !!y

  let rec is_ordered sf df = function
    | [] | [_] -> true_
    | x :: (y :: _ as rest) ->
      bitv x >>= fun x ->
      bitv y >>= fun y ->
      match const x, const y with
      | Some x, Some y ->
        const_bool@@sf x y >>= fun r ->
        is_ordered sf df rest >>= fun r' ->
        r &&& r'
      | _ ->
        df !!x !!y >>= fun r ->
        is_ordered sf df rest >>= fun r' ->
        CT.and_ !!r !!r'

  let order sf df xs = forget@@is_ordered sf df xs

  let all sf df xs =
    true_ >>= fun init ->
    KB.List.fold ~init xs ~f:(fun r x ->
        bitv x >>= fun x ->
        let r' = match const x with
          | Some x -> const_bool (sf x)
          | None -> df !!x in
        r' >>= fun r' ->
        r &&& r') |>
    forget


  let full eff res =
    res >>= fun res ->
    eff >>| fun eff ->
    KB.Value.put Theory.Semantics.value eff res

  let pure res = full (seq []) res

  let ctrl eff =
    let* lbl = fresh in
    CT.blk lbl (seq []) eff

  let data eff =
    let* lbl = fresh in
    CT.blk lbl eff (seq [])

  let memory eff res =
    let* lbl = fresh in
    full CT.(blk lbl (perform eff) skip) res

  let loads = memory Theory.Effect.Sort.rmem
  let stores = memory Theory.Effect.Sort.wmem
  let loads = pure

  let is_negative x = CT.msb x
  let is_positive x =
    CT.(and_ (non_zero x) (inv (is_negative x)))

  let word_width s xs =
    nbitv xs >>= fun xs ->
    List.max_elt xs ~compare:(fun x y ->
        Int.compare (bits x) (bits y)) |>
    Option.value_map ~f:(fun x ->
        int s (bits x))
      ~default:(int s (size s)) |>
    forget

  let exec_addr xs =
    unary xs >>= bitv >>= fun dst ->
    CT.jmp !!dst

  let memory_read t xs =
    unary xs >>= bitv >>= fun src ->
    CT.(load (var (Theory.Target.data t)) !!src) |>
    forget

  let memory_write t xs =
    binary xs @@ fun dst data ->
    bitv dst >>= fun dst ->
    bitv data >>= fun data ->
    let mem = Theory.Target.data t in
    let (:=) = CT.set in
    CT.(mem := store (var mem) !!dst !!data)


  let rec prefix p = function
    | [] -> []
    | x::xs -> (p,x) :: prefix p xs

  let rec combinations = function
    | [] -> []
    | x :: xs -> prefix x xs @ combinations xs


  let distinct_pair (x,y) =
    bitv x >>= fun x ->
    bitv y >>= fun y ->
    match const x, const y with
    | Some x, Some y -> const_bool Bitvec.(x <> y)
    | _ -> CT.neq !!x !!y

  let distinct = function
    | [] | [_] -> true_
    | xs ->
      true_ >>= fun init ->
      KB.List.fold (combinations xs) ~init ~f:(fun t p ->
          distinct_pair p >>= fun t' ->
          t &&& t')

  let neg x =
    bitv x >>= fun x -> forget@@CT.neg !!x

  let one_op_x op x =
    bitv x >>= fun x ->
    let s = Theory.Value.sort x in
    forget@@CT.(op (int s Bitvec.one) !!x)

  let reciprocal = one_op_x CT.div
  let sreciprocal = one_op_x CT.sdiv

  let dispatch lbl name args =
    Theory.Label.target lbl >>= fun t ->
    let bits = Theory.Target.bits t in
    let module Z = struct
      include Bitvec.Make(struct
          let modulus = Bitvec.modulus bits
        end)
      let is_zero = Bitvec.equal zero
      let is_negative = msb
      let is_positive x =
        not (is_negative x) && not (is_zero x)
    end in
    let s = Theory.Bitv.define bits in
    match name,args with
    | "+",_-> pure@@monoid s Z.add CT.add Z.zero args
    | "-",[x]|"neg",[x] -> pure@@neg x
    | "-",_-> pure@@monoid s Z.sub CT.sub Z.zero args
    | "*",_-> pure@@monoid s Z.mul CT.mul Z.one args
    | "/",[x]-> pure@@reciprocal x
    | "/",_-> pure@@monoid s Z.div CT.div Z.one args
    | "s/",[x]-> pure@@sreciprocal x
    | "s/",_-> pure@@monoid s Z.sdiv CT.sdiv Z.one args
    | "mod",_-> pure@@monoid s Z.rem CT.modulo Z.one args
    | "signed-mod",_-> pure@@monoid s Z.srem CT.smodulo Z.one args
    | "lshift",_-> pure@@monoid s Z.lshift CT.lshift Z.one args
    | "rshift",_-> pure@@monoid s Z.rshift CT.rshift Z.one args
    | "arshift",_-> pure@@monoid s Z.arshift CT.arshift Z.one args
    | "logand",_-> pure@@monoid s Z.logand CT.logand Z.ones args
    | "logor",_-> pure@@monoid s Z.logor CT.logor Z.zero args
    | "logxor",_-> pure@@monoid s Z.logxor CT.logxor Z.zero args
    | "=",_-> pure@@order Bitvec.(=) CT.eq args
    | "<",_-> pure@@order Bitvec.(<) CT.ult args
    | ">",_-> pure@@order Bitvec.(>) CT.ugt args
    | "<=",_-> pure@@order Bitvec.(<=) CT.ule args
    | ">=",_-> pure@@order Bitvec.(>=) CT.uge args
    | "/=",_| "distinct",_-> pure@@forget@@distinct args
    | "is-zero",_| "not",_-> pure@@all Bitvec.(equal zero) CT.is_zero args
    | "is-positive",_-> pure@@all Z.is_positive is_positive args
    | "is-negative",_-> pure@@all Z.is_negative is_negative args
    | "word-width",_-> pure@@word_width s args
    | "exec-addr",_-> ctrl@@exec_addr args
    | "memory-read",_-> pure@@memory_read t args
    | "memory-write",_-> data@@memory_write t args
    | _ -> !!nothing
end

module Sema = Primus.Lisp.Semantics

let provide () =
  KB.Rule.(begin
      declare "primus-lisp-core-primitives" |>
      require Sema.primitive   |>
      provide Theory.Semantics.slot |>
      comment "implements semantics for the core primitives"
    end);
  List.iter export ~f:(fun (name,types,docs) ->
      Primus.Lisp.Semantics.Primitive.declare ~types ~docs name);
  KB.promise Theory.Semantics.slot @@ fun obj ->
  KB.collect Sema.primitive obj >>= function
  | None -> !!nothing
  | Some p ->
    Theory.instance () >>= Theory.require >>= fun (module CT) ->
    let module P = Primitives(CT) in
    let name = Sema.Primitive.name p
    and args = Sema.Primitive.args p in
    P.dispatch obj name args
