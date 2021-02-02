open Core_kernel
open Bap.Std
open Bap_core_theory
open Monads.Std

open Bap_primus_lisp_types

module Program = Bap_primus_lisp_program
module Resolve = Bap_primus_lisp_resolve
module Def = Bap_primus_lisp_def
module Check = Bap_primus_lisp_type.Check
module Key = Bap_primus_lisp_program.Items

module Meta = struct
  module State = struct
    type t = {
      bound : int Map.M(Theory.Var.Top).t;
      stack : (unit Theory.Var.t * unit Theory.Value.t) list;
      arith : (module Bitvec.S);
    }
  end
  include Monad.State.T1(State)(KB)
  include Monad.State.Make(State)(KB)
end

open Meta.Syntax
open Meta.Let

type value = unit Theory.Value.t
type effect = unit Theory.Effect.t

type KB.Conflict.t += Unresolved_definition of Resolve.resolution

let language = Theory.Language.declare ~package:"bap" "primus-lisp"

let program = KB.Class.property Theory.Source.cls "primus-lisp-program" @@
  KB.Domain.flat "lisp-program"
    ~empty:Program.empty
    ~equal:Program.equal
    ~join:(fun x y -> Ok (Program.merge x y))
    ~inspect:(fun p ->
        let r = Format.asprintf "%a" Program.pp p in
        Sexp.Atom r)

let fail s = Meta.lift (KB.fail s)

let lookup prog item name =
  match Resolve.semantics prog item name () with
  | None -> !!None
  | Some (Error problem) ->
    fail (Unresolved_definition problem)
  | Some (Ok (fn,_)) -> !!(Some fn)

let check_arg _ _ = true

module Primitive = struct
  open KB.Syntax
  type t = {
    name : string;
    args : Theory.Value.Top.t list;
  } [@@deriving compare, equal, sexp]
  let name p = p.name
  let args p = p.args

  let slot = KB.Class.property Theory.Program.cls "lisp-primitive" @@
    KB.Domain.optional "lisp-primitive"
      ~equal
      ~inspect:sexp_of_t

  let eval name args =
    KB.Object.scoped Theory.Program.cls @@ fun obj ->
    KB.provide slot obj (Some {name;args}) >>= fun () ->
    KB.collect Theory.Semantics.slot obj
end

let primitive = Primitive.slot

type primitive = Primitive.t

let sort = Theory.Value.sort
let size x = Theory.Bitv.size (sort x)
let lisp_machine =
  Theory.Effect.Sort.(join [data "unrepresented-lisp-machine"] [top])

let forget = Theory.Value.forget
let create eff res =
  KB.Value.put Theory.Semantics.value eff (forget res)


let symbol =
  KB.Class.property Theory.Value.cls "lisp-symbol" @@
  KB.Domain.optional "symbol"
    ~equal:String.equal
    ~inspect:(fun x -> Sexp.Atom x)


let static_slot =
  KB.Class.property Theory.Value.cls "static-value"
    ~package:"bap"
    ~public:true
    ~persistent:(KB.Persistent.of_binable (module struct
                   type t = Bitvec_binprot.t option [@@deriving bin_io]
                 end)) @@
  KB.Domain.optional "bitvec"
    ~equal:Bitvec.equal
    ~inspect:(fun x -> Sexp.Atom (Bitvec.to_string x))

let update_value r f =
  let v = KB.Value.get Theory.Semantics.value r in
  KB.Value.put Theory.Semantics.value r (f v)

let set_static r x = update_value r @@ fun v ->
  KB.Value.put static_slot v (Some x)

let symsort = Bap_primus_value.Index.key_width
let res = KB.Value.get Theory.Semantics.value
let bits = Theory.Bitv.define
let static x =
  KB.Value.get static_slot (res x)

let (!) = KB.(!!)


module Stack = struct
  open Meta.State

  let is_bound v =
    Meta.get () >>| fun {bound} ->
    Map.mem bound (Theory.Var.forget v)

  let lookup v =
    let v = Theory.Var.forget v in
    Meta.get () >>| fun {bound; stack} ->
    if Map.mem bound (Theory.Var.forget v)
    then List.Assoc.find stack v
        ~equal:Theory.Var.Top.equal
    else None


  let push_var v x = function {bound; stack} as s -> {
      s with
      stack = (v,x) :: stack;
      bound = Map.update bound v ~f:(function
          | None -> 1
          | Some n -> n+1)
    }

  let push v x =
    let v = Theory.Var.forget v in
    Meta.update @@ push_var v x

  let rec update xs x z = match xs with
    | [] -> []
    | (x',_) :: xs when Theory.Var.Top.(x' = x) -> (x,z) :: xs
    | xw :: xs -> xw :: update xs x z

  let set v x =
    let v = Theory.Var.forget v
    and x = forget x in
    Meta.update @@ function {stack} as s -> {
        s with stack = update stack v x
      }

  let make_var word {data={exp=n; typ=t}} =
    let s = match t with
      | Any | Name _ -> word
      | Symbol -> symsort
      | Type m -> m in
    Theory.Var.forget@@Theory.Var.define (bits s) n

  let push_frame ws bs =
    Meta.get () >>= fun s ->
    let s,n = List.fold ~init:(s,0) bs ~f:(fun (s,n) (v,x) ->
        push_var (make_var ws v) x s,n+1) in
    Meta.put s >>| fun () ->
    n

  let rec pop_vars n s =
    if n = 0 then s else match s.stack with
      | [] -> failwith "bug: broken stack"
      | (v,_) :: vs -> pop_vars (n-1) {
          s with
          stack = vs;
          bound = Map.change s.bound v ~f:(function
              | Some 1 | None -> None
              | Some n -> Some (n-1))
        }


  let tear_frame size = Meta.update @@ fun s ->
    pop_vars size s
end

module Prelude(CT : Theory.Core) = struct
  let label = Meta.lift (KB.Object.create Theory.Program.cls)

  let rec seq = function
    | [] -> Meta.lift@@CT.perform Theory.Effect.Sort.bot
    | [x] -> x
    | x :: xs ->
      let* xs = seq xs in
      let* x = x in
      Meta.lift@@CT.seq (KB.return x) (KB.return xs)

  let skip = CT.perform Theory.Effect.Sort.bot
  let pass = CT.perform Theory.Effect.Sort.bot
  let empty = Theory.Effect.empty Theory.Effect.Sort.bot

  let pure res =
    res >>| fun res ->
    create empty res

  let bigint x m =
    let s = bits m in
    let m = Bitvec.modulus m in
    let x = Bitvec.(bigint x mod m) in
    pure @@ Meta.lift (CT.int s x) >>| fun r ->
    set_static r x

  let (:=) v x = Meta.lift@@CT.set v x

  let full eff res =
    seq eff >>= fun eff ->
    res >>| fun res ->
    create eff res

  let data xs =
    label >>= fun lbl ->
    let* data = seq xs in
    Meta.lift@@CT.blk lbl !data skip

  let ctrl xs =
    label >>= fun lbl ->
    let* ctrl = seq xs in
    Meta.lift@@CT.blk lbl pass !ctrl

  let blk lbl xs = seq [
      Meta.lift@@CT.blk lbl pass skip;
      seq xs;
    ]

  let cast s x =
    Meta.lift@@CT.cast (bits s) CT.b0 !x

  let nil = !!(Theory.Value.empty Theory.Bool.t)
  let undefined = full [] nil
  let purify eff =
    full [] !!(res eff)

  let unified x y f =
    Theory.Value.Match.(begin
        let|() = both
            Theory.Bitv.refine x
            Theory.Bitv.refine y @@ fun x y ->
          let s = Int.max (size x) (size y) in
          cast s x >>= fun x ->
          cast s y >>= fun y ->
          f x y in
        undefined
      end)


  let sym = function
    | "nil" -> Meta.return@@set_static empty Bitvec.zero
    | name ->
      Meta.lift @@
      KB.Symbol.intern name Theory.Program.cls >>|
      KB.Object.id >>| Int63.to_int64 >>|
      Bitvec.M64.int64 >>|
      set_static empty

  let coerce_bits s x f =
    let open Theory.Value.Match in
    let| () = can Theory.Bitv.refine x @@ fun x ->
      Meta.lift@@CT.cast s CT.b0 !x >>= f in
    let| () = can Theory.Bool.refine x @@ fun cnd ->
      Meta.lift@@CT.ite !cnd
        (CT.int s Bitvec.one)
        (CT.int s Bitvec.zero) >>= fun x ->
      f x in
    undefined

  let coerce_bool x f =
    let open Theory.Value.Match in
    let| () = can Theory.Bool.refine x f in
    let| () = can Theory.Bitv.refine x @@ fun x ->
      Meta.lift@@CT.non_zero !x >>= fun x -> f x in
    undefined




  let reify prog target name =
    let word = Theory.Target.bits target in
    let rec eval : ast -> unit Theory.Effect.t Meta.t = function
      | {data=Int {data={exp=x; typ=Type m}}} -> bigint x m
      | {data=Int {data={exp=x; typ=Any}}} -> bigint x word
      | {data=Var {data={exp=n; typ=Type m}}} -> var n m
      | {data=Var {data={exp=n; typ=Any}}} -> var n word
      | {data=Sym {data=s}} -> sym s
      | {data=Ite (cnd,yes,nay)} -> ite cnd yes nay
      | {data=Let ({data={exp=n; typ=Type t}},x,y)} -> let_ n t x y
      | {data=Let ({data={exp=n; typ=Any}},x,y)} -> let_ n word x y
      | {data=App (Dynamic name,args)} -> app name args
      | {data=Seq xs} -> seq_ xs
      | {data=Set ({data={exp=n; typ=Type t}},x)} -> set_ n t x
      | {data=Set ({data={exp=n; typ=Any}},x)} -> set_ n word x
      | {data=Rep (cnd,body)} -> rep cnd body
      | _ -> undefined
    and var n t =
      let v = Theory.Var.define (bits t) n in
      Stack.lookup v >>= function
      | None -> pure@@Meta.lift@@CT.var v
      | Some v -> pure (!!v)
    and ite cnd yes nay =
      let* cnd = eval cnd in
      match static cnd with
      | Some cnd ->
        if Bitvec.(equal cnd zero)
        then eval nay
        else eval yes
      | None ->
        coerce_bool (res cnd) @@ fun cres ->
        Meta.lift@@Theory.Var.fresh Theory.Bool.t >>= fun c ->
        let* yes = eval yes in
        let* nay = eval nay in
        full [
          !!cnd;
          data [c := !cres];
          Meta.lift@@CT.branch (CT.var c) !yes !nay;
        ] @@
        Meta.lift@@CT.ite (CT.var c) !(res yes) !(res nay)
    and rep cnd body =
      let* r = eval cnd in
      match static r with
      | Some x ->
        if Bitvec.(equal x zero)
        then !!r
        else
          eval body >>= fun _ ->
          rep cnd body
      | None ->
        let* body = eval body in
        let* head = label and* loop = label and* tail = label in
        coerce_bool (res r) @@ fun cres ->
        full [
          blk head [ctrl [Meta.lift@@CT.goto tail]];
          blk loop [!!body];
          blk tail [!!r; ctrl [
              Meta.lift@@CT.branch !cres (CT.goto head) skip
            ]]
        ] !!cres
    and app name xs =
      map xs >>= fun (aeff,xs) ->
      match Resolve.defun check_arg prog Key.func name xs with
      | None ->
        Meta.lift@@Primitive.eval name xs >>= fun peff ->
        full [!!aeff; !!peff] !!(res peff)
      | Some (Ok (fn,bs)) ->
        Stack.push_frame word bs >>= fun size ->
        eval (Def.Func.body fn) >>= fun eff ->
        Stack.tear_frame size >>= fun () ->
        !!eff
      | Some (Error _) ->
        assert false

    and map args =
      seq [] >>= fun eff ->
      Meta.List.fold args ~init:(eff,[]) ~f:(fun (eff,args) arg ->
          let* eff' = eval arg in
          let+ eff = seq [!!eff; !!eff'] in
          (eff,forget (res eff')::args)) >>| fun (eff,args) ->
      eff, List.rev args
    and seq_ xs =
      pure nil >>= fun init ->
      Meta.List.fold ~init xs ~f:(fun eff x  ->
          let* eff' = eval x in
          full [!!eff; !!eff'] !!(res eff'))
    and set_ n t x =
      let* eff = eval x in
      let v = Theory.Var.define (bits t) n in
      Stack.is_bound v >>= function
      | true ->
        Stack.set v (res eff) >>= fun () ->
        purify eff
      | false -> match static eff with
        | Some _ ->
          Stack.push v (res eff) >>= fun () ->
          purify eff
        | None ->
          coerce_bits (bits t) (res eff) @@ fun reff ->
          full [!!eff; data [v := !reff]] !!reff
    and let_ v t x b =
      let* x = eval x in
      let v = Theory.Var.define (bits t) v in
      match static x with
      | Some _ ->
        Stack.push v (res x) >>= fun () ->
        eval b
      | None ->
        coerce_bits (bits t) (res x) @@ fun rx ->
        cast t rx >>= fun rx ->
        let* b = eval b in
        full [
          !!x;
          data [v := !rx];
          !!b;
        ] !!(res b) in
    lookup prog Key.func name >>= function
    | Some fn -> eval (Def.Func.body fn)
    | None -> !!Insn.empty
end

module Unit = struct
  open KB.Syntax
  open KB.Let

  let slot = KB.Class.property Theory.Unit.cls "lisp-unit"
      ~package:"bap"
      ~public:true @@ KB.Domain.optional "unit-name"
      ~inspect:sexp_of_string
      ~equal:equal_string


  let create ?(name="core") target : Theory.Unit.t KB.t =
    let* unit = KB.Symbol.intern ~package:"lisp" name Theory.Unit.cls in
    KB.sequence [
      KB.provide slot unit (Some name);
      KB.provide Theory.Unit.target unit target
    ] >>| fun () ->
    unit

  let is_lisp obj =
    KB.collect slot obj >>| Option.is_some

  let language = language
end


let provide () =
  let open KB.Syntax in
  KB.Rule.(begin
      declare "primus-lisp-semantics" |>
      require Theory.Label.name |>
      require Theory.Label.unit |>
      require Theory.Unit.source |>
      require Theory.Unit.target |>
      require program |>
      provide Theory.Semantics.slot |>
      comment "reifies Primus Lisp definitions"
    end);
  KB.promise Theory.Semantics.slot @@ fun obj ->
  KB.collect Theory.Label.unit obj >>= function
  | None -> !!Insn.empty
  | Some unit ->
    Unit.is_lisp unit >>= function
    | false -> !!Insn.empty
    | true ->
      KB.collect Theory.Label.name obj >>= function
      | None -> !!Insn.empty
      | Some name ->
        KB.collect Theory.Unit.source unit >>= fun src ->
        KB.collect Theory.Unit.target unit >>= fun target ->
        let prog = KB.Value.get program src in
        let bits = Theory.Target.bits target in
        let module Arith = Bitvec.Make(struct
            let modulus = Bitvec.modulus bits
          end) in
        let meta = Meta.State.{
            stack = [];
            bound = Map.empty (module Theory.Var.Top);
            arith = (module Arith);
          } in
        Theory.instance () >>= Theory.require >>= fun (module Core) ->
        let open Prelude(Core) in
        Meta.run (reify prog target name) meta >>| fun (res,_) ->
        res

let enable () = provide ()

let static = static_slot
