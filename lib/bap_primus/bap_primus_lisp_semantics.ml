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
  type state = {
    bound : Set.M(Theory.Var.Top).t;
    stack : (unit Theory.Var.t * Bitvec.t) list;
    arith : (module Bitvec.S);
  }
  module State = struct type t = state end
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

let lookup prog item name = match Resolve.semantics prog item name () with
  | None -> !!None
  | Some (Error problem) ->
    fail (Unresolved_definition problem)
  | Some (Ok (fn,_)) -> !!(Some fn)

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

let res = KB.Value.get Theory.Semantics.value

let symbol =
  KB.Class.property Theory.Value.cls "lisp-symbol" @@
  KB.Domain.optional "symbol"
    ~equal:String.equal
    ~inspect:(fun x -> Sexp.Atom x)

let static =
  KB.Class.property Theory.Value.cls "static-value" @@
  KB.Domain.optional "bitvec"
    ~equal:Bitvec.equal
    ~inspect:(fun x -> Sexp.Atom (Bitvec.to_string x))

let (!) = KB.(!!)

module Prelude(CT : Theory.Core) = struct
  let bits = Theory.Bitv.define
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

  let pure res =
    res >>| fun res ->
    create (Theory.Effect.empty Theory.Effect.Sort.bot) res

  let bigint x m =
    let s = bits m in
    let m = Bitvec.modulus m in
    let x = Bitvec.(bigint x mod m) in
    Meta.lift (CT.int s x)

  let zero = bigint Z.zero 1

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

  let undefined = full [] zero

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


  let nil = pure @@ zero

  let var n m =
    Meta.lift@@CT.var@@Theory.Var.define (bits m) n

  let symsort = Bap_primus_value.Index.key_width


  let sym name =
    Meta.return @@
    KB.Value.put symbol (Theory.Value.empty (bits symsort)) (Some name)


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
      | {data=Int {data={exp=x; typ=Type m}}} -> pure@@bigint x m
      | {data=Int {data={exp=x; typ=Any}}} -> pure@@bigint x word
      | {data=Var {data={exp=n; typ=Type m}}} -> pure@@var n m
      | {data=Var {data={exp=n; typ=Any}}} -> pure@@var n word
      | {data=Sym {data=s}} -> pure@@sym s
      | {data=Ite (cnd,yes,nay)} -> ite cnd yes nay
      | {data=Let ({data={exp=n; typ=Type t}},x,y)} -> let_ n t x y
      | {data=Let ({data={exp=n; typ=Any}},x,y)} -> let_ n word x y
      | {data=App (Dynamic name,args)} -> app name args
      | {data=Seq xs} -> seq_ xs
      | {data=Set ({data={exp=n; typ=Type t}},x)} -> set_ n t x
      | {data=Set ({data={exp=n; typ=Any}},x)} -> set_ n word x
      | {data=Rep (cnd,body)} -> rep cnd body
      | _ -> undefined
    and ite cnd yes nay =
      let* cnd = eval cnd in
      let* yes = eval yes in
      let* nay = eval nay in
      coerce_bool (res cnd) @@ fun cres ->
      Meta.lift@@Theory.Var.fresh Theory.Bool.t >>= fun c ->
      full [
        !!cnd;
        data [c := !cres];
        Meta.lift@@CT.branch (CT.var c) !yes !nay;
      ] @@
      Meta.lift@@CT.ite (CT.var c) !(res yes) !(res nay)
    and rep cnd body =
      let* cnd = eval cnd in
      let* body = eval body in
      let* head = label and* loop = label and* tail = label in
      coerce_bool (res cnd) @@ fun cres ->
      full [
        blk head [ctrl [Meta.lift@@CT.goto tail]];
        blk loop [!!body];
        blk tail [!!cnd; ctrl [
            Meta.lift@@CT.branch !cres (CT.goto head) skip
          ]]
      ] !!cres
    and app name xs =
      map xs >>= fun (aeff,xs) ->
      let xs = List.map ~f:forget xs in
      Meta.lift@@Primitive.eval name xs >>= fun peff ->
      if KB.Domain.is_empty Theory.Semantics.domain peff
      then
        let* dst = Meta.lift@@Theory.Label.for_name name in
        let* eff = args name xs in
        full [
          !!aeff;
          !!eff;
          ctrl [Meta.lift@@CT.goto dst]
        ] !!(res eff)
      else
        full [!!aeff; !!peff] !!(res peff)
    and map args =
      seq [] >>= fun eff ->
      Meta.List.fold args ~init:(eff,[]) ~f:(fun (eff,args) arg ->
          let* eff' = eval arg in
          let+ eff = seq [!!eff; !!eff'] in
          (eff,forget (res eff')::args)) >>| fun (eff,args) ->
      eff, List.rev args
    and seq_ xs =
      nil >>= fun init ->
      Meta.List.fold ~init xs ~f:(fun eff x  ->
          let* eff' = eval x in
          full [!!eff; !!eff'] !!(res eff'))
    and set_ n t x =
      let* eff = eval x in
      let v = Theory.Var.define (bits t) n in
      coerce_bits (bits t) (res eff) @@ fun reff ->
      full [!!eff; data [v := !reff]] !!reff
    and let_ v t x b =
      let* x = eval x in
      let* b = eval b in
      let v = Theory.Var.define (bits t) v in
      coerce_bits (bits t) (res x) @@ fun rx ->
      cast t rx >>= fun rx ->
      full [
        !!x;
        data [v := !rx];
        !!b;
      ] !!(res b)
    and args name xs =
      sym name >>= fun x ->
      Meta.lift@@Primitive.eval "invoke-subroutine" (forget x::xs) in
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
        let meta = Meta.{
            stack = [];
            bound = Set.empty (module Theory.Var.Top);
            arith = (module Arith);
          } in
        Theory.instance () >>= Theory.require >>= fun (module Core) ->
        let open Prelude(Core) in
        Meta.run (reify prog target name) meta >>| fun (res,_) ->
        res

let enable () = provide ()
