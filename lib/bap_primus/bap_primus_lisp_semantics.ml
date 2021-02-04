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
      binds : unit Theory.Value.t Map.M(Theory.Var.Top).t;
      arith : (module Bitvec.S);
      scope : unit Theory.var list Map.M(Theory.Var.Top).t;
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

let find_def prog item name =
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

let empty = Theory.Effect.empty Theory.Effect.Sort.bot

let sym str =
  let v = update_value empty @@ fun v ->
    KB.Value.put symbol v (Some str) in
  match str with
  | "nil" -> Meta.return@@set_static v Bitvec.zero
  | name ->
    Meta.lift @@
    KB.Symbol.intern name Theory.Value.cls >>|
    KB.Object.id >>| Int63.to_int64 >>|
    Bitvec.M64.int64 >>|
    set_static v

let is_machine_var t v =
  Set.mem (Theory.Target.vars t) (Theory.Var.forget v)

let machine_var_by_name t name =
  Set.find (Theory.Target.vars t) ~f:(fun v ->
      String.equal (Theory.Var.name v) name)

let make_var ?t:constr target name  =
  let word = Theory.Target.bits target in
  match machine_var_by_name target name with
  | Some v -> v
  | None ->
    let t = Option.value constr ~default:word in
    Theory.Var.forget@@Theory.Var.define (bits t) name

let lookup_parameter prog v =
  let name = Theory.Var.name v in
  Program.get prog Key.para |>
  List.find ~f:(fun p ->
      String.equal (Def.name p) name) |> function
  | None -> None
  | Some p -> Some (Def.Para.default p)

module Env = struct
  open Meta.State

  let lookup v =
    let v = Theory.Var.forget v in
    Meta.get () >>| fun {binds} ->
    Map.find binds v


  let set v x =
    Meta.update @@ fun s -> {
      s with binds = Map.set s.binds
                 (Theory.Var.forget v) x
    }

  let del v =
    Meta.update @@ fun s -> {
      s with binds = Map.remove s.binds (Theory.Var.forget v)
    }

  let is_bound v = lookup v >>| Option.is_some

  let var word {data={exp=n; typ=t}} =
    let s = match t with
      | Any | Name _ -> word
      | Symbol -> symsort
      | Type m -> m in
    Theory.Var.forget@@Theory.Var.define (bits s) n

  let set_args ws bs = Meta.update @@ fun s -> {
      s with binds = List.fold bs ~init:s.binds ~f:(fun s (v,x) ->
      Map.set s (var ws v) x)
    }

  let del_args ws bs = Meta.update @@ fun s -> {
      s with binds = List.fold bs ~init:s.binds ~f:(fun s (v,_) ->
      Map.remove s (var ws v))
    }
end

module Scope = struct
  let forget = Theory.Var.forget

  let push orig =
    let orig = forget orig in
    Meta.lift@@Theory.Var.fresh (Theory.Var.sort orig) >>= fun v ->
    Meta.update  (fun s -> {
          s with scope = Map.update s.scope orig ~f:(function
        | None -> [v]
        | Some vs -> v::vs)
        }) >>| fun () ->
    v

  let lookup orig =
    let+ {scope} = Meta.get () in
    match Map.find scope orig with
    | None | Some [] -> None
    | Some (x :: _) -> Some x

  let pop orig =
    Meta.update @@ fun s -> {
      s with scope = Map.change s.scope (forget orig) ~f:(function
        | None | Some [] | Some [_] -> None
        | Some (_::xs) -> Some xs)
    }

  let clear =
    let* s = Meta.get () in
    let+ () = Meta.put {
        s with scope = Map.empty (module Theory.Var.Top)
      } in
    s.scope

  let restore scope = Meta.update @@ fun s -> {
      s with scope
    }


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

  let is_static eff = Option.is_some (static eff)

  let assign ?(local=false) target v eff =
    let v = Theory.Var.forget v in
    match static eff with
    | Some _ when local || not (is_machine_var target v) ->
      Env.set v (res eff) >>= fun () ->
      purify eff
    | _ ->
      Env.del v >>= fun () ->
      full [!!eff; data [v := !(res eff)]] !!(res eff)

  let reify prog target name =
    let word = Theory.Target.bits target in
    let var ?t n = make_var ?t target n in
    let rec eval : ast -> unit Theory.Effect.t Meta.t = function
      | {data=Int {data={exp=x; typ=Type m}}} -> bigint x m
      | {data=Int {data={exp=x}}} -> bigint x word
      | {data=Var {data={exp=n; typ=Type t}}} -> lookup@@var ~t n
      | {data=Var {data={exp=n}}} -> lookup@@var n
      | {data=Sym {data=s}} -> sym s
      | {data=Ite (cnd,yes,nay)} -> ite cnd yes nay
      | {data=Let ({data={exp=n; typ=Type t}},x,y)} -> let_ ~t n x y
      | {data=Let ({data={exp=n}},x,y)} -> let_ n x y
      | {data=App (Dynamic name,args)} -> app name args
      | {data=Seq xs} -> seq_ xs
      | {data=Set ({data={exp=n; typ=Type t}},x)} -> set_ (var ~t n) x
      | {data=Set ({data={exp=n}},x)} -> set_ (var n) x
      | {data=Rep (cnd,body)} -> rep cnd body
      | _ -> undefined
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
        Env.set_args word bs >>= fun () ->
        Scope.clear >>= fun scope ->
        eval (Def.Func.body fn) >>= fun eff ->
        Scope.restore scope >>= fun () ->
        Env.del_args word bs >>= fun () ->
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
    and lookup v =
      Scope.lookup v >>= function
      | Some v -> lookup v
      | None ->
        Env.lookup v >>= function
        | Some v -> pure !!v
        | None -> match lookup_parameter prog v with
          | None -> pure@@Meta.lift@@CT.var v
          | Some def ->
            let* eff = eval def in
            assign target v eff
    and set_ v x =
      let* eff = eval x in
      Scope.lookup v >>= function
      | Some v ->
        assign target ~local:true v eff
      | None ->
        assign target v eff
    and let_ ?(t=word) v x b =
      let* xeff = eval x in
      let orig = Theory.Var.define (bits t) v in
      Scope.push orig >>= fun v ->
      let* aeff = assign ~local:true target v xeff in
      let* beff = eval b in
      Scope.pop orig >>= fun () ->
      full [
        !!aeff;
        !!beff;
      ] !!(res beff) in
    find_def prog Key.func name >>= function
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
            binds = Map.empty (module Theory.Var.Top);
            scope = Map.empty (module Theory.Var.Top);
            arith = (module Arith);
          } in
        Theory.instance () >>= Theory.require >>= fun (module Core) ->
        let open Prelude(Core) in
        Meta.run (reify prog target name) meta >>| fun (res,_) ->
        res

let enable () = provide ()

let static = static_slot
