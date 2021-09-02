open Core_kernel
open Bap_knowledge

module KB = Knowledge

open KB.Syntax
open KB.Let

module Target = Bap_core_theory_target
module Program = Bap_core_theory_program
module Effect = Bap_core_theory_effect
module Origin = Target.Origin
module Var = Bap_core_theory_var
module Val = Bap_core_theory_value
module type Core = Bap_core_theory_definition.Core

module type trans = functor (_ : Core) -> Core

type pass = (module trans)

let package = "core"

let passes = Hashtbl.create (module KB.Name)
let info = Hashtbl.create (module KB.Name)

let no_such_pass name =
  invalid_argf "Unknown core theory pass %s"
    (KB.Name.show name) ()

let name_is_taken name =
  invalid_argf "The name %s is already taken, please
    select a unique name" (KB.Name.show name) ()

let register ?desc ?package name pass =
  let name = KB.Name.create ?package name in
  if Hashtbl.mem passes name then name_is_taken name;
  Option.iter desc ~f:(fun desc -> Hashtbl.add_exn info name desc);
  Hashtbl.add_exn passes name pass


let lookup name = match Hashtbl.find passes name with
  | None -> no_such_pass name
  | Some p -> p

let compose : pass -> pass -> pass =
  fun (module T1) (module T2) ->
  let module T(X : Core) = T2(T1(X)) in
  (module T)

let apply names : (module Core) -> (module Core) =
  fun (module CT) ->
  match List.map names ~f:lookup with
  | [] -> (module CT)
  | ps ->
    let (module T) = List.reduce_balanced_exn ps ~f:compose in
    (module T(CT))

module Scope = struct
  let vars = KB.Context.declare ~package "desugar-scope-vars"
      !!(Map.empty (module Var.Ident))

  let update slot obj f =
    KB.collect slot obj >>| f >>=
    KB.provide slot obj

  let push var =
    KB.Context.update vars @@ fun vars ->
    Map.update vars (Var.ident var) ~f:(function
        | None -> 1
        | Some n -> n + 1)

  let pop var =
    KB.Context.update vars @@ fun vars ->
    Map.change vars (Var.ident var) ~f:(function
        | None | Some 1 -> None
        | Some n -> Some (n-1))

  let mem var =
    KB.Context.get vars >>| fun vars ->
    Map.mem vars (Var.ident var)
end


module Desugar(CT : Core) : Core = struct

  module Delta = struct
    let target =
      KB.Object.scoped Program.cls Program.Label.target

    let pass = Effect.empty Effect.Sort.bot

    let assign_sub dst src off =
      src >>= fun src ->
      let s = Var.sort dst in
      let dst_len = Val.Bitv.size s
      and src_len = Val.Bitv.size @@ Val.sort src in
      let src = CT.unsigned s !!src in
      let open Bitvec.Make(struct
          let modulus = Bitvec.modulus dst_len
        end) in
      let mask =
        lnot ((one lsl int src_len - one) lsl int off) in
      let x = CT.(logand (var dst) (int s mask)) in
      let off = int off in
      let y = if Bitvec.equal off zero
        then src
        else CT.(lshift src (int s off)) in
      CT.(set dst (logor x y))


    let pos x =
      let module Pos = Bitvec.M32 in
      CT.int (Val.Bitv.define 32) (Pos.int x)

    let assign_regs lhs rhs =
      rhs >>= fun rhs ->
      let total = Val.(Bitv.size (sort rhs)) in
      fst@@List.fold lhs ~init:(!!pass,total+1) ~f:(fun (data,hi) lhs ->
          let s = Var.sort lhs in
          let bits = Val.Bitv.size s in
          let lo = hi - bits in
          let rhs = CT.extract s (pos hi) (pos lo) !!rhs in
          CT.(seq data (set lhs rhs),hi - bits))


    (* module Alias ensures that only bitvec registers
       are involved in aliasing *)
    let cast_var v =
      match Val.Bitv.refine @@ Val.Sort.forget @@ Var.sort v with
      | None -> assert false
      | Some s -> Var.resort v s
    and cast_val v = v >>| fun v ->
      match Val.resort Val.Bitv.refine (Val.forget v) with
      | None -> assert false
      | Some v -> v

    let set v x =
      Scope.mem v >>= function
      | true -> CT.set v x
      | false ->
        let* t = target in
        if Target.has_roles t [Target.Role.Register.constant] v
        then !!pass
        else match Target.unalias t v with
          | None -> CT.set v x
          | Some origin ->
            let x = cast_val x in
            match Origin.cast_sub origin with
            | Some s when Origin.is_alias s ->
              CT.set (Origin.reg s) x
            | Some s -> assign_sub (Origin.reg s) x (Origin.lo s)
            | None -> match Origin.cast_sup origin with
              | None -> !!pass
              | Some s -> assign_regs (Origin.regs s) x

    let var r =
      Scope.mem r >>= function
      | true -> CT.var r
      | false ->
        let* t = target in
        let s = Var.sort r in
        let ret x = x >>| fun x -> KB.Value.refine x s in
        if Target.has_roles t [Target.Role.Register.zero] r
        then match Val.Bitv.refine (Val.Sort.forget s) with
          | None -> CT.unk s
          | Some s' ->
            ret@@CT.int s' Bitvec.zero
        else
          match Target.unalias t r with
          | None -> CT.var r
          | Some origin ->
            match Origin.cast_sub origin with
            | Some sub when Origin.is_alias sub ->
              CT.var (Var.resort (Origin.reg sub) s)
            | Some sub ->
              let hi = Origin.hi sub and lo = Origin.lo sub in
              let bs = Val.Bitv.define (hi-lo+1) in
              ret @@
              CT.extract bs (pos hi) (pos lo) (CT.var (Origin.reg sub))
            | None -> match Origin.cast_sup origin with
              | None -> CT.unk s
              | Some sup ->
                let regs = Origin.regs sup in
                let total = List.sum (module Int) regs ~f:(fun r ->
                    Val.Bitv.size (Var.sort r)) in
                let bs = Val.Bitv.define total in
                ret @@ CT.concat bs (List.map regs ~f:CT.var)

    let let_ v x y =
      x >>= fun x ->
      Scope.push v >>= fun () ->
      y >>= fun y ->
      Scope.pop v >>= fun () ->
      CT.let_ v !!x !!y

  end

  include CT
  include Delta
end

(*
   Notes and ideas:
   1) in repeat, branch, and ite we can push
      path-constraints to the paths
   2) We may opt to use `seq` to chain instructions
      in the sema lifter
   3) (f x (f y ...(f z))) => (f x y .. z) is called flattening,
      thus flat_assoc means that an operation is associative and
      should be flattened


 *)

type arith_props = {
  lass : bool;   (* left-associative:  xyz = (xy)z*)
  rass : bool;   (* right-associative: xyz = x(yz) *)
  comm : bool;   (* commutative:       xy  = yx *)
  idem : bool;   (* idempotent:        xx  = x *)
}
[@@deriving compare, equal, hash]

type bool_props =
  | Chainable  (* (f x y .. z) = (and (f x y) ...(f y z) *)
  | Pairwise   (* (f x y .. z) = (and (f x y) (f x z) ... (f y z)*)



module Op = struct
  module Self = KB.Enum.Make()
  let arith_props = Hashtbl.create (module Self)
  let bool_props = Hashtbl.create (module Self)

  let def = Self.declare ~package

  let (%:) name ps =
    let p = def name in
    Hashtbl.add_exn arith_props p ps;
    p

  let (&:) name ps =
    let p = def name in
    Option.iter ps (fun ps -> Hashtbl.add_exn bool_props p ps);
    p


  let only = {lass=false; rass=false; comm=false; idem=false}
  let lass = true
  let rass = true
  let comm = true
  let idem = true
  let none = only

  let chainable = Some Chainable
  let pairwise = Some Pairwise


  let add = "add" %: {only with lass; rass; comm}
  let sub = "sub" %: {only with lass}
  let mul = "mul" %: {only with lass; rass; comm}
  let div = "div" %: {only with lass}
  let sdiv = "sdiv" %: {only with lass}
  let smul = "smul" %: {only with lass}
  let modulo = "modulo" %: {only with lass}
  let smodulo = "smodulo" %: {only with lass}
  let logand = "logand" %: {lass; rass; comm; idem}
  let logor = "logor" %: {lass; rass; comm; idem}
  let logxor = "logxor" %: {only with lass; rass; comm}
  let shiftr = def "shiftr"
  let shiftl = def "shiftl"
  let sle = "sle" &: chainable
  let ule = "ule" &: chainable
  let cast = def "cast"


  include Self

  let can_flatten op1 op2 op3 =
    op1 = op2 && op2 = op3 &&
    match Hashtbl.find arith_props op1 with
    | Some {lass; rass} -> lass || rass
    | None -> false


end

module Herbrand = struct
  type id = int [@@deriving compare, equal, sexp, bin_io]

  type typ =
    | Any
    | Bool
    | Bitv of int
    | Mem of int * int
    | Gen of Val.Sort.Top.t
  [@@deriving compare, equal, sexp, bin_io]


  type ('op, 'ty) term = {
    id : id;
    ty : 'ty;
    op : 'op;
  } [@@deriving sexp, bin_io]

  let compare_term cmp_op cmp_ty t1 t2 =
    if t1.id <> 0 && t2.id <> 0 then compare_id t1.id t2.id
    else match cmp_ty t1.ty t2.ty with
      | 0 -> cmp_op t1.op t2.op
      | n -> n

  let equal_term equal_op equal_ty t1 t2 =
    t1.id <> 0 && t2.id <> 0 && equal_id t1.id t2.id ||
    equal_ty t1.ty t2.ty && equal_op t1.op t2.op

  module Word = struct
    let compare = Bitvec.compare
    let equal = Bitvec.equal
    include Bitvec_order.Natural
    include Bitvec_sexp.Functions
    include Bitvec_binprot
  end

  module Effects = (val KB.Value.derive Effect.cls)


  type const =
    | W of Word.t
    | B of bool
  [@@deriving compare, equal, sexp, bin_io]

  type pure =
    | Unk
    | Var of Var.Top.t
    | Cst of const
    | Let of Var.Top.t * exp * exp
    | App of Op.t * exp list
  [@@deriving compare, equal, sexp, bin_io]

  and exp = (pure,Val.Sort.Top.t) term
  [@@deriving compare, equal, sexp, bin_io]

  and stmt = (eff,Effects.t) term
  [@@deriving compare, equal, sexp, bin_io]

  and eff =
    | Seq of Program.Label.t option * stmt list
    | Set of Var.Ident.t * exp
    | Jmp of dst
    | Branch of exp * stmt * stmt
    | Repeat of exp * stmt
  [@@deriving compare, equal, sexp, bin_io]

  and dst =
    | Unresolved of Program.Label.t
    | Resolved of exp


  module Pure = struct
    type t = pure [@@deriving bin_io]
    include Base.Comparable.Make(struct
        type t = pure [@@deriving compare, sexp]
      end)
  end

  module Eff = struct
    type t = eff [@@deriving bin_io]
    include Base.Comparable.Make(struct
        type t = eff [@@deriving compare, sexp]
      end)
  end

  module Universe = struct
    let cls = KB.Class.declare "herbrand-universe" ()
        ~package


    let pure_mapping =
      KB.Domain.mapping (module Pure) "pures"
        ~equal:equal_exp

    let eff_mapping =
      KB.Domain.mapping (module Eff) "effs"
        ~equal:equal_stmt

    let expressions =
      KB.Class.property cls "expressions"
        ~package pure_mapping
        ~persistent:(KB.Persistent.of_binable (module struct
                       type t = exp Map.M(Pure).t [@@deriving bin_io]
                     end))

    let statements =
      KB.Class.property cls "effects"
        ~package eff_mapping
        ~persistent:(KB.Persistent.of_binable (module struct
                       type t = stmt Map.M(Eff).t [@@deriving bin_io]
                     end))

    let self = KB.Symbol.intern ~public:true "the-universe" cls
        ~package

    let put slot data =
      let* this = self in
      KB.provide slot this data

    let get slot = self >>= KB.collect slot

    let hashcons slot ty x =
      get slot >>= fun uni ->
      match Map.find uni x with
      | Some r -> !!r
      | None ->
        let r = {
          op = x;
          ty;
          id = Map.length uni + 1
        } in
        put slot (Map.add_exn uni x r) >>| fun () ->
        r

    let mk_exp t = hashcons expressions (Val.Sort.forget t)
    let mk_stmt = hashcons statements
  end

  let mk_exp = Universe.mk_exp
  let mk_stmt = Universe.mk_stmt

  let exp_domain =
    KB.Domain.flat "exp" ~equal:equal_exp ~empty:{
      op = Unk;
      ty = Val.Sort.Top.t;
      id = 0
    }

  let exps = KB.Class.property Val.cls "hexp" exp_domain
      ~package
      ~persistent:(KB.Persistent.of_binable (module struct
                     type t = exp [@@deriving bin_io]
                   end))

  let stmt_domain =
    KB.Domain.flat "stmt" ~equal:equal_stmt ~empty:{
      op = Seq (None,[]);
      ty = Effects.empty;
      id = 0;
    }

  let stmts = KB.Class.property Effect.cls "hstmt" stmt_domain
      ~package
      ~persistent:(KB.Persistent.of_binable (module struct
                     type t = stmt [@@deriving bin_io]
                   end))

  type env = {
    env : exp Map.M(Var.Ident).t;
  }

  let empty_env = {
    env = Map.empty (module Var.Ident);
  }

  let env = KB.Context.declare ~package "env"
      !!empty_env


  let type_of_sort s =
    let s = Val.Sort.forget s in
    match Val.Bool.refine s with
    | Some _ -> Bool
    | None ->
      match Val.Bitv.refine s with
      | Some s -> Bitv (Val.Bitv.size s)
      | None -> match Val.Mem.refine s with
        | None -> Gen s
        | Some s ->
          let (%%) f s = Val.Bitv.size (f s) in
          Mem (Val.Mem.keys%%s, Val.Mem.vals%%s)

  let resolve exp = match exp with
    | {op=Var v}  ->
      KB.Context.get env >>| fun {env} ->
      Option.value (Map.find env (Var.ident v)) ~default:exp
    | x -> !!x


  let (>>->) x f =
    resolve (KB.Value.get exps x) >>= f


  module Eval(CT : Core) = struct
    let binary args f = match args with
      | [x; y] -> f x y
      | _ -> assert false

    let bitv x f = match Val.resort Val.Bitv.refine x with
      | None -> assert false
      | Some x -> f x

    let core_arith f args =
      binary args @@ fun x y ->
      bitv x @@ fun x ->
      bitv y @@ fun y ->
      f !!x !!y >>| Val.forget

    let core_add = core_arith CT.add
    let core_sub = core_arith CT.sub


    let ops = Hashtbl.of_alist_exn (module Op) [
        Op.add, core_add;
        Op.sub, core_sub;
      ]

    let app op args = Hashtbl.find_exn ops op args

    let forget x = x >>| Val.forget

    let int s x =
      match Val.Bitv.refine s with
      | None -> assert false
      | Some s -> forget@@CT.int s x

    let rec exp x = match x.op with
      | Unk -> CT.unk x.ty
      | Var v -> CT.var v
      | Let (v,x,y) -> CT.let_ v (exp x) (exp y)
      | App (op,args) -> KB.List.map ~f:exp args >>= app op
      | Cst (W w) -> int x.ty w
      | Cst (B true) -> forget@@CT.b1
      | Cst (B false) -> forget@@CT.b0

    let exp s x : 'a Val.t knowledge =
      let+ x = exp x in
      KB.Value.refine x s
  end

  module Concrete = struct
    let reduce f size args =
      let m = Bitvec.modulus size in
      let f x y = Bitvec.(f x y mod m) in
      List.reduce_exn ~f args

    let operation_semantics = Hashtbl.of_alist_exn (module Op) [
        Op.add, reduce Bitvec.add;
        Op.sub, reduce Bitvec.sub;
        Op.mul, reduce Bitvec.mul;
        Op.div, reduce Bitvec.div;
      ]

    let semantics = Hashtbl.find_exn operation_semantics
  end

  module Optimization = struct
    (* for a commutatives we can rearrange terms as we like, e.g.,
       x + 1 + 2 + y + 3
       -----------------
       5 + x + y

    *)
    let reduce_comm sort op args =
      let consts,exprs =
        List.partition_map args ~f:(function
            | {op=Cst (W x)} -> First x
            | x -> Second x) in
      match consts with
      | [] -> !!None
      | _ ->
        let size = Val.Bitv.size sort in
        let const = Concrete.semantics op size consts in
        let* const = mk_exp sort (Cst (W const)) in
        mk_exp sort (App (op,const::exprs)) >>| Option.some

    (* we can reduce only consecutive leading constants, e.g.,
       1 - 2 - x - y - 3
       -----------------
       -1 - x - 3

       but we can turn some non-commutative operations to
       commutative, e.g.,

       x - y - z => -(-x + y + z)
                 or  x + -y + -z


       also modular subtraction is addition, e.g.,

       x - y - z
       ================
       ~x + ~y + ~z + 3


    *)
  end

  module Make(CT : Core) : Core = struct
    module Eval = Eval(CT)

    (* the problem is that we're dropping the already
       evaluated terms and every time re-evaluate them,
       which leads to the quadratic complexity of the
       algorithm.

       The question is: how to re-evaluate and reuse
       the existing terms when there was no optimization
       opportunity.

       A cheap alternative, instead of implementing optimizations
       here, we can write an optimizer that uses Z3.

       But for that we need to write Z3 -> Core parser.

    *)

    let ret s exp =
      let* exp = mk_exp s exp in
      let+ x = Eval.exp s exp in
      KB.Value.put exps x exp

    let mk_binary op t x y =
      let s = t (Val.sort x) (Val.sort y) in
      x >>-> fun x ->
      y >>-> fun y ->
      match x,y with
      | {op=App (opx,xs)}, {op=App (opy,ys)}
        when Op.can_flatten op opx opy ->
        ret s (App (op, xs @ ys))
      | _ ->
        ret s (App (op,[x;y]))

    let t_eq x _ = x


    module Delta = struct
      let add x y =
        x >>= fun x ->
        y >>= fun y ->
        mk_binary Op.add t_eq x y
    end
    include CT
    include Delta
  end

end


let () = register "desugar-variables" (module Desugar)
    ~package
    ~desc:"desugars assignments and access to register aliases"
