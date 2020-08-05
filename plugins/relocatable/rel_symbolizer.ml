open Bap_knowledge
open Core_kernel
open Bap.Std
open Bap_future.Std
open Monads.Std
open Bap_core_theory

include Self ()

let width = Ogre.(require Image.Scheme.bits >>| Int64.to_int_trunc)

type ref = Addr of Bitvec.t | Name of string

module Plt : sig
  type t
  val create : Ogre.doc -> t
  val mem : t -> Bitvec.t -> bool
end = struct
  open Image.Scheme
  open Ogre.Syntax
  type t = {lower : Bitvec.t; upper : Bitvec.t}

  let empty = {lower = Bitvec.zero; upper = Bitvec.zero}
  let find =
    width >>= fun width ->
    let module Addr = Bitvec.Make(struct
        let modulus = Bitvec.modulus width
      end) in
    Ogre.request named_region ~that:(fun {info=name} ->
        name = ".plt") >>| function
    | Some {addr; size} -> {
        lower = Addr.int64 addr;
        upper = Addr.(int64 addr + int64 size)
      }
    | None -> empty

  let mem {lower; upper} addr =
    Bitvec.(lower <= addr) &&
    Bitvec.(upper > addr)

  let create doc = match Ogre.eval find doc with
    | Ok plt -> plt
    | Error err ->
      warning "failed to find plt entries: %a" Error.pp err;
      empty
end

module References : sig
  type t
  val create : Ogre.doc -> t
  val lookup : t -> Bitvec.t -> ref option
  val search : t -> Word.t -> ref option
end = struct
  open Image.Scheme
  open Ogre.Syntax
  type t = ref list Map.M(Bitvec_order).t

  let empty = Map.empty (module Bitvec_order)

  let collect init map src =
    width >>| Bitvec.modulus >>= fun m ->
    Ogre.collect Ogre.Query.(select (from src)) >>|
    Seq.fold ~init ~f:(fun exts (addr, value) ->
        Map.add_multi exts Bitvec.(int64 addr mod m) (map m value))

  let name _ x = Name x and addr m x = Addr Bitvec.(int64 x mod m)
  let extract =
    collect empty name external_reference >>= fun names ->
    collect names addr relocation

  let create doc = match Ogre.eval extract doc with
    | Ok exts -> exts
    | Error err ->
      warning "Failed to obtain external references: %a" Error.pp err;
      empty

  let lookup exts addr = match Map.find exts addr with
    | Some [name] -> Some name
    | _ -> None
  let search exts addr = lookup exts (Word.to_bitvec addr)
end

let plt_agent = Knowledge.Agent.register
    ~package:"bap" "plt-symbolizer"
    ~desc:"extracts symbols from external relocations"
    ~reliability:Knowledge.Agent.doubtful

open KB.Syntax

let optimize = Bil.fixpoint @@ Fn.compose
    Bil.propagate_consts
    Bil.fold_consts

let collect_insns number_of_instructions entry =
  let return bils = KB.return @@
    optimize @@ List.(concat @@ rev bils) in
  let rec collect bils addr collected =
    if collected < number_of_instructions then
      Theory.Label.for_addr addr >>= fun label ->
      KB.collect Theory.Program.Semantics.slot label >>= fun insn ->
      KB.collect Memory.slot label >>= function
      | None -> return bils
      | Some mem ->
        let next = Addr.to_bitvec @@ Addr.succ @@ Memory.max_addr mem in
        collect (Insn.bil insn :: bils) next (collected+1)
    else return bils  in
  collect [] entry 0

let plt_size_of_arch : arch -> int option = function
  | #Arch.arm -> Some 4
  | #Arch.x86 -> Some 1
  | #Arch.ppc -> Some 4
  | _ -> None

let plt_size label =
  KB.collect Arch.slot label >>| plt_size_of_arch


let find_reference = List.find_map ~f:(function
    | Bil.Jmp (Load (_,Int dst,_,_))
    | Bil.Jmp (Int dst)
    | Bil.Move (_, Load (_,Int dst,_,_)) -> Some dst
    | _ -> None)

let resolve_plt_entries refs plt path =
  KB.propose plt_agent Theory.Label.possible_name @@ fun label ->
  KB.collect Theory.Label.addr label >>=? fun addr ->
  KB.collect Theory.Label.unit label >>=? fun unit ->
  KB.collect Theory.Unit.path unit >>=? fun file ->
  plt_size label >>=? fun size ->
  if file <> path || not (Plt.mem plt addr) then KB.return None
  else collect_insns size addr >>| fun bil ->
    match find_reference bil with
    | None ->
      info "PLT at %a@\n%a@\n" Bitvec.pp addr Bil.pp bil;
      None
    | Some dst -> match References.search refs dst with
      | Some (Name s) -> Some s
      | _ -> None


let label_for_ref = function
  | Name s -> Theory.Label.for_name s
  | Addr x -> Theory.Label.for_addr x

let addresses mem =
  let start = Memory.min_addr mem in
  let len = Memory.length mem in
  Seq.init len ~f:(Addr.nsucc start)

let matches refs addr mem =
  let addr =
    Option.value_map addr ~default:Seq.empty ~f:Seq.singleton in
  Seq.append addr (addresses mem) |>
  Seq.find_map ~f:(References.search refs)

let provide_dests refs plt path =
  let (>>=?) x f = x >>= function
    | None -> KB.return Insn.empty
    | Some x -> f x in
  KB.promise Theory.Program.Semantics.slot @@ fun label ->
  KB.collect Theory.Label.addr label >>=? fun addr ->
  KB.collect Theory.Label.unit label >>=? fun unit ->
  KB.collect Memory.slot label >>=? fun mem ->
  KB.collect Theory.Unit.path unit >>=? fun file ->
  if file <> path || Plt.mem plt addr then KB.return Insn.empty
  else collect_insns 1 addr >>| find_reference >>= fun addr ->
    match matches refs addr mem with
    | None -> KB.return Insn.empty
    | Some ref ->
      label_for_ref ref >>| fun dst ->
      KB.Value.put Insn.Slot.dests Insn.empty @@
      Some (Set.singleton (module Theory.Label) dst)

let init () =
  Stream.observe (Stream.zip Project.Info.spec Project.Info.file)
  @@ fun (spec, file) ->
  let refs = References.create spec in
  let plts = Plt.create spec in
  resolve_plt_entries refs plts file;
  provide_dests refs plts file

let () =
  Config.manpage [
    `S "DESCRIPTION";
    `P "Extracts symbol information from the program relocations.";

    `P "The relocation symbolizer leverages the relocation information stored in files
        to extract symbol names. Since a relocation references an external symbol which
        doesn't have an address we use an address of a callsite.";
    `S "SEE ALSO";
    `P "$(b,bap-plugin-llvm)(1) code";
  ];
  Config.when_ready (fun _ -> init ())
