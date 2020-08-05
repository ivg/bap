open Bap_knowledge
open Core_kernel
open Bap.Std
open Bap_future.Std
open Monads.Std
open Bap_core_theory

include Self ()

let width = Ogre.(require Image.Scheme.bits >>| Int64.to_int_trunc)

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
  val lookup : t -> Bitvec.t -> string option
  val search : t -> Word.t -> string option
end = struct
  open Image.Scheme
  open Ogre.Syntax
  type t = string list Map.M(Bitvec_order).t

  let empty = Map.empty (module Bitvec_order)

  let collect =
    width >>| Bitvec.modulus >>= fun m ->
    Ogre.collect Ogre.Query.(select (from external_reference)) >>|
    Seq.fold ~init:empty ~f:(fun exts (addr, name) ->
        Map.add_multi exts Bitvec.(int64 addr mod m) name)

  let create doc = match Ogre.eval collect doc with
    | Ok exts -> exts
    | Error err ->
      warning "Failed to obtain external references: %a" Error.pp err;
      empty

  let lookup exts addr = match Map.find exts addr with
    | Some [name] -> Some name
    | _ -> None
  let search exts addr = lookup exts (Word.to_bitvec addr)
end


let agent = Knowledge.Agent.register
    ~package:"bap" "rel-symbolizer"
    ~desc:"extracts symbols from external relocations"

let resolve_plt_entries refs plt path =
  let open KB.Syntax in
  KB.propose agent Theory.Label.possible_name @@ fun label ->
  KB.collect Theory.Label.addr label >>=? fun addr ->
  KB.collect Theory.Label.unit label >>=? fun unit ->
  KB.collect Theory.Unit.path unit >>=? fun file ->
  if file <> path || not (Plt.mem plt addr) then KB.return None
  else
    KB.collect Theory.Program.Semantics.slot label >>| fun insn ->
    match Insn.bil insn with
    | [Bil.Jmp (Load (_,Int dst,_,_))] ->
      References.search refs dst
    | _ -> None

let init () =
  Stream.observe (Stream.zip Project.Info.spec Project.Info.file)
  @@ fun (spec, file) ->
  let refs = References.create spec in
  let syms = Symbolizer.create (References.search refs) in
  let plts = Plt.create spec in
  resolve_plt_entries refs plts file;
  Symbolizer.provide agent (Symbolizer.set_path syms file)

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
