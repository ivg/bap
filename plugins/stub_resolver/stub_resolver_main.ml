open Core_kernel
open Bap.Std
open Bap_core_theory
open Bap_knowledge
open Bap_future.Std
include Self ()

open Bap_main
open KB.Syntax

let relink prog links =
  (object
    inherit Term.mapper

    method! map_jmp jmp =
      match Jmp.alt jmp with
      | None -> jmp
      | Some alt -> match Jmp.resolve alt with
        | Second _ -> jmp
        | First tid -> match Map.find links tid with
          | Some tid' ->
            Jmp.with_alt jmp (Some (Jmp.resolved tid'))
          | _ -> jmp
  end)#run prog


module Plt : sig
  type t
  val create : Ogre.doc -> t
  val mem : t -> Bitvec.t -> bool
end = struct
  open Image.Scheme
  open Ogre.Syntax
  type t = {lower : Bitvec.t; upper : Bitvec.t}


  let empty = {lower = Bitvec.zero; upper = Bitvec.zero}

  let width = Ogre.(require Image.Scheme.bits >>| Int64.to_int_trunc)

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

let mark_plt_as_stub () : unit =
  KB.Rule.(declare ~package:"bap" "stub-resolver" |>
           dynamic ["code"] |>
           require Theory.Label.addr |>
           provide (Value.Tag.slot Sub.stub) |>
           comment "Locates .plt entries in the project code
                        sections and tags them as stubs.");
  Project.Info.(Stream.(observe @@ zip file spec)) @@ fun (file,spec) ->
  let plts = Plt.create spec in
  KB.promise (Value.Tag.slot Sub.stub) @@ fun label ->
  KB.collect Theory.Label.unit label >>=? fun unit ->
  KB.collect Theory.Unit.path unit >>=? fun path ->
  KB.collect Theory.Label.addr label >>|? fun addr ->
  Option.some_if (path = file && Plt.mem plts addr) ()



let detect_ppc_plt () : unit =
  (* these are the common plt entries starting instructions,
   * that we have seen so far. If better method for identifying
   * PLT entries exist, please tell us! *)
  let signatures = [
    0x3d601001l; (* lis     r11,4097 *)
    0x3d601002l; (* lis     r11,4098 *)
    0x3d601003l; (* lis     r11,4099 *)
    0x3d601004l; (* lis     r11,4100 *)
  ] |> List.map ~f:Word.of_int32 in
  let word_of_mem mem =
    let pos_ref = ref (Memory.min_addr mem) in
    ok_exn @@ Memory.Input.int32 mem ~pos_ref in
  let matches mem =
    Memory.length mem = 4 &&
    List.exists signatures ~f:(Word.equal (word_of_mem mem)) in
  Project.Info.(Stream.(observe @@ zip file arch)) @@ fun (file,arch) ->
  if arch = `ppc then
    KB.promise (Value.Tag.slot Sub.stub) @@ fun label ->
    KB.collect Theory.Label.unit label >>=? fun unit ->
    KB.collect Theory.Unit.path unit >>=? fun path ->
    KB.collect Memory.slot label >>|? fun mem ->
    Option.some_if (path = file && matches mem) ()

let update prog = relink prog (Stub_resolver.run prog)

let main proj =
  Project.with_program proj (update @@ Project.program proj)

let () = Extension.documentation {|
  # DESCRIPTION

  Provides an abi pass that transforms a program by substituting calls
  to stubs with calls to real subroutines when they are present in
  the binary.

|}

let () = Extension.declare @@ fun _ctxt ->
  Bap_abi.register_pass main;
  mark_plt_as_stub ();
  detect_ppc_plt ();
  Ok ()
