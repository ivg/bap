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
  Ok ()
