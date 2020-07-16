open Core_kernel
open Bap.Std

module Scheme = struct
  open Ogre.Type
  include Image.Scheme

  let flag = "flag" %: bool

  (** pure symbol's value, without interpretation *)
  let value = "value" %: int

  let rel_addr = "relative-addr" %: int

  (** symbol file offset  *)
  let sym_off = "sym-off" %: int

  (** relocation file offset  *)
  let rel_off = "rel-off" %: int

  let file_type () =
    Ogre.declare ~name:"llvm:file-type" (scheme name) ident

  (** entry point  *)
  let entry () =
    Ogre.declare ~name:"llvm:entry" (scheme rel_addr) ident

  let default_base_address () =
    Ogre.declare ~name:"llvm:default-base-address" (scheme addr) ident

  (** reference to internal - symbol offset, relocation offset. *)
  let ref_internal () =
    Ogre.declare ~name:"llvm:ref-internal" (scheme sym_off $ rel_off) Tuple.T2.create

  (** reference to external - relocation offset, name *)
  let ref_external () =
    Ogre.declare ~name:"llvm:ref-external" (scheme rel_off $ name) Tuple.T2.create

  (** file is relocatable *)
  let is_relocatable () = Ogre.declare ~name:"llvm:relocatable" (scheme flag) ident

  let section_entry () =
    Ogre.declare ~name:"llvm:section-entry" (scheme name $ rel_addr $ size $ off)
      (fun name addr size off -> name, addr, size, off)

  (** named entry that contains code *)
  let code_entry () =
    Ogre.declare ~name:"llvm:code-entry" (scheme name $ off $ size) Tuple.T3.create

  (** symbol *)
  let symbol_entry () =
    Ogre.declare ~name:"llvm:symbol-entry"
      (scheme name $ rel_addr $ size $ off $ value)
      (fun name addr size off value -> name, addr, size, off, value)

end

module type S = sig
  type 'a m
  val segments : unit m
  val sections : unit m
  val symbols  : unit m
  val code_regions : unit m
end

module type Rules = sig
  module Make (F : Ogre.S) : S with type 'a m := 'a F.t
end

module type Loader_target = sig
  include Rules
  module Relocatable : Rules
end
