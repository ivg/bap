open Core_kernel
(** Name demangling.

    This library provides an interface for creating and registering
    demanglers, that can be used on demand by fronends and plugins.

*)

module Std : sig
  type demangler

  (** Demangler is a named string transformation.  *)
  module Demangler : sig
    type t = demangler

    val create : string -> (string -> string) -> t
    (** [create name demangler]  *)

    val run : t -> string -> string
    (** [run demangler name] demangle given [name] *)

    val name : t -> string
    (** [name demangler] returns a [demangler]'s name.  *)
  end

  (** Registry of demanglers.  *)
  module Demanglers : sig
    val register : demangler -> unit
    (** [register demangler] register new demangler.  *)

    val available : unit -> demangler list
    (** [available ()] lists currently registered demanglers.  *)
  end
end
