open Core_kernel
open Bap.Std
open Bap_primus.Std

module Words : sig
  type t = String.Set.t

  include Value.S with type t := t

  val to_string : t -> string
end

type words = Words.t

type t

val caught : (t * words) Primus.observation

val detected : t Primus.observation
(** made when a prey is detected  *)

val finished : t Primus.statement
(** a statement that a prey was detected  *)

val catch : (t * words) Primus.statement

val create : tid seq -> string -> t
(** [create terms chars] the result of beagle hunting stating that
    during the execution of a sequence of [terms] we observed [chars]
    in that specific order.  *)

val data : t -> string
(** [data prey] is a sequence of chars, that was caught  *)

val terms : t -> tid seq
(** [terms prey] a sequence of terms that, when executed, consumed
    or produced one of the [data prey] characters. *)

(** Attributes that are added by beagle analysis.  *)

val chars : words tag
(** Each string in a set is a sequence of characters
    that were detected by Beagle on each emulation (it is possible,
    that beagle will sniff the same term more than once).
    The characters are specified in an order in which they were
    observed.*)

val strings : words tag
(** a set of static strings that we directly or indirectly referenced
    the emulation of a term.*)

val words : words tag
(** a set of words that can be built from a specified alphabet with
    the observed characters. *)

val statics : string Addr.Map.t tag
(** statically discovered strings  *)
