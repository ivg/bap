open Bap.Std
(** Attribute processing.

    This module allows to attach a semantic action for C attributes.
    Each action is a term transformation, that should do nothing, if
    an attribute is not known to him.
*)

open Bap_c_type

type 'a pass = attr -> 'a term -> 'a term
(** a type of action   *)

val register : sub pass -> unit
(** register an action  *)

val apply : sub pass
(** apply all registered actions *)
