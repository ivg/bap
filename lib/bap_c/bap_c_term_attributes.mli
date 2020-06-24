open Bap.Std
(** BIR attributes.  *)

open Bap_c_type

val data : Bap_c_data.t tag
(** Abstraction of a data representation of C value. This
    attribute is attached to each inserted arg term, but can be
    further propagated by other passes  *)

val proto : proto tag
(** Function prototype. This attribute is inserted into each
    annotated function. *)

val t : t tag
(** A c type associated with a term. This attribute is attached to
    each inserted arg term, but maybe propagated by further by other
    passes. *)
