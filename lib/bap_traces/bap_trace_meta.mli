open Bap.Std
open Bap_trace_meta_types

(** Common trace meta attributes.

    This file contains common meta attributes. Real traces may or may not
    contain them. They may also contain attributes that are not
    specified in this module.
*)

val tracer : tracer tag
(** description of a tracer that was used to create the trace  *)

val binary : binary tag
(** description of a target binary (executable) that was traced.*)

val arch : arch tag
(** description of binary architecture. *)

val binary_file_stats : file_stats tag
(** file stats of the traced binary  *)

val trace_stats : trace_stats tag
