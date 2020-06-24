open Bap.Std

val ppc_fail : ('a, unit, string, 'b) format4 -> 'a
(** [ppc_fail error_string] - raise a failure with [error_string] *)
