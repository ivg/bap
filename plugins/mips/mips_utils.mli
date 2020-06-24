open Bap.Std

val mips_fail : ('a, unit, string, 'b) format4 -> 'a
(** [mips_fail error_string] - raise a failure with [error_string] *)
