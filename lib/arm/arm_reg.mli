open Bap.Std
open Regular.Std

type t = Arm_types.reg [@@deriving bin_io, compare, sexp]

val create : reg -> t option
(** lifts basic register to a ARM one  *)

include Regular.S with type t := t
