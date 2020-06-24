open Core_kernel
open Regular.Std
open Bap.Std
open Arm_types

type t = cond [@@deriving bin_io, compare, sexp]

val create : word -> cond Or_error.t
(** decodes condition value from a word  *)

include Regular.S with type t := t
