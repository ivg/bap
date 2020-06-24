open Bap.Std

val lift : lifter
(** [lift mem insn] lifts instruction.  *)

module CPU : sig
  include module type of Arm_env

  include Bap.Std.CPU
end
