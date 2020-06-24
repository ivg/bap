open Bap.Std
open Regular.Std

type t = Arm_types.insn [@@deriving bin_io, compare, sexp]
(** insn opcode.

    In contradicition with BAP insn, the ARM one is just an opcode,
    without operands. (Possibly, opcode would be a much better
    name).  *)

val create : insn -> t option
(** [create insn] translate from BAP [insn]  *)

val of_basic : ('a, 'b) Disasm_expert.Basic.insn -> t option
(** [of_basic insn] translate from BAP basic [insn]  *)

include Regular.S with type t := t
