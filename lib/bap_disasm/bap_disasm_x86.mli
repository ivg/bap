open Core_kernel.Std
open Bap_types.Std

open Bap_disasm_x86_types

val parse_instr: mode -> (Z.t -> Char.t) -> Z.t -> int list * prefix * opcode * Z.t

val parse_prefixes: mode -> int list -> opcode -> var option * int List.t
