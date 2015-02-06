open Bap_types.Std
open Bap_disasm_x86_types

(** register widths *)
val r1    : Type.t
val r4    : Type.t
val r8    : Type.t
val r16   : Type.t
val r32   : Type.t
val r64   : Type.t
val r128  : Type.t
val r256  : Type.t

(** new multi-mode variable *)
type multimodereg = { v32: var; v64: var }

val nmv: string -> Type.t -> string -> Type.t -> multimodereg
val mvs: multimodereg -> var list

(** registers *)
val rbp : multimodereg
val rsp : multimodereg
val rsi : multimodereg
val rdi : multimodereg
val rip : multimodereg
val rax : multimodereg
val rbx : multimodereg
val rcx : multimodereg
val rdx : multimodereg
val rflags : multimodereg

(** condition flag bits *)
val cf : var
val pf : var
val af : var
val zf : var
val sf : var
val oF : var
val df : var

(** segment registers let bases *)
val fs_base : multimodereg
val gs_base : multimodereg

val cs : var
val ds : var
val es : var
val fs : var
val gs : var
val ss : var

val gdt : multimodereg
val ldt : multimodereg

val fpu_ctrl : var
val mxcsr    : var

(** r8 -> r15 *)
val nums: multimodereg array
val ymms: var array

val regs_x86   : var list
val regs_x86_64: var list

val o_rax : operand
val o_rcx : operand
val o_rdx : operand
val o_rbx : operand
val o_rsp : operand
val o_rbp : operand
(* val o_rsi : operand *)
(* val o_rdi : operand *)

(* val o_es : operand *)
(* val o_cs : operand *)
(* val o_ss : operand *)
(* val o_ds : operand *)
val o_fs : operand
val o_gs : operand

val mem : multimodereg
