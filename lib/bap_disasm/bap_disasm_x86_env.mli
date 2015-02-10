open Bap_types.Std
open Bap_disasm_x86_types

(** new multi-mode variable *)
type multimodereg

val gv: mode -> multimodereg -> var

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

val o_rax : operand
val o_rcx : operand
val o_rdx : operand
val o_rbx : operand
val o_rsp : operand
val o_rbp : operand
val o_fs  : operand
val o_gs  : operand

val mem : multimodereg

(** prefix names *)
val pref_lock : int
val repnz     : int
val repz      : int
val hint_bnt  : int
val hint_bt : int
val pref_cs : int
val pref_ss : int
val pref_ds : int
val pref_es : int
val pref_fs : int
val pref_gs : int
val pref_opsize : int
val pref_addrsize : int

(** Prefixes that we can usually handle automatically *)
val standard_prefs : int list

val seg_ss : multimodereg option
val seg_es : multimodereg option
val seg_cs : multimodereg option
val seg_ds : multimodereg option
val seg_fs : multimodereg option
val seg_gs : multimodereg option
