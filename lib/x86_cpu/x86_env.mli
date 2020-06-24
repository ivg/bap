open Bap.Std
open X86_types

(** {2 condition flag bits} *)

val cf : var
(** carry flag  *)

val pf : var
(** parity flag  *)

val af : var
(** adjust flag  *)

val zf : var
(** zero flag  *)

val sf : var
(** sign flag  *)

val oF : var
(** overflow flag  *)

val df : var
(** direction flag  *)

val cs : var
(** code segment  *)

val ds : var
(** data segment  *)

val es : var
(** extra data segment #1 *)

val fs : var
(** extra data segment #2 *)

val gs : var
(** extra data segment #3  *)

val ss : var
(** stack segment  *)

val fpu_ctrl : var
(** fpu control register  *)

val mxcsr : var
(** mx status control register  *)

val o_rax : operand

val o_rcx : operand

val o_rdx : operand

val o_rbx : operand

val o_rsp : operand

val o_rbp : operand

val o_fs : operand

val o_gs : operand

val pref_lock : int

val repnz : int

val repz : int

val hint_bnt : int

val hint_bt : int

val pref_cs : int

val pref_ss : int

val pref_ds : int

val pref_es : int

val pref_fs : int

val pref_gs : int

val pref_opsize : int

val pref_addrsize : int

val standard_prefs : int list
(** Prefixes that we can usually handle automatically *)

(** CPU BIL variables.

    For simplicity we're using the same names for registers in
    32 and 64 mode. For example, the A register, has a name [rax] on
    both 32-bit and 64-bit processors. However, on the former it is
    32-bit (contrary to the name), and on the latter it is 64-bit.

*)
module type ModeVars = sig
  val rbp : var
  (** base pointer *)

  val rsp : var
  (** stack pointer  *)

  val rsi : var
  (** source index  *)

  val rdi : var
  (** destination index  *)

  val rip : var
  (** instruction pointer  *)

  val rax : var
  (** accumulator register *)

  val rbx : var
  (** base register *)

  val rcx : var
  (** counter register *)

  val rdx : var
  (** data register *)

  val rflags : var
  (** RFLAGS register  *)

  val gdt : var
  (** Global Descriptor Table  *)

  val ldt : var
  (** Local Descriptor Table  *)

  val fs_base : var
  (** segment registers let bases *)

  val gs_base : var

  val seg_ss : var option

  val seg_es : var option

  val seg_cs : var option

  val seg_ds : var option

  val seg_fs : var option

  val seg_gs : var option

  val mem : var
  (** memory  *)

  (* r8 -> r15 *)

  val r : var array
  (** r8-r15 registers.
      Due to a legacy issues r.(0) -> r8, r.(1) -> r8, ... *)

  val nums : var array
    [@@deprecated "[since 2018-01] use `r` instead"]
  (** Legacy version of the `r` array. *)

  val ymms : var array
  (** array of YMM registers  *)
end

module R32 : ModeVars
(** 32-bit mode registers  *)

module R64 : ModeVars
(** 64-bit mode registers  *)

val vars_of_mode : mode -> (module ModeVars)
(** [vars_of_mode mode] creates registers for a [mode]  *)
