open Core_kernel.Std
open Bap_types.Std

open Bap_disasm_x86_types

(* condition flag bits *)
let cf = Var.create "R_CF" bool_t
let pf = Var.create "R_PF" bool_t
let af = Var.create "R_AF" bool_t
let zf = Var.create "R_ZF" bool_t
let sf = Var.create "R_SF" bool_t
let oF = Var.create "R_OF" bool_t
let df = Var.create "R_DF" bool_t

let cs = Var.create "R_CS" reg16_t
let ds = Var.create "R_DS" reg16_t
let es = Var.create "R_ES" reg16_t
let fs = Var.create "R_FS" reg16_t
let gs = Var.create "R_GS" reg16_t
let ss = Var.create "R_SS" reg16_t

let fpu_ctrl = Var.create "R_FPU_CONTROL" reg16_t
let mxcsr = Var.create "R_MXCSR" reg32_t

let ymms = Array.init 16 ~f:(fun i -> Var.create (Printf.sprintf "R_YMM%d" i) reg256_t)

(* floating point registers *)

let o_rax = Oreg 0
let o_rcx = Oreg 1
let o_rdx = Oreg 2
let o_rbx = Oreg 3
let o_rsp = Oreg 4
let o_rbp = Oreg 5
let o_rsi = Oreg 6
let o_rdi = Oreg 7

let o_es = Oseg 0
let o_cs = Oseg 1
let o_ss = Oseg 2
let o_ds = Oseg 3
let o_fs = Oseg 4
let o_gs = Oseg 5

(* prefix names *)
let pref_lock = 0xf0
let repnz = 0xf2
let repz = 0xf3
let hint_bnt = 0x2e
let hint_bt = 0x3e
let pref_cs = 0x2e
let pref_ss = 0x36
let pref_ds = 0x3e
let pref_es = 0x26
let pref_fs = 0x64
let pref_gs = 0x65
let pref_opsize = 0x66
let pref_addrsize = 0x67

(* Prefixes that we can usually handle automatically *)
let standard_prefs = [pref_opsize; pref_addrsize; hint_bnt; hint_bt; pref_cs; pref_ss; pref_ds; pref_es; pref_fs; pref_gs]

module type ModeVars = sig
  (** registers *)
  val rbp : var
  val rsp : var
  val rsi : var
  val rdi : var
  val rip : var
  val rax : var
  val rbx : var
  val rcx : var
  val rdx : var
  val rflags  : var

  val gdt : var
  val ldt : var

  (** segment registers let bases *)
  val fs_base : var
  val gs_base : var

  val seg_ss : var option
  val seg_es : var option
  val seg_cs : var option
  val seg_ds : var option
  val seg_fs : var option
  val seg_gs : var option

  val mem : var

  (* r8 -> r15 *)
  val nums : var array
end

module R32 = struct

  (** registers *)
  let rbp = Var.create "R_EBP_32" reg32_t
  let rsp = Var.create "R_ESP_32" reg32_t
  let rsi = Var.create "R_ESI_32" reg32_t
  let rdi = Var.create "R_EDI_32" reg32_t
  let rip = Var.create "R_EIP" reg32_t   (* XXX why is eip here? *)
  let rax = Var.create "R_EAX_32" reg32_t
  let rbx = Var.create "R_EBX_32" reg32_t
  let rcx = Var.create "R_ECX_32" reg32_t
  let rdx = Var.create "R_EDX_32" reg32_t
  let rflags = Var.create "R_EFLAGS" reg32_t  (* XXX why is eflags here? *)

  let gdt = Var.create "R_GDTR" reg32_t
  let ldt = Var.create "R_LDTR" reg32_t

  (** segment registers let bases *)
  let fs_base = Var.create "R_FS_BASE_32" reg32_t
  let gs_base = Var.create "R_GS_BASE_32" reg32_t

  let seg_ss = None
  let seg_es = None
  let seg_cs = None
  let seg_ds = None
  let seg_fs = Some fs_base
  let seg_gs = Some gs_base

  let mem = Var.create "mem32" (Type.mem `r32 `r8)

  (* r8 -> r15 *)
  let nums = Array.init 8 ~f:(fun i -> Var.create "ERROR" (Type.imm 0))
end

module R64 = struct

  (** registers *)
  let rbp = Var.create "R_RBP" reg64_t
  let rsp = Var.create "R_RSP" reg64_t
  let rsi = Var.create "R_RSI" reg64_t
  let rdi = Var.create "R_RDI" reg64_t
  let rip = Var.create "R_RIP" reg64_t
  let rax = Var.create "R_RAX" reg64_t
  let rbx = Var.create "R_RBX" reg64_t
  let rcx = Var.create "R_RCX" reg64_t
  let rdx = Var.create "R_RDX" reg64_t
  let rflags = Var.create "R_RFLAGS" reg64_t
  let gdt = Var.create "R_GDTR" reg64_t
  let ldt = Var.create "R_LDTR" reg64_t

  (** segment registers let bases *)
  let fs_base = Var.create "R_FS_BASE_64" reg64_t
  let gs_base = Var.create "R_GS_BASE_64" reg64_t

  let seg_ss = None
  let seg_es = None
  let seg_cs = None
  let seg_ds = None
  let seg_fs = Some fs_base
  let seg_gs = Some gs_base

  let mem = Var.create "mem64" (Type.mem `r64 `r8)

  (* r8 -> r15 *)
  let nums = Array.init 8 ~f:(fun i -> Var.create (Printf.sprintf "R_R%d" (i+8)) reg64_t)
end

let vars_of_mode mode =
  let module R =
    (val (match mode with
         | X86   -> (module R32)
         | X8664 -> (module R64)) : ModeVars) in
  (module R : ModeVars)
