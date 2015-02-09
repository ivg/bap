open Core_kernel.Std
open Bap_types.Std

open Bap_disasm_x86_types

(* register widths *)
let r4 = Type.imm 4
let r32 = Type.imm 32
let r64 = Type.imm 64
let r128 = Type.imm 128
let r256 = Type.imm 256
let xmm_t = r128
let ymm_t = r256
let st_t = Type.imm 80

type multimodereg = { v32: var; v64: var }

let gv mode { v32; v64 } = match mode with
  | X86 -> v32
  | X8664 -> v64

(* new multi-mode variable *)
let nmv n32 t32 n64 t64 = { v32=Var.create n32 t32; v64=Var.create n64 t64; }

(* registers *)
let rbp = nmv "R_EBP_32" r32 "R_RBP" r64
let rsp = nmv "R_ESP_32" r32 "R_RSP" r64
let rsi = nmv "R_ESI_32" r32 "R_RSI" r64
let rdi = nmv "R_EDI_32" r32 "R_RDI" r64
let rip = nmv "R_EIP" r32 "R_RIP" r64 (* XXX why is eip here? *)
let rax = nmv "R_EAX_32" r32 "R_RAX" r64
let rbx = nmv "R_EBX_32" r32 "R_RBX" r64
let rcx = nmv "R_ECX_32" r32 "R_RCX" r64
let rdx = nmv "R_EDX_32" r32 "R_RDX" r64
let rflags = nmv "R_EFLAGS" r32 "R_RFLAGS" r64 (* XXX why is eflags here? *)

(* condition flag bits *)
let cf = Var.create "R_CF" bool_t
let pf = Var.create "R_PF" bool_t
let af = Var.create "R_AF" bool_t
let zf = Var.create "R_ZF" bool_t
let sf = Var.create "R_SF" bool_t
let oF = Var.create "R_OF" bool_t
let df = Var.create "R_DF" bool_t

(* segment registers let bases *)
let fs_base = nmv "R_FS_BASE_32" r32 "R_FS_BASE_64" r64
let gs_base = nmv "R_GS_BASE_32" r32 "R_GS_BASE_64" r64

let cs = Var.create "R_CS" reg16_t
let ds = Var.create "R_DS" reg16_t
let es = Var.create "R_ES" reg16_t
let fs = Var.create "R_FS" reg16_t
let gs = Var.create "R_GS" reg16_t
let ss = Var.create "R_SS" reg16_t

let gdt = nmv "R_GDTR" r32 "R_GDTR" r64
let ldt = nmv "R_LDTR" r32 "R_LDTR" r64

let fpu_ctrl = Var.create "R_FPU_CONTROL" reg16_t
let mxcsr = Var.create "R_MXCSR" r32

(* r8 -> r15 *)
let nums = Array.init 8 ~f:(fun i -> nmv "ERROR" (Type.imm 0) (Printf.sprintf "R_R%d" (i+8)) r64)

let ymms = Array.init 16 ~f:(fun i -> Var.create (Printf.sprintf "R_YMM%d" i) ymm_t)

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

let mem = nmv "mem32" (Type.mem `r32 `r8) "mem64" (Type.mem `r64 `r8)

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
