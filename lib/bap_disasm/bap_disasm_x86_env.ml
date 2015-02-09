open Core_kernel.Std
open Bap_types.Std

open Bap_disasm_x86_types

(* register widths *)
let r4 = Type.imm 4
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
let rbp = nmv "R_EBP_32" reg32_t "R_RBP" reg64_t
let rsp = nmv "R_ESP_32" reg32_t "R_RSP" reg64_t
let rsi = nmv "R_ESI_32" reg32_t "R_RSI" reg64_t
let rdi = nmv "R_EDI_32" reg32_t "R_RDI" reg64_t
let rip = nmv "R_EIP" reg32_t "R_RIP" reg64_t (* XXX why is eip here? *)
let rax = nmv "R_EAX_32" reg32_t "R_RAX" reg64_t
let rbx = nmv "R_EBX_32" reg32_t "R_RBX" reg64_t
let rcx = nmv "R_ECX_32" reg32_t "R_RCX" reg64_t
let rdx = nmv "R_EDX_32" reg32_t "R_RDX" reg64_t
let rflags = nmv "R_EFLAGS" reg32_t "R_RFLAGS" reg64_t (* XXX why is eflags here? *)

(* condition flag bits *)
let cf = Var.create "R_CF" bool_t
let pf = Var.create "R_PF" bool_t
let af = Var.create "R_AF" bool_t
let zf = Var.create "R_ZF" bool_t
let sf = Var.create "R_SF" bool_t
let oF = Var.create "R_OF" bool_t
let df = Var.create "R_DF" bool_t

(* segment registers let bases *)
let fs_base = nmv "R_FS_BASE_32" reg32_t "R_FS_BASE_64" reg64_t
let gs_base = nmv "R_GS_BASE_32" reg32_t "R_GS_BASE_64" reg64_t

let cs = Var.create "R_CS" reg16_t
let ds = Var.create "R_DS" reg16_t
let es = Var.create "R_ES" reg16_t
let fs = Var.create "R_FS" reg16_t
let gs = Var.create "R_GS" reg16_t
let ss = Var.create "R_SS" reg16_t

let gdt = nmv "R_GDTR" reg32_t "R_GDTR" reg64_t
let ldt = nmv "R_LDTR" reg32_t "R_LDTR" reg64_t

let fpu_ctrl = Var.create "R_FPU_CONTROL" reg16_t
let mxcsr = Var.create "R_MXCSR" reg32_t

(* r8 -> r15 *)
let nums = Array.init 8 ~f:(fun i -> nmv "ERROR" (Type.imm 0) (Printf.sprintf "R_R%d" (i+8)) reg64_t)

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
