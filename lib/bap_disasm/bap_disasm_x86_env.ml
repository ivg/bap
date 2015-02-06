open Core_kernel.Std
open Bap_types.Std

open Bap_disasm_x86_types

(* register widths *)
let r1 = Type.imm 1
let r4 = Type.imm 4
let r8 = Type.imm 8
let r16 = Type.imm 16
let r32 = Type.imm 32
let r64 = Type.imm 64
let r128 = Type.imm 128
let r256 = Type.imm 256
let xmm_t = r128
let ymm_t = r256
let st_t = Type.imm 80

type multimodereg = { v32: var; v64: var }
(* new multi-mode variable *)

let nmv n32 t32 n64 t64 = { v32=Var.create n32 t32; v64=Var.create n64 t64; }
let mvs {v64; v32} = [v64; v32]

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
let cf = Var.create "R_CF" r1
let pf = Var.create "R_PF" r1
let af = Var.create "R_AF" r1
let zf = Var.create "R_ZF" r1
let sf = Var.create "R_SF" r1
let oF = Var.create "R_OF" r1
let df = Var.create "R_DF" r1

(* segment registers let bases *)
let fs_base = nmv "R_FS_BASE_32" r32 "R_FS_BASE_64" r64
let gs_base = nmv "R_GS_BASE_32" r32 "R_GS_BASE_64" r64

let cs = Var.create "R_CS" r16
let ds = Var.create "R_DS" r16
let es = Var.create "R_ES" r16
let fs = Var.create "R_FS" r16
let gs = Var.create "R_GS" r16
let ss = Var.create "R_SS" r16

let gdt = nmv "R_GDTR" r32 "R_GDTR" r64
let ldt = nmv "R_LDTR" r32 "R_LDTR" r64

let fpu_ctrl = Var.create "R_FPU_CONTROL" r16
let mxcsr = Var.create "R_MXCSR" r32

(* r8 -> r15 *)
let nums = Array.init 8 ~f:(fun i -> nmv "ERROR" (Type.imm 0) (Printf.sprintf "R_R%d" (i+8)) r64)

(*
let xmms = Array.init 8 ~f:(fun i -> Var.new_var (Printf.sprintf "R_XMM%d" i) xmm_t)
*)

let ymms = Array.init 16 ~f:(fun i -> Var.create (Printf.sprintf "R_YMM%d" i) ymm_t)

(* floating point registers *)
let st = Array.init 8 ~f:(fun i -> Var.create (Printf.sprintf "R_ST%d" i) st_t)

let shared_regs =
  [cf; pf; af; zf; sf; oF; df; cs; ds; es; fs; gs; ss; fpu_ctrl; mxcsr]
  @ Array.to_list st

let shared_multi_regs =
  [rbp; rsp; rsi; rdi; rip; rax; rbx; rcx; rdx; rflags; fs_base;
  gs_base]

let regs_x86 : var list =
  shared_regs
  @ List.map ~f:(fun {v32; _} -> v32) shared_multi_regs
  (*@ List.map ~f:(fun {v64; v32} -> v32) shared_multi_regs*)
  @ Array.to_list (Array.sub ymms ~pos:0 ~len:8)

let (r_8, r_9, r_10, r_11, r_12, r_13, r_14, r_15) = match Array.to_list nums with
  | (r_8::r_9::r_10::r_11::r_12::r_13::r_14::r_15::[]) -> (r_8, r_9, r_10, r_11, r_12, r_13, r_14, r_15)
  | _ -> failwith "Impossible, matching against a list of known size"

let regs_x86_64 : var list =
  shared_regs
  @ List.map ~f:(fun {v64; _} -> v64) (shared_multi_regs @ (Array.to_list nums))
  @ Array.to_list ymms

let regs_full : var list =
  shared_regs
  @ List.concat (List.map ~f:(fun {v64; v32} -> [v32; v64]) shared_multi_regs)
  @ List.map ~f:(fun {v64; _} -> v64) (Array.to_list nums)
  @ Array.to_list ymms

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

(* let esp_e = Var esp *)
(* and ebp_e = Var ebp *)
(* and esi_e = Var esi *)
(* and edi_e = Var edi *)
(* and ecx_e = Var ecx *)
(* and eax_e = Var eax *)
(* and edx_e = Var edx *)

let mem = nmv "mem32" (Type.mem `r32 `r8) "mem64" (Type.mem `r64 `r8)

(* 32-bit registers *)
module R32 = struct
  let eip = rip.v32
  let eax = rax.v32
  let ecx = rcx.v32
  let edx = rdx.v32
  let ebx = rbx.v32
  let esp = rsp.v32
  let ebp = rbp.v32
  let esi = rsi.v32
  let edi = rdi.v32
  let mem = mem.v32
end

(* 64-bit registers *)
module R64 = struct
  let rip = rip.v64
  let rax = rax.v64
  let rcx = rcx.v64
  let rdx = rdx.v64
  let rbx = rbx.v64
  let rsp = rsp.v64
  let rbp = rbp.v64
  let rsi = rsi.v64
  let rdi = rdi.v64
  let mem = mem.v64
  let r8 = r_8.v64
  let r9 = r_9.v64
  let r10 = r_10.v64
  let r11 = r_11.v64
  let r12 = r_12.v64
  let r13 = r_13.v64
  let r14 = r_14.v64
  let r15 = r_15.v64
end
