(** LLVM object file *)

open Core_kernel.Std

type t

external create : Bigstring.t -> t Or_error.t =
  "object_file_create_stub"

type archType =
  | UnknownArch
  | Arm      (* ARM: arm, armv.*, xscale *)
  | Aarch64  (* AArch64: aarch64 *)
  | Hexagon  (* Hexagon: hexagon *)
  | Mips     (* MIPS: mips, mipsallegrex *)
  | Mipsel   (* MIPSEL: mipsel, mipsallegrexel *)
  | Mips64   (* MIPS64: mips64 *)
  | Mips64el (* MIPS64EL: mips64el *)
  | Msp430   (* MSP430: msp430 *)
  | Ppc      (* PPC: powerpc *)
  | Ppc64    (* PPC64: powerpc64, ppu *)
  | Ppc64le  (* PPC64LE: powerpc64le *)
  | R600     (* R600: AMD GPUs HD2XXX - HD6XXX *)
  | Sparc    (* Sparc: sparc *)
  | Sparcv9  (* Sparcv9: Sparcv9 *)
  | Systemz  (* SystemZ: s390x *)
  | Tce      (* TCE (http:tce.cs.tut.fi/): tce *)
  | Thumb    (* Thumb: thumb, thumbv.* *)
  | X86      (* X86: i[3-9]86 *)
  | X86_64   (* X86-64: amd64, x86_64 *)
  | Xcore    (* XCore: xcore *)
  | Nvptx    (* NVPTX: 32-bit *)
  | Nvptx64  (* NVPTX: 64-bit *)
  | Le32     (* le32: generic little-endian 32-bit CPU (PNaCl / Emscripten) *)
  | Amdil    (* amdil: amd IL *)
  | Spir     (* SPIR: standard portable IR for OpenCL 32-bit version *)
  | Spir64   (* SPIR: standard portable IR for OpenCL 64-bit
                version *)

external get_arch : t -> archType =
  "object_file_get_arch_stub"

external symbols : t -> SymbolRef.t list =
  "object_file_symbols_stub"

external dynamic_symbols : t -> SymbolRef.t list =
  "object_file_dynamic_symbols_stub"

external sections : t -> SectionRef.t list =
  "object_file_sections_stub"
