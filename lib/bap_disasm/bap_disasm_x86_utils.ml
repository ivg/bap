open Core_kernel.Std
open Bap_types.Std

open Bap_disasm_x86_types
open Bap_disasm_x86_env

(* open Bil *)
module BZ = Big_int_Z
(* open Type *)
(* open Arch *)
(* open Var *)

module BV = Bitvector

module Util = struct
  let id x = x (* useful for tabulating lists, for instance. *)

  (* some of the "imported" zarith-processing functions use this. *)
  (* XXX we don't like mutable state. Bitvector should be written so that the 2 big_int_Z
   * functions that use this can be killed. *)

  let index_ofq elt lst =
    match List.findi lst ~f:(fun _ e -> e = elt) with
    | None -> raise Not_found
    | Some (i,_) -> i

  let concat_explist elist =
    List.reduce_exn
      ~f:Exp.(^) elist
end

module Strip = struct
  let bits_of_width typ =
    let open Type in
    match typ with
    | Imm n -> n
    | Mem _ -> invalid_arg "bits_of_width"
  let bytes_of_width t =
    let b = bits_of_width t in
    if not ((b mod 8) = 0) then invalid_arg "bytes_of_width";
    b / 8
end


let exp_false = Exp.int BV.b0
let exp_true  = Exp.int BV.b1
let exp_not = Exp.lnot

(* XXX this is a lot of crap copied from arithmetic.ml
 * I'm starting to think we should just pull in the old
 * big_int code instead. unless I'm missing something and this stuff
 * is slated to go away? *)
let it n t = BV.of_int n ~width:t |> Exp.int

module Big_int_temp = struct
  (* the 2 with_width functions are versions of functions that
   * already exist that don't throw away existing width information
   * so that we can avoid calling Typecheck.infer_ast *)
  let extract_element_symbolic_with_width t e n et =
    let open Exp in
    let t = Strip.bits_of_width t in
    cast Cast.low t (e lsl (n * (it t et)))
  let extract_byte_symbolic_with_width e n et =
    extract_element_symbolic_with_width (Type.imm 8) e n et
  (* the following functions were used in Big_int_Z stuff
   * and have temporarily been put here, since Bitvector functionality is
   * not up to speed yet. *)
  let extract_element t e n =
    let nbits = t in
    Exp.extract (n*nbits+(nbits-1)) (n*nbits) e
  let extract_byte e n = extract_element 8 e n
  let reverse_bytes e t =
    let bytes = Strip.bytes_of_width t in
    let get_byte n = extract_byte e n in
    List.reduce_exn
      ~f:(fun bige e -> Exp.(bige ^ e))
      (List.map ~f:get_byte (List.init ~f:Util.id bytes))
  let min_symbolic ~signed e1 e2 =
    let open Exp in
    let cond = match signed with
      | true -> e1 <$ e2
      | false -> e1 < e2 in
    ite cond e1 e2
  let max_symbolic ~signed e1 e2 =
    let open Exp in
    let cond = match signed with
      | true -> e1 <$ e2
      | false -> e1 < e2 in
    ite (lnot cond) e1 e2

  let ( <<$ ) = BZ.shift_left_big_int
  let ( +$ ) = BZ.add_big_int
  let bi1 = BZ.big_int_of_int 0x1
  let power_of_two = BZ.shift_left_big_int bi1
  let bitmask = let (-%) = BZ.sub_big_int in
    (fun i -> power_of_two i -% bi1)
  let to_big_int (i,t) =
    let bits = Strip.bits_of_width t in
    BZ.and_big_int i (bitmask bits)
  (* sign extend to type t *)
  let to_sbig_int (i,t) =
    let (>>%) = BZ.shift_right_big_int in
    let (-%) = BZ.sub_big_int in
    let bi_is_zero bi = BZ.(big_int_of_int 0x0 |> eq_big_int bi) in
    let bits = Strip.bits_of_width t in
    let final = to_big_int (i, Type.imm (bits-1)) in
    (* mod always returns a positive number *)
    let sign = i >>% (bits-1) in
    if bi_is_zero sign then
      (* positive *) final
    else (* negative *) BZ.minus_big_int ((power_of_two (bits-1) -% final))
  let to_signed i t = to_sbig_int (i, t)
  let to_val t i =
    (to_big_int (i,t), t)
  let cast ct ((_,t) as v) t2 =
    let bits1 = Strip.bits_of_width t in
    let bits = Strip.bits_of_width t2 in
    let open Exp.Cast in
    match ct with
    | UNSIGNED -> to_val t2 (to_big_int v)
    | SIGNED -> to_val t2 (to_sbig_int v)
    | HIGH -> to_val t2 (BZ.shift_right_big_int (to_big_int v) (bits1-bits))
    | LOW -> to_val t2 (to_big_int v)
end

module BITEMP = Big_int_temp

module Cpu_exceptions = struct
  let general_protection = Stmt.cpuexn 0xd
  let divide_by_zero = Stmt.cpuexn 0x0
end


let compute_segment_bases = ref false

(* Note: In general, the function g is the get memory function.  The
   variable na refers to the next address or next instruction.

   To help understand this file, please refer to the Intel Instruction
   Set Reference. For consistency, any section numbers here are wrt
   Order Number: 253666-035US June 2010 and 253667-035US.


   The x86 instruction format is as follows:
   Instruction Prefixes: 0-4bytes (1 byte per prefix)
   Optional Rex Prefix: 1 byte
   Opcode: 1 - 3 bytes.
   ModR/M: 1 optional byte
   SIB: 1 optional byte
   Displacement: 0,1,2, or 4 bytes.
   Immediate: 0,1,2, or 4 bytes

   ModR/M has the following format:
   7:6 Mod
   5:3 Reg or extra opcode bits
   2:0 R/M

   SIB:
   7:6 Scale
   5:3 Index
   2:0 Base


   In order to get the most common unsupported opcodes, you can run something like:
   for f in bin/*; do BAP_DEBUG_MODULES=AsmirV ~/bap/trunk/utils/iltrans -bin $f ; done 2>&1  >/dev/null  | grep opcode | sed 's/.*opcode: //' | sort | uniq -c | sort -n

   To optimize for number of programs disassembled:
   for f in bin/*; do echo -n "$f "; BAP_DEBUG_MODULES=AsmirV iltrans -bin $f 2>&1  >/dev/null  | grep opcode | sed 's/.*opcode: //' | sort | uniq -c | sort -n  | wc -l; done | sort -n -k 2

*)

(* type segment = CS | SS | DS | ES | FS | GS *)

let type_of_mode = function
  | X86 -> Type.imm 32
  | X8664 -> Type.imm 64

let width_of_mode mode = Strip.bits_of_width (type_of_mode mode)

let sig_to_mask =
  let open Pcmpstr in
  function
  | LSB -> Bitmask
  | MSB -> Bytemask

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

exception Arch_exception of Arch.x86 * string
(** disfailwith is a non-fatal disassembly exception. *)
let disfailwith m s =
  let a = match m with
    | X86   -> `x86
    | X8664 -> `x86_64 in
  raise (Arch_exception (a, s))

let unimplemented a s  = disfailwith a ("disasm_i386: unimplemented feature: "^s)

let gv mode { v32; v64 } = match mode with
  | X86 -> v32
  | X8664 -> v64

let ge mode mv = gv mode mv |> Exp.var

let regs_of_mode = function
  | X86 -> regs_x86
  | X8664 -> regs_x86_64

let cf_e = Exp.var cf
let pf_e = Exp.var pf
let af_e = Exp.var af
let zf_e = Exp.var zf
let sf_e = Exp.var sf
let of_e = Exp.var oF

let df_e = Exp.var df

let seg_cs = None
let seg_ss = None
let seg_ds = None
let seg_es = None
let seg_fs = Some fs_base
let seg_gs = Some gs_base

(* eflags *)
let df_to_offset mode e =
  match type_of_mode mode with
  | Type.Mem _ -> failwith "type_of_mode shouldn't be returning this"
  | Type.Imm t ->
    let open Exp in
    ite (e = exp_false) (it 1 t) (it (-1) t)

let bap_to_rflags =
  let undefined d = Exp.unknown (Printf.sprintf "Undefined RFLAGS bit %d" d) r1 in
  let unmodeled s = Exp.unknown ("Unmodeled RFLAGS bit " ^ s) r1 in
  (List.map ~f:undefined (List.range ~stride:(-1) ~stop:`inclusive 64 32))
  @  undefined 31               (* 31 *)
     :: undefined 30               (* 30 *)
     :: undefined 29               (* 29 *)
     :: undefined 28               (* 28 *)
     :: undefined 27               (* 27 *)
     :: undefined 26               (* 26 *)
     :: undefined 25               (* 25 *)
     :: undefined 24               (* 24 *)
     :: undefined 23               (* 23 *)
     :: undefined 22               (* 22 *)
     :: unmodeled "ID"             (* 21 *)
     :: unmodeled "VIP"            (* 20 *)
     :: unmodeled "VIF"            (* 19 *)
     :: unmodeled "AC"             (* 18 *)
     :: unmodeled "VM"             (* 17 *)
     :: unmodeled "RF"             (* 16 *)
     :: undefined 15               (* 15 *)
     :: unmodeled "NT"             (* 14 *)
     :: unmodeled "IOPL1"          (* 13 *)
     :: unmodeled "IOPL2"          (* 12 *)
     :: of_e                       (* 11 *)
     :: df_e                       (* 10 *)
     :: unmodeled "IF"             (*  9 *)
     :: unmodeled "TF"             (*  8 *)
     :: sf_e                       (*  7 *)
     :: zf_e                       (*  6 *)
     :: undefined 5                (*  5 *)
     :: af_e                       (*  4 *)
     :: undefined 3                (*  3 *)
     :: pf_e                       (*  2 *)
     :: undefined 1                (*  1 *)
     :: cf_e                       (*  0 *)
     :: []
let bap_to_eflags = List.drop bap_to_rflags 32
let bap_to_flags = List.drop bap_to_eflags 16
let bap_to_lflags = List.drop bap_to_flags 16

let rflags_e = List.reduce_exn ~f:Exp.(^) bap_to_rflags
let eflags_e = List.reduce_exn ~f:Exp.(^) bap_to_eflags
let flags_e = List.reduce_exn ~f:Exp.(^) bap_to_flags
let lflags_e = List.reduce_exn ~f:Exp.(^) bap_to_lflags

let rflags_to_bap =
  let assn v = Some (v, Util.id) in
  (List.map ~f:(fun _ -> None) (List.range ~stride:(-1) ~stop:`inclusive 63 32))
  @  None                       (* 31 *)
     :: None                       (* 30 *)
     :: None                       (* 29 *)
     :: None                       (* 28 *)
     :: None                       (* 27 *)
     :: None                       (* 26 *)
     :: None                       (* 25 *)
     :: None                       (* 24 *)
     :: None                       (* 23 *)
     :: None                       (* 22 *)
     :: None                       (* 21 *)
     :: None                       (* 20 *)
     :: None                       (* 19 *)
     :: None                       (* 18 *)
     :: None                       (* 17 *)
     :: None                       (* 16 *)
     :: None                       (* 15 *)
     :: None                       (* 14 *)
     :: None                       (* 13 *)
     :: None                       (* 12 *)
     :: assn oF                    (* 11 *)
     :: assn df                    (* 10 *)
     :: None                       (* 09 *)
     :: None                       (* 08 *)
     :: assn sf                    (* 07 *)
     :: assn zf                    (* 06 *)
     :: None                       (* 05 *)
     :: assn af                    (* 04 *)
     :: None                       (* 03 *)
     :: assn pf                    (* 02 *)
     :: None                       (* 01 *)
     :: assn cf                    (* 00 *)
     :: []
let eflags_to_bap = List.drop rflags_to_bap 32
let flags_to_bap = List.drop eflags_to_bap 16
let lflags_to_bap = List.drop flags_to_bap 8
(* A list of functions for assigning each bit in rflags *)
let assns_rflags_to_bap =
  List.map
    ~f:(function
        | None -> (fun _ -> [])
        | Some (v,f) -> (fun e -> [Stmt.move v (f e)]))
    rflags_to_bap
let assns_eflags_to_bap = List.drop assns_rflags_to_bap 32
let assns_flags_to_bap = List.drop assns_eflags_to_bap 16
let assns_lflags_to_bap = List.drop assns_flags_to_bap 8

(* exp helpers *)

let load_s mode s t a =
  let mem_e = ge mode mem in
  match s with
  | None -> Exp.load mem_e a LittleEndian t
  | Some v -> Exp.(load mem_e (var v + a) LittleEndian t)

(* exp from int64 *)
(* we are not sure of the big int to bitvector stuff *)
let bitvector_of_bil = function
  | Exp.Int bv -> bv
  | _ -> failwith "not a BIL int"

let lt n t = (BV.of_int64 n ~width:t) |> Exp.int
let l64 i = lt i 64
let l32 i = lt i 32
let l16 i = lt i 16

let int64_of_mode m i = match m with
  | X86 -> bitvector_of_bil (l32 i)
  | X8664 -> bitvector_of_bil (l64 i)

(* exp from int *)
(* let it n t = Int (BV.lit n t) *) (*defined in previous code*)
let i64 i = it i 64
let i32 i = it i 32


let int_of_mode m i = match m with
  | X86 -> bitvector_of_bil (i32 i)
  | X8664 -> bitvector_of_bil (i64 i)

let bitvector_of_z z width = Z.to_bits z |> BV.of_binary ~width LittleEndian
(* exp from big int *)
let bt n t = bitvector_of_z n t |> Exp.int
let b64 i = bt i 64
let b32 i = bt i 32
let b16 i = bt i 16

let big_int_of_mode m i = match m with
  | X86 -> bitvector_of_bil (b32 i)
  | X8664 -> bitvector_of_bil (b64 i)

(* Get elemt from low opcode bits *)
let lowbits2elemt b =
  match b land 3 with
  | 0 -> Type.imm 8
  | 1 -> Type.imm 16
  | 2 -> Type.imm 32
  | 3 -> Type.imm 64
  | _ -> disfailwith X86 "invalid"

(* converts a register number to the corresponding register variable *)
let bits2genreg = function
  | 0 -> rax
  | 1 -> rcx
  | 2 -> rdx
  | 3 -> rbx
  | 4 -> rsp
  | 5 -> rbp
  | 6 -> rsi
  | 7 -> rdi
  | i when i >= 8 && i <= 15 -> nums.(i-8)
  | _ -> failwith "bits2genreg takes 4 bits"

let reg2bits r =
  Util.index_ofq r [rax; rcx; rdx; rbx; rsp; rbp; rsi; rdi]

let bits2segreg = function
  | 0 -> es
  | 1 -> cs
  | 2 -> ss
  | 3 -> ds
  | 4 -> fs
  | 5 -> gs
  | 6 | 7 -> disfailwith X86 "bits2segreg: reserved"
  | _ -> failwith "bits2regseg: invalid"

let bits2segrege b = bits2segreg b |> Exp.var

let bits2ymm b = ymms.(b)

let bits2ymme b = bits2ymm b |> Exp.var

(*FIXME: This is conversion from typ to nat1. It's used in cast expressions*)
let (!!) = function Type.Imm v -> v | _ -> failwith "internal error"

let bits2ymm128e b =
  bits2ymme b |> Exp.(cast Cast.low (!!r128))

let bits2ymm64e b =
  bits2ymme b |> Exp.(cast Cast.low (!!r64))

let bits2ymm32e b =
  bits2ymme b |> Exp.(cast Cast.low (!!r32))

let bits2xmm = bits2ymm128e

let bits2xmm64e = bits2ymm64e

let bits2xmm32e = bits2ymm32e

let ymm0 = ymms.(0)

let bits2reg64e mode b =
  ge mode (bits2genreg b)

let bits2reg32e mode b =
  bits2genreg b |> ge mode |> Exp.(cast Cast.low (!!r32))

let bits2reg16e mode b =
  bits2reg32e mode b |> Exp.(cast Cast.low (!!r16))

let bits2reg8e mode ?(has_rex=false) b =
  if b < 4 || has_rex then
    bits2reg32e mode b |> Exp.(cast Cast.low (!!r8))
  else
    b land 3 |> bits2reg32e mode |>
    Exp.(cast Cast.low (!!r16)) |>  Exp.(cast Cast.high (!!r8))

let reg2xmm r =  reg2bits r |> bits2xmm

(* effective addresses for 16-bit addressing *)
let eaddr16 mode =
  let e v = ge mode v |> Exp.(cast Cast.low (!!r16)) in
  let open Exp in
  function
  (* R/M byte *)
  | 0 -> e rbx + e rsi
  | 1 -> e rbx + e rdi
  | 2 -> e rbp + e rsi
  | 3 -> e rbp + e rdi
  | 4 -> e rsi
  | 5 -> e rdi
  | 6 -> e rbp
  | 7 -> e rbx
  | _ -> disfailwith X86 "eaddr16 takes only 0-7"

let eaddr16e mode b = eaddr16 mode b

let ah_e mode = bits2reg8e mode 4
let ch_e mode = bits2reg8e mode 5
let dh_e mode = bits2reg8e mode 6
let bh_e mode = bits2reg8e mode 7
