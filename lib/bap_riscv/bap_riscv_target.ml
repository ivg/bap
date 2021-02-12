open Core_kernel
open Bap_core_theory

let package = "bap"

type r64 and r32 and r8

type 'a bitv = 'a Theory.Bitv.t Theory.Value.sort

let r64 : r64 bitv = Theory.Bitv.define 64
let r32 : r32 bitv = Theory.Bitv.define 32
let r8  : r8  bitv = Theory.Bitv.define 8

let reg t n = Theory.Var.define t n
let untyped = List.map ~f:Theory.Var.forget
let (@<) xs ys = untyped xs @ untyped ys


let regs = [
  "zero";
  "ra";
  "sp";
  "gp";
  "tp";
  "t0"; "t1"; "t2";
  "s0"; "s1";
  "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7";
  "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11";
  "t3"; "t4"; "t5"; "t6"
]

let array t = List.map regs ~f:(reg t)
let x t = List.init 32 ~f:(fun i -> reg t (sprintf "X%d" i))

let parent = Theory.Target.declare ~package "riscv"

let riscv t =
  let mems = Theory.Mem.define t r8 in
  let mem = reg mems "mem" in
  let vars = x t @< [mem] in
  let bits = Theory.Bitv.size t in
  let name = sprintf "riscv%d"  bits in
  Theory.Target.declare ~package name ~parent
    ~bits
    ~code:mem
    ~data:mem
    ~vars

let riscv64 = riscv r64
let riscv32 = riscv r32

let llvm_encoding = Theory.Language.declare ~package "llvm-riscv"
