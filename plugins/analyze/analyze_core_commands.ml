open Core_kernel
open Bap_main
open Bap_knowledge
open Bap_core_theory
open Bap.Std

open KB.Syntax

let package = "bap"


let print_semantics label slots =
  KB.collect Theory.Program.Semantics.slot label >>| fun sema ->
  match slots with
  | [] -> Format.printf "%a@\n" KB.Value.pp sema
  | slots -> Format.printf "%a@\n" (KB.Value.pp_slots slots) sema


let register () =
  let open Project.Analysis in
  register ~package "print-semantics"
    (args program $ rest string) print_semantics
