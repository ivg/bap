open Core_kernel
open Bap_main
open Bap_knowledge
open Bap_core_theory
open Bap.Std

open KB.Syntax

include Loggers()


let print_semantics label slots =
  KB.collect Theory.Program.Semantics.slot label >>= fun sema ->
  KB.Symbol.package >>| fun package ->
  let slots = List.map slots ~f:(fun name ->
      KB.Name.to_string (KB.Name.read ~package name)) in
  match slots with
  | [] -> Format.printf "%a@\n" KB.Value.pp sema
  | slots -> Format.printf "%a@\n" (KB.Value.pp_slots slots) sema

let list_objects cls () =
  KB.objects cls >>=
  KB.Seq.iter ~f:(fun obj ->
      KB.Object.repr cls obj >>| fun str ->
      Format.printf "%s@\n" str)


let register () =
  let open Project.Analysis in
  let package = "bap" in
  register ~package "print-semantics"
    (args program $ rest string) print_semantics;
  register ~package "list-instructions"
    (args empty) (list_objects Theory.Program.cls);
  register ~package "list-units"
    (args empty) (list_objects Theory.Unit.cls)
