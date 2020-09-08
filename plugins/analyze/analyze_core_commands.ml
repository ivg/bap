open Core_kernel
open Bap_main
open Bap_knowledge
open Bap_core_theory
open Bap.Std

open KB.Syntax

include Loggers()

let read_name name =
  KB.Symbol.package >>| fun package ->
  KB.Name.to_string (KB.Name.read ~package name)


let print_semantics label slots =
  KB.collect Theory.Program.Semantics.slot label >>= fun sema ->
  KB.List.map slots ~f:read_name >>| fun slots ->
  match slots with
  | [] -> Format.printf "%a@\n" KB.Value.pp sema
  | slots -> Format.printf "%a@\n" (KB.Value.pp_slots slots) sema


let require prop label has =
  KB.collect prop label >>= function
  | None -> KB.return false
  | Some p -> has p

let belongs_to_unit unit insn =
  match unit with
  | None -> KB.return true
  | Some unit ->
    require Theory.Label.unit insn @@ fun unit' ->
    KB.return @@ Theory.Unit.equal unit unit'

let belongs_to_subr subr insn =
  match subr with
  | None -> KB.return true
  | Some subr ->
    require Theory.Label.addr subr @@ fun subr ->
    require Theory.Label.unit insn @@ fun unit ->
    require Theory.Label.addr insn @@ fun insn ->
    require Theory.Unit.Target.bits unit @@ fun bits ->
    KB.collect Project.State.slot unit >>| fun state ->
    let subrs = Project.State.subroutines state in
    let insn = Word.create insn bits
    and subr = Word.create subr bits in
    Disasm.Subroutines.belongs subrs ~entry:subr insn

let list_objects cls () =
  KB.objects cls >>=
  KB.Seq.iter ~f:(fun obj ->
      KB.Object.repr cls obj >>| fun str ->
      Format.printf "%s@\n" str)

let ensure x yes =
  x >>= function
  | true -> yes ()
  | false -> KB.return ()

let list_insns unit subr =
  KB.objects Theory.Program.cls >>=
  KB.Seq.iter ~f:(fun obj ->
      ensure (belongs_to_unit unit obj) @@ fun () ->
      ensure (belongs_to_subr subr obj) @@ fun () ->
      KB.Object.repr Theory.Program.cls obj >>| fun str ->
      Format.printf "%s@\n" str)



let register () =
  let open Project.Analysis in
  let package = "bap" in
  register ~package "semantics"
    (args program $ rest string) print_semantics
    ~desc:"prints the semantics of instruction \
           (all or the specified slots)";
  register ~package "instructions"
    (args @@
     keyword "unit" unit $
     keyword "subroutine" program) list_insns;
  register ~package "units"
    (args empty) (list_objects Theory.Unit.cls)
