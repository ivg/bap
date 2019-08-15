open Bap_core_theory
open Core_kernel
open Bap.Std
open Bap_llvm.Std
open Bap_future.Std
open Monads.Std

include Self()

let disasm_init x86_syntax =
  match init_disassembler ~x86_syntax () with
  | Ok () -> ()
  | Error er ->
    eprintf "%s\n" (Error.to_string_hum er)

let print_version () =
  printf "%s\n" llvm_version ;
  exit 0

module Fact = Ogre.Make(Monad.Ident)

let nonexec_regions =
  let open Fact.Syntax in
  let open Bap_llvm_ogre_types.Scheme in
  let open Bap_llvm_elf_scheme in
  let open Bitvec.M64 in
  Fact.require Image.Scheme.base_address >>= fun base ->
  Fact.foreach Ogre.Query.(begin
      select (from section_entry $ section_flags)
        ~join:[[field name]]
    end) ~f:(fun (_,_,len,off) (_, w, x) ->
      if Int64.(len > 0L) && not w && not x then
        let start = int64 off + int64 base in
        Some (start, start + int64 len - one)
      else None)


module Sections = Interval_tree.Make(struct
    type point = Bitvec.t [@@deriving compare]
    let sexp_of_point = Bitvec_sexp.sexp_of_t
    type t = point * point [@@deriving compare, sexp_of]
    let lower = fst
    let upper = snd
  end)

let collect_nonexec_regions scheme =
  match Fact.eval nonexec_regions scheme with
  | Error err ->
    warning "unable to collect sections: %a" Error.pp err;
    Sections.empty
  | Ok secs ->
    Seq.filter_opt secs |>
    Seq.fold ~init:Sections.empty ~f:(fun s i ->
        Sections.add s i ())


let invalid_nonexec_memory () =
  let open KB.Syntax in
  Stream.observe Project.Info.spec @@ fun spec ->
  collect_nonexec_regions spec |> fun nonexecs ->
  KB.promise Theory.Label.is_valid @@ fun obj ->
  KB.collect Theory.Label.addr obj >>| function
  | None -> None
  | Some addr ->
    Option.some_if (Sections.contains nonexecs addr) false

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Uses LLVM library to provide disassembler and file loader" ;
      `S "SEE ALSO";
      `P "$(b,bap-plugin-elf-loader)(1), $(b,bap-plugin-relocatable)(1)";
    ] in
  let x86_syntax =
    let names = ["att", "att"; "intel", "intel"] in
    let doc = sprintf "Choose style of code for x86 syntax between %s"
      @@ Config.doc_enum names in
    Config.(param (enum names) ~default:"att" "x86-syntax" ~doc) in
  let base_addr =
    let doc ="\
Replace image base address. If not set, a reasonable default corresponded
to a file type will be used. For example, for any executable file a
default image base is equal to lowest image virtual address.
For relocatable files a default image base is equal to 0xC0000000." in
    Config.(param (some int64) ~default:None "base" ~doc) in
  let version =
    let doc ="Prints LLVM version and exits" in
    Config.(flag "version" ~doc) in
  let filter_nonexec = Config.(flag "filter-nonexec") in
  Config.when_ready (fun {Config.get=(!)} ->
      if !filter_nonexec
      then invalid_nonexec_memory ();
      if !version then
        print_version();
      let () = init_loader ?base:!base_addr () in
      match !x86_syntax with
      | "att" | "intel" as s ->
        let syn = x86_syntax_of_sexp (Sexp.of_string s) in
        disasm_init syn
      | s -> eprintf "unknown x86-asm-syntax: %s" s)
