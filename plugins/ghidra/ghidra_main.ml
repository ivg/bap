open Core_kernel
open Bap_main
open Bap_core_theory

open Result.Monad_infix
open Extension.Syntax

module Default = Bap_ghidra_config

module Bitvecs = Map.Make_binable_using_comparator(struct
    include Bitvec_order
    include Bitvec_sexp.Functions
    include Bitvec_binprot.Functions
  end)

type error =
  | Command_failed of string * int
  | No_ghidra of string
  | No_script of string
  | Sys_error of string
  | Failed_to_parse of string * exn

type Extension.Error.t += Fail of error

type output = {
  funcs : string Bitvecs.t;
  flows : Bitvec.t list Bitvecs.t;
}

type ghidra_ctxt = {
  ghidra : string;
  tmpdir : string;
  binary : string;
}

let (+/) = Filename.concat

let path =
  Extension.Configuration.parameter
    Extension.Type.(path =? Default.ghidra_path) "path"
    ~doc:"The installation path of Ghidra (that contains ghidraRun)"

let script_path =
  Default.prefix_path+/"share"+/"bap-ghidra"

let fail err = Fail err
let error err = Error (fail err)

let system cmd = match Sys.command cmd with
  | 0 -> Ok ()
  | n -> error (Command_failed (cmd,n))

let shell cmd = ksprintf system cmd

let rng = Caml.Random.State.make_self_init ()

let mkdtemp ?(mode=0o0700) ?tmp_dir ?(prefix="") ?(suffix="") () =
  let genname () = Uuidm.v4_gen rng () |> Uuidm.to_string in
  let rec create name =
    let tmp = match tmp_dir with
      | None -> Filename.get_temp_dir_name ()
      | Some tmp -> tmp in
    let path =
      String.concat [tmp; Filename.dir_sep; prefix; name; suffix] in
    match Unix.mkdir path mode with
    | () -> path
    | exception Unix.Unix_error(Unix.EEXIST,_,_) ->
      genname () |> create in
  genname () |> create

let create_ghidra ~binary ctxt =
  let ghidra = ctxt-->path +/ "support" +/ "analyzeHeadless" in
  let script = script_path +/ "BapGhidra.java" in
  if not (Sys.file_exists ghidra) then error (No_ghidra ghidra) else
  if not (Sys.file_exists script) then error (No_script script)
  else Ok {
      ghidra;
      binary;
      tmpdir = mkdtemp ~prefix:"bap-ghidra" ()
    }

let chdir dst =
  try Ok (Sys.chdir dst) with
  | Sys_error s -> error (Sys_error s)

let is_debugging = match Sys.getenv_opt "BAP_GHIDRA_DEBUG" with
  | None -> false
  | Some ("false"|"0"|"no") -> false
  | _ -> true

let parse_funcs sexps =
  List.rev_map sexps ~f:[%of_sexp: string * Bitvec_sexp.t] |>
  List.fold ~init:Bitvecs.empty ~f:(fun names (name,addr) ->
      Map.set names addr name)

let parse_flows sexps =
  let open Bitvec_sexp in
  List.rev_map sexps ~f:[%of_sexp: t * t option * t list] |>
  List.fold ~init:Bitvecs.empty ~f:(fun addrs (addr,_fall,flows) ->
      Map.set addrs addr flows)

let read parse path =
  try
    Result.return @@
    parse @@
    In_channel.with_file path ~f:Sexp.input_rev_sexps
  with exn ->
    error (Failed_to_parse (path,exn))

let read_funcs = read parse_funcs
let read_flows = read parse_flows

let process_output () =
  read_funcs "funcs.sexp" >>= fun funcs ->
  read_flows "flows.expe" >>= fun flows ->
  Ok {funcs; flows}

let exec {ghidra; binary; tmpdir} =
  chdir tmpdir >>= fun () ->
  shell "%s %s BAP -import %s \
         -scriptPath %s -postScript BapGhidra.java"
    ghidra tmpdir binary script_path >>=
  process_output

let run ctxt =
  let cwd = Sys.getcwd () in
  let res = exec ctxt in
  let keep_folder = is_debugging || is_error res in
  Sys.chdir cwd;
  if not keep_folder then FileUtil.rm ~recurse:true [ctxt.tmpdir];
  res

let agent = KB.Agent.register "ghidra"

let (>>=?) x f =
  let open KB.Syntax in
  x >>= function
  | None -> KB.return None
  | Some x -> f x

let (>>|?) x f =
  let open KB.Syntax in
  x >>| function
  | None -> None
  | Some x -> f x

let provide_ghidra ctxt =
  let memo = ref None in
  fun path -> match !memo with
    | Some (path',result) when String.equal path path' -> Some result
    | _ -> match create_ghidra ~binary:path ctxt with
      | Error _ -> assert false
      | Ok ghidra -> match run ghidra with
        | Error _ -> assert false
        | Ok outcome ->
          memo := Some (path,outcome);
          Some outcome



let provide_common_name path ctxt =
  let open KB.Syntax in
  KB.promise Theory.Label.name @@ fun prog ->
  KB.collect path prog >>=? fun path ->
  KB.collect Theory.Label.addr prog >>|? fun addr ->
  match create_ghidra ~binary:path ctxt with
  | Error _ -> None
  | Ok ghidra -> match run ghidra with
    | Error _ -> None
    | Ok {funcs} -> Map.find funcs addr



(* let main ctxt =
 *   create_ghidra ctxt >>= fun ghidra ->
 *   Ok ()
 *
 * let () = Extension.declare main *)
