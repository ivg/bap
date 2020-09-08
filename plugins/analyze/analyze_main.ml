let doc = {|
# DESCRIPTION

Reads the knowledge base $(b,KB) and applies the specified analyses
to it. The knowledge base is left unmodified, unless the $(b,--update)
flag is set.

To get the list of available analyses, use $(b,bap list
analyses). Each analysis is represented by its name. If the name is
not fully qualified (i.e., doesn't include the package name) then the
bap package is assumed.

# EXAMPLE

```
  bap analyze knowledge.kb print-instructions
```

|}


open Core_kernel
open Bap_main
open Bap_knowledge
open Bap.Std

include Loggers()

type problem =
  | Unknown_command of {package : string; name : string}
  | Conflict of Knowledge.Conflict.t * string * string list
  | Bad_directive of {dir : string; msg : string}

type Extension.Error.t += Fail of problem

type ctxt = {
  path : string;
  package : string;
  history : string;
  quit : bool;
}

let initial_ctxt ~history path = {
  path;
  package = "user";
  quit = false;
  history;
}

let fail problem = Error (Fail problem)

let break_line str =
  String.split ~on:' ' str |>
  List.filter ~f:(fun s -> not (String.is_empty s))

let exec_command ?(package="bap") cmd args =
  match Project.Analysis.find ~package cmd with
  | None -> fail (Unknown_command {package; name=cmd})
  | Some analysis ->
    let code = Project.Analysis.apply analysis args in
    match Toplevel.try_exec code with
    | Ok () -> Ok ()
    | Error err -> fail (Conflict (err,cmd,args))

let in_package ctxt package =
  Toplevel.exec @@ Knowledge.Symbol.set_package package;
  {ctxt with package}

let set_package args ctxt = match args with
  | [pkg] -> Ok (in_package ctxt pkg)
  | _ -> fail (Bad_directive {
      dir = "in-package";
      msg = "expects exactly one argument";
    })


let save_base args ctxt = match args with
  | [] | [_] ->
    let path = Option.value (List.hd args)
        ~default:ctxt.path in
    Knowledge.save (Toplevel.current ()) path;
    Ok ctxt
  | _ -> fail (Bad_directive {
      dir = "save";
      msg = "takes at most one argument";
    })

let no_args dir f args ctxt = match args with
  | [] -> f ctxt
  | _ -> fail (Bad_directive {dir; msg = "doesn't accept arguments"})

let quit = no_args "quit" @@ fun ctxt -> Ok {ctxt with quit = true}
let clear = no_args "clear" @@ fun ctxt ->
  LNoise.clear_screen ();
  Ok ctxt

let list_commands = no_args "commands" @@ fun ctxt ->
  Project.Analysis.registered () |>
  List.iter ~f:(fun analysis ->
      let name = Project.Analysis.name analysis
      and desc = Project.Analysis.desc analysis in
      Format.printf "%-40s %s@\n%!" (Knowledge.Name.to_string name) desc);
  Ok ctxt

let rec directives = [
  "in-package", set_package, "sets the default package";
  "save", save_base, "saves the knowledge base on disk";
  "quit", quit, "quits the interactive session";
  "clear", clear, "clears the screen";
  "commands", list_commands, "lists known commands";
  "directives", (fun _args ctxt ->
      List.iter directives ~f:(fun (dir,_,desc) ->
          Format.printf "%-40s %s@\n%!" dir desc);
      Ok ctxt), "lists directives";
]

let find_directive dir = List.find_map directives ~f:(fun (s,f,_) ->
    if String.equal s dir then (Some f) else None)

let dispatch str ctxt = match break_line str with
  | [] -> Ok ctxt
  | cmd :: args -> match find_directive cmd with
    | Some dir -> dir args ctxt
    | None ->
      match exec_command ~package:ctxt.package cmd args with
      | Ok () -> Ok ctxt
      | Error err -> Error err

let report_error err =
  Format.eprintf "%a@\n%!" Extension.Error.pp err

let load_history filename =
  if Sys.file_exists filename
  then match LNoise.history_load ~filename with
    | Ok () -> ()
    | Error msg ->
      Format.eprintf "Failed to load the history file: %s@\n%!" msg

let remember ctxt input =
  match LNoise.history_add input with
  | Error msg -> warning "failed to remember the input: %s" msg
  | Ok () ->
    match LNoise.history_save ~filename:ctxt.history with
    | Ok () -> ()
    | Error msg -> warning "failed to write history: %s" msg



let rec interactive_loop ctxt =
  Format.printf "%!";
  match LNoise.linenoise (ctxt.package ^ "> ") with
  | None -> Ok ()
  | Some input ->
    remember ctxt input;
    match dispatch input ctxt with
    | Error err -> report_error err; interactive_loop ctxt
    | Ok {quit=true} -> Ok ()
    | Ok ctxt -> interactive_loop ctxt

let run_non_interactive _commands _script _ctxt = Ok ()

let string_of_problem = function
  | Unknown_command {package; name} ->
    sprintf "Can't find a command %S in the package %S" name package
  | Conflict (err,cmd,_) ->
    sprintf "Command %s failed with %s" cmd
      (Knowledge.Conflict.to_string err)
  | Bad_directive {msg} -> sprintf "Error: %s" msg

let () =
  let open Extension in
  let open Extension.Command in
  let knowledge = argument Type.("knowledge-base" %: file =? "a.bkb")
      ~doc:"The path to a knowledge base." in
  let commands = arguments Extension.Type.("command" %: string)
      ~doc:"The command to execute." in
  let script = parameter Type.("path" %: some file) "script"
      ~aliases:["s"]
      ~doc:"THe path to a script file with commands." in
  let history =
    let history_location =
      match Sys.getenv_opt "HOME" with
      | None | Some "" -> ".bap_history"
      | Some home -> Filename.concat home ".bap_history" in
    parameter Type.("path" %: file =? history_location) "history"
      ~aliases:["H"] in
  declare "analyze" (args $knowledge $commands $script $history) @@
  fun base commands script history _ctxt ->
  Analyze_core_commands.register ();
  Toplevel.set @@ Knowledge.load base;
  let ctxt = in_package (initial_ctxt history base) "bap" in
  match commands,script with
  | [], None ->
    load_history history;
    interactive_loop ctxt
  | _ -> run_non_interactive commands script ctxt


let () = Extension.Error.register_printer @@ function
  | Fail problem -> Some (string_of_problem problem)
  | _ -> None
