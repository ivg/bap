open Core_kernel
open Bap_knowledge
open Knowledge.Syntax

let package = "bap"
type 'a t = 'a
type main = Main
type 'p var = (main,'p option) Knowledge.slot

type env = {
  mutable locked : bool;
  mutable state : Knowledge.state;
}

let env = {
  locked = false;
  state = Knowledge.empty
}

let slots = ref 0

let set s = env.state <- s
let reset () =
  env.state <- Knowledge.empty;
  env.locked <- false

let current () = env.state

exception Conflict of Knowledge.conflict
exception Not_found

let require c =
  if not c then Printf.exitf
      "Aborting due to a logic error: a nested call to the toplevel\n\
       Consult the Bap.Std.Toplevel module for more information\n\
       Backtrace:\n%s\n"
      (Printexc.get_backtrace ())
      ()


let unlock () =
  require env.locked;
  env.locked <- false

let lock () =
  require (not env.locked);
  env.locked <- true

let try_eval slot exp =
  lock ();
  let cls = Knowledge.Slot.cls slot in
  match Knowledge.run cls exp env.state with
  | Ok (v,s) ->
    unlock ();
    env.state <- s;
    Ok (Knowledge.Value.get slot v)
  | Error conflict ->
    unlock ();
    Error conflict

let eval slot exp =
  try_eval slot exp |> function
  | Ok v -> v
  | Error x -> raise (Conflict x)

let main = Knowledge.Class.declare ~package "toplevel" Main


let var name =
  incr slots;
  let name = sprintf "%s%d" name !slots in
  let order x y : Knowledge.Order.partial = match x, y with
    | Some _, Some _ | None, None -> EQ
    | None,Some _ -> LT
    | Some _,None -> GT in
  let dom = Knowledge.Domain.define ~empty:None ~order "any" in
  Knowledge.Class.property ~package main name dom

let this =
  Knowledge.Symbol.intern ~package "main" main

let try_exec stmt =
  let stmt = stmt >>= fun () -> this in
  lock ();
  match Knowledge.run main stmt env.state with
  | Ok (_,s) ->
    unlock ();
    Ok (env.state <- s)
  | Error conflict ->
    unlock ();
    Error conflict

let exec stmt =
  try_exec stmt |> function
  | Ok () -> ()
  | Error err -> raise (Conflict err)

let put slot exp = exec @@begin
    exp >>= fun v -> this >>= fun x ->
    Knowledge.provide slot x (Some v)
  end

let get slot = eval slot this |> function
  | None -> raise Not_found
  | Some x -> x

let acquire () = lock (); current ()
let release s = unlock (); set s
let discard _ = unlock ()
let borrow f =
  let s = acquire () in
  protect ~f:(fun () -> f s) ~finally:unlock

let update f =
  let s = acquire () in
  let s = try f s with exn ->
    discard s;
    raise exn in
  release s


let () = Caml.Printexc.register_printer @@ function
  | Conflict err ->
    Some (Format.asprintf "%a" Knowledge.Conflict.pp err)
  | _ -> None
