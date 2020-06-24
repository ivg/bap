open Core_kernel
(** Provides signatures storage  *)

open Regular.Std
open Bap.Std

type error =
  [ `Corrupted of string  (** Signature file is corrupted  *)
  | `No_signatures  (** Signature file is not found    *)
  | `No_entry of string  (** Corresponding entry not found  *)
  | `Sys_error of string  (** System error has occurred     *) ]
(** Error conditions  *)

val save :
  ?comp:string ->
  mode:string ->
  path:string ->
  arch ->
  bytes ->
  (unit, error) Result.t
(** [save ?comp ~mode ~path arch data] store signatures data in the
    database of signatures specified by the [path] parameter. The
    triple [arch-comp-mode] defines a key for the created entry. If an
    entry with the same name existed, then it would be overwritten
    with the new data. If the database, doesn't exist, then it will be
    created and the specified destination.*)

val load :
  ?comp:string -> ?path:string -> mode:string -> arch -> (bytes, error) Result.t
(** [load ?comp ?path ~mode arch] finds a signature for the specified
    [arch-comp-path] triple. The [path] defaults to [default_path].*)

val default_path : string
(** default path for the signatures database  *)

val string_of_error : error -> string
(** a human readable representation of an error.  *)
