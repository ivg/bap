open Core_kernel

type mode = [ `m32 | `m64 ]

type t

val create : string -> bool -> t Or_error.t
(** [create path is_headless] *)

val find_ida : t -> mode option -> string -> string
(** [find_ida info mode target] - returns a full path
    to ida executable depending on [mode] and [target] name *)

val is_headless : t -> bool

val check : t -> unit Or_error.t

val require_ncurses : t -> bool
(** [require_ncurses ida] returns true if [ida] need ncureses lib to operate*)
