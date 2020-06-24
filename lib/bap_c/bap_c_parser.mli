open Core_kernel
(** A parser interface.

    The module doesn't provide any parsers by itself, but allows it to
    be provided by a third party module.
*)

type decls = (string * Bap_c_type.t) list

type parser = Bap_c_size.base -> string -> decls Or_error.t

val run : parser
(** [run filename] parses file and returns a mapping from identifier
    to its type.*)

val provide : parser -> unit
(** called by a plugin that provides a parser.  *)
