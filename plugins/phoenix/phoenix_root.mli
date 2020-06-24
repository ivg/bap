exception Target_directory_is_a_file

type t

type channel = Format.formatter

val create : ?root:string -> string -> t
(** [create filename]  *)

val path : t -> string

val with_cfg_file : t -> string -> f:(channel -> unit) -> unit
(** [with_cfg_file data symname ~f]  *)

val with_bil_file : t -> string -> f:(channel -> unit) -> unit
(** [with_bil_file data symname ~f]  *)

val with_index_file : t -> f:(channel -> unit) -> unit

val with_funcs_file : t -> f:(channel -> unit) -> unit

val with_dump_file : t -> f:(channel -> unit) -> unit
