(** IDA integration.

    This module provides for a interface to ingegrate with IDA, by
    running IDA in batch mode, obtain database, run script on
    database.

    Plugins can be written to provide this service, or to use the
    service provided by other plugins (usually provided by
    plugins/ida) through this interface.
*)
module Std : sig
  type ida

  type 'a command

  (** Interaction with ida instance  *)
  module Ida : sig
    exception Failed of string
    (** exception External_command_failed occurs when the external IDA
        command was not executed successfully *)

    exception Not_in_path

    type t = ida
    (** IDA instance *)

    val create : string -> t
    (** [create target] create an IDA instance that will work with
        [target] executable. *)

    val exec : t -> 'a command -> 'a
    (** [exec ida command] execute the given [command].  *)

    val close : t -> unit
    (** [close ida] finish interaction with IDA and clean all resources *)

    val with_file : string -> 'a command -> 'a
    (** [with_file target analysis] creates ida instance on [target],
        perform [analysis] and close [ida] *)
  end

  (** Commands that can be passed into an IDA session *)
  module Command : sig
    type 'a t = 'a command

    type language = [ `python | `idc ]

    val create : language -> script:string -> parser:(string -> 'a) -> 'a t
    (** [create lang ~script ~parser] create a command that will
        execute a [script], written in a given language, at then
        parse the output using the specified parser function.

        If a script needs to output information, it must use a
        filename [$output] (that will be substituted with the real
        file name).

        The parse name will get the name of the output file, so that
        it can read and parse it. *)

    val language : 'a t -> language
    (** [language command] is a script language  *)

    val script : 'a t -> string
    (** [script command] the script text *)

    val parser : 'a t -> string -> 'a
    (** [parser command] the associated parser function.  *)
  end

  (** Allow plugins to specify that they can provide IDA service *)
  module Service : sig
    type t = { exec : 'a. 'a command -> 'a; close : unit -> unit }

    val provide : (string -> t) -> unit
    (** [provide creator] provides for a service that can perform the
        roles of [Ida.create], [Ida.exec], [Ida.close].

        The [creator] function accepts a path to a target file and
        returns an instance of ida service.*)
  end
end
