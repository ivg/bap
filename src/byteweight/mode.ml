open Core_kernel.Std
open Bap.Std
type bytes = char list
type prob = float

module type MODE =
  sig
    type t
    type key = t list
    module Tree : Trie.TRIE with type key = key
    module Table : Hashtbl.S with type key = key
    val generate_keys : Memory.t -> ?from:addr -> int -> key list
    val consecutive : ?arch:arch -> bytes -> t list
    val load : string -> unit
    val find : key -> prob
    val len : int
    val string_of_key : key -> string
  end
