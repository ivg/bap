open Core_kernel.Std
module type M = sig
  type 'a t
  type key
  val create : unit -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val add : 'a t -> key -> 'a -> unit
  val replace : 'a t -> key -> 'a -> unit
  val find : 'a t -> key -> 'a option
  val format : key -> string
end

module MakeM (Key : Hashtbl.Key) : (M with type key = Key.t)

module type TRIE = sig
  type 'a t
  type key
  val init : 'a -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val find : 'a t -> key -> 'a
  val output : 'a t -> string -> ('a -> string) -> unit
end

module Make (M : M) : (TRIE with type key = M.key list)
