open Core_kernel
(** Memory map.

    Memory map is an assosiative data structure that maps memory
    regions to values. Unlike in the Table, memory
    regions in the Memmap can intersect in an arbitrary ways. This
    data structure is also known as Interval Tree or Segmented Tree.

    Underneath the hood it is implemented using augumented AVL tree,
    so that all operations are logarithmic.
*)

open Bap_types.Std

type mem = Bap_memory.t

type 'a t [@@deriving sexp_of]

val empty : 'a t
(** [empty] map  *)

val singleton : mem -> 'a -> 'a t
(** [singleton] a memory map containing only one memory region  *)

val min_addr : 'a t -> addr option
(** [min_addr map] is a minimum addr mapped in [map] *)

val max_addr : 'a t -> addr option
(** [max_addr map] is a maximum addr mapped in [map] *)

val min_binding : 'a t -> (mem * 'a) option
(** [min_binding map] is a minimum binding mapped in [map] *)

val max_binding : 'a t -> (mem * 'a) option
(** [max_binding map] is a maximum binding mapped in [map] *)

val add : 'a t -> mem -> 'a -> 'a t
(** [add map mem tag] adds a new memory region [mem] tagged with
    [tag]. If the same region was already in the [map] it will be
    tagged with the [tag] again, even if it has had the same tag. *)

val dominators : 'a t -> mem -> (mem * 'a) seq
(** [dominators map mem] an ordered sequence of all memory regions,
    containing [mem]. A memory region [(x,y)] contains region [(p,q)],
    iff [p >= x && q <= y], where memory regions are depicted using
    closed intervals. *)

val intersections : 'a t -> mem -> (mem * 'a) seq
(** [intersections map mem] an ordered sequence of all memory regions,
    that intersects with [mem]. Memory region [(x,y)] intersects with
    region [(p,q)] iff there exists such [z] that

    [z >= p || z <= q && z >= x && z <= y].

    In other words if there exists such byte that belongs to both memory
    regions. *)

val intersects : 'a t -> mem -> bool
(** [intersects map mem] is true if [intersections map mem] is not empty *)

val dominates : 'a t -> mem -> bool
(** [dominates map mem] if there is a non empty set of dominators  *)

val contains : 'a t -> addr -> bool
(** [contains map addr] true if there exists such memory region [mem],
    that [Memory.contains mem addr] *)

val lookup : 'a t -> addr -> (mem * 'a) seq
(** [lookup map addr] returns an ordered sequence of all memory
    containing the [addr] *)

val map : 'a t -> f:('a -> 'b) -> 'b t
(** [map m f] returns a new map with each tag mapped
    with function [f] *)

val mapi : 'a t -> f:(mem -> 'a -> 'b) -> 'b t
(** [mapi m f] the same as [map], but [f] is called with two
    arguments: [mem] and [tag], where [mem] is a memory region,
    and [tag] is a [tag] associated with that region. *)

val filter : 'a t -> f:('a -> bool) -> 'a t

val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
(** [filter_map m f] creates a new map by applying a function [f] to
    each tag. If [f] returns [Some x] then this region will be mapped
    to [x] in a new map, otherwise it will be dropped. *)

val filter_mapi : 'a t -> f:(mem -> 'a -> 'b option) -> 'b t
(** [filter_mapi] is like [filter_map] but use function also accepts
    would associated memory region *)

val remove : 'a t -> mem -> 'a t
(** [remove map mem] removes all bindings to [mem]  *)

val remove_intersections : 'a t -> mem -> 'a t
(** [remove_intersections map mem] removes all bindings that
    that intersects with [mem] *)

val remove_dominators : 'a t -> mem -> 'a t
(** [remove_dominators map mem] removes all bindings that are
    dominators to [mem] *)

val to_sequence : 'a t -> (mem * 'a) Sequence.t
(** [to_sequence map] converts the memmap ['a t] to a sequence of
    key-value pairs *)

include Container.S1 with type 'a t := 'a t

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
