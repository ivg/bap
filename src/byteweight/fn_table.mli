(** Function Table.
    Reads a function table from a binary file. *)
open Core_kernel.Std
open Bap.Std

(** function table  *)
type t

(** a path in a filesystem  *)
type path = string

(** a probabiliry ranging from [0..1]  *)
type prob = float

(** function. See {!Fn} submodule for the interface.  *)
type fn

(** [create ?signatures ?threshold ?work_on_bytes arch data] creates a
    function table from provided binary [data], assuming that it
    contains codes compiled for the [arch] architecture.

    @param signatures - a path to a file, containing code signatures,
                        by default compiled in signatures are used.

    @weight_threshold - make a descisions with a specified confidence level
                        (at minimum)

    @work_on_bytes - if true, then do not lift code and work on a
                     binary. Defaults to false


    @base_addr - a virtual address of the first byte in a provided data.
            Defaults to [Addr.zero] *)
val create
  : ?signatures:path             (* defaults to system signature *)
  -> ?weight_threshold:prob      (* defaults to zero *)
  -> ?work_on_bytes:bool         (* defaults to false     *)
  (* TODO: check if should use Bap_common *)
  (* TODO: where defined type table? *)
  -> Bap_common.arch
  -> segments:Image.sec table
  -> t

(** [mem table fn] true if table contains the function  *)
val mem : t -> addr -> bool

(** [length table] is the amount of functions found  *)
val length : t -> int

(** [is_empty table] returns true if no function can be found in the
    specified data with a given [confidence] level *)
val is_empty : t -> bool

(** [iter table f] apply a user provided function to each table
    element *)
val iter : t -> f:(addr -> fn -> unit) -> unit

(** [fold table ~init ~f] returns
    {[f an en (... f a3 e3 (f a2 e2 (f a1 e1 init)))]},
    where [e1..en] are the elements of [table],
          [a1..an] corresponding addresses,
          [init] is the inital argument, provided by a user *)
val fold : t -> init:'a -> f:(addr -> fn -> 'a -> 'a) -> 'a

(** [fold_range table min max init f] performs folding over a spec *)
val fold_range
  : t
  -> min:addr
  -> max:addr
  -> init:'a
  -> f:(addr -> fn -> 'a -> 'a) -> 'a

(** Returns [true] if and only if there exists an element for which the provided
    function evaluates to [true].  This is a short-circuiting operation. *)
val exists : t -> f:(fn -> bool) -> bool

(** Returns [true] if and only if the provided function evaluates to [true] for all
    elements.  This is a short-circuiting operation. *)
val for_all : t -> f:(fn -> bool) -> bool

(** [find table addr] returns as an [option] the first element for
    which [f] evaluates to true. *)
val find : t -> addr -> fn option

(** [find_exn table addr] the same as [find table exn] but raises
    exception [Not_found], when nothing found *)
val find_exn : t -> addr -> fn

(** [min_elt] returns [Some (addr,fn)] pair corresponding to the
    minimum address in a table *)
val min_elt : t -> (addr * fn) option
(** [max_elt] returns [Some (addr,fn)] pair corresponding to the
    maximum address in a table *)
val max_elt : t -> (addr * fn) option

(** [rank table addr] returns the number of functions with addresses
    stricrly less than [addr] *)
val rank : t -> addr -> int option

(** [next table addr] returns the smalles [(addr',fn)] pair in table
    [t] that such that [addr' > addr]*)
val next : t -> addr -> (addr * fn) option

(** [prev table addr] returns the largest [(addr',fn)] pair in table
    [t] that such that [addr' < addr]*)
val prev : t -> addr -> (addr * fn) option


(** [addrs table] returns a list of addresses *)
val addrs : t -> addr list

(** [fns table] returns a list of functions  *)
val fns : t -> fn list

module Fn : sig
  type t = fn

  (** [name fn] is a [Some name] of a function [fn] if it is known  *)
  val name : t -> string option

  (** [weight fn] the probability that [fn] is a function *)
  val weight : t -> prob
end
