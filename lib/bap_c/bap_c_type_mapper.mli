open Core_kernel
open Bap.Std
open Monads.Std
open Bap_c_type_mapper_intf

include S with type ('a, 'e) m = 'a
(** include visitor/mapper with the monad stripped away. *)

(** Search Monad.

    A monad for searching with abnormal exit, i.e., the computation
    terminates as soon as an item is found.  *)
module Search : sig
  include Monad.S2

  val finished : 'e -> ('a, 'e) t
  (** [finished needle] is called when a search is finished, it will
      terminate the search with the [needle] as a result.  *)

  val result : ('a, 'e) t -> 'e option
  (** [result s] runs the computation [s] and extracts the result.   *)
end

module State : S with type ('a, 'e) m = ('a, 'e) Monad.State.t
(** the mapper lifted into a regular state monad.   *)

module Finder : S with type ('a, 'e) m = ('a, 'e) Search.t
(** the visitor lifted into the search monad.

    For example, the following code will find the first pointer:

    {[
      module Search = C.Type.Mapper.Search

      let find_pointer t = Search.result @@ (object
          inherit [C.Type.t] C.Type.Mapper.Finder.base
          method! enter_pointer = Search.finished
        end)#run t
    ]} *)

(** [Make(M)] lifts the visitor into monad [M].  *)
module Make (M : Monad.S2) : S with type ('a, 'e) m = ('a, 'e) M.t
