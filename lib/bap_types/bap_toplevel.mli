[@@@alert toplevel]

open Bap_knowledge

type 'a t = 'a
type 'p var

exception Conflict of Knowledge.conflict

val eval : ('a,'p) Knowledge.slot -> 'a Knowledge.obj knowledge -> 'p t
val exec : unit knowledge -> unit t

val try_eval : ('a,'p) Knowledge.slot -> 'a Knowledge.obj knowledge ->
  ('p,Knowledge.conflict) result t

val try_exec : unit knowledge -> (unit,Knowledge.conflict) result t

val get : 'p var -> 'p t
val put : 'p var -> 'p knowledge -> unit t
val var : string -> 'p var


val reset : unit -> unit
val set : Knowledge.state -> unit
val current : unit -> Knowledge.state

val acquire : unit -> Knowledge.state
val discard : Knowledge.state -> unit
val release : Knowledge.state -> unit

val update : (Knowledge.state -> Knowledge.state) -> unit
val borrow : (Knowledge.state -> 'a) -> 'a
