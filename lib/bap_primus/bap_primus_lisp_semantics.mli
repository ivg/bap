open Bap_core_theory
open Bap.Std
open Bap_primus_lisp_types
open Bap_primus_lisp_program


type KB.conflict += Unresolved_definition of string
type KB.conflict += Illtyped_program of Type.error list

type primitive

module Primitive : sig
  type t = primitive
  val name : t -> string
  val args : t -> unit Theory.Value.t list
  val eval : string -> unit Theory.Value.t list -> unit Theory.Effect.t KB.t
  val declare :
    ?types:(Theory.Target.t -> Bap_primus_lisp_type.signature) ->
    ?docs:string ->
    string -> unit
end



val program : (Theory.Source.cls, program) KB.slot
val primitive : (Theory.program, primitive option) KB.slot
val symbol : (Theory.Value.cls, String.t option) KB.slot
val static : (Theory.Value.cls, Bitvec.t option) KB.slot
val enable : unit -> unit

module Unit : sig
  val create : ?name:string -> Theory.Target.t -> Theory.Unit.t KB.t
  val is_lisp : Theory.Unit.t -> bool KB.t
  val language : Theory.language
end
