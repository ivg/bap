open Bap_knowledge
open Bap_core_theory_definition
module Grammar = Bap_core_theory_grammar_definition

type ('a,'e) bitv_parser =
  (module Grammar.Bitv with type t = 'a
                        and type exp = 'e) ->
  'e -> 'a

type ('a,'e) bool_parser =
  (module Grammar.Bool with type t = 'a
                        and type exp = 'e) ->
  'e -> 'a

type ('a,'e) mem_parser =
  (module Grammar.Mem with type t = 'a
                       and type exp = 'e) ->
  'e -> 'a

type ('a,'e,'s) stmt_parser =
  (module Grammar.Stmt with type t = 'a
                        and type exp = 'e
                        and type stmt = 's) ->
  's -> 'a

type ('a,'e) float_parser =
  (module Grammar.Float with type t = 'a
                         and type exp = 'e) ->
  'e -> 'a

type ('a,'e) rmode_parser =
  (module Grammar.Rmode with type t = 'a
                         and type exp = 'e) ->
  'e -> 'a

type ('e,'r,'s) t = {
  bitv : 'a. ('a,'e) bitv_parser;
  bool : 'a. ('a,'e) bool_parser;
  mem  : 'a. ('a,'e) mem_parser;
  stmt : 'a. ('a,'e,'s) stmt_parser;
  float : 'a . ('a,'e) float_parser;
  rmode : 'a . ('a,'r) rmode_parser;
}

type ('e,'r,'s) parser = ('e,'r,'s) t

module Make(S : Core) : sig
  val run : ('e,'r,'s) parser -> 's list -> unit eff knowledge
end
