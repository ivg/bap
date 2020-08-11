(** A representation of relations between two sets.

    A relation between two sets is a set of pairs made from the
    elements of these sets. The precise mathematical defition is given
    below. This module implements a bidirectional mapping between two
    sets and computes their matching that defines bijections between
    the sets.

    {2 Format Definition and Notation}

    Given a set of keys [K] and a set of values [S], with
    meta-variables [x,y,z] ranging over [K] and meta-variables
    [r,s,t] ranging over [S] we will denote a finitary relation [R]
    as a subset of the cartesian product [K x S], which is a set of
    pairs [(x,r), ..., (z,t)], which we represent as a bipartite
    graph [G = (K,S,R)].
*)

(** the type for relation between ['a] and ['d].  *)
type ('a,'d) t

(** [empty (module K)] the empty relation for the set of keys [K].

    For example,
    [empty (module Int)] will create an empty relation for the set of
    keys represented with the module [Int].
*)
val empty : ('k -> 'k -> int) -> ('d -> 'd -> int) -> ('k,'d) t


(** [add relation x s] establishes a relation between [x] and [s].  *)
val add : ('k,'d) t -> 'k -> 'd -> ('k,'d) t

(** {2 Bijections and matching}

    The set of independent edges [M] (the matching) of the graph [G]
    forms a finite bijection between [K] and [S]. It is guaranteed
    that for each pair [(x,s)] in [M] there is no other pair in [M],
    that will include [x] or [s].

    Edges [R] that are not in the matching [M] represent a set of
    keys and values that do not match because of one the two
    anomalies:
    - non-injective-key occurs when the same value is in relation
      with more than one key, e.g., [(x,s), (y,s)];
    - non-injective-value occurs when the same value is in relation with
      more than one key, e.g., [(x,r), (x,s)];
*)

(** the reason why was the pair left unmatched  *)
type ('a,'d) reason =
  | Non_injective_key of 'd * 'a list
  | Non_injective_value of 'd list

(** [matching relation data] computes matching for the given [relation].

    Calls [saturated x s data] for each [(x,s)] in the matching
    [M] (see the module description) and [unmatched z reason d] for
    each [(z,t)] in the relation that are not matched, the reason
    is one of the:

    - [Non_injective_key (s,xs)] if the mapping from keys to
      values that is induced by the [relation] is non-injective,
      because the set of keys [xs] are mapped to the same value
      [s].

    - [Non_injective_value ss] if the mapping from values to keys
      that is induced by the [relation] is non-injective, because
      the set of values [ss] are mapped to the same key.
*)
val matching : ('k,'d) t ->
  ?saturated : ('k -> 'd -> 'a -> 'a) ->
  ?unmatched : ('k -> ('k,'d) reason -> 'a -> 'a) -> 'a -> 'a
