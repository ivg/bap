open Core_kernel.Std
open Bap_common
open Bap_bil
open Bap_visitor

val find_map : 'a #finder -> bil -> 'a option
val find : unit #finder -> bil -> bool
val iter : unit #visitor -> bil -> unit

(** [is_referenced x p] is [true] if [x] is referenced in some expression or
    statement in program [p] *)
val is_referenced : var -> bil -> bool

(** [is_modified x p] is [true] is [x] is assigned in [p]  *)
val is_modified : var -> bil -> bool

(** [is_assigned x p] is [true] if there exists such [Move]
    statement, that [x] occures on the left side of it. If [strict]
    is true, then only unconditional assignments. By default,
    [strict] is [false] *)
val is_assigned : ?strict:bool -> var -> bil -> bool

(** [prune_unreferenced p] remove all assignments to variables that
    are not used in the program [p] *)
val prune_unreferenced : bil -> bil

(** [normalize_negatives p] transform [x + y] to [x - abs(y)] if [y < 0] *)
val normalize_negatives : bil -> bil

(** [substitute x y p] substitutes each occurrence of expression [x] by
    expression [y] in program [p] *)
val substitute : exp -> exp -> bil -> bil

(** [fold_consts] evaluate constant expressions.
    Note: this function performs only one step, and has no loops,
    it is supposed to be run using a fixpoint combinator.
*)
val fold_consts : bil -> bil

(** [constant_folder] is a class that implements the [fold_consts]  *)
class constant_folder : mapper

(** [fixpoint f] applies transformation [f] until fixpoint is
    reached. If the transformation orbit contains non-trivial
    cycles, then a arbitrary point of cycle will be returned. *)
val fixpoint : (bil -> bil) -> (bil -> bil)
