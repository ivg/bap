open Core_kernel
open Bap.Std
open Bap_core_theory
open Bap_knowledge

val run : program term -> tid Tid.Map.t
(** [run prog] - returns the mapping from
    stubs to implementations *)
