open Core_kernel
open Bap.Std

module IA32 : Target
(** IA32 target *)

module AMD64 : Target
(** AMD64 target *)

module IA32L : Target
(** IA32 legacy target *)

module AMD64L : Target
(** AMD64 legacy target *)

module IA32M : Target
(** IA32 target has been merged with legacy lifter *)

module AMD64M : Target
(** AMD64 target has been merged with legacy lifter *)
