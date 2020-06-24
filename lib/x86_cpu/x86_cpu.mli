open Bap.Std

(** IA32 Architecture Registers.

    For simplicity we're using the same names for registers in
    32 and 64 mode. For example, the A register, has a name [rax] on
    both 32-bit and 64-bit processors. However, on the former it is
    32-bit (contrary to the name), and on the latter it is 64-bit.
*)
module IA32 : sig
  include Bap.Std.CPU

  val flags : Var.Set.t
  (** [flags] is a set of flag registers  *)

  val rbp : var
  (** base pointer  *)

  val rsp : var
  (** stack pointer  *)

  val rsi : var
  (** source index  *)

  val rdi : var
  (** destination index  *)

  val rip : var
  (** instruction pointer  *)

  val rax : var
  (** accumulator register  *)

  val rbx : var
  (** base register  *)

  val rcx : var
  (** counter register  *)

  val rdx : var
  (** data register  *)

  val ymms : var array
  (** YMM registers that are available *)
end

(** AMD64 registers  *)
module AMD64 : sig
  include Bap.Std.CPU

  include module type of IA32

  val r : var array
  (** r8-r15 registers.
      Due to a legacy issues r.(0) -> r8, r.(1) -> r8, ... *)
end
