open Core_kernel
open Bap.Std
open Arm_types

val spsr : var
(** [spsr]  Saved Processor Status Register *)

val cpsr : var
(** [cpsr] Current Processor Status Register  *)

val nf : var
(** [nf] Negative Flag  *)

val zf : var
(** [zf] Zero Flag  *)

val cf : var
(** [cf] Carry Flag  *)

val vf : var
(** [vf] oVerfrlow Flag  *)

val qf : var
(** [qf] underflow (saturation) Flag  *)

val ge : var array
(** [ge] array of general registers  *)

val itstate : var
(** [itstate] ITSTATE register  *)

val lr : var
(** [lr] Link Register   *)

val pc : var
(** [pc] Program Counter  *)

val sp : var
(** [sp] Stack Pointer  *)

val r0 : var
(** general purpose register  *)

val r1 : var
(** general purpose register  *)

val r2 : var
(** general purpose register  *)

val r3 : var
(** general purpose register  *)

val r4 : var
(** general purpose register  *)

val r5 : var
(** general purpose register  *)

val r6 : var
(** general purpose register  *)

val r7 : var
(** general purpose register  *)

val r8 : var
(** general purpose register  *)

val r9 : var
(** general purpose register  *)

val r10 : var
(** general purpose register  *)

val r11 : var
(** general purpose register  *)

val r12 : var
(** general purpose register  *)

val of_reg : reg -> var
(** [of_reg arm_reg] lifts arm register into BIL variable  *)

val new_var : string -> var
(** [new_var name] creates a freshly new variable prefixed with [name]  *)

val mem : var
(** [mem] BIL variable that denotes the system memory.  *)
