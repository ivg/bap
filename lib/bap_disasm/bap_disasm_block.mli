(** A basic block  *)

(** {4 Navigating blocks structure}

    The following functions allows you to navigate through blocks
    without explicitly using graphs. Each neighborhood function
    returns closest neighbors as a lazy sequence. Please, be cautious,
    since this can contain loops, i.e. block can contain itself as a
    predecessor.

    You can use [Block.compare] or [compare_block] functions, to safely
    compare blocks with each other, without a risk of non-termination.
*)

(** returns a sequence of instructions that was decoded.
    Note, the sequence is not guaranteed to be contigious, instructions
    that wasn't decoded will be skipped.
*)
open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_basic

type t with compare, sexp_of
type insn = Bap_disasm_insn.t with compare,bin_io,sexp

(** block destinations  *)
type dest = [
  | `Block of t * [
      | `Jump     (** unconditional jump                  *)
      | `Cond     (** conditional jump                    *)
      | `Fall     (** a pseudo jump to a next instruction *)
    ]
  | `Unresolved of [`Jump | `Cond ]
] with compare, sexp_of

(** [addr block] address of the first instruction  *)
val addr : t -> addr

(** [memory blk] memory region, occupied by a block*)
val memory : t -> mem

(** [leader blk] the first instruction *)
val leader : t -> insn

(** [terminator blk] last instruction of the block  *)
val terminator : t -> insn

(** [insns blk] returns a list of block instructions  *)
val insns : t -> (mem * insn) seq

(** [dests blk] block immediate destinations including unresolved
    one *)
val dests : t -> dest seq

(** [succs blk] block immediate successors  *)
val succs : t -> t seq
(** [preds blk] block immediate predecessors  *)
val preds : t -> t seq

(** all the printing stuff, including [to_string] function *)
include Printable with type t := t

(** lifting from a lower level  *)
val of_rec_block : Bap_disasm_rec.block -> t
