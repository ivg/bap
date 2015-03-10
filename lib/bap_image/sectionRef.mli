(** Signle section in the list of sections int the object file *)

open Core_kernel.Std

type t

external get_name : t -> string Or_error.t =
  "section_ref_get_name_stub"

external get_address : t -> Int64.t Or_error.t =
  "section_ref_get_address_stub"

external get_alignment : t -> Int64.t Or_error.t =
  "section_ref_get_alignment_stub"

external get_size : t -> Int64.t Or_error.t =
  "section_ref_get_size_stub"

external is_text : t -> bool Or_error.t =
  "section_ref_is_text"

external is_data : t -> bool Or_error.t =
  "section_ref_is_data"

external is_bss : t -> bool Or_error.t =
  "section_ref_is_bss"

external is_required_for_execution : t -> bool Or_error.t =
  "section_ref_is_required_for_execution"

external is_virtual : t -> bool Or_error.t =
  "section_ref_is_virtual"

external is_zero_init : t -> bool Or_error.t =
  "section_ref_is_zero_init"

external is_read_only_data : t -> bool Or_error.t =
  "section_ref_is_read_only_data"
