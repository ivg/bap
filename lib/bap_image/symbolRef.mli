(** Single symbol in the list of symbols int the object file *)

open Core_kernel.Std

type t

external get_name : t -> string Or_error.t =
  "symbol_ref_get_name_stub"

external get_address : t -> Int64.t Or_error.t =
  "symbol_ref_get_address_stub"

external get_file_offset : t -> Int64.t Or_error.t =
  "symbol_ref_get_file_offset_stub"

external get_alignment : t -> Int64.t Or_error.t =
  "symbol_ref_get_alignment_stub"

external get_size : t -> Int64.t Or_error.t =
  "symbol_ref_get_size_stub"

type stype =
  | ST_Unknown (* Type not specified *)
  | ST_Data
  | ST_Debug
  | ST_File
  | ST_Function
  | ST_Other

external get_type : t -> stype Or_error.t =
  "symbol_ref_get_type"

external get_flags : t -> Int32.t Or_error.t =
  "symbol_ref_get_flags"
