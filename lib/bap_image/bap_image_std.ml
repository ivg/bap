include Image_internal_std
(** bring to scope some basic types like [mem] and [table] *)

module Image = Bap_image
module Segment = Image.Segment
module Symbol = Image.Symbol

type image = Image.t

type symbol = Symbol.t [@@deriving bin_io, compare, sexp]

type segment = Segment.t [@@deriving bin_io, compare, sexp]
