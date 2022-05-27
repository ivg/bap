(** C Type System.

    We represent a C type structurally, i.e., the type representation
    is self-containted and doesn't require any typing requirement.

    Polymorphic variants are used to represent C type constructors and
    type groups.

    The type system is extended with attributes, i.e., it is possible
    to attach attributes of the form [attr(args)] to C type
    declarations.
*)
open Core_kernel[@@warning "-D"]

type char =
  [ `schar | `char | `uchar]
[@@deriving bin_io,compare,sexp,enumerate]

type short =
  [`sshort | `ushort]
[@@deriving bin_io,compare,sexp,enumerate]

type cint =
  [`uint | `sint]
[@@deriving bin_io,compare,sexp,enumerate]

type long =
  [`slong | `ulong]
[@@deriving bin_io,compare,sexp,enumerate]

type long_long =
  [`slong_long | `ulong_long]
[@@deriving bin_io,compare,sexp,enumerate]

type signed =
  [`schar | `sshort | `sint | `slong | `slong_long]
[@@deriving bin_io,compare,sexp,enumerate]

type unsigned =
  [`bool | `uchar | `ushort | `uint | `ulong | `ulong_long]
[@@deriving bin_io,compare,sexp,enumerate]

type enum =
  [`enum of (string * int64 option) list]
[@@deriving bin_io,compare,sexp]

type integer =
  [char | signed | unsigned | enum]
[@@deriving bin_io,compare,sexp]

type real =
  [`float | `double | `long_double]
[@@deriving bin_io,compare,sexp,enumerate]

type complex =
  [`cfloat | `cdouble | `clong_double]
[@@deriving bin_io,compare,sexp,enumerate]

type floating = [real | complex]
[@@deriving bin_io,compare,sexp,enumerate]

type basic = [integer | floating]
[@@deriving bin_io,compare,sexp]


type cv = unit [@@deriving bin_io,compare,sexp]
type cvr = Bool.t [@@deriving bin_io,compare,sexp]

module Qualifier = struct
  type 'a t = {
    const : Bool.t;
    volatile : Bool.t;
    restrict : 'a;
  } [@@deriving bin_io,compare,sexp]
end

type 'a qualifier = 'a Qualifier.t [@@deriving bin_io, compare, sexp]

module Attr = struct
  type t = {
    name : string;
    args : string list [@sexp.list];
  } [@@deriving bin_io, compare, sexp]
end

type attr = Attr.t
[@@deriving bin_io, compare, sexp]

module Spec = struct
  type ('a,'b) t = {
    qualifier : 'a;
    t : 'b;
    attrs : attr list [@sexp.list];
  } [@@deriving bin_io, compare, sexp]

end

type ('a,'b) spec = ('a,'b) Spec.t
[@@deriving bin_io, compare, sexp]

type no_qualifier = [`no_qualifier]
[@@deriving bin_io, compare, sexp]

module Proto = struct
  type 'a t = {
    return : 'a;
    args   : (string * 'a) list;
    variadic : Bool.t;
  } [@@deriving bin_io, compare, sexp]
end

module Compound = struct
  type 'a t = {
    name : string;
    fields : (string * 'a) list;
  } [@@deriving bin_io, compare, sexp]
end

module Array = struct
  type 'a t = {
    element : 'a;
    size : Int.t option
  } [@@deriving bin_io, compare, sexp]
end

type t = [
  | `Void
  | `Basic     of (cv  qualifier, basic) spec
  | `Pointer   of (cvr qualifier, t) spec
  | `Array     of (cvr qualifier, array) spec
  | `Structure of (no_qualifier,  compound) spec
  | `Union     of (no_qualifier,  compound) spec
  | `Function  of (no_qualifier,  proto) spec
] [@@deriving bin_io, compare, sexp]
and proto     = t Proto.t [@@deriving bin_io, compare, sexp]
and compound  = t Compound.t [@@deriving bin_io, compare, sexp]
and array     = t Array.t [@@deriving bin_io, compare, sexp]


type scalar = [
  | `Basic of (cv qualifier,basic) spec
  | `Pointer of (cvr qualifier, t) spec
] [@@deriving bin_io, compare, sexp]

type aggregate = [
  | `Array of (no_qualifier, t) spec
  | `Structure of (no_qualifier, t list) spec
] [@@deriving bin_io, compare, sexp]

let attrs : t -> attr list = function
  | `Void -> []
  | `Basic {attrs}
  | `Pointer {attrs}
  | `Array {attrs}
  | `Structure {attrs}
  | `Union {attrs}
  | `Function {attrs} -> attrs

let is_const : t -> Bool.t = function
  | `Void | `Union _ | `Structure _ | `Function _ -> false
  | `Basic {qualifier={const}}
  | `Array {qualifier={const}}
  | `Pointer {qualifier={const}} -> const

let is_volatile : t -> Bool.t = function
  | `Void | `Union _ | `Structure _ | `Function _ -> false
  | `Basic {qualifier={volatile}}
  | `Array {qualifier={volatile}}
  | `Pointer {qualifier={volatile}} -> volatile

let is_restrict : t -> Bool.t = function
  | `Void | `Union _ | `Structure _ | `Function _ | `Basic _ -> false
  | `Array {qualifier={restrict}}
  | `Pointer {qualifier={restrict}} -> restrict


let is_void = function `Void -> true | _ -> false

let qualifier ?(const=false) ?(volatile=false) restrict =
  Qualifier.{const; volatile; restrict}

let basic ?(attrs=[]) ?const ?volatile t : t =
  `Basic {
    t;
    attrs;
    qualifier = qualifier ?const ?volatile ();
  }

let is_basic : t -> Bool.t =
  function `Basic _ -> true |  _ -> false

let is_char : t -> Bool.t =
  function `Basic {t=#char} -> true | _ -> false

let is_short : t -> Bool.t =
  function `Basic {t=#short} -> true | _ -> false

let is_cint : t -> Bool.t =
  function `Basic {t=#cint} -> true | _ -> false

let is_signed : t -> Bool.t =
  function `Basic {t=#signed} -> true | _ -> false

let is_unsigned : t -> Bool.t =
  function `Basic {t=#unsigned} -> true | _ -> false

let is_enum : t -> Bool.t =
  function `Basic {t=#enum} -> true | _ -> false

let is_integer : t -> Bool.t =
  function `Basic {t=#integer} -> true | _ -> false

let is_real : t -> Bool.t =
  function `Basic {t=#real} -> true | _ -> false

let is_complex : t -> Bool.t =
  function `Basic {t=#complex} -> true | _ -> false

let is_floating : t -> Bool.t =
  function `Basic {t=#floating} -> true | _ -> false

let pointer ?(attrs=[]) ?const ?volatile ?(restrict=false) t : t =
  `Pointer {
    t;
    attrs;
    qualifier = qualifier ?const ?volatile restrict;
  }

let is_pointer : t -> Bool.t = function `Pointer _ -> true | _ -> false

let array ?(attrs=[]) ?const ?volatile ?(restrict=false) ?size t : t =
  `Array {
    t = Array.{element=t; size};
    attrs;
    qualifier = qualifier ?const ?volatile restrict;
  }

let is_array : t -> Bool.t = function `Array _ -> true | _ -> false

let structure ?(attrs=[]) name fields : t =
  `Structure {
    t = Compound.{name; fields};
    attrs;
    qualifier = `no_qualifier;
  }

let is_structure : t -> Bool.t = function `Structure _ -> true | _ -> false


let union ?(attrs=[]) name fields : t =
  `Union {
    t = Compound.{name; fields};
    attrs;
    qualifier = `no_qualifier;
  }

let is_union : t -> Bool.t = function `Union _ -> true | _ -> false

let function_ ?(attrs=[]) ?(variadic=false) ?(return=`Void) args : t =
  `Function {
    t = Proto.{return; args; variadic};
    attrs;
    qualifier = `no_qualifier;
  }

let is_function : t -> Bool.t = function `Function _ -> true | _ -> false
