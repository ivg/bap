open Format
(*
   Since Z.t is able to store itself in OCaml's int when the stored
   value fits into the OCaml representation (i.e., into 63 bits on
   a 64 bit system), we're going to store our meta information (the
   size or modulo of the word) directly in the Z.t representation.
   In order to maximize the used size we will use a prefixed notation,
   which will store the most common sizes (1,8,32,64) more
   efficiently, than an arbitrary size. If the prefix (aka tag) is 0
   then the bitvector has one of the ordinary types (1,8,32,64),
   denoted by the next 2 bits. Otherwise, if the tag is one, then it
   has an arbitrary size encoded in the next 14 bits. However, this
   14 bits of size may contain 5 anomalies:
   - the zero size
   - one of the four ordinary types.

   This five values of the first 15 bits (tag=1,size={0,1,8,32,64}),
   are reserved for special values, denoted as special(0), special(1),
   special(32), and special(64), and they use a different format, with
   the size field moved to the payload, and the payload itself made
   optional.


     ordinary:
     +---------------+----+-+
     |  payload      |type|0|
     +---------------+----+-+
      size+4        3 2  1 0


     arbitrary:
     +-----------+--------+-+
     |  payload  |  size  |1|
     +-----------+--------+-+
      size+15  15 14     1 0


     special(x):
     +-----------+--------+-+
     |   size    |    x   |1|
     +-----------+--------+-+
      28       15 14     1 0


   where the [type] field has the following encoding:
     - 00: bool (1-bit vector)
     - 01: int8 (8-bit vector)
     - 10: int32 (32-bit vector)
     - 11: int64 (64-bit vector)

   and x = {0,1,8,32,64}

   The detailed semanitcs of [x] in special(x) is yet to be defined,
   but it is guaranteed that any operation on a special value will
   produce a special value.


   Note: this representation has a notable feature - the [false]
   value is denoted by all zeros.
*)
module Bignum = Z
type t = Bignum.t

let maxlen = (1 lsl 14) - 1

let is_ordinary = Z.is_even

let is_ordinary_size = function
  | 1 | 8 | 32 | 64 -> true
  | _ -> false

let is_special x =
  Z.is_odd x &&
  let s = Z.to_int (Z.extract x 1 14) in
  s = 0 || is_ordinary_size s

let metasize x =
  if is_ordinary x then 4
  else if is_special x then 29 else 15

let bitwidth x =
  if is_ordinary x then match Z.to_int (Z.extract x 1 2) with
    | 0b00 -> 1
    | 0b01 -> 8
    | 0b10 -> 32
    | 0b11 -> 64
    | _ -> assert false
  else
    let s = Z.to_int (Z.extract x 1 14) in
    if s = 0 || is_ordinary_size s
    then Z.to_int (Z.extract x 15 14)
    else s

let meta x = Z.extract x 0 (metasize x)

let payload ?(signed=false) x =
  let w = bitwidth x in
  let meta = metasize x in
  if signed
  then Z.signed_extract x meta w
  else Z.extract x meta w

let with_payload x v =
  let w = bitwidth x in
  let v = Z.(extract v 0 w lsl metasize x) in
  Z.(v lor meta x)

let z = payload
let with_z = with_payload

let pack data dbits meta mbits  =
  let data = Z.extract data 0 dbits in
  Z.((data lsl mbits) lor meta)

let create data width =
  if width > maxlen
  then invalid_arg @@
    "Bitvector.create: Overflow, the maximum number of bits is "
    ^ string_of_int maxlen ;
  if width <= 0
  then invalid_arg "A nonpositive width is specified (%s,%d)";
  match width with
  | 01 -> pack data width (Z.of_int 0b00) 3
  | 08 -> pack data width (Z.of_int 0b01) 3
  | 32 -> pack data width (Z.of_int 0b10) 3
  | 64 -> pack data width (Z.of_int 0b11) 3
  | _ ->  pack data width (Z.of_int width) 15

let unsigned x = create (z x) (bitwidth x)
let hash x = Z.hash (z x)
let bits_of_z x = Z.to_bits (z x)
let unop op t = op (z t)
let binop op t1 t2 = op (z t1) (z t2)
let lift1 op t = create (unop op t) (bitwidth t)
let lift2 op t1 t2 = create (binop op t1 t2) (bitwidth t1)

let lift2_triple op t1 t2 : t * t * t =
  let (a, b, c) = binop op t1 t2 in
  let w = bitwidth t1 in
  create a w, create b w, create c w

let pp_generic
    ?(case:[`lower|`upper]=`upper)
    ?(prefix:[`auto|`base|`none|`this of string]=`auto)
    ?(suffix:[`full|`none|`size]=`none)
    ?(format:[`hex|`dec|`oct|`bin]=`hex) ppf x =
  let width = bitwidth x in
  let is_signed = is_signed x in
  let is_negative = Z.compare (z x) Z.zero < 0 in
  let x = Z.abs (z x) in
  let word = Z.of_int in
  let int  = Z.to_int in
  let base = match format with
    | `dec -> word 10
    | `hex -> word 0x10
    | `oct -> word 0o10
    | `bin -> word 0b10 in
  let pp_prefix ppf = match format with
    | `dec -> ()
    | `hex -> fprintf ppf "0x"
    | `oct -> fprintf ppf "0o"
    | `bin -> fprintf ppf "0b" in
  if is_negative then fprintf ppf "-";
  let () = match prefix with
    | `none -> ()
    | `this x -> fprintf ppf "%s" x
    | `base -> pp_prefix ppf
    | `auto ->
      if Z.compare x (Z.min (word 10) base) >= 0
      then pp_prefix ppf in
  let fmt = format_of_string @@ match format, case with
    | `hex,`upper -> "%X"
    | `hex,`lower -> "%x"
    | _ -> "%d" in
  let rec print x =
    let d = int Z.(x mod base) in
    if x >= base
    then print Z.(x / base);
    fprintf ppf fmt d in
  print x;
  match suffix with
  | `full -> fprintf ppf ":%d%c" width (if is_signed then 's' else 'u')
  | `size -> fprintf ppf ":%d" width
  | `none -> ()

let compare l r =
  let s = compare (bitwidth l) (bitwidth r) in
  if s <> 0 then s
  else match is_signed l, is_signed r with
    | true,true | false,false -> Bignum.compare (z l) (z r)
    | true,false -> Bignum.compare (z l) (z (signed r))
    | false,true -> Bignum.compare (z (signed l)) (z r)

let pp_full ppf = pp_generic ~suffix:`full ppf
let pp = pp_full

let to_string x =
  let z = z x in
  match bitwidth x with
  | 1 -> if Z.equal z Z.zero then "false" else "true"
  | _ -> asprintf "%a" pp_full x

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let of_suffixed stem suffix =
  let z = Bignum.of_string stem in
  let sl = String.length suffix in
  if sl = 0
  then invalid_arg "Bitvector.of_string: an empty suffix";
  let chop x = String.sub x 0 (sl - 1) in
  match suffix.[sl-1] with
  | 's' -> create z (int_of_string (chop suffix)) |> signed
  | 'u' -> create z (int_of_string (chop suffix))
  | x when is_digit x -> create z (int_of_string suffix)
  | _ -> invalid_arg "Bitvector.of_string: invalid prefix format"


let of_string = function
  | "false" -> create Bignum.zero 1
  | "true"  -> create Bignum.one  1
  | s -> match String.split_on_char ':' s with
    | [z; n] -> of_suffixed z n
    | _ -> failwith ("Bitvector.of_string: " ^ s)

let extract ?hi ?(lo=0) t =
  let n = bitwidth t in
  let z = z t in
  let hi = match hi with
    | Some hi -> hi
    | None -> n - 1 in
  let len = hi-lo+1 in
  if len <= 0
  then failwith ("Bitvector.extract: len is negative: " ^ string_of_int len);
  create (Z.extract z lo len) len


module Cons = struct
  let b0 = create (Bignum.of_int 0) 1
  let b1 = create (Bignum.of_int 1) 1
  let of_bool v = if v then b1 else b0
  let of_int32 ?(width=32) n = create (Bignum.of_int32 n) width
  let of_int64 ?(width=64) n = create (Bignum.of_int64 n) width
  let of_int ~width v = create (Bignum.of_int v) width
  let ones  n = of_int (-1) ~width:n
  let zeros n = of_int (0)  ~width:n
  let zero  n = of_int 0    ~width:n
  let one   n = of_int 1    ~width:n
end
include Cons

let to_int = unop Bignum.to_int
let to_int32 = unop Bignum.to_int32
let to_int64 = unop Bignum.to_int64

let reversed_string s = match String.length s with
  | 0 | 1 -> s
  | n -> String.init n (fun p -> s.[n - p - 1])

let of_binary ?width ?(reversed=false) num  =
  (* Zarith's of_bits always interpret bytes in little endian *)
  let num = if reversed then num else reversed_string num in
  let w = match width with
    | Some w -> w
    | None -> String.length num * 8 in
  create (Bignum.of_bits num) w

let nsucc t n = with_z t Bignum.(z t + of_int n)
let npred t n = with_z t Bignum.(z t - of_int n)

let (++) t n = nsucc t n
let (--) t n = npred t n
let succ n = n ++ 1
let pred n = n -- 1

let gcd    = lift2 Bignum.gcd
let lcm    = lift2 Bignum.lcm
let gcdext = lift2_triple Bignum.gcdext

let concat x y =
  let w = bitwidth x + bitwidth y in
  let x = Bignum.(z x lsl bitwidth y) in
  let z = Bignum.(x lor z y) in
  create z w

let (@.) = concat

let succ = lift1 Bignum.succ
let pred = lift1 Bignum.pred
let abs  = lift1 Bignum.abs
let neg  = lift1 Bignum.neg
let lnot = lift1 Bignum.lognot
let logand = lift2 Bignum.logand
let logor  = lift2 Bignum.logor
let logxor = lift2 Bignum.logxor
let add    = lift2 Bignum.add
let sub    = lift2 Bignum.sub
let mul    = lift2 Bignum.mul
let sdiv   = lift2 Bignum.div
let udiv   = lift2 Bignum.ediv
let srem   = lift2 Bignum.rem
let urem   = lift2 Bignum.erem

let sign_disp ~signed ~unsigned x y =
  let op = if is_signed x || is_signed y then signed else unsigned in
  op x y

let div = sign_disp ~signed:sdiv ~unsigned:udiv
let rem = sign_disp ~signed:srem ~unsigned:urem
let modulo  = rem

let shift dir x n = create (dir (z x) (Z.to_int (z n))) (bitwidth x)
let lshift = shift Bignum.shift_left
let rshift = shift Bignum.shift_right
let arshift x y = shift Bignum.shift_right (signed x) y
let is_zero = unop Bignum.(equal zero)
let is_one = unop Bignum.(equal one)


module Syntax = struct
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( ~-) = neg
  let (mod) = modulo
  let (land) = logand
  let (lor) = logor
  let (lxor) = logxor
  let (lsl) = lshift
  let (lsr) = rshift
  let (asr) = arshift
end


let pp_hex ppf = pp_generic ppf
let pp_dec ppf = pp_generic ~format:`dec ppf
let pp_oct ppf = pp_generic ~format:`oct ppf
let pp_bin ppf = pp_generic ~format:`bin ppf

let pp_hex_full ppf = pp_generic ~suffix:`full ppf
let pp_dec_full ppf = pp_generic ~format:`dec ~suffix:`full ppf
let pp_oct_full ppf = pp_generic ~format:`oct ~suffix:`full ppf
let pp_bin_full ppf = pp_generic ~format:`bin ~suffix:`full ppf

let string_of_value ?(hex=true) x =
  if hex
  then asprintf "%a" (fun p -> pp_generic ~prefix:`none ~case:`lower p) x
  else asprintf "%a" (fun p -> pp_generic ~format:`dec p) x
