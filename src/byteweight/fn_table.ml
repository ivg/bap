open Core_kernel.Std
open Bap.Std

module Bitvector_Map = struct
  module T = struct
    type t = Bap_bitvector.t
    let compare = Bap_bitvector.compare
    let of_string _ : t = failwith "Not implemented"
    let to_string = Bap_bitvector.to_string
  end
  include T
  include Sexpable.Of_stringable(T)
  include Comparable.Make(struct
      include T
      include Sexpable.Of_stringable(T)
    end)
end

(* module Addr = Bitvector_Map *)

type arch
type addr = Addr.t with sexp,compare

type path = string

type prob = float with sexp,compare

module Fn = struct
  type t = {
    name : string option;
    addr : addr;
    weight : prob;
  } with fields,sexp,compare

  let create ?name addr weight = {name;addr;weight}
end

type fn = Fn.t with sexp,compare

module Map = struct
  open Addr.Map
  let addrs = keys
  let mem = mem
  let length = length
  let is_empty = is_empty
  let iter map ~f =
    iter map ~f:(fun ~key ~data -> f key data)
  (* let fold = fold *)
  let fold map ~init ~f =
    fold map ~init ~f:(fun ~key ~data a -> f key data a)
  let fold_range map ~min ~max ~init ~f =
    fold_range_inclusive map ~min ~max ~init ~f:(fun ~key ~data a -> f key data a)
  let exists = exists
  let for_all = for_all
  let find = find
  let find_exn = find_exn
  let min_elt = min_elt
  let max_elt = max_elt
  let rank = rank
  let next = next_key
  let prev = prev_key
  let fns = data
  let of_alist_exn = of_alist_exn
end

type t = Fn.t Addr.Map.t

let default_weight_threshold = 0.5

(** output : (addr * fn) list *)
(** This function needs to create Fn module, so I put it inside fn_table.  *)

let decide_signature work_on_byte arch =
  let mode = if work_on_byte then "byte" else "dism" in
  let arch = Bap_arch.to_string arch in
  Printf.sprintf "signatures/%s_%s" arch mode

let debug a w =
  Printf.printf "%s -> %f\n" (Addr.to_string a) w

let fsi segments arch weight_threshold (module LocalMode : Mode.MODE) : t =
  let rec fsi_rec byte_list addr res : (addr * fn) list =
    match byte_list with
    | [] -> res
    | _ :: tl ->
      let key = LocalMode.consecutive ~arch:arch byte_list in
      let weight = LocalMode.find key in
      (* debug addr weight; *)
      if weight > weight_threshold then
        let fn = Fn.create addr weight in
        fsi_rec tl (Addr.(++) addr 1) ((addr, fn) :: res)
      else
        fsi_rec tl (Addr.(++) addr 1) res in
  let fsi_list = Table.foldi segments ~init:[] ~f:(fun mem _ res ->
    let start = Memory.min_addr mem in
    let byte_list = List.rev (String.to_list_rev (Memory.hexdump mem)) in
    fsi_rec byte_list start res
  ) in
  Map.of_alist_exn fsi_list
  (*
    let open List in
    let segments_bl = segments >>| (fun (addr, bytes) ->
    addr, List.rev (String.to_list_rev bytes)) in
    let folded_fsi = segments_bl >>| (fun (addr, byte_list) ->
    (* List.iter byte_list (fun byte -> Printf.printf "%02x " (Char.to_int byte));
    Printf.printf "\n"; *)
    fsi_rec byte_list addr []) in
    Map.of_alist_exn (List.join folded_fsi) *)

let create ?signatures ?(weight_threshold=default_weight_threshold)
    ?(work_on_bytes=false)
    arch
    ~segments
  : t =
  let sig_file = match signatures with
    | Some s -> s
    | None -> decide_signature work_on_bytes arch in
  let mode =
    Byte.load sig_file;
    (module Byte : Mode.MODE) in
    (* if work_on_bytes then (
      Byte.load sig_file;
      (module Byte : Mode.MODE)
    )
    else (
      Dism.load sig_file;
      (module Dism : Mode.MODE)
    )
    *)
  fsi segments arch weight_threshold mode

include Map
