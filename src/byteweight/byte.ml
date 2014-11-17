open Core_kernel.Std
open Or_error
open Bap.Std
(* open Mode *)

module Byte : (Mode.MODE with type t = char) = struct
  type t = char
  type key = t list

  module TList = struct
    type t = key
    let to_str = String.of_char_list
    let hash k = String.hash (to_str k)
    let compare a b = String.compare (to_str a) (to_str b)
    let t_of_sexp sexp =
      let str = String.t_of_sexp sexp in
      List.rev (String.to_list_rev str)
    let sexp_of_t k = String.sexp_of_t (to_str k)
  end
  module Table = Hashtbl.Make(TList)
  module Tree = Trie.Make(Trie.MakeM(Char))

  let trie = Tree.init 0.0

  let find = Tree.find trie
  let len = 30
  let generate_keys mem ?from len : key list =
    let max_key_rev =
      let sub_mem =
        match Memory.copy ?from ~words:len mem with
        | Ok m -> m
        | Error _ -> mem
      in
      let hexdump = Memory.hexdump sub_mem in
      String.to_list_rev hexdump
    in
    let rec rec_g res key_rev =
      match key_rev with
      | [] -> List.map res ~f:List.rev
      | _ :: tl ->
        rec_g (key_rev :: res) tl
    in
    rec_g [] max_key_rev


  let read_from_ic ic =
    let to_bytes_score line =
      let words = Str.split (Str.regexp "->") line in
      match words with
      | [bytes_str; counts] ->
        let p, n =
          let p_n = Str.split (Str.regexp ",") counts in
          match p_n with
          | [p;n] -> Float.of_string p, Float.of_string n
          | _ -> failwith "WPT File Format error"
        in
        List.rev (String.to_list_rev bytes_str), (p /. (p +. n))
      | _ -> failwith "WPT File Format error"
    in
    let sigs = ref [] in
    let line = ref "" in
    try
      while true; do
        line := !line ^ (input_line ic);
        match String.substr_index !line "->" with
        | None -> ()
        | Some _ -> (
          let bytes, score = to_bytes_score !line in
          sigs := (bytes, score) :: !sigs;
          line := "" )
      done;
      []
    with End_of_file ->
      In_channel.close ic;
      !sigs

  let load file =
    let ic = open_in_bin file in
    let sigs = read_from_ic ic in
    List.iter sigs (fun (k, v) -> Tree.add trie k v)

  let consecutive ?arch byte_list = List.sub byte_list 0 len
  let string_of_key = TList.to_str
end

include Byte
