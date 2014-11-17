open Core_kernel.Std

module type M = sig
  type 'a t
  type key
  val create : unit -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val add : 'a t -> key -> 'a -> unit
  val replace : 'a t -> key -> 'a -> unit
  val find : 'a t -> key -> 'a option
  val format : key -> string
end

module type TRIE = sig
  type 'a t
  type key
  val init : 'a -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val find : 'a t -> key -> 'a
  val output : 'a t -> string -> ('a -> string) -> unit
end

module MakeM (Key : Hashtbl.Key) : (M with type key = Key.t) = struct
  module Table = Hashtbl.Make(Key)
  type 'a t = 'a Table.t
  type key = Table.key
  let create ()= Table.create ()
  let iter f map = Table.iter map ~f:(fun ~key ~data -> f key data)
  let add map key data = Table.replace map ~key:key ~data:data
  let find = Table.find
  let replace = add
  let format a = Sexp.to_string_hum (Key.sexp_of_t a)
end

module Make (M : M) : (TRIE with type key = M.key list) = struct
  type key = M.key list
  type 'a t = Node of 'a * 'a t M.t

  let init v = Node (v, M.create ())

  (* add : 'a t -> key -> 'a -> unit *)
  let rec add trie k v = match k with
    | [] -> ()
    | hd :: [] -> (
        match trie with
        | Node (_, m) ->
          match (M.find m hd) with
            | Some (Node (_, sub)) -> M.replace m hd (Node (v, sub))
            | None ->
              (* If this is a new node, add to its father node's map *)
              let subtrie_init = init v in
              M.add m hd subtrie_init
      )
    | hd :: tl ->
      match trie with
      | Node (_, m) ->
        let subtrie = match M.find m hd with
          | Some s -> s
          | None ->
            (* If this is a new node, add to its father node's map *)
            let subtrie_init = init v in
            M.add m hd subtrie_init;
            subtrie_init in
        add subtrie tl v

  (* find : 'a t -> key -> 'a -> 'a *)
  (* find : return the longest match *)
  let find trie k =
    let rec rec_find trie k t_v = match k with
      | [] -> t_v
      | hd :: tl ->
        match trie with
        | Node (_, m) ->
          match M.find m hd with
          | Some (Node (v, _) as subtrie) ->
            rec_find subtrie tl v
          | None -> t_v in
    let root_v = match trie with
      | Node (v, _) -> v in
    rec_find trie k root_v

  (* output : 'a t -> string -> ('a -> string) -> unit *)
  let output trie file format_v =
    let oc = open_out file in
    let rec rec_output prefix = function
      | Node (v, m) ->
        let s = Printf.sprintf "%s->%s\n"
                  (String.concat ~sep:";" (List.rev prefix))
                  (format_v v) in
        Out_channel.output_string oc s;
        M.iter (fun k v ->
            rec_output (M.format k :: prefix) v
        ) m in
    rec_output [] trie;
    Out_channel.close oc
end
