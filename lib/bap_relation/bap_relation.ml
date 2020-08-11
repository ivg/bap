open Base

type ('k,'d) t = Rel : {
    vals : ('k, 'd list, _) Map.t;
    keys : ('d, 'k list, _ ) Map.t;
  } -> ('k,'d) t

let empty (type key) (type data) compare_key compare_data =
  let module D = struct
    type t = data
    include Comparator.Make(struct
        type t = data
        let compare = compare_data
        let sexp_of_t _ = Sexp.List []
      end)
  end in
  let module K = struct
    type t = key
    include Base.Comparator.Make(struct
        type t = key
        let compare = compare_key
        let sexp_of_t _ = Sexp.List []
      end)
  end in
  Rel {
    vals = Map.empty (module K);
    keys = Map.empty (module D);
  }

let add (Rel {vals; keys}) key value = Rel {
    vals = Map.add_multi vals key value;
    keys = Map.add_multi keys value key;
  }

type ('k,'s) non_injective =
  | Non_injective_fwd of 'k list * 's
  | Non_injective_bwd of 's list * 'k

let skips _ _ x = x
let skipu _ x = x

let matching (Rel {vals; keys}) ?(saturated=skips) ?(unmatched=skipu) init =
  Map.fold ~init vals ~f:(fun ~key ~data:vals init ->
      match vals with
      | [] -> assert false
      | _ :: _ :: _ ->
        unmatched (Non_injective_bwd (vals,key)) init
      | [s] -> match Map.find keys s with
        | Some [_] -> saturated key s init
        | None -> assert false
        | Some xs ->
          unmatched (Non_injective_fwd (xs,s)) init)


let fold (Rel {vals}) ~init ~f =
  Map.fold vals ~init ~f:(fun ~key:left ~data:rights init ->
      List.fold ~init rights ~f:(fun init right -> f left right init))

let iter rels ~f = fold rels ~init:() ~f:(fun x s () -> f x s)

let is_empty (Rel {vals}) = Map.is_empty vals
let findl (Rel {vals}) = Map.find_multi vals
let findr (Rel {keys}) = Map.find_multi keys
let mem (Rel {vals; keys}) x s = Map.mem vals x && Map.mem keys s
