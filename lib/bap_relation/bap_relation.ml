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

type ('k,'d) reason =
  | Non_injective_key of 'd * 'k list
  | Non_injective_value of 'd list

let skip _ _ x = x

let matching (Rel {vals; keys}) ?(saturated=skip) ?(unmatched=skip) init =
  Map.fold ~init vals ~f:(fun ~key ~data:vals init ->
      match vals with
      | [] -> assert false
      | _ :: _ :: _ ->
        unmatched key (Non_injective_value vals) init
      | [s] -> match Map.find keys s with
        | Some [_] -> saturated key s init
        | None -> assert false
        | Some xs ->
          unmatched key (Non_injective_key (s,xs)) init)
