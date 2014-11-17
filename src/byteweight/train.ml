open Core_kernel.Std
open Bap.Std
(* TODO:pass mode module *)
module Sigs = Byte.Table
let usage = "Train: ./train -bin-dir [test binary directory] -sig [output signature file]"
let d_bin = ref None
let sig_out = ref None
let k = 10

let arg_specs =
  ("-bin-dir", Arg.String(fun s -> d_bin := Some s), "train binary directory")
  :: ("-sig", Arg.String(fun s -> sig_out := Some s), "output signature file")
  :: []

let anon_fun _ = raise (Arg.Bad usage)

let parse_command =
  Arg.parse arg_specs anon_fun usage;
  match !d_bin, !sig_out with
  | Some d, Some out -> d, out
  | _ -> raise (Arg.Bad usage)

let funs_of_section img sec =
  let syms = Image.symbols_of_section img sec in
  Sequence.filter_map syms ~f:(fun s ->
    if Image.Sym.is_function s then None
    else
      let mem, mem_seq = Image.memory_of_symbol img s in
      let fs =
        let a = Memory.min_addr mem in
        let a_min =
          let a_seq = Sequence.map mem_seq ~f:Memory.min_addr in
          Sequence.min_elt a_seq ~cmp:Addr.compare
        in
        match a_min with
        | Some aa -> Addr.min a aa
        | None -> a
      in
      Some fs
  )

let build_sigs imgs : 'a Sigs.t =
  let sigs = Sigs.create () in
  List.iter imgs ~f:(fun img ->
    let code_sections =
      Table.filter (Image.sections img) ~f:Image.Sec.is_executable
    in
    Table.iteri code_sections ~f:(fun mem sec ->
      let fsi = funs_of_section img sec in
      Sequence.iter fsi ~f:(fun addr ->
        let keys = Byte.generate_keys mem ~from:addr k in
        List.iter keys ~f:(fun key ->
          (* TODO: include this into Sigs *)
          match Sigs.find sigs key with
          | Some (p, n) ->
              Sigs.replace sigs key (p + 1, n)
          | None ->
              Sigs.add_exn sigs key (1, 0)
        )
      )
    )

  );
  sigs

let update_sigs (sigs:'a Sigs.t) imgs =
  List.iter imgs ~f:(fun img ->
    let code_sections =
      Table.filter (Image.sections img) ~f:Image.Sec.is_executable
    in
    Table.iteri code_sections ~f:(fun mem sec ->
      let fsi = funs_of_section img sec in
      let sec_st = Memory.min_addr mem in
      let sec_nd = Memory.max_addr mem in
      let rec iterate addr =
        if addr >= sec_nd then ()
        else if Sequence.mem fsi addr then
          iterate Addr.(addr ++ 1)
        else
          let keys = Byte.generate_keys mem ~from:addr k in
          List.iter ~f:(fun key ->
            match Sigs.find sigs key with
            | Some (p, n) ->
              Sigs.replace sigs key (p, n + 1)
            | None -> ()
          ) keys;
      in
      iterate sec_st
    )
  )

let train d =
  let bins =
    List.map ~f:(Filename.concat d) (Array.to_list (Sys.readdir d))
  in
  let imgs = List.filter_map bins ~f:(fun bin ->
    match Image.create bin with
    | Ok (img, _) -> Some img
    | Error err ->
      eprintf "Program failed with: \%s\n" @@ Error.to_string_hum err;
      None
  ) in
  let sigs = build_sigs imgs in
  update_sigs sigs imgs;
  sigs

let output sigs file =
  let oc = Out_channel.create ~binary:true file in
  Sigs.iter ~f:(fun ~key:k ~data:(p, n) ->
      Printf.fprintf oc "%s->%d,%d\n"
        (Byte.string_of_key k) p n
    ) sigs;
  Out_channel.close oc

let () =
  let d, out = parse_command in
  let sigs = train d in
  output sigs out
