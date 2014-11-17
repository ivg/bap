open Core_kernel.Std

let of_bin_file = Bap.Std.Image.create
(* let of_binfile bin =
  let ic = open_in_bin bin in
  let buf = String.create (in_channel_length ic) in
  let () = really_input ic buf 0 (String.length buf) in
  let () = close_in ic in
  Elf_container.load_executable buf
*)

let to_code_segments container : (Exec_container.addr * string) list =
  let sections = Exec_container.Reader.get_sections container in
  List.fold_left (
    fun res {Exec_container.permissions=permissions;
             Exec_container.data=data;
             Exec_container.start_addr=start_addr
            } ->
      if List.mem Exec_container.X permissions then
        (start_addr, data) :: res
      else
        res
  ) [] sections
