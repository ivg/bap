open Bap_main
open Bap.Std
open Bap_core_theory
open KB.Syntax
module CT = Theory

include Bap_main.Loggers()

module Target = Bap_riscv_target
module Dis = Disasm_expert.Basic




(* the same target may have different encodings
   or share encodings with other targets, in addition,
   the encoding may differ per each instruction and even
   be context-dependent, therefore target and encodings
   are separate properties of different classes. In our
   case, everything is trivial.
*)
let provide_decoding () =
  KB.promise CT.Label.encoding @@ fun label ->
  CT.Label.target label >>| fun t ->
  if CT.Target.belongs Target.parent t
  then if Theory.Target.belongs Target.riscv64 t
    then Target.llvm64
    else Target.llvm32
  else CT.Language.unknown


let enable_llvm encoding triple =
  Dis.register encoding @@ fun _ ->
  Dis.create ~attrs:"+a,+c,+d,+m" ~backend:"llvm" triple

let enable_loader () =
  let request_arch doc =
    let open Ogre.Syntax in
    match Ogre.eval (Ogre.request Image.Scheme.arch) doc with
    | Error _ -> assert false
    | Ok arch -> arch in
  KB.promise CT.Unit.target @@ fun unit ->
  KB.collect Image.Spec.slot unit >>| request_arch >>| function
  | Some "riscv64" -> Target.riscv64
  | Some "riscv32" -> Target.riscv32
  | _ -> CT.Target.unknown


let main _ctxt =
  enable_llvm Target.llvm64 "riscv64";
  enable_llvm Target.llvm32 "riscv32";
  enable_loader ();
  provide_decoding ();
  Ok ()

(* semantic tags that describe what our plugin is providing,
   setting them is important not only for introspection but
   for the proper function of the cache subsystem.
*)
let provides = [
  "riscv";
  "riscv64";
  "riscv32";
]

(* finally, let's register our extension and call the main function  *)
let () = Bap_main.Extension.declare main
    ~provides
