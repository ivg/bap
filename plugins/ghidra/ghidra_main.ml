open Core_kernel
open Bap_main

module Default = Bap_ghidra_config

let path =
  Extension.Configuration.parameter
    Extension.Type.(path =? Default.ghidra_path) "path"
    ~doc:"The installation path of Ghidra (that contains ghidraRun)"
