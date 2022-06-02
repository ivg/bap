open Core_kernel[@@warning "-D"]
open Bap_core_theory
open Bap.Std
open Bap_c.Std

module Arg = C.Abi.Arg
open Arg.Let
open Arg.Syntax

include Self()

let either preds thing = List.exists preds ~f:(fun is -> is thing)
let choice options arg =
  Arg.choice (List.map options ~f:(fun f -> f arg))

let otherwise = Fn.const true

let is_integer =
  either C.Type.[is_integer; is_pointer; is_function]

let is_compound =
  either C.Type.[is_structure; is_union]

let is_sse : C.Type.t -> bool = function
  | `Basic {t=(`float|`double)} -> true
  | _ -> false

let is_csse : C.Type.t -> bool = function
  | `Basic {t=(`cfloat|`cdouble)} -> true
  | _ -> false

let is_x87 : C.Type.t -> bool = function
  | `Basic {t=`long_double} -> true
  | _ -> false


let select arg options =
  List.find_map options ~f:(fun (cnd,action) ->
      if cnd arg then Some (action arg) else None) |> function
  | Some action -> action
  | None -> Arg.reject ()

let seq xs arg = Arg.List.iter xs ~f:(fun x -> x arg)

let skip _ = Arg.return ()

let make_return t k = match t with
  | `Void -> Arg.return ()
  | t ->
    let* size = Arg.size t in
    select t (k size)


let arena ?low t names = Arg.Arena.of_exps @@
  List.map names ~f:(fun name ->
      match Theory.Target.var t name with
      | None -> failwithf "unknown register: %s" name ()
      | Some reg ->
        let reg = Var.reify reg in
        match low with
        | None -> Bil.var reg
        | Some bits -> Bil.(cast low bits (var reg)))

let ia16 memory t =
  let data = new C.Size.base `LP32 in
  C.Abi.define t data @@ fun _ {C.Type.Proto.return=r; args} ->
  let* irets = arena t ["AX"; "DX"] in
  let return r = Arg.choice [
      Arg.registers irets r;
      memory r;
    ] in
  Arg.define ~return:(return r) @@
  Arg.List.iter args ~f:(fun (_,arg) -> memory arg)

let cdecl16 = ia16 Arg.memory

(* pascal or fortran *)
let pascal16 = ia16 Arg.push


let ia32 t k =
  let data = new C.Size.base `ILP32 in
  let is_big size _ = size > 64 in
  C.Abi.define t data @@ fun _ {C.Type.Proto.return=r; args} ->
  let* irets = arena t ["EAX"; "EDX"] in
  let* frets = arena t ["ST0"] in
  let pass = Arg.memory in
  let return r = make_return r @@ fun size -> [
      C.Type.is_real, Arg.register frets;
      is_big size, seq [
        Arg.reference irets;
        Arg.hidden;
      ];
      otherwise, Arg.registers irets;
    ] in
  k @@ fun ?(return=return) ?(pass=pass) () ->
  Arg.define ~return:(return r) @@ Arg.sequence [
    Arg.rebase 1;
    Arg.List.iter args ~f:(fun (_,arg) ->
        pass arg)
  ]

(* stdcall, cdecl, watcom-stack, or ms32 *)
let cdecl t = ia32 t @@ fun accept -> accept ()

let fastcall t = ia32 t @@ fun override ->
  let is_big size _ = size > 32 in
  let* iregs = arena t ["ECX"; "EDX"] in
  let pass arg =
    let* size = Arg.size arg in
    select arg [
      either [
        is_big size;
        C.Type.is_floating;
      ], Arg.memory;
      otherwise, choice [
        Arg.register iregs;
        Arg.memory;
      ]
    ] in
  override ~pass ()

let watcomregs t = ia32 t @@ fun override ->
  let* iregs = arena t ["eax"; "edx"; "ebx"; "ecx"] in
  let pass arg = Arg.choice [
      Arg.register iregs arg;
      Arg.memory arg;
    ] in
  override ~pass ()

(* aka borland register *)
let pascal t = ia32 t @@ fun override ->
  let* iregs = arena t ["eax"; "edx"; "ecx"] in
  let pass arg = select arg [
      either C.Type.[is_cint; is_char; is_pointer;],
      choice Arg.[register iregs; memory];
    ] in
  override ~pass ()


let ms64 t =
  let data = new C.Size.base `LP64 in
  let is_big size _ = size > 64 in
  C.Abi.define t data @@ fun _ {C.Type.Proto.return=r; args} ->
  let* iregs = arena t ["rcx"; "rdx"; "r8"; "r9"] in
  let* irets = arena t ["rax"; "rdx"] in
  let* vregs = arena t ~low:64 @@ List.init 4 ~f:(sprintf "ymm%d") in
  let* vrets = arena t ~low:128 ["ymm0"] in
  let iregs = (iregs,[vregs]) and vregs = (vregs,[iregs])
  and irets = (irets,[]) and vrets = (vrets,[]) in
  let use pass (arena,coarena) arg = Arg.sequence [
      pass arena arg;
      Arg.List.iter coarena ~f:Arg.discard
    ] in
  let pass how arena = choice [
      use how arena;
      Arg.memory;
    ] in
  let args = Arg.List.iter args ~f:(fun (_,t) ->
      let* size = Arg.size t in
      select t [
        is_big size, pass Arg.reference iregs;
        C.Type.is_floating, pass Arg.register vregs;
        otherwise, pass Arg.register iregs;
      ]) in
  let return =
    let* size = Arg.size r in
    select r [
      is_x87, pass Arg.reference iregs;
      C.Type.is_floating, pass Arg.register vrets;
      is_big size, pass Arg.reference iregs;
      otherwise, pass Arg.register irets;
    ] in
  Arg.define ~return args


let merge_kinds k1 k2 = match k1,k2 with
  | `Nil, t | t, `Nil -> t
  | `Int, _ | _, `Int -> `Int
  | `Sse, `Sse -> `Sse

let partition fields =
  List.fold fields ~init:(`Nil,0,[])
    ~f:(fun (k,s,words) (k',s') ->
        if s + s' <= 64
        then merge_kinds k k',s+s',words
        else k',s',k :: words) |>
  fun (k,_,words) -> List.rev (k::words)

let rec classify : C.Type.t -> [`Nil | `Int | `Sse] = function
  | t when is_integer t -> `Int
  | t when is_sse t || is_csse t -> `Sse
  | `Structure {t={fields}}
  | `Union {t={fields}} -> classify_fields fields
  | `Array {t={element}} -> classify element
  | _ -> `Nil
and classify_fields fields =
  List.fold ~init:`Nil fields ~f:(fun k (_,t) ->
      merge_kinds k (classify t))

let sysv t =
  let data = new C.Size.base `LP64 in
  let is_large bits _ = bits > 128 in
  C.Abi.define t data @@ fun _ {C.Type.Proto.return=r; args} ->

  let* iregs = arena t ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"] in
  let* vregs = arena t ~low:64 @@ List.init 8 ~f:(sprintf "ymm%d") in
  let* irets = arena t ["rax"; "rdx"] in
  let* vrets = arena t ~low:64 ["ymm0"; "ymm1"] in

  let union_fields size fields =
    Arg.return [classify_fields fields,size] in
  let rec fields {C.Type.Compound.fields=xs} =
    Arg.List.fold ~init:(0,[]) xs ~f:(fun (off,acc) (_,t) ->
        let alignment = data#alignment t in
        let* size = Arg.size t in
        let size = size + C.Size.padding alignment off in
        let off = off + size in
        match t with
        | t when is_integer t ->
          Arg.return (off, [`Int,size] :: acc)
        | t when is_sse t || is_csse t ->
          Arg.return (off, [`Sse,size] :: acc)
        | `Structure {t} ->
          let+ fields = fields t in
          off, fields :: acc
        | `Union {t={fields}} ->
          let+ fields = union_fields size fields in
          off,fields :: acc
        | `Array {t={C.Type.Array.element; size=Some n}} ->
          let+ size = Arg.size element in
          let kind = classify element in
          off,List.init n ~f:(fun _ -> kind,size) :: acc
        | _ -> Arg.reject ()) >>| fun (_,acc) ->
    List.rev acc |> List.concat in

  let compound_fields : C.Type.t -> _ list Arg.t = function
    | `Structure {t} -> fields t >>| partition
    | `Union {t={fields}} as s ->
      let* size = Arg.size s in
      union_fields size fields >>| partition
    | _ -> Arg.return [] in

  let registers = Arg.registers ~rev:true ~limit:2 in

  let pass_compound memory iregs vregs t =
    Arg.choice [
      compound_fields t >>= begin function
        | [`Int] -> Arg.register iregs t
        | [`Sse] -> Arg.register vregs t
        | [`Int; `Int] -> registers iregs t
        | [`Int; `Sse] -> Arg.split iregs vregs t
        | [`Sse; `Int] -> Arg.split vregs iregs t
        | [`Sse; `Sse] -> registers vregs t
        | _ -> Arg.reject ()
      end;
      memory t;
    ] in

  let args = Arg.List.iter args ~f:(fun (_,arg) ->
      let* bits = Arg.size arg in
      select arg [
        is_large bits, Arg.memory;
        is_integer, Arg.register iregs;
        is_sse, Arg.register vregs;
        is_csse, registers vregs;
        is_compound, pass_compound Arg.memory iregs vregs
      ]) in

  let return = make_return r @@ fun bits -> [
      is_large bits, Arg.reference iregs;
      is_integer, Arg.register irets;
      is_sse, Arg.register vrets;
      is_csse, registers vrets;
      is_compound, pass_compound (Arg.reference iregs) irets vrets;
    ] in
  Arg.define ~return args

let calling_conventions = X86_target.[
    (* 16-bit ABI *)
    [i86; i186; i286], [
      Abi.cdecl, cdecl16;
      Abi.pascal, pascal16;
      Abi.fortran, pascal16;
    ];

    (* 32-bit ABI  *)
    [i386; i486; i586; i686], [
      Abi.cdecl, cdecl;
      Abi.pascal, pascal;
      Abi.fastcall, fastcall;
      Abi.stdcall, cdecl;
      Abi.watcomstack, cdecl;
      Abi.ms, cdecl;
    ];

    (* 64-bit ABI *)
    [amd64], [
      Abi.ms, ms64;
      Abi.sysv, sysv
    ]
  ]

let name_with_abi target abi =
  Format.asprintf "%s-%s"
    (KB.Name.unqualified (Theory.Target.name target))
    (KB.Name.unqualified (Theory.Abi.name abi))

let register_target parent abi install =
  install @@ Theory.Target.declare ~package:"bap"
    (name_with_abi parent abi)
    ~parent ~abi


let default_calling_conventions = X86_target.[
    [i86], cdecl16;
    [i386; i486; i586; i686], cdecl;
    [amd64], sysv;
  ]

let install_calling_conventions () =
  List.iter calling_conventions ~f:(fun (targets,args) ->
      List.cartesian_product targets args |>
      List.iter ~f:(fun (target,(abi,install)) ->
          register_target target abi install));
  List.iter default_calling_conventions ~f:(fun (targets,install) ->
      List.iter targets ~f:install)

let strip_leading_underscore s =
  match String.chop_prefix s ~prefix:"_" with
  | None -> s
  | Some s -> s

let symbols proj =
  let spec = Project.specification proj in
  let syms = Ogre.(collect Query.(select (from Image.Scheme.named_symbol))) in
  match Ogre.eval syms spec with
  | Ok syms -> Seq.map ~f:snd syms
  | Error _ -> Seq.empty

let has_symbol fn proj =
  Seq.mem (symbols proj) fn ~equal:String.equal

let demangle demangle prog =
  Term.map sub_t prog ~f:(fun sub ->
      let name = demangle (Sub.name sub) in
      Sub.with_name sub name)
