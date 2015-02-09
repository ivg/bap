(** Native lifter of x86 instructions to the BAP IL *)

open Core_kernel.Std
open Bap_types.Std

open Bap_disasm_x86_types
open Bap_disasm_x86_utils
open Bap_disasm_x86_env

module ToIR = struct

  let cf_e = Exp.var cf
  let pf_e = Exp.var pf
  let af_e = Exp.var af
  let zf_e = Exp.var zf
  let sf_e = Exp.var sf
  let of_e = Exp.var oF
  let df_e = Exp.var df

  (* stmt helpers *)

  let size_of_typ s = Size.of_int_exn (!!s) (** doubts here  *)
  let litz = bitvector_of_z
  let lit: int -> int -> BV.t = fun n w -> BV.of_int ~width:w n

  let store_s mode s t a e =
    let mem = gv mode mem in
    let sz = size_of_typ t in
    match s with
    | None -> Stmt.Move (mem, Exp.(Store (Var mem, a, e, LittleEndian, sz)))
    | Some v -> Stmt.Move (mem, Exp.(Store (Var mem, Var v + a, e,
                                   LittleEndian, sz)))

  let storem mode t a e =
    Stmt.Move (mode, Exp.(Store (Var mode, a, e, LittleEndian, t)))

  (* copypasted from op2e_s below, but keeps the opcode width *)
  let op2e_s_keep_width mode ss has_rex t = function
    | Ovec r when t = r256 -> (bits2ymme r, t)
    | Ovec r when t = r128 -> (bits2ymm128e r, t)
    | Ovec r when t = reg64_t -> (bits2ymm64e r, t)
    | Ovec r when t = reg32_t -> (bits2ymm32e r, t)
    | Ovec _ ->
      let i = match t with
        | Type.Imm n -> ": "^(string_of_int n)
        | _ -> "" in
      disfailwith mode ("invalid SIMD register size for op2e"^i)
    | Oreg r when t = reg64_t -> (bits2reg64e mode r, t)
    | Oreg r when t = reg32_t -> (bits2reg32e mode r, t)
    | Oreg r when t = reg16_t -> (bits2reg16e mode r, t)
    | Oreg r when t = reg8_t -> (bits2reg8e mode ~has_rex r, t)
    | Oreg _ -> unimplemented mode "unknown register"
    | Oseg r when t = reg64_t -> Exp.(Cast (Cast.UNSIGNED, !!reg64_t, bits2segrege r), t)
    (* There is no 32-bit extension of segment selectors; this is not a bug *)
    | Oseg r when t = reg16_t -> (bits2segrege r, t)
    | Oseg _ -> (disfailwith mode "Segment register when t is not r16", t)
    | Oaddr e -> (load_s mode ss (size_of_typ t) e, t)
    | Oimm i -> Exp.(Int (litz i (Strip.bits_of_width t)), t)

  let op2e_s mode ss has_rex t = function
    | Ovec r when t = r256 -> bits2ymme r
    | Ovec r when t = r128 -> bits2ymm128e r
    | Ovec r when t = reg64_t -> bits2ymm64e r
    | Ovec r when t = reg32_t -> bits2ymm32e r
    | Ovec _ ->
      let i = match t with
        | Type.Imm n -> ": "^(string_of_int n)
        | _ -> "" in
      disfailwith mode ("invalid SIMD register size for op2e"^i)
    | Oreg r when t = reg64_t -> bits2reg64e mode r
    | Oreg r when t = reg32_t -> bits2reg32e mode r
    | Oreg r when t = reg16_t -> bits2reg16e mode r
    | Oreg r when t = reg8_t -> bits2reg8e mode ~has_rex r
    | Oreg _ -> unimplemented mode "unknown register"
    | Oseg r when t = reg64_t -> Exp.(Cast (Cast.UNSIGNED, !!reg64_t, bits2segrege r))
    (* There is no 32-bit extension of segment selectors; this is not a bug *)
    | Oseg r when t = reg16_t -> bits2segrege r
    | Oseg _ -> disfailwith mode "Segment register when t is not r16"
    | Oaddr e -> load_s mode ss (size_of_typ t) e
    | Oimm i -> Exp.Int (litz i (!!t))

  let assn_s mode s has_rex has_vex t v e =
    (* Assign to some bits of v, starting at bit off, while preserving the other bits *)
    let sub_assn ?(off=0) t v e =
      let concat_exps = ref [] in
      let bits = Strip.bits_of_width (Var.typ v) in
      let assnbits = Strip.bits_of_width t in

      (* Add the upper preserved bits, if any *)
      let ubh = (bits-1) and ubl = (assnbits+off) in
      if ubh > ubl then concat_exps := (Exp.(Extract (ubh, ubl, Var v)))::!concat_exps;

      (* Add e *)
      concat_exps := e::!concat_exps;

      (* Add the lower preserved bits, if any *)
      let lbh = (off-1) and lbl = 0 in
      if lbh > lbl then
        concat_exps := (Exp.(Extract (lbh, lbl, Var v)))::!concat_exps;

      let final_e = List.reduce_exn ~f:(fun big_e e -> Exp.Concat (e, big_e)) !concat_exps in
      Stmt.Move (v, final_e)
    in
    let is8664 = mode = X8664 in
    match v, t with
    (* Zero-extend 128-bit assignments to 256-bit ymms. *)
    | Ovec r, Type.Imm (128|64|32) when has_vex ->
      let v = bits2ymm r in
      sub_assn r256 v Exp.(Cast (Cast.UNSIGNED, !!r256, e))
    | Ovec r, Type.Imm (256|128|64|32) ->
      let v = bits2ymm r in
      sub_assn t v e
    | Ovec _, _ -> disfailwith mode "invalid SIMD register size for assignment"
    (* Zero-extend 32-bit assignments to 64-bit registers. *)
    | Oreg r, Type.Imm 32 when is8664 ->
      let v = gv mode (bits2genreg r) in
      sub_assn reg64_t v Exp.(Cast (Cast.UNSIGNED, !!reg64_t, e))
    | Oreg r, Type.Imm (64|32|16) ->
      let v = gv mode (bits2genreg r) in
      sub_assn t v e
    | Oreg r, Type.Imm 8 when r < 4 || (mode = X8664 && has_rex) ->
      let v = gv mode (bits2genreg r) in
      sub_assn t v e
    | Oreg r, Type.Imm 8 ->
      let v = gv mode (bits2genreg (r land 3)) in
      sub_assn ~off:8 t v e
    | Oreg _, _ -> unimplemented mode "assignment to sub registers"
    | Oseg r, _ when t = reg16_t ->
      let v = bits2segreg r in
      Stmt.Move (v, e)
    | Oseg _, _ -> disfailwith mode "Can't assign to non 16 bit segment register"
    | Oaddr a, _ -> store_s mode s t a e
    | Oimm _, _ -> disfailwith mode "disasm_i386: Can't assign to an immediate value"


  (* Double width operands, as used by multiplication and division *)
  let op_dbl t =
    let open Type in
    match t with
    | Imm 8 -> [reg16_t, o_rax]
    | Imm 16 -> [reg16_t, o_rdx; reg16_t, o_rax]
    | Imm 32 -> [reg32_t, o_rdx; reg32_t, o_rax]
    | Imm 64 -> [reg64_t, o_rdx; reg64_t, o_rax]
    | _ -> disfailwith X86 "op_dbl only defined for Reg 8, 16, 32, and 64"

  (* Return an expression for a double-width operand, as used by the div
     instruction. *)
  let op2e_dbl_s mode ss has_rex t =
    let cf (ct, o) = op2e_s mode ss has_rex ct o in
    let ol = List.map ~f:cf (op_dbl t) in
    List.reduce_exn
      ~f:(fun bige little -> Exp.(bige ^ little))
      ol

  (* Double width assignments, as used by multiplication *)
  let assn_dbl_s mode s has_rex has_vex t e =
      match op_dbl t with
    | (t,o) :: [] -> [assn_s mode s has_rex has_vex t o e], op2e_s mode s has_rex t o
    | l ->
      let tmp = Var.create ~tmp:true "t" (Type.Imm (Strip.bits_of_width t * 2)) in
      let f (stmts, off) (ct, o) =
        let newoff = off + Strip.bits_of_width ct in
        assn_s mode s has_rex has_vex ct o (Exp.Extract (newoff-1, off, Exp.Var tmp))::stmts, newoff
      in
      List.rev (fst (List.fold_left ~f:f ~init:([Stmt.Move (tmp, e)], 0) (List.rev l))), Exp.Var tmp

  (* A function for computing the target of jumps. *)
  let compute_jump_target mode s has_rex =
    let t = type_of_mode mode in
    function
    | Jabs o -> op2e_s mode s has_rex t o
    | Jrel (na,offset) ->
      (*let i,t = Arithmetic.binop PLUS (na,t) (offset,t) in*)
      Exp.((Int na) + (Int offset))
  let jump_target = compute_jump_target

  let string_incr mode t v =
    let i n = Exp.Int (int_of_mode mode n) in
    if t = reg8_t then
      Stmt.Move (v, Exp.(Var v + df_to_offset mode df_e))
    else
      Stmt.Move (v, Exp.(Var v + (df_to_offset mode df_e * i (Strip.bytes_of_width t))))

  let rep_wrap ?check_zf ~mode ~addr ~next stmts =
    let bi = big_int_of_mode mode in
    let bi0 = Z.(~$0) in
    let bi1 = Z.(~$1) in
    let endstmt = match check_zf with
      | None -> Stmt.Jmp(Exp.Int (bi addr))
      | Some x when x = repz ->
        (* a conditional jump was replaced with the new If statement here *)
        Stmt.If (zf_e, [Stmt.Jmp (Exp.Int (bi addr))], [])
      | Some x when x = repnz ->
        Stmt.If (zf_e, [], [Stmt.Jmp (Exp.Int (bi addr))])
      | _ -> failwith "invalid value for ?check_zf"
    in
    let rcx = gv mode rcx in
    let rcx_e = Exp.Var rcx in
    let open Stmt in
    (* a conditional jump was replaced with the new If statement here *)
    (If (Exp.(rcx_e = (Int (bi (bi0)))), [Jmp (Exp.Int (bi next))], []))
    :: stmts
    @ Move (rcx, Exp.(rcx_e - (Int (bi (bi1)))))
      :: [(If (Exp.(rcx_e = (Int (bi (bi0)))), [Jmp (Exp.Int (bi next))], []))]
    @ [endstmt]

  let compute_sf result = Exp.(Cast (Cast.HIGH, !!bool_t, result))

  let compute_zf t result =
    let bi0 = lit 0 t in
    Exp.((Int bi0) = result)

  let compute_pf t r =
    let acc = Var.create ~tmp:true "acc" t in
    let var_acc = Exp.Var acc in
    let t' = Strip.bits_of_width t in
    (* extra parens do not change semantics but do make it pretty
    print nicer *)
    let open Exp in
    exp_not (Cast (Cast.LOW, !!bool_t,
                   ((Let(acc, (r lsr (it 4 t')) lxor r, Let(acc, (var_acc lsr (it 2 t')) lxor var_acc, (var_acc lsr (it 1 t')) lxor var_acc))))))

  let set_sf r = Stmt.Move (sf, compute_sf r)
  let set_zf t r = Stmt.Move (zf, compute_zf t r)
  let set_pf t r = Stmt.Move (pf, compute_pf t r)

  let set_pszf t r =
    let t' = Strip.bits_of_width t in
    [set_pf t r;
     set_sf r;
     set_zf t' r]

  (* Adjust flag

     AF is set when there is a carry to or borrow from bit 4 (starting
     at 0), when considering unsigned operands. Let X_i denote bit i of
     value X.  Note that in addition, r_4 = c + [(op1_4 + op2_4) mod 2],
     where c is the carry bit from the lower four bits. Since AF = c,
     and we want to know the value of AF, we can rewrite as AF = c = r_4
     - [(op1_4 + op2_4) mod 2]. Noting that addition and subtraction mod
     2 is just xor, we can simplify to AF = r_4 xor op1_4 xor op2_4.
  *)

  let set_apszf t s1 s2 r =
    let bit4 = it (1 lsl 4) t in
    let t = Type.Imm t in
    Stmt.Move (af, Exp.(bit4 = (bit4 land ((r lxor s1) lxor s2))))
    ::set_pszf t r

  (* Helper functions to set flags for adding *)
  let set_aopszf_add t s1 s2 r =
    (* Move (oF, Bop.(Cast (CAST_HIGH, r1, (s1 = s2) land (s1 lxor r)))) *)
    let s1_high = Exp.(Cast (Cast.HIGH, !!bool_t, s1)) in
    let s2_high = Exp.(Cast (Cast.HIGH, !!bool_t, s2)) in
    let r_high  = Exp.(Cast (Cast.HIGH, !!bool_t, r)) in
    Stmt.Move (oF, Exp.((s1_high = s2_high) land (s1_high lxor r_high)))
    ::set_apszf t s1 s2 r

  let set_flags_add t s1 s2 r =
    Stmt.Move (cf, Exp.(r < s1))
    ::set_aopszf_add t s1 s2 r

  (* Helper functions to set flags for subtracting *)
  let set_apszf_sub t s1 s2 r = set_apszf t s1 s2 r

  let set_aopszf_sub t s1 s2 r =
    Stmt.Move (oF, Exp.(Cast (Cast.HIGH, !!bool_t, (s1 lxor s2) land (s1 lxor r))))
    ::set_apszf_sub t s1 s2 r

  let set_flags_sub t s1 s2 r =
    Stmt.Move (cf, Exp.(s2 > s1))
    ::set_aopszf_sub t s1 s2 r

  let rec to_ir mode addr next ss pref has_rex has_vex =
    let load = load_s mode ss in (* Need to change this if we want seg_ds <> None *)
    let op2e = op2e_s mode ss has_rex in
    let op2e_keep_width = op2e_s_keep_width mode ss has_rex in
    let op2e_dbl = op2e_dbl_s mode ss has_rex in
    let store = store_s mode ss in
    let assn = assn_s mode ss has_rex has_vex in
    let assn_dbl = assn_dbl_s mode ss has_rex has_vex in
    let mi = int_of_mode mode in
    (* let mi64 = int64_of_mode mode *) (* unused *)
    let mt = type_of_mode mode in
    let _fs_base_e = ge mode fs_base in
    let fs_base = gv mode fs_base in
    let _gs_base_e = ge mode gs_base in
    let gs_base = gv mode gs_base in
    let _rbp = gv mode rbp in
    let rbp_e = ge mode rbp in
    let rsp_e = ge mode rsp in
    let rsp = gv mode rsp in
    let rsi_e = ge mode rsi in
    let rsi = gv mode rsi in
    let rdi_e = ge mode rdi in
    let rdi = gv mode rdi in
    let rax_e = ge mode rax in
    let rax = gv mode rax in
    let _rbx = gv mode rbx in
    let _rbx_e = ge mode rbx in
    let _rcx_e = ge mode rcx in
    let rcx = gv mode rcx in
    let _rdx = gv mode rdx in
    let rdx_e = ge mode rdx in
    let ah_e = ah_e mode in
    let _ch_e = ch_e mode in
    let _dh_e = dh_e mode in
    let _bh_e = bh_e mode in
    let disfailwith = disfailwith mode in
    let unimplemented = unimplemented mode in function
    | Nop -> []
    | Bswap(t, op) ->
      let e = match t with
        | Type.Imm 32 | Type.Imm 64 -> let (op', t') = op2e_keep_width t op in
          BITEMP.reverse_bytes op' t'
        | _ -> disfailwith "bswap: Expected 32 or 64 bit type"
      in
      [assn t op e]
    | Retn (op, far_ret) when pref = [] || pref = [repz]  || pref = [repnz]->
      let temp = Var.create ~tmp:true "ra" mt in
      let load_stmt = if far_ret
        then (* TODO Mess with segment selectors here *)
          unimplemented "long retn not supported"
        else Stmt.Move (temp, load_s mode seg_ss (size_of_typ mt) rsp_e)
      in
      let rsp_stmts =
        Stmt.Move (rsp, Exp.(rsp_e + (Int (mi (Strip.bytes_of_width mt)))))::
        (match op with
         | None -> []
         | Some(t, src) ->
           [Stmt.Move (rsp, Exp.(rsp_e + (op2e t src)))]
        ) in
      load_stmt::
      rsp_stmts@
      [Stmt.Jmp (Exp.Var temp)]
    | Mov(t, dst, src, condition) ->
      let c_src = (match condition with
          | None -> op2e t src
          | Some c -> Exp.Ite (c, op2e t src, op2e t dst))
      in
      (* Find base by looking at LDT or GDT *)
      let base_e e =
        (* 0 = GDT, 1 = LDT *)
        let ti = Exp.Extract (3, 3, e) in
        let base = Exp.Ite (ti, ge mode ldt, ge mode gdt) in
        (* Extract index into table *)
        let entry_size, entry_shift = match mode with
          | X86 -> reg64_t, 6  (* "1<<6 = 64" *)
          | X8664 -> r128, 7 (* "1<<7 = 128" *)
        in
        let index = Exp.(Cast (Cast.UNSIGNED, !!mt, (Extract (15, 4, e)) lsl (Int (mi entry_shift)))) in
        (* Load the table entry *)
        let table_entry = load_s mode None (size_of_typ entry_size) (Exp.(base + index)) in
        (* Extract the base *)
        Util.concat_explist
          ((match mode with
              | X86 -> []
              | X8664 -> (Exp.Extract (95, 64, table_entry)) :: [])
           @  (Exp.Extract (63, 56, table_entry))
              :: (Exp.Extract (39, 32, table_entry))
              :: (Exp.Extract (31, 16, table_entry))
              :: [])
      in
      let bs =
        let dst_e = op2e t dst in
        if dst = o_fs && !compute_segment_bases then [Stmt.Move (fs_base, base_e dst_e)]
        else if dst = o_gs && !compute_segment_bases then [Stmt.Move (gs_base, base_e dst_e)]
        else []
      in
      assn t dst c_src :: bs
    | Movs(Type.Imm _bits as t) ->
      let stmts =
        store_s mode seg_es t rdi_e (load_s mode seg_es (size_of_typ t) rsi_e)
        :: string_incr mode t rsi
        :: string_incr mode t rdi
        :: []
      in
      if pref = [] then
        stmts
      else if List.mem pref repz || List.mem pref repnz then
        (* movs has only rep instruction others just considered to be rep *)
        rep_wrap ~mode ~addr ~next stmts
      else
        unimplemented "unsupported prefix for movs"
    | Movzx(t, dst, ts, src) ->
      [assn t dst Exp.(Cast (Cast.UNSIGNED, !!t, op2e ts src))]
    | Movsx(t, dst, ts, src) ->
      [assn t dst Exp.(Cast (Cast.SIGNED, !!t, op2e ts src))]
    | Movdq(ts, s, td, d, align) ->
      let (s, al) = match s with
        | Ovec _ | Oreg _-> op2e ts s, []
        | Oaddr a -> op2e ts s, [a]
        | Oimm _ | Oseg _ -> disfailwith "invalid source operand for movdq"
      in
      let (d, al) = match d with
        (* Behavior is to clear the xmm bits *)
        | Ovec _ -> assn td d Exp.(Cast (Cast.UNSIGNED, !!td, s)), al
        | Oreg _ -> assn td d s, al
        | Oaddr a -> assn td d s, a::al
        | Oimm _ | Oseg _ -> disfailwith "invalid dest operand for movdq"
      in
      (* sources tell me that movdqa raises a general protection exception
       * if its operands aren't aligned on a 16-byte boundary *)
      let im i = Exp.Int (int_of_mode mode i) in
      let al =
        if align then
          List.map ~f:(fun a -> Stmt.If (Exp.((a land im 15) = im 0), [], [Cpu_exceptions.general_protection])) al
        else []
      in
      d::al
    | Movoffset((tdst, dst), offsets) ->
      (* If a vex prefix is present, then extra space is filled with 0.
         Otherwise, the bits are preserved. *)
      let padding hi lo =
        if hi < lo then []
        else if has_vex then [it 0 (hi - lo + 1)]
        else [Exp.Extract (hi, lo, op2e tdst dst)]
      in
      let offsets = List.sort ~cmp:(fun {offdstoffset=o1; _} {offdstoffset=o2; _} -> Int.compare o1 o2) offsets in
      let add_exp (elist,nextbit) {offlen; offtyp; offop; offsrcoffset; offdstoffset} =
        Exp.Extract (((Strip.bits_of_width offlen) + offsrcoffset - 1), offsrcoffset, (op2e offtyp offop))
        :: padding (offdstoffset - 1) nextbit
        @ elist, offdstoffset + (Strip.bits_of_width offlen)
      in
      let elist, nextbit = List.fold_left ~f:add_exp ~init:([],0) offsets in
      let elist = padding ((Strip.bits_of_width tdst) - 1) nextbit @ elist in
      [assn tdst dst (Util.concat_explist elist)]
    | Punpck(t, et, o, d, s, vs) ->
      let nelem = match t, et with
        | Type.Imm n, Type.Imm n' -> n / n'
        | _ -> disfailwith "invalid"
      in
      assert (nelem mod 2 = 0);
      let nelem_per_src = nelem / 2 in
      let halft = Type.Imm ((Strip.bits_of_width t)/2) in
      let castf = match o with
        | High -> fun e -> Exp.(Cast (Cast.HIGH, !!halft, e))
        | Low -> fun e -> Exp.(Cast (Cast.LOW, !!halft, e))
      in
      let se, de = castf (op2e t s), castf (op2e t d) in
      let st, dt = Var.create ~tmp:true "s" halft, Var.create ~tmp:true "d" halft in
      let et' = Strip.bits_of_width et in
      let mape i =
        [BITEMP.extract_element et' (Exp.Var st) i; BITEMP.extract_element et' (Exp.Var dt) i]
      in
      let e = Util.concat_explist (List.concat (List.map ~f:mape (List.range ~stride:(-1) ~stop:`inclusive (nelem_per_src-1) 0))) in
      let dest = match vs with
        | None -> d
        | Some vdst -> vdst
      in
      [Stmt.Move (st, se);
       Stmt.Move (dt, de);
       assn t dest e]
    | Ppackedbinop(t, et, fbop, _, d, s, vs) ->
      let nelem = match t, et with
        | Type.Imm n, Type.Imm n' -> n / n'
        | _ -> disfailwith "invalid"
      in
      let getelement o i =
        (* assumption: immediate operands are repeated for all vector
           elements *)
        match o with
        | Oimm _ -> op2e et o
        | _ -> BITEMP.extract_element (Strip.bits_of_width et) (op2e t o) i
      in
      let f i =
        fbop (getelement d i) (getelement s i)
      in
      let e = Util.concat_explist (List.map ~f:f (List.range ~stride:(-1) ~stop:`inclusive (nelem-1) 0)) in
      (match vs with
       | None -> [assn t d e]
       | Some vdst -> [assn t vdst e])
    | Pbinop(t, fbop, _s, o1, o2, vop) ->
      (match vop with
       | None -> [assn t o1 (fbop (op2e t o1) (op2e t o2))]
       | Some vop -> [assn t o1 (fbop (op2e t vop) (op2e t o2))])
    | Pcmp (t,elet,bop,_,dst,src,vsrc) ->
      let elebits = Strip.bits_of_width elet in
      let ncmps = (Strip.bits_of_width t) / elebits in
      let src = match src with
        | Ovec _ -> op2e t src
        | Oaddr a -> load (size_of_typ t) a
        | Oreg _ | Oimm _ | Oseg _ -> disfailwith "invalid"
      in
      let dst, vsrc = match vsrc with
        | None -> dst, dst
        | Some vsrc -> dst, vsrc
      in
      let compare_region i =
        let byte1 = Exp.Extract(i*elebits-1, (i-1)*elebits, src) in
        let byte2 = Exp.Extract(i*elebits-1, (i-1)*elebits, op2e t vsrc) in
        let tmp = Var.create ~tmp:true ("t" ^ string_of_int i) elet in
        Exp.Var tmp, Stmt.Move (tmp, Exp.(Ite (BinOp (bop, byte1, byte2), lt (-1L) (Strip.bits_of_width elet), lt 0L (Strip.bits_of_width elet))))
      in
      let indices = List.init ~f:(fun i -> i + 1) ncmps in (* list 1-nbytes *)
      let comparisons = List.map ~f:compare_region indices in
      let temps, cmps = List.unzip comparisons in
      begin match List.rev temps with
        | [] -> disfailwith "Pcmp element size mismatch" (* XXX what's actually going on in Pcmp? *)
        | t_first::t_rest ->
          (* could also be done with shifts *)
          let store_back = List.fold_left ~f:(fun acc i -> Exp.Concat(acc,i)) ~init:t_first t_rest in
          cmps @ [assn t dst store_back] end
    | Pmov (t, dstet, srcet, dst, src, ext, _) ->
      let nelem = match t, dstet with
        | Type.Imm n, Type.Imm n' -> n / n'
        | _ -> disfailwith "invalid"
      in
      let getelt op i = BITEMP.extract_element (Strip.bits_of_width srcet) (op2e t op) i in
      let extcast =
        let open Exp in
        match ext with
        | Cast.UNSIGNED | Cast.SIGNED -> fun e -> Exp.Cast (ext, !!dstet, e)
        | _ -> disfailwith "invalid"
      in
      let extend i = extcast (getelt src i) in
      let e = Util.concat_explist (List.map ~f:extend (List.range ~stride:(-1) ~stop:`inclusive (nelem-1) 0)) in
      [assn t dst e]
    | Pmovmskb (t,dst,src) ->
      let nbytes = Strip.bytes_of_width t in
      let src = match src with
        | Ovec _ -> op2e t src
        | _ -> disfailwith "invalid operand"
      in
      let get_bit i = Exp.Extract(i*8-1, i*8-1, src) in
      let byte_indices = List.init ~f:(fun i -> i + 1) nbytes in (* list 1-nbytes *)
      let all_bits = List.rev (List.map ~f:get_bit byte_indices) in
      (* could also be done with shifts *)
      let padt = Type.Imm(32 - nbytes) in
      let or_together_bits = List.fold_left ~f:(fun acc i -> Exp.Concat(acc,i)) ~init:(it 0 (Strip.bits_of_width padt)) all_bits in
      [assn reg32_t dst or_together_bits]
    | Palignr (t,dst,src,vsrc,imm) ->
      let (dst_e, t_concat) = op2e_keep_width t dst in
      let (src_e, t_concat') = op2e_keep_width t src in
      (* previously, this code called Typecheck.infer_ast.
       * We're now just preserving the widths, so here we assert
       * that our 2 "preserved widths" are the same. *)
      assert (Strip.bits_of_width t_concat = Strip.bits_of_width t_concat');
      let imm = op2e t imm in
      let conct = Exp.(dst_e ^ src_e) in
      let shift = Exp.(conct lsr (Cast (Cast.UNSIGNED, !!t_concat, imm lsl (it 3 (Strip.bits_of_width t))))) in
      let high, low = match t with
        | Type.Imm 256 -> 255, 0
        | Type.Imm 128 -> 127, 0
        | Type.Imm 64 -> 63, 0
        | _ -> disfailwith "impossible: used non 64/128/256-bit operand in palignr"
      in
      let result = Exp.Extract (high, low, shift) in
      let im i = Exp.Int (int_of_mode mode i) in
      let addresses = List.fold
          ~f:(fun acc -> function Oaddr a -> a::acc | _ -> acc) ~init:[] [src;dst] in
      (* Palignr seems to cause a CPU general protection exception if this fails.
       * previously this code used the ast.ml Assert statement, which is gone,
       * so it's been replaced with Bil's CpuExn *)
      List.map ~f:(fun addr -> Stmt.If (Exp.((addr land im 15) = im 0),
                                   [], [Cpu_exceptions.general_protection])) addresses
      @ (match vsrc with
          | None -> [assn t dst result]
          | Some vdst -> [assn t vdst result])
    | Pcmpstr(t,xmm1,xmm2m128,_imm,imm8cb,pcmpinfo) ->
      (* All bytes and bits are numbered with zero being the least
         significant. This includes strings! *)
      (* NOTE: Strings are backwards, at least when they are in
         registers.  This doesn't seem to be documented in the Intel
         manual.  This means that the NULL byte comes before the
         string. *)
      let xmm1_e = op2e t xmm1 in
      let xmm2m128_e = op2e t xmm2m128 in
      let regm = type_of_mode mode in

      let open Pcmpstr in
      let nelem, _nbits, elemt = match imm8cb with
        | {ssize=Bytes; _} -> 16, 8, Type.imm 8
        | {ssize=Words; _} -> 8, 16, Type.imm 16
      in
      (* Get element index in e *)
      let get_elem = BITEMP.extract_element (Strip.bits_of_width elemt) in
      (* Get from xmm1/xmm2 *)
      let get_xmm1 = get_elem xmm1_e in
      let get_xmm2 = get_elem xmm2m128_e in
      (* Build expressions that assigns the correct values to the
         is_valid variables using implicit (NULL-based) string
         length. *)
      let build_implicit_valid_xmm_i is_valid_xmm_i get_xmm_i =
        let f acc i =
          (* Previous element is valid *)
          let prev_valid = if i = 0 then exp_true else Exp.var (is_valid_xmm_i (i-1)) in
          (* Current element is valid *)
          let curr_valid = Exp.(get_xmm_i i <> (it 0 (Strip.bits_of_width elemt))) in
          Exp.Let(is_valid_xmm_i i, Exp.(prev_valid land curr_valid), acc)
        in (fun e -> List.fold_left ~f:f ~init:e (List.range ~stride:(-1) ~stop:`inclusive (nelem-1) 0))
      in
      (* Build expressions that assigns the correct values to the
         is_valid variables using explicit string length. *)
      let build_explicit_valid_xmm_i is_valid_xmm_i sizee =
        (* Max size is nelem *)
        let sizev = Var.create ~tmp:true "sz" regm in
        let sizee = Exp.(Ite (BinOp (Binop.LT, it nelem (Strip.bits_of_width regm), sizee), it nelem (Strip.bits_of_width regm), sizee)) in
        let f acc i =
          (* Current element is valid *)
          let curr_valid = Exp.(BinOp (Binop.LT, it i (Strip.bits_of_width regm), Var sizev)) in
          Exp.Let(is_valid_xmm_i i, curr_valid, acc)
        in (fun e -> Exp.Let(sizev, sizee, List.fold_left ~f:f ~init:e (List.range ~stride:(-1) ~stop:`inclusive (nelem-1) 0)))
      in

      (* Get var name indicating whether index in xmm num is a valid
         byte (before NULL byte). *)
      (* XXX more hashtable code *)
      let is_valid =
        let vh = Hashtbl.Poly.create () ~size:(2*nelem) in
        (fun xmmnum index -> match Hashtbl.find vh (xmmnum,index) with
           | Some v -> v
           | None ->
             let v = Var.create ~tmp:true ("is_valid_xmm"^string_of_int xmmnum^"_ele"^string_of_int index) bool_t in
             Hashtbl.add_exn vh ~key:(xmmnum,index) ~data:v;
             v)
      in

      let is_valid_xmm1 index = is_valid 1 index in
      let is_valid_xmm2 index = is_valid 2 index in
      let is_valid_xmm1_e index = Exp.Var(is_valid_xmm1 index) in
      let is_valid_xmm2_e index = Exp.Var(is_valid_xmm2 index) in

      let build_valid_xmm1,build_valid_xmm2 =
        match pcmpinfo with
        | {len=Implicit; _} ->
          build_implicit_valid_xmm_i is_valid_xmm1 get_xmm1,
          build_implicit_valid_xmm_i is_valid_xmm2 get_xmm2
        | {len=Explicit; _} ->
          build_explicit_valid_xmm_i is_valid_xmm1 rax_e,
          build_explicit_valid_xmm_i is_valid_xmm2 rdx_e
      in

      let get_intres1_bit index =
        let open Exp.Binop in
        match imm8cb with
        | {agg=EqualAny; _} ->
          (* Is xmm2[index] at xmm1[j]? *)
          let check_char acc j =
            let eq = Exp.((get_xmm2 index) = (get_xmm1 j)) in
            let valid = is_valid_xmm1_e j in
            Exp.(Ite ((eq land valid), exp_true, acc))
          in
          Exp.BinOp (AND, is_valid_xmm2_e index,
                 (* Is xmm2[index] included in xmm1[j] for any j? *)
                 (List.fold ~f:check_char ~init:exp_false (List.range
                                                             ~stride:(-1) ~stop:`inclusive (nelem-1) 0)))
        | {agg=Ranges; _} ->
          (* Is there an even j such that xmm1[j] <= xmm2[index] <=
             xmm1[j+1]? *)
          let check_char acc j =
            (* XXX: Should this be AND? *)
            let rangevalid = Exp.(is_valid_xmm1_e Pervasives.(2*j) land is_valid_xmm1_e Pervasives.(2*j+1)) in
            let lte = match imm8cb with
              | {ssign=Unsigned; _} -> LE
              | {ssign=Signed; _} -> SLE
            in
            let inrange =
              Exp.((BinOp (lte, (get_xmm1 Pervasives.(2*j)), (get_xmm2 index)))
                   land (BinOp (lte, (get_xmm2 index), (get_xmm1 Pervasives.(2*j+1)))))
            in
            Exp.(Ite (UnOp (Unop.NOT, rangevalid), exp_false, Ite (inrange, exp_true, acc)))
          in
          Exp.(is_valid_xmm2_e index
               (* Is xmm2[index] in the jth range pair? *)
               land List.fold_left ~f:check_char ~init:exp_false (List.range ~stride:(-1) ~stop:`inclusive Pervasives.(nelem/2-1) 0))
        | {agg=EqualEach; _} ->
          (* Does xmm1[index] = xmm2[index]? *)
          let xmm1_invalid = Exp.(UnOp (Unop.NOT, (is_valid_xmm1_e index))) in
          let xmm2_invalid = Exp.(UnOp (Unop.NOT, (is_valid_xmm2_e index))) in
          let bothinvalid = Exp.(xmm1_invalid land xmm2_invalid) in
          let eitherinvalid = Exp.(xmm1_invalid lor xmm2_invalid) in
          let eq = Exp.(get_xmm1 index = get_xmm2 index) in
          (* both invalid -> true
             one invalid -> false
             both valid -> check same byte *)
          Exp.Ite (bothinvalid, exp_true,
               Exp.Ite (eitherinvalid, exp_false,
                    Exp.Ite (eq, exp_true, exp_false)))
        | {agg=EqualOrdered; _} ->
          (* Does the substring xmm1 occur at xmm2[index]? *)
          let check_char acc j =
            let neq = Exp.(get_xmm1 j <> get_xmm2 Pervasives.(index+j)) in
            let substrended = Exp.(UnOp (Unop.NOT, (is_valid_xmm1_e j))) in
            let bigstrended = Exp.UnOp (Exp.Unop.NOT, (is_valid_xmm2_e (index+j))) in
            (* substrended => true
               bigstrended => false
               byte diff => false
               byte same => keep going  *)
            Exp.Ite (substrended, exp_true,
                 Exp.Ite (bigstrended, exp_false,
                      Exp.Ite (neq, exp_false, acc)))
          in
          (* Is xmm1[j] equal to xmm2[index+j]? *)
          List.fold_left ~f:check_char ~init:exp_true (List.range
      ~stride:(-1) ~stop:`inclusive (nelem-index-1) 0)
      in
      let bits = List.map ~f:get_intres1_bit (List.range ~stride:(-1) ~stop:`inclusive (nelem-1) 0) in
      let res_e = build_valid_xmm1 (build_valid_xmm2 (Util.concat_explist bits)) in
      let int_res_1 = Var.create ~tmp:true "IntRes1" reg16_t in
      let int_res_2 = Var.create ~tmp:true "IntRes2" reg16_t in

      let contains_null e =
        List.fold_left ~f:(fun acc i ->
            Exp.Ite (Exp.(get_elem e i = it 0 (Strip.bits_of_width elemt)), exp_true, acc)) ~init:exp_false (List.init ~f:Util.id nelem)
      in
      (* For pcmpistri/pcmpestri *)
      let sb e =
        List.fold_left ~f:(fun acc i ->
            Exp.Ite (Exp.(exp_true = Extract (i, i, e)),
                 (it i (Strip.bits_of_width regm)),
                 acc))
          ~init:(it nelem (Strip.bits_of_width regm))
          (match imm8cb with
           | {outselectsig=LSB; _} -> List.range ~stride:(-1) ~start:`exclusive ~stop:`inclusive nelem 0
           | {outselectsig=MSB; _} -> List.init ~f:Util.id nelem)
      in
      (* For pcmpistrm/pcmpestrm *)
      let mask e =
        match imm8cb with
        | {outselectmask=Bitmask; _} ->
          Exp.(Cast (Cast.UNSIGNED, !!r128, e))
        | {outselectmask=Bytemask; _} ->
          let get_element i =
            Exp.(Cast (Cast.UNSIGNED, !!elemt, Extract (i, i, e)))
          in
          Util.concat_explist (List.map ~f:get_element (List.range ~stride:(-1) ~start:`exclusive ~stop:`inclusive nelem 0))
      in
      (* comment crap from earlier *)
      (*comment
        ::*)
      let open Stmt in
      Move (int_res_1, Exp.(Cast (Cast.UNSIGNED, !!reg16_t, res_e)))
      :: (match imm8cb with
          | {negintres1=false; _} ->
            Move (int_res_2, Exp.Var int_res_1)
          | {negintres1=true; maskintres1=false; _} ->
            (* int_res_1 is bitwise-notted *)
            Move (int_res_2, Exp.(UnOp (Unop.NOT, (Var int_res_1))))
          | {negintres1=true; maskintres1=true; _} ->
            (* only the valid elements in xmm2 are bitwise-notted *)
            (* XXX: Right now we duplicate the valid element computations
               when negating the valid elements.  They are also used by the
               aggregation functions.  A better way to implement this might
               be to write the valid element information out as a temporary
               bitvector.  The aggregation functions and this code would
               then extract the relevant bit to see if an element is
               valid. *)
            let validvector =
              let bits = List.map ~f:is_valid_xmm2_e (List.range ~stride:(-1) ~start:`exclusive ~stop:`inclusive nelem 0) in
              build_valid_xmm2 (Exp.(Cast (Cast.UNSIGNED, !!reg16_t, Util.concat_explist bits)))
            in
            Move (int_res_2, Exp.(validvector lxor Var int_res_1)))
      :: (match pcmpinfo with
          | {out=Index; _} -> Move (rcx, sb (Exp.Var int_res_2))
          (* FIXME: ymms should be used instead of xmms here *)
          | {out=Mask; _} -> Move (ymms.(0), mask (Exp.Var int_res_2)))
      :: Move (cf, Exp.(Var int_res_2 <> it 0 16))
      :: Move (zf ,contains_null xmm2m128_e)
      :: Move (sf, contains_null xmm1_e)
      :: Move (oF, Exp.(Extract (0, 0, Var int_res_2)))
      :: Move (af, it 0 1)
      :: Move (pf, it 0 1)
      :: []
    | Pshufd (t, dst, src, vsrc, imm) ->
      let src_e = op2e t src in
      let imm_e = op2e t imm in
      (* XXX: This would be more straight-forward if implemented using
         map, instead of fold *)
      let get_dword ndword =
        let high = 2 * (ndword mod 4) + 1 in
        let low = 2 * (ndword mod 4) in
        let index = Exp.(Cast (Cast.UNSIGNED, !!t, Extract (high, low, imm_e))) in
        let t' = Strip.bits_of_width t in
        (* Use the same pattern for the top half of a ymm register *)
        (* had to stop using extract_element_symbolic, since that calls
         * Typecheck.infer_ast. I believe this captures the same
         * "width logic", but this is a good place to check if things start
         * going wrong later. *)
        let (index, index_width) = if t' = 256 && ndword > 3 then
            (Exp.(index + (Int (lit 4 (Strip.bits_of_width t)))), 256)
          else (index, t') in
        BITEMP.extract_element_symbolic_with_width (Type.imm 32) src_e index index_width
      in
      let topdword = match t with Type.Imm 128 -> 3 | _ -> 7 in
      let dwords = Util.concat_explist (List.map ~f:get_dword (List.range ~stride:(-1) ~stop:`inclusive topdword 0)) in
      (match vsrc with
       | None -> [assn t dst dwords]
       | Some vdst -> [assn t vdst dwords])
    | Pshufb (t, dst, src, vsrc) ->
      let order_e = op2e t src in
      let dst_e = op2e t dst in
      let get_bit i =
        let highbit = Exp.Extract (((i*8)+7), ((i*8)+7), order_e) in
        (* this part of the code previously also used Typecheck.infer_ast
         * (indirectly, by calling Ast_convenience.extract_byte_symbolic with
         * index as the last argument).
         * I've read the relevant parts of infer_ast and extract_byte_symbolic
         * and also the helpful comments below "3 bits" and "4 bits"
         * and it seems those are the appropriate widths we get out of
         * infer_ast, so I'm passing those in directly now.
         * Imo, this is a lot less dubious than the thingy above. *)
        let (index, index_width) = match t with
          | Type.Imm 64 -> (Exp.Extract (((i*8)+2), ((i*8)+0), order_e), 64) (* 3 bits *)
          | Type.Imm 128 -> (Exp.Extract (((i*8)+3), ((i*8)+0), order_e), 128) (* 4 bits *)
          | Type.Imm 256 -> (Exp.Extract (((i*8)+3), ((i*8)+0), order_e), 256) (* 4 bits *)
          | _ -> disfailwith "invalid size for pshufb"
        in
        let index = Exp.(Cast (Cast.UNSIGNED, !!t, index)) in
        let atindex = BITEMP.extract_byte_symbolic_with_width dst_e index index_width in
        Exp.Ite (highbit, it 0 8, atindex)
      in
      let n = (Strip.bits_of_width t) / 8 in
      let e = Util.concat_explist (List.map ~f:get_bit (List.range ~stride:(-1) ~start:`exclusive ~stop:`inclusive n 0)) in
      (match vsrc with
       | None -> [assn t dst e]
       | Some vdst -> [assn t vdst e])
    | Lea(t, r, a) when pref = [] ->
      (* See Table 3-64 *)
      (* previously, it checked whether addrbits > opbits before the cast_low.
       * The conclusion we came to was that
         - if they were equal, the cast is basically a nop
         - if it was the other way round, you want to extend it anyway.
       * (I may be remembering things wrongly) *)
      [assn t r Exp.(Cast (Cast.LOW, !!t, a))]
    | Call(o1, ra) when pref = [] ->
      (* If o1 is an immediate, we should syntactically have Jump(imm)
         so that the CFG algorithm knows where the jump goes.  Otherwise
         it will point to BB_Indirect.

         Otherwise, we should evaluate the operand before decrementing esp.
         (This really only matters when esp is the base register of a memory
         lookup. *)
      let target = op2e mt o1 in
      (match o1 with
       | Oimm _ ->
         [Stmt.Move (rsp, Exp.(rsp_e - (Int (mi (Strip.bytes_of_width mt)))));
          store_s mode None mt rsp_e (Exp.Int ra);
          Stmt.Jmp target]
       | _ ->
         let t = Var.create ~tmp:true "target" mt in
         [Stmt.Move (t, target);
          Stmt.Move (rsp, Exp.(rsp_e - (Int (mi (Strip.bytes_of_width mt)))));
          store_s mode None mt rsp_e (Exp.Int ra);
          Stmt.Jmp (Exp.Var t)])
    | Jump(o) ->
      [Stmt.Jmp (jump_target mode ss has_rex o)]
    | Jcc(o, c) ->
      [Stmt.If (c, [Stmt.Jmp (jump_target mode ss has_rex o)], [])]
    | Setcc(t, o1, c) ->
      [assn t o1 Exp.(Cast (Cast.UNSIGNED, !!t, c))]
    | Shift(st, s, dst, shift) ->
      let open Exp.Binop in
      assert (List.mem [reg8_t; reg16_t; reg32_t; reg64_t] s);
      let origCOUNT, origDEST = Var.create ~tmp:true "origCOUNT" s,
                                Var.create ~tmp:true "origDEST" s in
      let s' = Strip.bits_of_width s in
      let size = it s' s' in
      let s_f = Exp.(match st with LSHIFT -> (lsl)  | RSHIFT -> (lsr)
                                 | ARSHIFT -> (asr) | _ -> disfailwith
         "invalid shift type") in
      let dste = op2e s dst in
      let count_mask = Exp.(size - (it 1 s')) in
      let count = Exp.((op2e s shift) land count_mask) in
      let ifzero t e = Exp.(Ite ((Var origCOUNT = it 0 s'), t, e)) in
      let new_of = match st with
        | LSHIFT -> Exp.((Cast (Cast.HIGH, !!bool_t, dste)) lxor cf_e)
        | RSHIFT -> Exp.(Cast (Cast.HIGH, !!bool_t, Var origDEST))
        | ARSHIFT -> exp_false
        | _ -> disfailwith "impossible"
      in
      let unk_of = Exp.Unknown ("OF undefined after shift", bool_t) in
      let new_cf =
        (* undefined for SHL and SHR instructions where the count is greater than
           or equal to the size (in bits) of the destination operand *)
        match st with
        | LSHIFT -> Exp.(Cast (Cast.LOW, !!bool_t, Var origDEST lsr (size - Var origCOUNT)))
        | RSHIFT | ARSHIFT ->
          Exp.(Cast (Cast.HIGH, !!bool_t, Var origDEST lsl (size - Var origCOUNT)))
        | _ -> failwith "impossible"
      in
      [Stmt.Move (origDEST, dste);
       Stmt.Move (origCOUNT, count);
       assn s dst (s_f dste count);
       Stmt.Move (cf, ifzero cf_e new_cf);
       Stmt.Move (oF, Exp.(ifzero of_e (Ite (Var origCOUNT = it 1 s', new_of, unk_of))));
       Stmt.Move (sf, ifzero sf_e (compute_sf dste));
       Stmt.Move (zf, ifzero zf_e (compute_zf s' dste));
       Stmt.Move (pf, ifzero pf_e (compute_pf s dste));
       Stmt.Move (af, ifzero af_e (Exp.Unknown ("AF undefined after shift", bool_t)))
      ]
    | Shiftd(st, s, dst, fill, count) ->
      let open Exp.Binop in
      let origDEST, origCOUNT = Var.create ~tmp:true "origDEST" s,
                                Var.create ~tmp:true "origCOUNT" s in
      let e_dst = op2e s dst in
      let e_fill = op2e s fill in
      let s' = Strip.bits_of_width s in
      (* Check for 64-bit operand *)
      let size = it s' s' in
      let count_mask = Exp.(size - it 1 s') in
      let e_count = Exp.((op2e s count) land count_mask) in
      let new_cf =  match st with
        | LSHIFT -> Exp.(Cast (Cast.LOW, !!bool_t, Var origDEST lsr (size - Var origCOUNT)))
        | RSHIFT -> Exp.(Cast (Cast.HIGH, !!bool_t, Var origDEST lsl (size - Var origCOUNT)))
        | _ -> disfailwith "impossible" in
      let ifzero t e = Exp.(Ite ((Var origCOUNT = it 0 s'), t, e)) in
      let new_of = Exp.(Cast (Cast.HIGH, !!bool_t, (Var origDEST lxor e_dst))) in
      let unk_of =
        Exp.Unknown ("OF undefined after shiftd of more then 1 bit", bool_t) in
      let ret1 = match st with
        | LSHIFT -> Exp.(e_fill lsr (size - Var origCOUNT))
        | RSHIFT -> Exp.(e_fill lsl (size - Var origCOUNT))
        | _ -> disfailwith "impossible" in
      let ret2 = match st with
        | LSHIFT -> Exp.(e_dst lsl Var origCOUNT)
        | RSHIFT -> Exp.(e_dst lsr Var origCOUNT)
        | _ -> disfailwith "impossible" in
      let result = Exp.(ret1 lor ret2) in
      (* SWXXX If shift is greater than the operand size, dst and
         flags are undefined *)
      [ Stmt.Move (origDEST, e_dst);
        Stmt.Move (origCOUNT, e_count);
        assn s dst result;
        Stmt.Move (cf, ifzero cf_e new_cf);
        (* For a 1-bit shift, the OF flag is set if a sign change occurred;
           otherwise, it is cleared. For shifts greater than 1 bit, the OF flag
           is undefined. *)
        Stmt.Move (oF, Exp.(ifzero of_e (Ite (Var origCOUNT = it 1 s', new_of, unk_of))));
        Stmt.Move (sf, ifzero sf_e (compute_sf e_dst));
        Stmt.Move (zf, ifzero zf_e (compute_zf s' e_dst));
        Stmt.Move (pf, ifzero pf_e (compute_pf s e_dst));
        Stmt.Move (af, ifzero af_e (Exp.Unknown ("AF undefined after shiftd", bool_t)))
      ]
    | Rotate(rt, s, dst, shift, use_cf) ->
      let open Exp.Binop in
      (* SWXXX implement use_cf *)
      if use_cf then unimplemented "rotate use_vf";
      let origCOUNT = Var.create ~tmp:true "origCOUNT" s in
      let e_dst = op2e s dst in
      let shift_val = match s with
        | Type.Imm 64 -> 63
        | _ -> 31
      in
      let s' = Strip.bits_of_width s in
      let e_shift = Exp.(op2e s shift land it shift_val s') in
      let size = it (Strip.bits_of_width s) s' in
      let new_cf = match rt with
        | LSHIFT -> Exp.(Cast (Cast.LOW, !!bool_t, e_dst))
        | RSHIFT -> Exp.(Cast (Cast.HIGH, !!bool_t, e_dst))
        | _ -> disfailwith "impossible" in
      let new_of = match rt with
        | LSHIFT -> Exp.(cf_e lxor Cast (Cast.HIGH, !!bool_t, e_dst))
        | RSHIFT -> Exp.(Cast (Cast.HIGH, !!bool_t, e_dst) lxor Cast (Cast.HIGH, !!bool_t, e_dst lsl it 1 s'))
        | _ -> disfailwith "impossible" in
      let unk_of =
        Exp.Unknown ("OF undefined after rotate of more then 1 bit", bool_t) in
      (* this is repeated often enough in the cases perhaps
       * we should bind it at the top of the function... *)
      let ifzero t e = Exp.(Ite (Var origCOUNT = it 0 s', t, e)) in
      let ret1 = match rt with
        | LSHIFT -> Exp.(e_dst lsl Var origCOUNT)
        | RSHIFT -> Exp.(e_dst lsr Var origCOUNT)
        | _ -> disfailwith "impossible" in
      let ret2 = match rt with
        | LSHIFT -> Exp.(e_dst lsr (size - Var origCOUNT))
        | RSHIFT -> Exp.(e_dst lsl (size - Var origCOUNT))
        | _ -> disfailwith "impossible" in
      let result = Exp.(ret1 lor ret2) in
      [
        Stmt.Move (origCOUNT, e_shift);
        assn s dst result;
        (* cf must be set before of *)
        Stmt.Move (cf, ifzero cf_e new_cf);
        Stmt.Move (oF, ifzero of_e Exp.(Ite (Var origCOUNT = it 1 s', new_of, unk_of)));
      ]
    | Bt(t, bitoffset, bitbase) ->
      let t' = Strip.bits_of_width t in
      let offset = op2e t bitoffset in
      let value, shift = match bitbase with
        | Oreg _ ->
          let reg = op2e t bitbase in
          let shift = Exp.(offset land it Pervasives.(t' - 1) t') in
          reg, shift
        | Oaddr a ->
          let byte = load (size_of_typ reg8_t) Exp.(a + (offset lsr it 3 t')) in
          let shift = Exp.(Cast (Cast.LOW, !!reg8_t, offset) land it 7 8) in
          byte, shift
        | Ovec _ | Oseg _ | Oimm _ -> disfailwith "Invalid bt operand"
      in
      [
        Stmt.Move (cf, Exp.(Cast (Cast.LOW, !!bool_t, value lsr shift)));
        Stmt.Move (oF, Exp.Unknown ("OF undefined after bt", bool_t));
        Stmt.Move (sf, Exp.Unknown ("SF undefined after bt", bool_t));
        Stmt.Move (af, Exp.Unknown ("AF undefined after bt", bool_t));
        Stmt.Move (pf, Exp.Unknown ("PF undefined after bt", bool_t))
      ]
    | Bs(t, dst, src, dir) ->
      let t' = Strip.bits_of_width t in
      let source_is_zero = Var.create ~tmp:true "t" bool_t in
      let source_is_zero_v = Exp.Var source_is_zero in
      let src_e = op2e t src in
      let bits = Strip.bits_of_width t in
      let check_bit bitindex next_value =
        Exp.(Ite (Extract (bitindex,bitindex,src_e) = it 1 1, it bitindex t', next_value))
      in
      let bitlist = List.init ~f:Util.id bits in
      (* We are folding from right to left *)
      let bitlist = match dir with
        | Forward -> (* least significant first *) bitlist
        | Backward -> (* most significant *) List.rev bitlist
      in
      let first_one = List.fold_right ~f:check_bit bitlist
          ~init:(Exp.Unknown("bs: destination undefined when source is zero", t)) in
      [
        Stmt.Move (source_is_zero, Exp.(src_e = it 0 t'));
        assn t dst first_one;
        Stmt.Move (zf, Exp.Ite (source_is_zero_v, it 1 1, it 0 1));
      ]
      @
      let undef r =
        let (n,_,t) = Var.V1.serialize r in
        Stmt.Move (r, Exp.Unknown (n^" undefined after bsf", t)) in
      List.map ~f:undef [cf; oF; sf; af; pf]
    | Hlt -> [] (* x86 Hlt is essentially a NOP *)
    | Rdtsc ->
      let undef reg = assn reg32_t reg (Exp.Unknown ("rdtsc", reg32_t)) in
      List.map ~f:undef [o_rax; o_rdx]
    | Cpuid ->
      let undef reg = assn reg32_t reg (Exp.Unknown ("cpuid", reg32_t)) in
      List.map ~f:undef [o_rax; o_rbx; o_rcx; o_rdx]
    | Xgetbv ->
      let undef reg = assn reg32_t reg (Exp.Unknown ("xgetbv", reg32_t)) in
      List.map ~f:undef [o_rax; o_rdx]
    | Stmxcsr (dst) ->
      let dst = match dst with
        | Oaddr addr -> addr
        | _ -> disfailwith "stmxcsr argument cannot be non-memory"
      in
      [store reg32_t dst (Exp.Var mxcsr);(*(Unknown ("stmxcsr", reg32_t));*) ]
    | Ldmxcsr (src) ->
      let src = match src with
        | Oaddr addr -> addr
        | _ -> disfailwith "ldmxcsr argument cannot be non-memory"
      in
      [ Stmt.Move (mxcsr, load (size_of_typ reg32_t) src); ]
    | Fnstcw (dst) ->
      let dst = match dst with
        | Oaddr addr -> addr
        | _ -> disfailwith "fnstcw argument cannot be non-memory"
      in
      [store reg16_t dst (Exp.Var fpu_ctrl); ]
    | Fldcw (src) ->
      let src = match src with
        | Oaddr addr -> addr
        | _ -> disfailwith "fldcw argument cannot be non-memory"
      in
      [ Stmt.Move (fpu_ctrl, load (size_of_typ reg16_t) src); ]
    | Fld _src ->
      unimplemented "unsupported FPU register stack"
    | Fst (_dst,_pop) ->
      unimplemented "unsupported FPU flags"
    | Cmps(Type.Imm _bits as t) ->
      let t' = Strip.bits_of_width t in
      let src1 = Var.create ~tmp:true "src1" t in
      let src2 = Var.create ~tmp:true "src2" t in
      let tmpres = Var.create ~tmp:true "tmp" t in
      let stmts =
        Stmt.Move (src1, op2e t (Oaddr rsi_e))
        :: Stmt.Move (src2, op2e_s mode seg_es has_rex t (Oaddr rdi_e))
        :: Stmt.Move (tmpres, Exp.(Var src1 - Var src2))
        :: string_incr mode t rsi
        :: string_incr mode t rdi
        :: set_flags_sub t' (Exp.Var src1) (Exp.Var src2) (Exp.Var tmpres)
      in
      begin match pref with
        | [] -> stmts
        | [single] when single = repz || single = repnz ->
          rep_wrap ~mode ~check_zf:single ~addr ~next stmts
        | _ -> unimplemented "unsupported flags in cmps" end
    | Scas(Type.Imm _bits as t) ->
      let t' = Strip.bits_of_width t in
      let src1 = Var.create ~tmp:true "src1" t in
      let src2 = Var.create ~tmp:true "src2" t in
      let tmpres = Var.create ~tmp:true "tmp" t in
      let stmts =
        let open Stmt in
        Move (src1, Exp.(Cast (Cast.LOW, !!t, Var rax)))
        :: Move (src2, op2e_s mode seg_es has_rex t (Oaddr rdi_e))
        :: Move (tmpres, Exp.(Var src1 - Var src2))
        :: string_incr mode t rdi
        :: set_flags_sub t' (Exp.Var src1) (Exp.Var src2) (Exp.Var tmpres)
      in
      begin match pref with
        | [] -> stmts
        | [single] when single = repz || single = repnz ->
          rep_wrap ~mode ~check_zf:single ~addr ~next stmts
        | _ -> unimplemented "unsupported flags in scas" end
    | Stos(Type.Imm _bits as t) ->
      let stmts = [store_s mode seg_es t rdi_e (op2e t o_rax);
                   string_incr mode t rdi]
      in
      begin match pref with
        | [] -> stmts
        | [single] when single = repz -> rep_wrap ~mode ~addr ~next stmts
        | _ -> unimplemented "unsupported prefix for stos" end
    | Push(t, o) ->
      let tmp = Var.create ~tmp:true "t" t in (* only really needed when o involves esp *)
      Stmt.Move (tmp, op2e t o)
      :: Stmt.Move (rsp, Exp.(rsp_e - Int (mi (Strip.bytes_of_width t))))
      :: store_s mode seg_ss t rsp_e (Exp.Var tmp) (* FIXME: can ss be overridden? *)
      :: []
    | Pop(t, o) ->
      (* From the manual:

         "The POP ESP instruction increments the stack pointer (ESP)
         before data at the old top of stack is written into the
         destination"

         So, effectively there is no incrementation.
      *)
      assn t o (load_s mode seg_ss (size_of_typ t) rsp_e)
      :: if o = o_rsp then []
      else [Stmt.Move (rsp, Exp.(rsp_e + Int (mi (Strip.bytes_of_width t))))]
    | Pushf(t) ->
      (* Note that we currently treat these fields as unknowns, but the
         manual says: When copying the entire EFLAGS register to the
         stack, the VM and RF flags (bits 16 and 17) are not copied;
         instead, the values for these flags are cleared in the EFLAGS
         image stored on the stack. *)
      let flags_e = match t with
        | Type.Imm 16 -> flags_e
        | Type.Imm 32 -> eflags_e
        | Type.Imm 64 -> rflags_e
        | _ -> failwith "impossible"
      in
      Stmt.Move (rsp, Exp.(rsp_e - Int (mi (Strip.bytes_of_width t))))
      :: store_s mode seg_ss t rsp_e flags_e
      :: []
    | Popf t ->
      let assnsf = match t with
        | Type.Imm 16 -> assns_flags_to_bap
        | Type.Imm 32 -> assns_eflags_to_bap
        | Type.Imm 64 -> assns_rflags_to_bap
        | _ -> failwith "impossible"
      in
      let tmp = Var.create ~tmp:true "t" t in
      let extractlist =
        List.map
          ~f:(fun i ->
              Exp.(Extract (i, i, Var tmp)))
          (List.range ~stride:(-1) ~start:`exclusive ~stop:`inclusive (Strip.bits_of_width t) 0)
      in
      Stmt.Move (tmp, load_s mode seg_ss (size_of_typ t) rsp_e)
      :: Stmt.Move (rsp, Exp.(rsp_e + Int (mi (Strip.bytes_of_width t))))
      :: List.concat (List.map2_exn ~f:(fun f e -> f e) assnsf
      extractlist)
    | Popcnt(t, s, d) ->
      let width = Strip.bits_of_width t in
      let bits = op2e t s in
      let bitvector = Array.to_list (Array.init width ~f:(fun i -> Exp.(Ite (Extract (i, i, bits), it 1 width, it 0 width)))) in
      let count = List.reduce_exn ~f:Exp.(+) bitvector in
      set_zf width bits
      :: assn t d count
      :: List.map ~f:(fun r -> Stmt.Move (r, it 0 1)) [cf; oF; sf; af; pf]
    | Sahf ->
      let assnsf = assns_lflags_to_bap in
      let tah = Var.create ~tmp:true "AH" reg8_t in
      let extractlist =
        List.map
          ~f:(fun i ->
              Exp.(Extract (i, i, Var tah)))
          (List.range ~stride:(-1) ~stop:`inclusive 7 0)
      in
      Stmt.Move (tah, ah_e)
      :: List.concat (List.map2_exn ~f:(fun f e -> f e) assnsf extractlist)
    | Lahf ->
      let o_ah = Oreg 4 in
      [assn reg8_t o_ah lflags_e]
    | Add(t, o1, o2) ->
      let tmp = Var.create ~tmp:true "t1" t in
      let tmp2 = Var.create ~tmp:true "t2" t in
      Stmt.Move (tmp, op2e t o1)
      :: Stmt.Move (tmp2, op2e t o2)
      :: assn t o1 Exp.(op2e t o1 + Var tmp2)
      :: let s1 = Exp.Var tmp in let s2 = Exp.Var tmp2 in let r = op2e t o1 in
      set_flags_add (Strip.bits_of_width t) s1 s2 r
    | Adc(t, o1, o2) ->
      let orig1 = Var.create ~tmp:true "orig1" t in
      let orig2 = Var.create ~tmp:true "orig2" t in
      let bits = Strip.bits_of_width t in
      let t' = Type.Imm (bits + 1) in
      let c e = Exp.(Cast (Cast.UNSIGNED, !!t', e)) in
      (* Literally compute the addition with an extra bit and see
         what the value is for CF *)
      let s1 = Exp.Var orig1 in let s2 = Exp.Var orig2 in let r = op2e t o1 in
      let bige = Exp.(c s1 + c s2 + c (Cast (Cast.UNSIGNED, !!t, cf_e))) in
      Stmt.Move (orig1, op2e t o1)
      :: Stmt.Move (orig2, op2e t o2)
      :: assn t o1 Exp.(s1 + s2 + Cast (Cast.UNSIGNED, !!t, cf_e))
      :: Stmt.Move (cf, Exp.Extract (bits, bits, bige))
      :: set_aopszf_add (Strip.bits_of_width t) s1 s2 r
    | Inc(t, o) (* o = o + 1 *) ->
      let t' = Strip.bits_of_width t in
      let tmp = Var.create ~tmp:true "t" t in
      Stmt.Move (tmp, op2e t o)
      :: assn t o Exp.(op2e t o + it 1 t')
      :: set_aopszf_add t' (Exp.Var tmp) (it 1 t') (op2e t o)
    | Dec(t, o) (* o = o - 1 *) ->
      let t' = Strip.bits_of_width t in
      let tmp = Var.create ~tmp:true "t" t in
      Stmt.Move (tmp, op2e t o)
      :: assn t o Exp.(op2e t o - it 1 t')
      :: set_aopszf_sub t' (Exp.Var tmp) (it 1 t') (op2e t o) (* CF is maintained *)
    | Sub(t, o1, o2) (* o1 = o1 - o2 *) ->
      let oldo1 = Var.create ~tmp:true "t" t in
      Stmt.Move (oldo1, op2e t o1)
      :: assn t o1 Exp.(op2e t o1 - op2e t o2)
      :: set_flags_sub (Strip.bits_of_width t) (Exp.Var oldo1) (op2e t o2) (op2e t o1)
    | Sbb(t, o1, o2) ->
      let tmp_s = Var.create ~tmp:true "ts" t in
      let tmp_d = Var.create ~tmp:true "td" t in
      let orig_s = Exp.Var tmp_s in
      let orig_d = Exp.Var tmp_d in
      let sube = Exp.(orig_s + Cast (Cast.UNSIGNED, !!t, cf_e)) in
      let d = op2e t o1 in
      let s1 = op2e t o2 in
      Stmt.Move (tmp_s, s1)
      :: Stmt.Move (tmp_d, d)
      :: assn t o1 Exp.(orig_d - sube)
      :: Stmt.Move (oF, Exp.(Cast (Cast.HIGH, !!bool_t, (orig_s lxor orig_d) land (orig_d lxor d))))
      (* When src = 0xffffffff and cf=1, the processor sets CF=1.

         Note that we compute dest = dest - (0xffffffff + 1) = 0, so the
         subtraction does not overflow.

         So, I am guessing that CF is set if the subtraction overflows
         or the addition overflows.

         Maybe we should implement this by doing the actual computation,
         like we do for adc.
      *)
      (* sub overflow | add overflow *)
      :: Stmt.Move (cf, Exp.((sube > orig_d) lor (sube < orig_s)))
      :: set_apszf (Strip.bits_of_width t) orig_s orig_d d
    | Cmp(t, o1, o2) ->
      let tmp = Var.create ~tmp:true "t" t in
      Stmt.Move (tmp, Exp.(op2e t o1 - op2e t o2))
      :: set_flags_sub (Strip.bits_of_width t) (op2e t o1) (op2e t o2) (Exp.Var tmp)
    | Cmpxchg(t, src, dst) ->
      let t' = Strip.bits_of_width t in
      let eax_e = op2e t o_rax in
      let dst_e = op2e t dst in
      let src_e = op2e t src in
      let tmp = Var.create ~tmp:true "t" t in
      Stmt.Move (tmp, Exp.(eax_e - dst_e))
      :: set_flags_sub t' eax_e dst_e (Exp.Var tmp)
      @ assn t dst (Exp.Ite (zf_e, src_e, dst_e))
        :: assn t o_rax (Exp.Ite (zf_e, eax_e, dst_e))
        :: []
    | Cmpxchg8b o -> (* only 32bit case *)
      let accumulator = Exp.Concat((op2e reg32_t o_rdx),(op2e reg32_t o_rax)) in
      let dst_e = op2e reg64_t o in
      let src_e = Exp.Concat((op2e reg32_t o_rcx),(op2e reg32_t o_rbx)) in
      let dst_low_e = Exp.Extract(63, 32, dst_e) in
      let dst_hi_e = Exp.Extract(31, 0, dst_e) in
      let eax_e = op2e reg32_t o_rax in
      let edx_e = op2e reg32_t o_rdx in
      let equal = Var.create ~tmp:true "t" bool_t in
      let equal_v = Exp.Var equal in
      [
        Stmt.Move (equal, Exp.(accumulator = dst_e));
        Stmt.Move (zf, equal_v);
        assn reg64_t o (Exp.Ite (equal_v, src_e, dst_e));
        assn reg32_t o_rax (Exp.Ite (equal_v, eax_e, dst_low_e));
        assn reg32_t o_rdx (Exp.Ite (equal_v, edx_e, dst_hi_e))
      ]
    | Xadd(t, dst, src) ->
      let tmp = Var.create ~tmp:true "t" t in
      Stmt.Move (tmp, Exp.(op2e t dst + op2e t src))
      :: assn t src (op2e t dst)
      :: assn t dst (Exp.Var tmp)
      :: let s = Exp.Var tmp in let src = op2e t src in let dst = op2e t dst in
      set_flags_add (Strip.bits_of_width t) s src dst
    | Xchg(t, src, dst) ->
      let tmp = Var.create ~tmp:true "t" t in
      [ Stmt.Move (tmp, op2e t src);
        assn t src (op2e t dst);
        assn t dst (Exp.Var tmp); ]
    | And(t, o1, o2) ->
      assn t o1 Exp.(op2e t o1 land op2e t o2)
      :: Stmt.Move (oF, exp_false)
      :: Stmt.Move (cf, exp_false)
      :: Stmt.Move (af, Exp.Unknown ("AF is undefined after and", bool_t))
      :: set_pszf t (op2e t o1)
    | Or(t, o1, o2) ->
      assn t o1 Exp.(op2e t o1 lor op2e t o2)
      :: Stmt.Move (oF, exp_false)
      :: Stmt.Move (cf, exp_false)
      :: Stmt.Move (af, Exp.Unknown ("AF is undefined after or", bool_t))
      :: set_pszf t (op2e t o1)
    | Xor(t, o1, o2) when o1 = o2 ->
      assn t o1 Exp.(Int (lit 0 (Strip.bits_of_width t)))
      :: Stmt.Move (af, Exp.Unknown ("AF is undefined after xor", bool_t))
      :: List.map ~f:(fun v -> Stmt.Move (v, exp_true)) [zf; pf]
      @  List.map ~f:(fun v -> Stmt.Move (v, exp_false)) [oF; cf; sf]
    | Xor(t, o1, o2) ->
      assn t o1 Exp.(op2e t o1 lxor op2e t o2)
      :: Stmt.Move (oF, exp_false)
      :: Stmt.Move (cf, exp_false)
      :: Stmt.Move (af, Exp.Unknown ("AF is undefined after xor", bool_t))
      :: set_pszf t (op2e t o1)
    | Test(t, o1, o2) ->
      let tmp = Var.create ~tmp:true "t" t in
      Stmt.Move (tmp, Exp.(op2e t o1 land op2e t o2))
      :: Stmt.Move (oF, exp_false)
      :: Stmt.Move (cf, exp_false)
      :: Stmt.Move (af, Exp.Unknown ("AF is undefined after and", bool_t))
      :: set_pszf t (Exp.Var tmp)
    | Ptest(t, o1, o2) ->
      let open Stmt in
      let t' = Strip.bits_of_width t in
      let tmp1 = Var.create ~tmp:true "t1" t in
      let tmp2 = Var.create ~tmp:true "t2" t in
      Move (tmp1, Exp.(op2e t o2 land op2e t o1))
      :: Move (tmp2, Exp.(op2e t o2 land (exp_not (op2e t o1))))
      :: Move (af, exp_false)
      :: Move (oF, exp_false)
      :: Move (pf, exp_false)
      :: Move (sf, exp_false)
      :: Move (zf, Exp.(Var tmp1 = Int (lit 0 t')))
      :: [Move (cf, Exp.(Var tmp2 = Int (lit 0 t')))]
    | Not(t, o) ->
      [assn t o (exp_not (op2e t o))]
    | Neg(t, o) ->
      let t' = Strip.bits_of_width t in
      let tmp = Var.create ~tmp:true "t" t in
      let min_int =
        Exp.BinOp (Exp.Binop.LSHIFT, it 1 t', it (t'-1) t')
      in
      Stmt.Move (tmp, op2e t o)
      ::assn t o Exp.(it 0 t' - op2e t o)
      ::Stmt.Move (cf, Exp.(Ite (Var tmp = it 0 t', it 0 1, it 1 1)))
      ::Stmt.Move (oF, Exp.(Ite (Var tmp = min_int, it 1 1, it 0 1)))
      ::set_apszf_sub t' (Exp.Var tmp) (it 0 t') (op2e t o)
    | Mul (t, src) ->
      (* Mul always multiplies EAX by src and stores the result in EDX:EAX
         starting from the "right hand side" based on the type t of src *)

      (* The OF and CF flags are set to 0 if the upper half of the result is 0;
         otherwise, they are set to 1 *)
      let new_t = Type.Imm ((Strip.bits_of_width t)*2) in
      let assnstmts, assne = Exp.(assn_dbl t ((Cast (Cast.UNSIGNED, !!new_t, op2e t o_rax)) * (Cast (Cast.UNSIGNED, !!new_t, op2e t src))))
      in
      let flag =
        let highbit = Strip.bits_of_width new_t - 1 in
        let lowbit = Strip.bits_of_width new_t / 2 in
        Exp.((Extract (highbit, lowbit, assne)) <> it 0 (Strip.bits_of_width t))
      in
      assnstmts
      @
      [
        Stmt.Move (oF, flag);
        Stmt.Move (cf, flag);
        Stmt.Move (sf, Exp.Unknown ("SF is undefined after Mul", bool_t));
        Stmt.Move (zf, Exp.Unknown ("ZF is undefined after Mul", bool_t));
        Stmt.Move (af, Exp.Unknown ("AF is undefined after Mul", bool_t));
        Stmt.Move (pf, Exp.Unknown ("PF is undefined after Mul", bool_t))
      ]
    | Imul (t, (oneopform, dst), src1, src2) ->
      let new_t = Type.Imm ((Strip.bits_of_width t)*2) in
      let mul_stmts =
        (match oneopform with
         | true ->
           (* For one operand form, use assn_double *)
           let assnstmts, assne =
             assn_dbl t Exp.((Cast (Cast.SIGNED, !!new_t, op2e t src1)) * (Cast (Cast.SIGNED, !!new_t, op2e t src2))) in
           let flag =
             (* Intel checks if EAX == EDX:EAX.  Instead of doing this, we are just
                going to check if the upper bits are != 0 *)
             let highbit = Strip.bits_of_width new_t - 1 in
             let lowbit = Strip.bits_of_width new_t / 2 in
             Exp.((Extract (highbit, lowbit, assne)) <> it 0 (Strip.bits_of_width t))
           in
           assnstmts @
           [Stmt.Move (oF, flag);
            Stmt.Move (cf, flag)]
         | false ->
           (* Two and three operand forms *)
           let tmp = Var.create ~tmp:true "t" new_t in
           (* Flag is set when the result is truncated *)
           let flag = Exp.(Var tmp <> Cast (Cast.SIGNED, !!new_t, op2e t dst)) in
           [(Stmt.Move (tmp, Exp.((Cast (Cast.SIGNED, !!new_t, op2e t src1)) * (Cast (Cast.SIGNED, !!new_t, op2e t src2)))));
            (assn t dst Exp.(Cast (Cast.LOW, !!t, Var tmp)));
            Stmt.Move (oF, flag);
            Stmt.Move (cf, flag)] )
      in
      mul_stmts@[
        Stmt.Move (pf, Exp.Unknown ("PF is undefined after imul", bool_t));
        Stmt.Move (sf, Exp.Unknown ("SF is undefined after imul", bool_t));
        Stmt.Move (zf, Exp.Unknown ("ZF is undefined after imul", bool_t));
        Stmt.Move (af, Exp.Unknown ("AF is undefined after imul", bool_t));]
    | Div(t, src) ->
      let dt' = Strip.bits_of_width t * 2 in
      let dt = Type.Imm dt' in
      let dividend = op2e_dbl t in
      let divisor = Exp.(Cast (Cast.UNSIGNED, !!dt, op2e t src)) in
      let tdiv = Var.create ~tmp:true "div" dt in
      let trem = Var.create ~tmp:true "rem" dt in
      let assne = Exp.((Cast (Cast.LOW, !!t, Exp.Var trem)) ^ (Cast (Cast.LOW, !!t, Var tdiv))) in
      Stmt.If (Exp.(divisor = it 0 dt'), [Cpu_exceptions.divide_by_zero], [])
      :: Stmt.Move (tdiv, Exp.(dividend / divisor))
      :: Stmt.Move (trem, Exp.(dividend mod divisor))
      (* Overflow is indicated with the #DE (divide error) exception
         rather than with the CF flag. *)
      :: Stmt.If (Exp.((Cast (Cast.HIGH, !!t, Var tdiv)) = it 0 (Strip.bits_of_width t)), [], [Cpu_exceptions.divide_by_zero])
      :: fst (assn_dbl t assne)
      @ (let undef r =
          let n,_,t = Var.V1.serialize r in
          Stmt.Move (r, Exp.Unknown ((n^" undefined after div"), t))
         in
         List.map ~f:undef [cf; oF; sf; zf; af; pf])
    | Idiv(t, src) ->
      let dt' = Strip.bits_of_width t * 2 in
      let dt = Type.Imm dt' in
      let dividend = op2e_dbl t in
      let divisor = Exp.(Cast (Cast.SIGNED, !!dt, op2e t src)) in
      let tdiv = Var.create ~tmp:true "div" dt in
      let trem = Var.create ~tmp:true "rem" dt in
      let assne = Exp.((Cast (Cast.LOW, !!t, Var trem)) ^ (Cast (Cast.LOW, !!t, Var tdiv))) in
      Stmt.If (Exp.(divisor = it 0 dt'), [Cpu_exceptions.divide_by_zero], [])
      :: Stmt.Move (tdiv, Exp.(dividend /$ divisor))
      :: Stmt.Move (trem, Exp.(dividend %$ divisor))
      (* Overflow is indicated with the #DE (divide error) exception
         rather than with the CF flag. *)
      (* SWXXX For signed division make sure quotient is between smallest and
         largest values.  For type t, this would be -2^(t/2) to (2^(t/2) - 1). *)
      :: Stmt.If (Exp.((Cast (Cast.HIGH, !!t, Var tdiv)) = it 0 (Strip.bits_of_width t)), [], [Cpu_exceptions.divide_by_zero])
      :: fst (assn_dbl t assne)
      @ (let undef r =
          let n,_,t = Var.V1.serialize r in
          Stmt.Move (r, Exp.Unknown (n^" undefined after div", t)) in
         List.map ~f:undef [cf; oF; sf; zf; af; pf])
    | Cld ->
      [Stmt.Move (df, exp_false)]
    | Leave t when pref = [] -> (* #UD if Lock prefix is used *)
      Stmt.Move (rsp, rbp_e)
      ::to_ir mode addr next ss pref has_rex has_vex (Pop(t, o_rbp))
    | Interrupt3 -> [Stmt.Special "int3"]
    | Interrupt(Oimm i) ->
      (** use [BV.string_of_value ~hex:true] here *)
      [Stmt.Special (Printf.sprintf "int 0x%x" (BZ.int_of_big_int i))]
    | Sysenter | Syscall -> [Stmt.Special "syscall"]
    (* Match everything exhaustively *)
    | Leave _ ->  unimplemented "to_ir: Leave"
    | Call _ ->  unimplemented "to_ir: Call"
    | Lea _ ->  unimplemented "to_ir: Lea"
    | Movs _ ->  unimplemented "to_ir: Movs"
    | Cmps _ ->  unimplemented "to_ir: Cmps"
    | Scas _ ->  unimplemented "to_ir: Scas"
    | Stos _ ->  unimplemented "to_ir: Stos"
    | Retn _ ->  unimplemented "to_ir: Retn"
    | Interrupt _ ->  unimplemented "to_ir: Interrupt"

end (* ToIR *)

let disasm_instr mode g addr =
  let module D = Bap_disasm_x86 in
  let (pref, prefix, op, na) = D.parse_instr mode g addr in
  let has_rex = prefix.rex <> None in
  let has_vex = prefix.vex <> None in
  let (ss, pref) = D.parse_prefixes mode pref op in
  let ir = ToIR.to_ir mode addr na ss pref has_rex has_vex op in
  (ir, na)

let insn mem insn = Or_error.unimplemented "not implemented"
