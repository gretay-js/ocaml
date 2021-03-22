(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction selection for the AMD64 *)

open Arch
open Proc
open Cmm
open Mach

(* Auxiliary for recognizing addressing modes *)

type addressing_expr =
    Asymbol of string
  | Alinear of expression
  | Aadd of expression * expression
  | Ascale of expression * int
  | Ascaledadd of expression * expression * int

let rec select_addr exp =
  match exp with
    Cconst_symbol (s, _) when not !Clflags.dlcode ->
      (Asymbol s, 0)
  | Cop((Caddi | Caddv | Cadda), [arg; Cconst_int (m, _)], _) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop(Csubi, [arg; Cconst_int (m, _)], _) ->
      let (a, n) = select_addr arg in (a, n - m)
  | Cop((Caddi | Caddv | Cadda), [Cconst_int (m, _); arg], _) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop(Clsl, [arg; Cconst_int((1|2|3 as shift), _)], _) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, 1 lsl shift), n lsl shift)
      | _ -> (Alinear exp, 0)
      end
  | Cop(Cmuli, [arg; Cconst_int((2|4|8 as mult), _)], _) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, mult), n * mult)
      | _ -> (Alinear exp, 0)
      end
  | Cop(Cmuli, [Cconst_int((2|4|8 as mult), _); arg], _) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, mult), n * mult)
      | _ -> (Alinear exp, 0)
      end
  | Cop((Caddi | Caddv | Cadda), [arg1; arg2], _) ->
      begin match (select_addr arg1, select_addr arg2) with
          ((Alinear e1, n1), (Alinear e2, n2)) ->
              (Aadd(e1, e2), n1 + n2)
        | ((Alinear e1, n1), (Ascale(e2, scale), n2)) ->
              (Ascaledadd(e1, e2, scale), n1 + n2)
        | ((Ascale(e1, scale), n1), (Alinear e2, n2)) ->
              (Ascaledadd(e2, e1, scale), n1 + n2)
        | (_, (Ascale(e2, scale), n2)) ->
              (Ascaledadd(arg1, e2, scale), n2)
        | ((Ascale(e1, scale), n1), _) ->
              (Ascaledadd(arg2, e1, scale), n1)
        | _ ->
              (Aadd(arg1, arg2), 0)
      end
  | arg ->
      (Alinear arg, 0)

(* Special constraints on operand and result registers *)

exception Use_default

let rax = phys_reg 0
let rcx = phys_reg 5
let rdx = phys_reg 4

let pseudoregs_for_operation op arg res =
  match op with
  (* Two-address binary operations: arg.(0) and res.(0) must be the same *)
    Iintop(Iadd|Isub|Imul|Iand|Ior|Ixor) | Iaddf|Isubf|Imulf|Idivf ->
      ([|res.(0); arg.(1)|], res)
  (* One-address unary operations: arg.(0) and res.(0) must be the same *)
  | Iintop_imm((Iadd|Isub|Imul|Iand|Ior|Ixor|Ilsl|Ilsr|Iasr), _)
  | Iabsf | Inegf
  | Ispecific(Ibswap (32|64)) ->
      (res, res)
  (* For xchg, args must be a register allowing access to high 8 bit register
     (rax, rbx, rcx or rdx). Keep it simple, just force the argument in rax. *)
  | Ispecific(Ibswap 16) ->
      ([| rax |], [| rax |])
  (* For imulq, first arg must be in rax, rax is clobbered, and result is in
     rdx. *)
  | Iintop(Imulh) ->
      ([| rax; arg.(1) |], [| rdx |])
  | Ispecific(Ifloatarithmem(_,_)) ->
      let arg' = Array.copy arg in
      arg'.(0) <- res.(0);
      (arg', res)
  (* For shifts with variable shift count, second arg must be in rcx *)
  | Iintop(Ilsl|Ilsr|Iasr) ->
      ([|res.(0); rcx|], res)
  (* For div and mod, first arg must be in rax, rdx is clobbered,
     and result is in rax or rdx respectively.
     Keep it simple, just force second argument in rcx. *)
  | Iintop(Idiv) ->
      ([| rax; rcx |], [| rax |])
  | Iintop(Imod) ->
      ([| rax; rcx |], [| rdx |])
  | Ispecific Irdtsc ->
  (* For rdtsc instruction, the result is in edx (high) and eax (low).
     Make it simple and force the result in rdx and rax clobbered. *)
    ([| |], [| rdx |])
  | Ispecific Irdpmc ->
  (* For rdpmc instruction, the argument must be in ecx
     and the result is in edx (high) and eax (low).
     Make it simple and force the argument in rcx, the result in rdx,
     and rax clobbered *)
    ([| rcx |], [| rdx |])
  | Ispecific Icrc32q ->
    (* arg.(0) and res.(0) must be the same *)
    ([|res.(0); arg.(1)|], res)
  | Ispecific (Ibswap _) -> assert false
  (* Other instructions are regular *)
  | Iintop (Ipopcnt|Iclz _|Icomp _|Icheckbound _)
  | Iintop_imm ((Imulh|Idiv|Imod|Ipopcnt|Iclz _|Icomp _|Icheckbound _), _)
  | Ispecific (Isqrtf|Isextend32|Izextend32|Ilzcnt|Ilea _|Istore_int (_, _, _)
              |Ioffset_loc (_, _)|Ifloatsqrtf _|Ibsr _|Ibsf _|Iprefetch _)
  | Imove|Ispill|Ireload|Ifloatofint|Iintoffloat|Iconst_int _|Iconst_float _
  | Iconst_symbol _|Icall_ind _|Icall_imm _|Itailcall_ind _|Itailcall_imm _
  | Iextcall _|Istackoffset _|Iload (_, _)|Istore (_, _, _)|Ialloc _
  | Iname_for_debugger _|Iprobe _|Iprobe_is_enabled _
    -> raise Use_default

(* If you update [inline_ops], you may need to update [is_simple_expr] and/or
   [effects_of], below. *)
(* XCR mshinwell: Please put these in the same order as in [select_operation]
   below, so it's easier to make sure none are missing. *)
(* Names in [inline_ops] that start with '*' are internal to Selection.
   They work around the limitation of [select_operation] that
   keeps [args] as Cmm terms even after translating
   the operation itself to Mach. *)
(* Keep in the same order as in [select_operation] below to make it easier
   to keep in sync. The new check in [select_operation]
   in combination with new tests helps guard against missing cases
   and misspelled names. *)
let inline_ops =
  [ "caml_bswap16_direct";
    "caml_int32_direct_bswap";
    "caml_int64_direct_bswap";
    "caml_nativeint_direc_bswap";
    "caml_rdtsc_unboxed";
    "caml_rdpmc_unboxed";
    "caml_int64_bsr_unboxed";
    "caml_nativeint_bsr_unboxed";
    "caml_int_bsr_untagged";
    (* XCR mshinwell: What is this one with the percent?  I didn't think %
       should appear at this stage.
       ...ah, I now see that there is a comment about this below.  There
       should be a comment here too, pointing out this is an unfortunate hack
       necessitated by the fact that [args] in the function below are still
       Cmm terms.

       gyorsh: I changed '%' to '*' here so it is less confusing
       with names of [external] primitives.
    *)
    "*int64_bsr";
    "caml_untagged_int_ctz";
    "caml_int64_ctz_unboxed";
    "caml_nativeint_ctz_unboxed";
    "caml_int64_crc_unboxed";
    "caml_int_crc_untagged";
    "caml_int_lzcnt_untagged";
    (* XCR mshinwell: This list appears to be missing:
       caml_int64_crc_unboxed
       caml_int_crc_untagged

       gyorsh: fixed, and also found one more that was missing.
     *)
  ]

let select_locality (l : Cmm.prefetch_temporal_locality_hint)
  : Arch.prefetch_temporal_locality_hint =
  match l with
  | Not_at_all -> Not_at_all
  | Low -> Low
  | Moderate -> Moderate
  | High -> High

(* The selector class *)


let one_arg name args =
  match args with
  | [arg] -> arg
  | _ ->
    Misc.fatal_errorf "Selection: expected exactly 1 argument for %s" name

class selector = object (self)

inherit Spacetime_profiling.instruction_selection as super

method is_immediate n = n <= 0x7FFF_FFFF && n >= (-1-0x7FFF_FFFF)
  (* -1-.... : hack so that this can be compiled on 32-bit
     (cf 'make check_all_arches') *)

method is_immediate_natint n = n <= 0x7FFFFFFFn && n >= -0x80000000n

method! is_simple_expr e =
  match e with
  | Cop(Cprefetch _, _, _) -> false
  (* inlined ops are simple if their arguments are *)
  | Cop(Cextcall { name = "sqrt" }, args, _) ->
      List.for_all self#is_simple_expr args
  | Cop(Cextcall { name = fn; builtin = true }, args, _)
    when List.mem fn inline_ops ->
      (* CR mshinwell: As per the CR in [effects_of] below, we should not be
         deeming these operations as "simple" if the original [external]
         declaration says that they do in fact have (co)effects. *)
      List.for_all self#is_simple_expr args
  | _ ->
      super#is_simple_expr e

method! effects_of e =
  match e with
  (* XCR mshinwell: This next line isn't needed, Selectgen takes care of this. *)
  | Cop(Cextcall { name = "sqrt" }, args, _) ->
      (* CR mshinwell: I don't think we should remove sqrt from the inline_ops
         list.  What about making inline_ops be a list of records, with each
         record specifying the expected name and also the expected value of
         [builtin]?

         gyorsh: ideally, "sqrt" should simply be marked with builtin,
         and the corresponding no_effects and no_coeffects,
         but it would require a change in stdlib.
      *)
      Selectgen.Effect_and_coeffect.join_list_map args self#effects_of
  | Cop(Cextcall { name = fn; builtin = true }, args, _)
    when List.mem fn inline_ops ->
      (* CR mshinwell: Shouldn't we use the effect/coeffect judgement
         provided on the [external] declaration?  This will require
         augmenting [Cextcall] with the relevant information. *)
      Selectgen.Effect_and_coeffect.join_list_map args self#effects_of
  | _ ->
      super#effects_of e

method select_addressing _chunk exp =
  let (a, d) = select_addr exp in
  (* PR#4625: displacement must be a signed 32-bit immediate *)
  if not (self # is_immediate d)
  then (Iindexed 0, exp)
  else match a with
    | Asymbol s ->
        (Ibased(s, d), Ctuple [])
    | Alinear e ->
        (Iindexed d, e)
    | Aadd(e1, e2) ->
        (Iindexed2 d, Ctuple[e1; e2])
    | Ascale(e, scale) ->
        (Iscaled(scale, d), e)
    | Ascaledadd(e1, e2, scale) ->
        (Iindexed2scaled(scale, d), Ctuple[e1; e2])

method! select_store is_assign addr exp =
  match exp with
    Cconst_int (n, _dbg) when self#is_immediate n ->
      (Ispecific(Istore_int(Nativeint.of_int n, addr, is_assign)), Ctuple [])
  | (Cconst_natint (n, _dbg)) when self#is_immediate_natint n ->
      (Ispecific(Istore_int(n, addr, is_assign)), Ctuple [])
  | (Cblockheader(n, _dbg))
      when self#is_immediate_natint n && not Config.spacetime ->
      (Ispecific(Istore_int(n, addr, is_assign)), Ctuple [])
  | Cconst_pointer (n, _dbg) when self#is_immediate n ->
      (Ispecific(Istore_int(Nativeint.of_int n, addr, is_assign)), Ctuple [])
  | Cconst_natpointer (n, _dbg) when self#is_immediate_natint n ->
      (Ispecific(Istore_int(n, addr, is_assign)), Ctuple [])
  | _ ->
      super#select_store is_assign addr exp

method! select_operation op args dbg =
  match op with
  (* Recognize the LEA instruction *)
    Caddi | Caddv | Cadda | Csubi ->
      begin match self#select_addressing Word_int (Cop(op, args, dbg)) with
        (Iindexed _, _)
      | (Iindexed2 0, _) -> super#select_operation op args dbg
      | (addr, arg) -> (Ispecific(Ilea addr), [arg])
      end
  (* Recognize float arithmetic with memory. *)
  | Caddf ->
      self#select_floatarith true Iaddf Ifloatadd args
  | Csubf ->
      self#select_floatarith false Isubf Ifloatsub args
  | Cmulf ->
      self#select_floatarith true Imulf Ifloatmul args
  | Cdivf ->
      self#select_floatarith false Idivf Ifloatdiv args
  | Cextcall { name = "sqrt"; alloc = false; } ->
     begin match args with
       [Cop(Cload ((Double|Double_u as chunk), _), [loc], _dbg)] ->
         let (addr, arg) = self#select_addressing chunk loc in
         (Ispecific(Ifloatsqrtf addr), [arg])
     | [arg] ->
         (Ispecific Isqrtf, [arg])
     | _ ->
         assert false
     end
  (* Recognize store instructions *)
  | Cstore ((Word_int|Word_val as chunk), _init) ->
      begin match args with
        [loc; Cop(Caddi, [Cop(Cload _, [loc'], _); Cconst_int (n, _dbg)], _)]
        when loc = loc' && self#is_immediate n ->
          let (addr, arg) = self#select_addressing chunk loc in
          (Ispecific(Ioffset_loc(n, addr)), [arg])
      | _ ->
          super#select_operation op args dbg
      end
  (* CR mshinwell for mshinwell: Re-read this case after first round of
     review *)
  | Cextcall { name; builtin = true; ret; label_after } ->
    (* XCR mshinwell: The standard in this file is unfortunately two more
       spaces of indentation for match cases; let's follow that for
       consistency.

       gyorsh: fixed.

    *)
    (* XCR mshinwell: Let's please remove the tuple brackets on these return
       values; they aren't needed, and tend to clutter.  This is quite
       complicated to look at as it is...

       gyorsh: fixed in the new code.
       Should I also fix it in the rest of the function, or leave it as is?
    *)
    (* CR mshinwell: Can we check [ret] in all of these cases?  It's presumably
       uniquely defined in each case.

       gyorsh: ah, that's a great idea! added.
    *)
      begin match name, ret with
      | "caml_bswap16_direct", [|Int|] -> Ispecific (Ibswap 16), args
      | "caml_int32_direct_bswap", [|Int|] -> Ispecific (Ibswap 32), args
      | "caml_int64_direct_bswap", [|Int|]
      | "caml_nativeint_direct_bswap", [|Int|] -> Ispecific (Ibswap 64), args
      | "caml_rdtsc_unboxed", [|Int|] -> Ispecific Irdtsc, args
      | "caml_rdpmc_unboxed", [|Int|] -> Ispecific Irdpmc, args
      | "caml_int64_bsr_unboxed", [|Int|]
      | "caml_nativeint_bsr_unboxed", [|Int|] ->
        Ispecific(Ibsr { arg_is_non_zero = false; }), args
      | "caml_int_bsr_untagged", [|Int|] ->
        (* CR mshinwell: Is it guaranteed that the Cop Cextcall will return a
           tagged integer?  There should be a comment explaining why that is
           the case, I think.  Can we add an assertion on [ret] to help check?
           If we're going to use Isub 1 to remove the tag, it is imperative that
           the value is tagged already, or the answer will be wrong (unlike if
           we used a masking).

           gyorsh: yes, it's guaranteed by the absence of [@untagged] annotation
           on the declaration of the intrinsic.
        *)
        Iintop_imm (Isub, 1),
        [ Cop(Cextcall{ name = "*int64_bsr"; builtin = true; ret;
                        alloc = false; label_after; },
              args, dbg) ]
      | "*int64_bsr", [|Int|] ->
        (* '*' guarantees that it won't clash with user defined names *)
        Ispecific(Ibsr {arg_is_non_zero=true}), args
      | "caml_untagged_int_ctz", [|Val|] ->
        (* CR mshinwell: It's hard to follow what the argument and return types
           of these intrinsics are.  Maybe we could establish a standard naming
           scheme that names both the argument and result types at all times?
           Also I think we should use proper names rather than abbreviations
           for the names, both to avoid confusion, and to avoid platform-specific
           names in code that is supposed to be generic.
           Combining both of these suggestions would yield names like:
           caml_count_trailing_zeroes_untagged_int_to_untagged_int

           gyorsh: yes, I like the explicit pattern <arg>_to_<res>.
           I didn't dare making the names even longer. I'll keep the instruction
           names for intrinsics that are archi
        *)
        (* Takes untagged int and returns untagged int.
           Setting the top bit does not change the result of 63 bit operation,
           and guarantees the input is non-zero, which is required because
           [bsf] instruction is not defined on input 0. *)
        (* XCR mshinwell: This should explain why it's beneficial for the input
           to be guaranteed not to be zero *)
        (* XCR mshinwell: This needs some more explanation.  Maybe augment the
           commented-out line with some explanatory text (presuming that this
           is the code that emits this extra byte)? *)
        (*
           The expression [x lor (1 lsl 63)] sets the top bit of x.
           [1 lsl 64] is a constant 64-bit value with the top bit 1
           and all other bits are 0. The constant can be precomputed statically:

             Cconst_natint ((Nativeint.shift_left 1n 63), dbg)

           However, the encoding of this OR instruction with the large static
           constant is 10 bytes long. Instead, we emit a shift instruction,
           which is 1 byte shorter. This will not require an extra register,
           unless both argument and result of bsf are in the same register. *)
        let c = Cop(Clsl, [Cconst_natint (1n, dbg); Cconst_int (63, dbg)], dbg) in
        Ispecific (Ibsf {arg_is_non_zero=true}),
        (* XCR mshinwell: As per comment elsewhere, don't use List.hd, as it
           might produce an unhelpful exception. *)
         [Cop(Cor, [one_arg "ctz" args; c], dbg)]
      | "caml_int64_ctz_unboxed", [|Int|]
      | "caml_nativeint_ctz_unboxed", [|Int|] ->
        Ispecific(Ibsf {arg_is_non_zero=false}), args
      | ("caml_int64_crc_unboxed"
         | "caml_int_crc_untagged"), [|Int|] when !Arch.crc32_support ->
        Ispecific Icrc32q, args
      (* Some Intel targets do not support popcnt and lzcnt *)
      | "caml_int_lzcnt_untagged", [|Int|] when !lzcnt_support ->
        (* XCR mshinwell: This appears to be a duplicate of the [Cclz] case
           below?  If this extcall should have always been caught in the Cmm
           stage, this should be a fatal error.

           It's not exactly the same.
           We have a special intrinsics to emit lzcnt instruction,
           whereas Cclz emits an instruction sequence using bsr unless lzcnt is
           supported.
        *)
        Ispecific Ilzcnt, args
      | _ ->
        (* CR mshinwell: Add a check here to make sure that [name] is not in
           [inline_ops]?  I'm worried about missing cases, since there are a lot
           of intrinsics now. *)
        super#select_operation op args dbg
      end
  | Cclz _ when !lzcnt_support -> Ispecific Ilzcnt, args
  | Cprefetch { is_write; locality; } ->
      (* Emit a regular prefetch hint when prefetchw is not supported.
         Matches the behavior of gcc's __builtin_prefetch *)
      let is_write =
        if is_write && not !prefetchw_support
        then false
        else is_write
      in
      (* XCR mshinwell: List.hd again *)
      let addr, eloc = self#select_addressing Word_int
                         (one_arg "prefetch" args) in
      let locality = select_locality locality in
      Ispecific (Iprefetch { is_write; addr; locality; }), [eloc]
  (* AMD64 does not support immediate operands for multiply high signed *)
  | Cmulhi ->
      (Iintop Imulh, args)
  | Casr ->
      begin match args with
        (* Recognize sign extension *)
        [Cop(Clsl, [k; Cconst_int (32, _)], _); Cconst_int (32, _)] ->
          (Ispecific Isextend32, [k])
        | _ -> super#select_operation op args dbg
      end
  (* Recognize zero extension *)
  | Cand ->
    begin match args with
    | [arg; Cconst_int (0xffff_ffff, _)]
    | [arg; Cconst_natint (0xffff_ffffn, _)]
    | [Cconst_int (0xffff_ffff, _); arg]
    | [Cconst_natint (0xffff_ffffn, _); arg] ->
      Ispecific Izextend32, [arg]
    | _ -> super#select_operation op args dbg
    end
  | _ -> super#select_operation op args dbg

(* Recognize float arithmetic with mem *)

method select_floatarith commutative regular_op mem_op args =
  match args with
    [arg1; Cop(Cload ((Double|Double_u as chunk), _), [loc2], _)] ->
      let (addr, arg2) = self#select_addressing chunk loc2 in
      (Ispecific(Ifloatarithmem(mem_op, addr)),
                 [arg1; arg2])
  | [Cop(Cload ((Double|Double_u as chunk), _), [loc1], _); arg2]
        when commutative ->
      let (addr, arg1) = self#select_addressing chunk loc1 in
      (Ispecific(Ifloatarithmem(mem_op, addr)),
                 [arg2; arg1])
  | [arg1; arg2] ->
      (regular_op, [arg1; arg2])
  | _ ->
      assert false

method! mark_c_tailcall =
  contains_calls := true

(* Deal with register constraints *)

method! insert_op_debug env op dbg rs rd =
  try
    let (rsrc, rdst) = pseudoregs_for_operation op rs rd in
    self#insert_moves env rs rsrc;
    self#insert_debug env (Iop op) dbg rsrc rdst;
    self#insert_moves env rdst rd;
    rd
  with Use_default ->
    super#insert_op_debug env op dbg rs rd

end

let fundecl f = (new selector)#emit_fundecl f
