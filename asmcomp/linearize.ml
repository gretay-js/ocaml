(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Transformation of Mach code into a list of pseudo-instructions. *)
open Linear

(* Cons a simple instruction (arg, res, live empty) *)

let cons_instr d n =
  { desc = d; next = n; arg = [||]; res = [||];
    dbg = Debuginfo.none; live = Reg.Set.empty }

(* Build an instruction with arg, res, dbg, live taken from
   the given Mach.instruction *)

let copy_instr d i n =
  { desc = d; next = n;
    arg = i.Mach.arg; res = i.Mach.res;
    dbg = i.Mach.dbg; live = i.Mach.live }

(*
   Label the beginning of the given instruction sequence.
   - If the sequence starts with a branch, jump over it.
   - If the sequence is the end, (tail call position), just do nothing
*)

let get_label n = match n.desc with
    Lbranch lbl -> (lbl, n)
  | Llabel lbl -> (lbl, n)
  | Lend -> (-1, n)
  | _ -> let lbl = Cmm.new_label() in (lbl, cons_instr (Llabel lbl) n)

(* Check the fallthrough label *)
let check_label n = match n.desc with
  | Lbranch lbl -> lbl
  | Llabel lbl -> lbl
  | _ -> -1


(* Add pseudo-instruction Ladjust_trap_depth in front of a continuation
   to notify assembler generation about updates to the stack as a result
   of differences in exception trap depths.
   The argument delta is the number of trap frames (not bytes). *)

let rec adjust_trap_depth delta_traps next =
  (* Simplify by merging and eliminating Ladjust_trap_depth instructions
     whenever possible. *)
  match next.desc with
  | Ladjust_trap_depth { delta_traps = k } ->
    adjust_trap_depth (delta_traps + k) next.next
  | _ ->
    if delta_traps = 0 then next
    else cons_instr (Ladjust_trap_depth { delta_traps }) next

(* Combine consecutive align directives. *)
let rec combine_align k n =
  match n.desc with
  | Lalign m -> combine_align (max k m) n.next
  | _ -> cons_instr (Lalign k) n

(* Discard all instructions up to the next label.
   This function is to be called before adding a non-terminating
   instruction. *)

let rec discard_dead_code n =
  let adjust trap_depth =
    adjust_trap_depth trap_depth (discard_dead_code n.next)
  in
  match n.desc with
    Lend -> n
  | Llabel _ -> n
    (* Do not discard Lpoptrap/Lpushtrap/Ladjust_trap_depth
       or Istackoffset instructions, as this may cause a stack imbalance
       later during assembler generation. Replace them
       with pseudo-instruction Ladjust_trap_depth with the corresponding
       stack offset and eliminate dead instructions after them. *)
  | Lpoptrap -> adjust (-1)
  | Lpushtrap _ -> adjust (+1)
  | Ladjust_trap_depth { delta_traps } -> adjust delta_traps
  | Lop(Istackoffset _) ->
    (* This dead instruction cannot be replaced by Ladjust_trap_depth,
       because the units don't match: the argument of Istackoffset is in bytes,
       whereas the argument of Ladjust_trap_depth is in trap frames,
       and the size of trap frames is machine-dependant and therefore not
       available here.  *)
    { n with next = discard_dead_code n.next; }
  | Lalign k ->
    (* CR gyorsh: Lalign implicitly refers to the next instruction.
       We need to keep track of this when
       instructions are discarded. The semantics isn't clear to me.
       Should alignment directives be accumulated like Ladjust_trap_depth?
       For full generality, we should be able to place .align anywhere
       in the instruction sequence, but we only use it before labels, currently.
       Alternatively, we could add an "align" field to Llabel,
       or add "align" field to Linear.instruction. *)
    let n = combine_align k n.next in
    (* Do not discard alignment directive immediately followed by a label. *)
    (match n.next.desc with
     | Llabel _ -> n (* CR gyorsh: how to avoid repeating the base case? *)
     | _ -> discard_dead_code n.next)
  | _ -> discard_dead_code n.next

(*
   Add a branch in front of a continuation.
   Discard dead code in the continuation.
   Does not insert anything if we're just falling through
   or if we jump to dead code after the end of function (lbl=-1)
*)

let add_branch lbl n =
  if lbl >= 0 then
    let n1 = discard_dead_code n in
    match n1.desc with
    | Llabel lbl1 when lbl1 = lbl -> n1
    | Lalign _ ->
      (match n1.next.desc with
       | Llabel lbl1 when lbl1 = lbl -> n1 (* CR gyorsh: repeat basecase *)
       | _ -> cons_instr (Lbranch lbl) n1)
    | _ -> cons_instr (Lbranch lbl) n1
  else
    discard_dead_code n

let try_depth = ref 0

(* Association list: exit handler -> (handler label, try-nesting factor) *)

let exit_label = ref []

let find_exit_label_try_depth k =
  try
    List.assoc k !exit_label
  with
  | Not_found -> Misc.fatal_error "Linearize.find_exit_label"

let find_exit_label k =
  let (label, t) = find_exit_label_try_depth k in
  assert(t = !try_depth);
  label

let is_next_catch n = match !exit_label with
| (n0,(_,t))::_  when n0=n && t = !try_depth -> true
| _ -> false

let local_exit k =
  snd (find_exit_label_try_depth k) = !try_depth

(* Detect local tail-recursion.
   CR gyorsh: it can be done earlier, but
   Selectgen.mark_tailcall does not currently have enough information
   to detect self-tailcalls.
   It needs current function name and the name of the function in Itailcall_imm.
   The result would need to be passed from in Mach.fundecl field to linearize.
*)
let tailrec_entry_point_used = ref false

(* Linearize an instruction [i]: add it in front of the continuation [n] *)
let linear i n contains_calls fun_name fun_fast =
  let rec linear i n =
    let tailcall op i n =
      if not Config.spacetime then
        copy_instr (Lop op) i (discard_dead_code n)
      else
        copy_instr (Lop op) i (linear i.Mach.next n)
    in
    match i.Mach.desc with
      Iend -> n
    | Iop(Itailcall_ind _ as op) ->
      tailcall op i n
    | Iop(Itailcall_imm { func; } as op) ->
      if String.equal func fun_name  then
        tailrec_entry_point_used := true;
      tailcall op i n
    | Iop(Imove | Ireload | Ispill)
      when i.Mach.arg.(0).loc = i.Mach.res.(0).loc ->
        linear i.Mach.next n
    | Iop op ->
        copy_instr (Lop op) i (linear i.Mach.next n)
    | Ireturn ->
        let n1 = copy_instr Lreturn i (discard_dead_code n) in
        if contains_calls
        then cons_instr Lreloadretaddr n1
        else n1
    | Iifthenelse(test, ifso, ifnot) ->
        let n1 = linear i.Mach.next n in
        begin match (ifso.Mach.desc, ifnot.Mach.desc, n1.desc) with
          Iend, _, Lbranch lbl ->
            copy_instr (Lcondbranch(test, lbl)) i (linear ifnot n1)
        | _, Iend, Lbranch lbl ->
            copy_instr (Lcondbranch(invert_test test, lbl)) i (linear ifso n1)
        | Iexit nfail1, Iexit nfail2, _
              when is_next_catch nfail1 && local_exit nfail2 ->
            let lbl2 = find_exit_label nfail2 in
            copy_instr
              (Lcondbranch (invert_test test, lbl2)) i (linear ifso n1)
        | Iexit nfail, _, _ when local_exit nfail ->
            let n2 = linear ifnot n1
            and lbl = find_exit_label nfail in
            copy_instr (Lcondbranch(test, lbl)) i n2
        | _,  Iexit nfail, _ when local_exit nfail ->
            let n2 = linear ifso n1 in
            let lbl = find_exit_label nfail in
            copy_instr (Lcondbranch(invert_test test, lbl)) i n2
        | Iend, _, _ ->
            let (lbl_end, n2) = get_label n1 in
            copy_instr (Lcondbranch(test, lbl_end)) i (linear ifnot n2)
        | _,  Iend, _ ->
            let (lbl_end, n2) = get_label n1 in
            copy_instr (Lcondbranch(invert_test test, lbl_end)) i
                       (linear ifso n2)
        | _, _, _ ->
          (* Should attempt branch prediction here *)
            let (lbl_end, n2) = get_label n1 in
            let (lbl_else, nelse) = get_label (linear ifnot n2) in
            copy_instr (Lcondbranch(invert_test test, lbl_else)) i
              (linear ifso (add_branch lbl_end nelse))
        end
    | Iswitch(index, cases) ->
        let lbl_cases = Array.make (Array.length cases) 0 in
        let (lbl_end, n1) = get_label(linear i.Mach.next n) in
        let n2 = ref (discard_dead_code n1) in
        for i = Array.length cases - 1 downto 0 do
          let (lbl_case, ncase) =
                  get_label(linear cases.(i) (add_branch lbl_end !n2)) in
          lbl_cases.(i) <- lbl_case;
          n2 := discard_dead_code ncase
        done;
        (* Switches with 1 and 2 branches have been eliminated earlier.
           Here, we do something for switches with 3 branches. *)
        if Array.length index = 3 then begin
          let fallthrough_lbl = check_label !n2 in
          let find_label n =
            let lbl = lbl_cases.(index.(n)) in
            if lbl = fallthrough_lbl then None else Some lbl in
          copy_instr (Lcondbranch3(find_label 0, find_label 1, find_label 2))
                     i !n2
        end else
          copy_instr (Lswitch(Array.map (fun n -> lbl_cases.(n)) index)) i !n2
    | Icatch(rec_flag, handlers, body) ->
        let (lbl_end, n1) = get_label(linear i.Mach.next n) in
        (* CR mshinwell for pchambart:
           1. rename "io"
           2. Make sure the test cases cover the "Iend" cases too *)
        let labels_at_entry_to_handlers = List.map (fun (_nfail, handler) ->
            match handler.Mach.desc with
            | Iend -> lbl_end
            | _ -> Cmm.new_label ())
            handlers in
        let exit_label_add = List.map2
            (fun (nfail, _) lbl -> (nfail, (lbl, !try_depth)))
            handlers labels_at_entry_to_handlers in
        let previous_exit_label = !exit_label in
        exit_label := exit_label_add @ !exit_label;
        let n2 = List.fold_left2 (fun n (_nfail, handler) lbl_handler ->
            match handler.Mach.desc with
            | Iend -> n
            | _ ->
              let k = (cons_instr (Llabel lbl_handler)
                         (linear handler (add_branch lbl_end n))) in
              match rec_flag with
              | Recursive ->
                if fun_fast && (!Clflags.align_loops > 0) then
                  (cons_instr (Lalign !Clflags.align_loops) k)
                else
                  k
              | Nonrecursive -> k)
            n1 handlers labels_at_entry_to_handlers
        in
        let n3 = linear body (add_branch lbl_end n2) in
        exit_label := previous_exit_label;
        n3
    | Iexit nfail ->
        let lbl, t = find_exit_label_try_depth nfail in
        assert (i.Mach.next.desc = Mach.Iend);
        let delta_traps = !try_depth - t in
        let n1 = adjust_trap_depth delta_traps n in
        let rec loop i tt =
          if t = tt then i
          else loop (cons_instr Lpoptrap i) (tt - 1)
        in
        loop (add_branch lbl n1) !try_depth
    | Itrywith(body, handler) ->
        let (lbl_join, n1) = get_label (linear i.Mach.next n) in
        let (lbl_handler, n2) =
          get_label (cons_instr Lentertrap (linear handler n1))
        in
        incr try_depth;
        assert (i.Mach.arg = [| |] || Config.spacetime);
        let n3 = cons_instr (Lpushtrap { lbl_handler; })
                   (linear body
                      (cons_instr
                         Lpoptrap
                         (add_branch lbl_join n2))) in
        decr try_depth;
        n3

    | Iraise k ->
        copy_instr (Lraise k) i (discard_dead_code n)
  in linear i n

let add_prologue first_insn prologue_required fun_fast =
  (* The prologue needs to come after any [Iname_for_debugger] operations that
     refer to parameters.  (Such operations always come in a contiguous
     block, cf. [Selectgen].) *)
  let rec skip_naming_ops (insn : instruction) : label * instruction =
    match insn.desc with
    | Lop (Iname_for_debugger _) ->
      let tailrec_entry_point_label, next = skip_naming_ops insn.next in
      tailrec_entry_point_label, { insn with next; }
    | _ ->
      let tailrec_entry_point_label = Cmm.new_label () in
      let tailrec_entry_point =
        { desc = Llabel tailrec_entry_point_label;
          next = insn;
          arg = [| |];
          res = [| |];
          dbg = insn.dbg;
          live = insn.live;
        }
      in
      let tailrec_entry_point =
        if fun_fast && !tailrec_entry_point_used &&
           (!Clflags.align_loops > 0 || !Clflags.align_tailrec > 0) then
          { desc = Lalign (max !Clflags.align_loops !Clflags.align_tailrec);
            next = tailrec_entry_point;
            arg = [| |];
            res = [| |];
            dbg = insn.dbg;
            live = insn.live;
          }
        else tailrec_entry_point
      in
      (* We expect [Lprologue] to expand to at least one instruction---as such,
         if no prologue is required, we avoid adding the instruction here.
         The reason is subtle: an empty expansion of [Lprologue] can cause
         two labels, one either side of the [Lprologue], to point at the same
         location.  This means that we lose the property (cf. [Coalesce_labels])
         that we can check if two labels point at the same location by
         comparing them for equality.  This causes trouble when the function
         whose prologue is in question lands at the top of the object file
         and we are emitting DWARF debugging information:
           foo_code_begin:
           foo:
           .L1:
           ; empty prologue
           .L2:
           ...
         If we were to emit a location list entry from L1...L2, not realising
         that they point at the same location, then the beginning and ending
         points of the range would be both equal to each other and (relative to
         "foo_code_begin") equal to zero.  This appears to confuse objdump,
         which seemingly misinterprets the entry as an end-of-list entry
         (which is encoded with two zero words), then complaining about a
         "hole in location list" (as it ignores any remaining list entries
         after the misinterpreted entry). *)
      if prologue_required then
        let prologue =
          { desc = Lprologue;
            next = tailrec_entry_point;
            arg = [| |];
            res = [| |];
            dbg = tailrec_entry_point.dbg;
            live = Reg.Set.empty;  (* will not be used *)
          }
        in
        tailrec_entry_point_label, prologue
      else
        tailrec_entry_point_label, tailrec_entry_point
  in
  skip_naming_ops first_insn

let fundecl f =
  let fun_prologue_required = Proc.prologue_required f in
  let contains_calls = f.Mach.fun_contains_calls in
  tailrec_entry_point_used := false;
  let fun_name = f.Mach.fun_name in
  let fun_fast =
    not (List.mem Cmm.Reduce_code_size f.Mach.fun_codegen_options) in
  let fun_tailrec_entry_point_label, fun_body =
    add_prologue
      (linear f.Mach.fun_body end_instr contains_calls fun_name fun_fast)
      fun_prologue_required fun_fast
  in
  { fun_name;
    fun_body;
    fun_fast;
    fun_dbg  = f.Mach.fun_dbg;
    fun_spacetime_shape = f.Mach.fun_spacetime_shape;
    fun_tailrec_entry_point_label;
    fun_contains_calls = contains_calls;
    fun_num_stack_slots = f.Mach.fun_num_stack_slots;
    fun_frame_required = Proc.frame_required f;
    fun_prologue_required;
  }
