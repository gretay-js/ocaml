[@@@ocaml.warning "+a-4-30-40-41-42"]
open Linearize

module Int = Numbers.Int

type label = Linearize.label

(* CR gyorsh: update label after? *)
type call_operation =
  | Indirect of { label_after : label; }
  | Immediate of { func : string; label_after : label; }
  | External of { func : string; alloc : bool; label_after : label; }
  | Alloc of { words : int; label_after_call_gc : label option;
               spacetime_index : int; }
  | Checkbound of {
      immediate : int option;
      label_after_error : label option;
      spacetime_index : int; }

type operation =
  | Move
  | Spill
  | Reload
  | Const_int of nativeint
  | Const_float of int64
  | Const_symbol of string
  | Stackoffset of int
  | Load of Cmm.memory_chunk * Arch.addressing_mode
  | Store of Cmm.memory_chunk * Arch.addressing_mode * bool
  | Intop of Mach.integer_operation
  | Intop_imm of Mach.integer_operation * int
  | Negf | Absf | Addf | Subf | Mulf | Divf | Floatofint | Intoffloat
  | Specific of Arch.specific_operation
  | Name_for_debugger of { ident : Ident.t; which_parameter : int option;
                           provenance : unit option; is_assignment : bool; }

type condition =
  | Always
  | Test of Mach.test

type successor = condition * label

(* basic block *)
type block = {
  start : label;
  mutable body : basic instruction list;
  mutable terminator : terminator instruction;
}

and 'a instruction = {
  desc : 'a;
  arg : Reg.t array;
  res : Reg.t array;
  dbg : Debuginfo.t;
  live : Reg.Set.t;
}

and basic =
  | Op of operation
  | Call of call_operation
  | Reloadretaddr
  | Entertrap
  | Adjust_trap_depth of { delta_traps : int }
  | Pushtrap of { lbl_handler : label }
  | Poptrap

and terminator =
  | Branch of successor list
  | Return
  | Raise of Cmm.raise_kind
  | Tailcall of call_operation

let successors block =
  match block.terminator.desc with
  | Branch successors -> successors
  | Return -> []
  | Raise _ -> []
  | Tailcall(op) -> []


type t = {
  blocks : (label, block) Hashtbl.t;            (* Map labels to blocks *)
  trap_depths : int Linear_invariants.LabelMap.t;(* Map labels to trap depths *)
  used_labels : (label, unit) Hashtbl.t;        (* Set of used labels *)
}

let create_empty_instruction desc =
  { desc;
    arg = [||]; res = [||]; dbg = Debuginfo.none; live = Reg.Set.empty; }

let create_empty_block t start =
  let terminator = create_empty_instruction (Branch []) in
  let block = { start; body = []; terminator; } in
  if Hashtbl.mem t.blocks start then
    Misc.fatal_error("Cannot create block, label exists: " ^
                     (string_of_int start));
  block

let register t block =
  if Hashtbl.mem t.blocks block.start then
    Misc.fatal_error("Cannot create block, label exists: "
                     ^ (string_of_int block.start));
  (* Body is constructed in reverse, fix it now: *)
  block.body <- List.rev block.body;
  Hashtbl.add t.blocks block.start block

let create_instr desc (i:Linearize.instruction) =
  {
    desc = desc;
    arg = i.arg;
    res = i.res;
    dbg = i.dbg;
    live = i.live;
  }

let add_terminator t desc i block =
  (* Ensure that terminator [i] is followed by a label to start a new block. *)
  begin match i.next.desc with
  | Lend | Llabel _ -> ()
  | _ -> Misc.fatal_error("Unexpected instruction after terminator")
  end;
  block.terminator <- create_instr desc i;
  register t block

(* Collect used labels *)
let mark_used_label t lbl =
  if not (Hashtbl.mem t.used_labels lbl) then begin
    Hashtbl.add t.used_labels lbl ()
  end;
  ()

(* check that all labels are used. *)
let check_used_labels t =
  let unused_labels =
    Hashtbl.fold (fun label  _ unused ->
      if Hashtbl.mem t.used_labels label then unused
      else label::unused)
      t.blocks
      []
  in
  if unused_labels = [] then
    (* CR gyorsh: add a separate pass remove unused labels
       and unreachable blocks transitively. *)
    Misc.fatal_error("Found " ^ string_of_int (List.length unused_labels)
                     ^ " unused labels")

let get_label (i : Linearize.instruction) =
  match i.desc with
  | Llabel lbl -> lbl
  | Lbranch lbl -> Misc.fatal_error("Unexpected branch instead of label")
  | Lend -> Misc.fatal_error("Unexpected end of function instead of label")
  | _ -> Misc.fatal_error("Unexpected instruction instead of label")

let from_basic = function
  | Reloadretaddr -> Lreloadretaddr
  | Entertrap -> Lentertrap
  | Adjust_trap_depth { delta_traps } -> Ladjust_trap_depth { delta_traps }
  | Pushtrap { lbl_handler } -> Lpushtrap { lbl_handler }
  | Poptrap -> Lpoptrap
  | Call(Indirect {label_after}) -> Lop(Icall_ind {label_after})
  | Call(Immediate {func;label_after}) -> Lop(Icall_imm {func; label_after})
  | Call(External {func; alloc; label_after}) ->
    Lop(Iextcall {func; alloc; label_after})
  | Call(Checkbound({immediate=None; label_after_error; spacetime_index})) ->
    Lop(Iintop(Icheckbound {label_after_error; spacetime_index}))
  | Call(Checkbound({immediate = Some i;label_after_error;spacetime_index})) ->
    Lop(Iintop_imm(Icheckbound {label_after_error; spacetime_index},i))
  | Call(Alloc {words;label_after_call_gc;spacetime_index}) ->
    Lop(Ialloc {words;label_after_call_gc;spacetime_index})
  | Op(op) ->
    match op with
    | Move -> Lop(Imove)
    | Spill -> Lop(Ispill)
    | Reload -> Lop(Ireload)
    | Const_int n -> Lop(Iconst_int n)
    | Const_float n -> Lop(Iconst_float n)
    | Const_symbol n -> Lop(Iconst_symbol n)
    | Stackoffset n -> Lop(Istackoffset n)
    | Load(c,m) -> Lop(Iload(c,m))
    | Store(c,m,b) -> Lop(Istore(c,m,b))
    | Intop op -> Lop(Iintop op)
    | Intop_imm(op, i) -> Lop(Iintop_imm(op, i))
    | Negf -> Lop(Inegf)
    | Absf -> Lop(Iabsf)
    | Addf -> Lop(Iaddf)
    | Subf -> Lop(Isubf)
    | Mulf -> Lop(Imulf)
    | Divf -> Lop(Idivf)
    | Floatofint -> Lop(Ifloatofint)
    | Intoffloat -> Lop(Iintoffloat)
    | Specific op -> Lop(Ispecific op)
    | Name_for_debugger {ident;which_parameter;provenance;is_assignment;} ->
      Lop(Iname_for_debugger {ident;which_parameter;provenance;is_assignment;})

let rec create_blocks t (i : Linearize.instruction) block =
    match i.desc with
    | Lend ->
      (* End of the function. Last block's successor is a self-loop successor. *)
      (* CR gyorsh: is there always another terminator before Lend? *)
      register t block
    | Llabel start ->
      (* Add the previos block, if it did not have an explicit terminator. *)
      if not (Hashtbl.mem t.blocks block.start) then begin
        (* Previous block falls through. Add start as explicit successor. *)
        let fallthrough = Branch [(Always,start)] in
        block.terminator <- create_empty_instruction fallthrough;
        register t block
      end;
      (* Start a new block *)
      let new_block = create_empty_block t start in
      create_blocks t i.next new_block

    | Lop(Itailcall_ind {label_after}) ->
      let desc = Tailcall(Indirect {label_after}) in
      add_terminator t desc i block;
      create_blocks t i.next block

    | Lop(Itailcall_imm {func; label_after}) ->
      let desc = Tailcall(Immediate{func;label_after}) in
      add_terminator t desc i block;
      create_blocks t i.next block

    | Lreturn ->
      add_terminator t Return i block;
      create_blocks t i.next block

    | Lraise(kind) ->
      add_terminator t (Raise kind) i block;
      create_blocks t i.next block

    | Lbranch lbl ->
      let successors = [(Always,lbl)] in
      add_terminator t (Branch successors) i block;
      create_blocks t i.next block

    | Lcondbranch(cond,lbl) ->
      let fallthrough = get_label i.next in
      let successors = [(Test cond,lbl);
                        (Test (invert_test cond),fallthrough)] in
      add_terminator t (Branch successors) i block;
      create_blocks t i.next block

    | Lcondbranch3(lbl0,lbl1,lbl2) ->
      let fallthrough = get_label i.next in
      let get_dest label =
        match label with
        | None -> fallthrough
        | Some lbl ->  lbl
      in
      let s0 = (Test(Iinttest_imm(Iunsigned Clt, 1)), get_dest lbl0) in
      let s1 = (Test(Iinttest_imm(Iunsigned Ceq, 1)), get_dest lbl1) in
      let s2 = (Test(Iinttest_imm(Isigned   Cgt, 1)), get_dest lbl2) in
      add_terminator t (Branch [s0;s1;s2]) i block;
      create_blocks t i.next block

    | Lswitch labels ->
      let successors =
        Array.mapi
          (fun i label ->
             (Test(Iinttest_imm(Iunsigned Ceq, i)), label))
          labels in
      add_terminator t (Branch (Array.to_list successors)) i block;
      create_blocks t i.next block

    | d ->
      let desc = begin match d with
        | Lreloadretaddr -> Reloadretaddr
        | Lentertrap -> Entertrap
        | Ladjust_trap_depth { delta_traps } -> Adjust_trap_depth { delta_traps }
        | Lpushtrap { lbl_handler } ->
          mark_used_label t lbl_handler;
          Pushtrap { lbl_handler }
        | Lpoptrap -> Poptrap
        | Lop(op) -> begin match op with
          | Icall_ind { label_after} -> Call(Indirect {label_after})
          | Icall_imm {func; label_after} -> Call(Immediate {func;label_after})
          | Iextcall {func; alloc; label_after} ->
            Call(External {func; alloc; label_after})
          | Iintop(op) -> begin match op with
            | Icheckbound {label_after_error; spacetime_index} ->
              Call(Checkbound{ immediate = None; label_after_error; spacetime_index})
            | _ -> Op(Intop(op))
          end
          | Iintop_imm(op,i) -> begin match op with
            | Icheckbound {label_after_error; spacetime_index} ->
              Call(Checkbound ({immediate = Some i;
                                label_after_error;
                                spacetime_index; }))
            |_ -> Op(Intop_imm(op, i))
          end
          | Ialloc {words;label_after_call_gc;spacetime_index} ->
            Call(Alloc {words;label_after_call_gc;spacetime_index})
          | Istackoffset i -> Op(Stackoffset i)
          | Iload(c,a) -> Op(Load(c,a))
          | Istore(c,a,b)-> Op(Store(c,a,b))
          | Imove -> Op(Move)
          | Ispill -> Op(Spill)
          | Ireload -> Op(Reload)
          | Iconst_int n -> Op(Const_int n)
          | Iconst_float n -> Op(Const_float n)
          | Iconst_symbol n -> Op(Const_symbol n)
          | Inegf -> Op(Negf)
          | Iabsf -> Op(Absf)
          | Iaddf -> Op(Addf)
          | Isubf -> Op(Subf)
          | Imulf -> Op(Mulf)
          | Idivf -> Op(Divf)
          | Ifloatofint -> Op(Floatofint)
          | Iintoffloat -> Op(Intoffloat)
          | Ispecific op -> Op(Specific op)
          | Iname_for_debugger {ident;which_parameter;provenance;is_assignment;} ->
            Op(Name_for_debugger {ident;which_parameter;provenance;is_assignment;})
          | Itailcall_ind _ | Itailcall_imm _ -> assert (false)
        end
        | Lend| Lreturn| Llabel _
        | Lbranch _| Lcondbranch (_, _)| Lcondbranch3 (_, _, _)
        | Lswitch _| Lraise _ -> assert (false)
      end
      in
      block.body <- (create_instr desc i)::block.body;
      create_blocks t i.next block

let make_empty_cfg i =
  let trap_depths = Linear_invariants.compute_trap_depths i in
  let blocks = (Hashtbl.create 31 : (label, block) Hashtbl.t) in
  let used_labels = (Hashtbl.create 17 : (label, unit) Hashtbl.t) in
  { trap_depths; blocks; used_labels; }

let from_linear f =
  let t = make_empty_cfg f in
  (* CR gyorsh: label of the function entry must not conflict with existing
     labels. Relies on the invariant: Cmm.new_label() is int > 99.
     An alternative is to create a new type for label here,
     but it is less efficient because label is used as a key to Hashtble. *)
  let func_start_lbl = 0 in
  let entry_block = create_empty_block t func_start_lbl in
  mark_used_label t func_start_lbl;
  create_blocks t f.fun_body entry_block;
  check_used_labels t;
  t

(* Set desc and next from inputs and the rest is empty *)
let make_simple_linear desc next =
  { desc; next;
    arg = [||]; res = [||]; dbg = Debuginfo.none; live = Reg.Set.empty }

(* Set desc and next from inputs and copy the rest from i *)
let to_linear_instr ~i desc next =
  { desc; next;
    arg = i.arg; res = i.res; dbg = i.dbg; live = i.live }

let basic_to_linear i next =
  let desc = from_basic i.desc in
  to_linear_instr desc next ~i

exception Not_switch
let is_switch successors =
  try
    let check_switch_case (cond, _) i =
      match cond with
      | Test(Iinttest_imm(Iunsigned Ceq, i)) -> i + 1
      | _ -> raise Not_switch
    in
    List.fold_left check_switch_case 0 successors;
    true
  with Not_switch -> false

let linearize_terminator terminator next =
  let desc_list =
    match terminator.desc with
    | Return -> [Lreturn]
    | Raise kind -> [Lraise kind]
    | Tailcall(Indirect {label_after}) ->
      [Lop(Itailcall_ind(label_after))]
    | Tailcall(Immediate {func;label_after}) ->
      [Lop(Itailcall_imm(func,label_after))]
    | Branch successors ->
      match successors with
      | [] -> Misc.fatal_error ("Branch without successors");
      | [(Always,label)] ->
        if next.label = label then []
        else [Lbranch(lbl)]
      | [(cond_p,label_p); (cond_q,label_q)] ->
        if cond_p <> invert_test cond_q then
          Misc.fatal_error ("Illegal successors")
        else if label_p = next.label && label_q = next.label then
          []
        else if label_p <> next.label && label_q <> next.label then
          (* CR gyorsh: if both label are not fall through, then arrangement
             should depend on the relative position of the target labels
             and the current block: whether the jumps are forward or back.
             This information can be obtained from layout but it needs
             to be made accessible here.
          *)
          [Lcondbranch(cond_p,label_p); Lbranch_cond(cond_q,label_q)]
        else if label_p = next.label then
          [Lcondbranch(cond_q,label_q)]
        else if label_q = next.label then
          [Lcondbranch(cond_p,label_p)]
        else assert false
      | [(Iinttest_imm(Iunsigned Clt, 1),label0);
         (Iinttest_imm(Iunsigned Ceq, 1),label1);
         (Iinttest_imm(Isigned   Cgt, 1),label2)] ->
        let find_label l =
          if next.label = l then None
          else Some l
        in
        [Lcondbranch3(find_label label0,
                      find_label label1,
                      find_label label2)]
      | _ ->
        (* It is more general than strictly necessary for reordering,
           where it must be a switch. *)
        if is_switch successors then
          let (_, successor_labels) = List.split successors in
          [Lswitch(Array.of_list successor_labels)]
        else
          let create_branch (cond, label) tail =
            if (label = next.label) then tail
            else Lcondbranch(cond,label)::tail
          in
          List.fold_right create_branch successors []
  in
  List.fold_right (to_linear_instr ~terminator) desc_list next.insn

let no_label = (-1)

type lin_record =
  { label : label;
    insn : Linearize.instruction;
  }

let rec linearize t layout =
  match layout with
  | [] -> no_label, end_instr
  | label::tail ->
    let next = linearize t tail in
    let block = Hashtbl.find t.blocks label in
    let terminator = linearize_terminator block.terminator next  in
    let body = List.fold_right basic_to_linear block.body terminator in
    let insn = make_simple_linear (Label label) body in
    { label; insn }

let linearize t layout =
  let lin = linearize t layout in
  lin.insn
