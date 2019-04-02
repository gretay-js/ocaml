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
  | Checkbounds of int option *
                   { label_after_error : label option;
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

type successor = {
  condition: condition;
  target : label;
}

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
  match block.terminator with
  | Branch successors -> successors
  | Return -> []
  | Raise _ -> []
  | Tailcall(op) -> []

type t = {
  blocks : (label, block) Hashtbl.t;    (* Map labels to blocks *)
  trap_depths : (label, int) Map.t
}

let create_empty_instruction desc =
  { desc; arg = [||]; res = [||]; Debuginfo.none; live = Reg.Set.empty; }

let create_empty_block t start =
  let terminator = create_empty_instruction Branch([]) in
  let block = { start; body = []; terminator; } in
  if Hashtbl.mem t.blocks lbl then
    Misc.fatal_error("Cannot create block, label exists: " ^ lbl);
  block

let register t block =
  if Hashtbl.mem t.blocks block.start then
    Misc.fatal_error("Cannot create block, label exists: " ^ block.start);
  (* Body is constructed in reverse, fix it now: *)
  block.body <- List.rev block.body;
  Hashtble.add t.blocks block.start block

let create_instr desc i =
  {
    desc = desc;
    arg = i.arg;
    res = i.res;
    dbg = i.dbg
    live = i.live;
  }

let add_instr desc i block =
  block.body <- (create_instr desc i)::block.body

let add_terminator desc i block =
  check_terminator i;
  block.terminator <- create_instr desc i;
  register t block

(* Collect used labels *)
let used_labels = Int.Set.empty ()

let mark_used_label lbl = Int.Set.add lbl used_labels

(* Ensure that terminator [i] is followed by a new block. *)
let check_terminator i =
  match i.next.desc with
  | Lend | Llabel _ -> ();
  | _ -> Misc.fatal_error("Unexpected instruction after terminator")

let get_label = function
  | Llabel lbl -> lbl
  | Lbranch lbl -> Misc.fatal_error("Unexpected branch instead of label")
  | Lend -> Misc.fatal_error("Unexpected end of function instead of label")
  | _ -> Misc.fatal_error("Unexpected instruction instead of label")

let to_basic = function
  | Lreloadretaddr -> Reloadretaddr
  | Lentertrap -> Entertrap
  | Ladjust_trap_depth delta_traps -> Adjust_trap_depth delta_traps
  | Lpushtrap lbl_handler ->
    Int.Set.add lbl_hanlder used_labels;
    Pushtrap lbl_handler
  | Lpoptrap -> Poptrap
  | Lop(op) -> begin match op with
    | Icall_ind label_after -> Call(Indirect label_after)
    | Icall_imm {func; label_after} -> Call(Immediate {func;label_after})
    | Iextcall {func; alloc; label_after} ->
      Call(External {func; alloc; label_after})
    | Iintop(op) -> begin match op with
      | Icheckbound {label_after_error; spacetime_index} ->
        Call(Checkbound(None, {label_after_error; spacetime_index}))
      |_ -> Intop(op)
    end
    | Iintop_imm(op,i) -> begin match op with
      | Icheckbound {label_after_error; spacetime_index} ->
        Call(Checkbound(Some i, {label_after_error; spacetime_index}))
      |_ -> Intop_imm(op, i)
    end
    | Ialloc r -> Call(Alloc r) (* {words;label_after_call_gc;spacetime_index} *)
    | Istackoffset(i) -> Stackoffset(i)
    | Iload(c,a) -> Load(c,a)
    | Istore(c,a,b)-> Store(c,a,b)
    | Imove -> Move
    | Ispill -> Spill
    | Ireload -> Reload
    | Iconst_int n -> const_int n
    | Iconst_float n -> const_float n
    | Iconst_symbol n -> const_symbol n
    | Inegf -> Negf
    | Iabsf -> Absf
    | Iaddf -> Addf
    | Isubf -> Subf
    | Imulf -> Mulf
    | Idivf -> Divf
    | Ifloatofint -> Floatofint
    | Iintoffloat -> Intoffloat
    | Ispecific op -> Specific op
    | Iname_for_debugger r -> Name_for_debugger r
  end

let from_basic = function
  | Reloadretaddr -> Lreloadretaddr
  | Entertrap -> Lentertrap
  | Adjust_trap_depth delta_traps -> Ladjust_trap_depth delta_traps
  | Pushtrap lbl_handler -> Lpushtrap lbl_handler
  | Poptrap -> Lpoptrap
  | Call(Indirect label_after) -> Lop(Icall_ind label_after))
  | Call(Immediate {func;label_after}) -> Lop(Icall_imm {func; label_after})
  | Call(External {func; alloc; label_after}) -> Lop(Iextcall {func; alloc; label_after})
  | Call(Checkbounds(None, {label_after_error; spacetime_index})) ->
    Lop(Iintop(Icheckbound {label_after_error; spacetime_index}))
  | Call(Checkbounds(Some i, {label_after_error; spacetime_index})) ->
    Lop(Iintop_imm(Icheckbound {label_after_error; spacetime_index},i))
  | Call(Alloc r) -> Lop(Ialloc r)
  | Op(op) ->
    let iop =
      match op with
      | Move -> Imove
      | Spill -> Ispill
      | Reload -> Ireload
      | Const_int n -> IConst_int n
      | Const_float n -> IConst_float n
      | Const_symbol n -> IConst_symbol n
      | Stackoffset n -> IStackoffset n
      | Load(c,m) -> ILoad(c,m)
      | Store(c,m,b) -> IStore(c,m,b)
      | Intop op -> Iintop op
      | Intop_imm(op, i) -> Iintop_imm(op, i)
      | Negf -> Inegf
      | Absf -> Iabsf
      | Addf -> Iaddf
      | Subf -> Isubf
      | Mulf -> Imulf
      | Divf -> Idivf
      | Floatofint -> Ifloatofint
      | Intoffloat -> Iintoffloat
      | Specific op -> Ispecific op
      | Name_for_debugger of r -> Iname_for_debugger
    in
    Lop(iop)

let rec create_blocks t i block =
    match i.desc with
    | Lend ->
      (* End of the function. Last block's successor is a self-loop successor. *)
      (* CR gyorsh: is there always another terminator before Lend? *)
      register t block
    | Llabel start ->
      (* Add the previos block, if it did not have an explicit terminator. *)
      if not Hashtbl.mem t.blocks block.start then begin
        (* Previous block falls through. Add start as explicit successor. *)
        let fallthrough = Branch ([(Always,start)]) in
        block.terminator <- create_empty_instruction fallthrough;
        register t block
      end;
      (* Start a new block *)
      let new_block = create_empty_block start in
      create_blocks t i.next new_block

    | Lop(Itailcall_ind label_after) ->
      let desc = Tailcall(Indirect(label_after)) in
      add_terminator t desc i block;
      create_blocks t i.next block

    | Lop(Itailcall_imm(func; label_after)) ->
      let desc = Tailcall(Indirect(label_after)) in
      add_terminator t desc i block;
      create_blocks t i.next block

    | Lreturn ->
      add_terminator t Return(kind) i block;
      create_blocks t i.next block

    | Lraise(kind) ->
      add_terminator t Raise(kind) i block;
      create_blocks t i.next block

    | Lbranch lbl ->
      let successors = [(Always,lbl)] in
      add_terminator t Branch(successors) i block;
      create_blocks t i.next block

    | Lcondbranch(cond,lbl) ->
      let fallthrough = get_label (i.next) in
      let successors = [(cond,lbl); (invert_test cond,fallthrough)] in
      add_terminator t Branch(successors) i block;
      create_blocks t i.next block

    | Lcondbranch3(lbl0,lbl1,lbl2) ->
      let fallthrough = get_label (i.next) in
      let get_label_all_fallthrough label fallthrough =
        match label with
        | None -> fallthrough
        | Some lbl ->  lbl
      in
      let s0 = (Iinttest_imm(Iunsigned Clt, 1), get_dest lbl0) in
      let s1 = (Iinttest_imm(Iunsigned Ceq, 1), get_dest lbl1) in
      let s2 = (Iinttest_imm(Isigned   Cgt, 1), get_dest lbl2) in
      add_terminator t Branch([s0;s1;s2]) i block;
      create_blocks t i.next block

    | Lswitch labels ->
      let successors =
        Array.mapi
          (fun i label ->
             (Iinttest(Iunsigned Ceq, i), get_dest lbl1))
          labels in
      add_terminator t Branch(Array.to_list successors) i block;
      create_blocks t i.next block

    | _ ->
      add_instr (to_basic i.desc) i body;
      create_blocks t i.next block

let from_linear i =
  trap_depths = Linear_invariants.compute_trap_depths i;
  blocks = Hashtbl.create 31 : (label, block) Hashtbl.t);
  Int.Set.reset used_labels;
  (* CR gyorsh: label of the function entry must not conflict with existing
     labels. Relies on the invariant: Cmm.new_label() is int > 99.
     An alternative is to create a new type for label here,
     but it is less efficient because label is used as a key to Hashtble. *)
  let func_start_lbl = 0 in
  let entry_block = create_empty_block func_start_lbl in
  mark_used_label func_start_lbl;
  create_blocks blocks i entry_block;
  (* check that all labels are used. *)
  let unused_labels = Int.Set.diff (Hashtble.keys blocks) used_labels in
  if not (Int.Set.is_empty unused_labels) then begin
    (* CR gyorsh: add a separate pass remove unused labels
       and unreachable blocks transitively. *)
    Misc.fatal_error("Found "^
                     Int.Set.cardinal unusued_labels
                     ^" unused labels in function " ^ f.func_name)
  end;
  { blocks; trap_depths; }

let instr_cons d a r n =
  { desc = d; next = n; arg = a; res = r;
    dbg = Debuginfo.none; live = Reg.Set.empty }

let linearize_terminator terminator next =
  let desc =
    match terminator.desc with
    | Branch successors ->
    | Return -> Lreturn
    | Raise kind -> Lraise kind
    | Tailcall call -> Lop(Itailcall ..)
  in to_linear_instr desc next i

let to_linear_instr desc next i =
  { desc; next; arg = i.arg; res = i.res;
    dbg = i.dbg; live = i.live }

let basic_to_linear i next =
  let desc = from_basic i.desc in
  to_linear_instr desc next i

(* Cons a simple instruction (arg, res, live empty) *)
let make_simple_linear d n =
  { desc = d; next = n; arg = [||]; res = [||];
    dbg = Debuginfo.none; live = Reg.Set.empty }

let rec linearize t layout =
  match layout with
  | [] -> -1, end_instr
  | label::tail ->
    let next = linearize t tail in
    let block = Hashtbl.find t.blocks label in
    let terminator = linearize_terminator block.terminator next  in
    let body = List.fold_right basic_to_linear block.body terminator in
    let insn = make_simple_linear (Label label) body in
    { label; insn }

let to_linear t layout =
  let lin = linearize t layout in
  lin.insn
