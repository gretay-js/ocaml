[@@@ocaml.warning "+a-4-30-40-41-42"]
open Linearize

type label = Linearize.label

module Layout = struct
  type t = label list

  (* CR gyorsh: missing cfg and parameters to determine new order *)
  let reorder t = t
end

module LabelSet = Set.Make(
struct
  type t = label
  let compare (x:t) y = compare x y
end)


(* CR gyorsh: update label after? *)
type func_call_operation =
  | Indirect of { label_after : label; }
  | Immediate of { func : string; label_after : label; }

type prim_call_operation =
  | External of { func : string; alloc : bool; label_after : label; }
  | Alloc of { words : int; label_after_call_gc : label option;
               spacetime_index : int; }
  | Checkbound of {
      immediate : int option;
      label_after_error : label option;
      spacetime_index : int; }

type call_operation =
  | P of prim_call_operation
  | F of func_call_operation

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
  mutable predecessors : LabelSet.t;
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
  | Switch of label array
  | Return
  | Raise of Cmm.raise_kind
  | Tailcall of func_call_operation

let successors block =
  match block.terminator.desc with
  | Branch successors -> successors
  | Return -> []
  | Raise _ -> []
  | Tailcall _ -> []
  | Switch labels ->
    Array.mapi (fun i label ->
      (Test(Iinttest_imm(Iunsigned Ceq, i)), label))
      labels
    |> Array.to_list

let successor_labels block =
  let (_, labels) = List.split (successors block) in
  labels

(* Control Flow Graph of a function. *)
type t = {
  blocks : (label, block) Hashtbl.t;                  (* Map labels to blocks *)
  fun_name : string;             (* Function name, used for printing messages *)
  entry_label : label;           (* Must be first in all layouts of this cfg. *)
  mutable layout : Layout.t;      (* Original layout: linear order of blocks. *)
  mutable new_labels : LabelSet.t;
  (* Labels added by cfg construction, except entry. Used for split_labels. *)

  split_labels : (label, Layout.t) Hashtbl.t;
  (* Maps original label [L] to the sequence of labels of blocks
     that represent block [L] in the cfg. Sparse: only contains information
     for blocks that were split or eliminated during cfg construction.
     Used for mapping information about original blocks,
     such as perf annotations, exection counts or new layout, to cfg blocks. *)

  trap_depths : int Linear_invariants.LabelMap.t;
  (* Map labels to trap depths for linearize. *)

  trap_labels : (label, label) Hashtbl.t;
  (* Maps trap handler block label [L] to the label of the block where the
     Lpushtrap L reference it. Used for dead block elimination.
     This mapping is one to one, but the reverse is not, because
     a block might contain multiple Lpushtrap, which is not a terminator. *)
}

let no_label = (-1)
type labelled_insn =
  { label : label;
    insn : Linearize.instruction;
  }

let create_empty_instruction desc =
  { desc;
    arg = [||]; res = [||]; dbg = Debuginfo.none; live = Reg.Set.empty; }

let create_empty_block t start =
  let terminator = create_empty_instruction (Branch []) in
  let block = { start;
                body = [];
                terminator;
                predecessors = LabelSet.empty } in
  if Hashtbl.mem t.blocks start then
    Misc.fatal_error("Cannot create block, label exists: " ^
                     (string_of_int start));
  t.layout <- start::t.layout;
  block

let register t block =
  if Hashtbl.mem t.blocks block.start then
    Misc.fatal_error("Cannot register block, label exists: "
                     ^ (string_of_int block.start));
  (* Printf.printf "registering block %d\n" block.start *)
  (* Body is constructed in reverse, fix it now: *)
  block.body <- List.rev block.body;
  Hashtbl.add t.blocks block.start block

let register_predecessors t =
  Hashtbl.iter (fun label block ->
    let targets = successor_labels block in
    List.iter (fun target ->
      let target_block = Hashtbl.find t.blocks target in
      (* Add label to predecessors of target  *)
      target_block.predecessors <-
        LabelSet.add label target_block.predecessors)
      targets
  ) t.blocks

let is_live_trap_handler t label =
  Hashtbl.mem t.trap_labels label

let register_split_labels t =
  List.fold_right (fun label new_labels_layout ->
    if (LabelSet.mem label t.new_labels) then
      (* Add a new label to accumulated layout *)
      label::new_labels_layout
    else begin (* Original label found *)
      if new_labels_layout <> [] then begin
        (* The original label was followed by some new ones,
           which we have gathers in new_labels_layout.
           Tuck on original label and register the split layout. *)
        Hashtbl.add t.split_labels label (label::new_labels_layout)
      end;
      []
    end)
    t.layout
    []
  |> ignore

(* Must be called after predecessors are registered
   and split labels are registered. *)
exception Dead_block of label * block

let eliminate_dead_blocks t =

  let dead_blocks = ref [] in
  let eliminate_dead_block label block =
    dead_blocks := (label,block)::!dead_blocks;
    Hashtbl.remove t.blocks label;
    (* Update successor blocks of the dead block *)
    List.iter (fun target ->
      let target_block = Hashtbl.find t.blocks target in
      (* Remove label from predecessors of target. *)
      target_block.predecessors <- LabelSet.remove
                                     label
                                     target_block.predecessors)
      (successor_labels block);
    (* Remove from layout and other data-structures that track labels. *)
    t.layout <- List.filter (fun l -> l = label) t.layout;
    (* If the dead block contains Lpushtrap, its handler becomes dead.
       Find all occurrences of label as values of trap_labels
       and remove them, because is_live_trap_handler depends on it. *)
    Hashtbl.filter_map_inplace
      (fun _ lbl_pushtrap_block ->
         if label = lbl_pushtrap_block then None
         else Some lbl_pushtrap_block)
      t.trap_labels;

    (* If dead block's label is not new, add it to split_labels,
       mapped to the empty layout!
       Needed for mapping annotations that may refer to dead blocks. *)
    if not (LabelSet.mem label t.new_labels) then
      Hashtbl.add t.split_labels label [];

    (* No need to update trap_depths,
       which is only referenced with live labels. *)
    (* LabelMap.remove label t.trap_depths; *)
  in
  let rec find_dead_block () =
    try
      Hashtbl.iter (fun label block ->
        if (LabelSet.is_empty block.predecessors) &&
           (not (is_live_trap_handler t label)) &&
           (t.entry_label <> label) then
          raise (Dead_block (label, block)))
        t.blocks;
    with Dead_block (label,block) -> begin
        eliminate_dead_block label block;
        find_dead_block ()
      end
  in
  find_dead_block ();
  let num_dead_blocks = List.length !dead_blocks in
  if num_dead_blocks > 0 && !Clflags.verbose then begin
    Printf.printf "Found %d dead blocks in function %s:"
      num_dead_blocks
      t.fun_name;
    List.iter (fun (lbl, _) -> Printf.printf "\n%d" lbl) !dead_blocks
  end

let create_instr desc (i:Linearize.instruction) =
  {
    desc = desc;
    arg = i.arg;
    res = i.res;
    dbg = i.dbg;
    live = i.live;
  }

let get_or_make_label t (i : Linearize.instruction) =
  match i.desc with
  | Llabel label -> { label; insn = i; }
  | Lbranch _ -> Misc.fatal_error("Unexpected branch instead of label")
  | Lend -> Misc.fatal_error("Unexpected end of function instead of label")
  | _ -> let label = Cmm.new_label () in
    t.new_labels <- LabelSet.add label t.new_labels;
    { label;
      insn = Linearize.instr_cons (Llabel label) [||] [||] i;
    }

(* Is [i] an existing label? *)
let has_label (i : Linearize.instruction) =
  begin match i.desc with
  | Lend | Llabel _ -> true
  | _ -> Misc.fatal_error("Unexpected instruction after terminator")
  end

let add_terminator t desc i block =
  block.terminator <- create_instr desc i;
  register t block

let mark_trap_label t ~lbl_handler ~lbl_pushtrap_block =
  if (Hashtbl.mem t.trap_labels lbl_handler) then
    Misc.fatal_errorf "Trap hanlder label already exists: \
                       Lpushtrap %d from block label %d\n"
      lbl_handler
      lbl_pushtrap_block;
  Hashtbl.add t.trap_labels lbl_handler lbl_pushtrap_block

let from_basic = function
  | Reloadretaddr -> Lreloadretaddr
  | Entertrap -> Lentertrap
  | Adjust_trap_depth { delta_traps } -> Ladjust_trap_depth { delta_traps }
  | Pushtrap { lbl_handler } -> Lpushtrap { lbl_handler }
  | Poptrap -> Lpoptrap
  | Call(F(Indirect {label_after})) -> Lop(Icall_ind {label_after})
  | Call(F(Immediate {func;label_after})) -> Lop(Icall_imm {func; label_after})
  | Call(P(External {func; alloc; label_after})) ->
    Lop(Iextcall {func; alloc; label_after})
  | Call(P(Checkbound({immediate=None; label_after_error; spacetime_index}))) ->
    Lop(Iintop(Icheckbound {label_after_error; spacetime_index}))
  | Call(P(Checkbound({immediate = Some i;
                       label_after_error;
                       spacetime_index}))) ->
    Lop(Iintop_imm(Icheckbound {label_after_error; spacetime_index},i))
  | Call(P(Alloc {words;label_after_call_gc;spacetime_index})) ->
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
      (* End of the function. Make sure the previous block is registered. *)
      if not (Hashtbl.mem t.blocks block.start) then
        Misc.fatal_errorf
          "End of function without terminator for block %d\n"
          block.start
    | Llabel start ->
      (* Add the previos block, if it did not have an explicit terminator. *)
      if not (Hashtbl.mem t.blocks block.start) then begin
        (* Previous block falls through. Add start as explicit successor. *)
        let fallthrough = Branch [(Always,start)] in
        block.terminator <- create_empty_instruction fallthrough;
        register t block
      end;
      (* Start a new block *)
      (* CR gyorsh: check for multpile consecutive labels *)
      let new_block = create_empty_block t start in
      create_blocks t i.next new_block

    | Lop(Itailcall_ind {label_after}) ->
      let desc = Tailcall(Indirect {label_after}) in
      assert (has_label i.next);
      add_terminator t desc i block;
      create_blocks t i.next block

    | Lop(Itailcall_imm {func; label_after}) ->
      let desc = Tailcall(Immediate{func;label_after}) in
      assert (has_label i.next);
      add_terminator t desc i block;
      create_blocks t i.next block

    | Lreturn ->
      assert (has_label i.next);
      add_terminator t Return i block;
      create_blocks t i.next block

    | Lraise(kind) ->
      assert (has_label i.next);
      add_terminator t (Raise kind) i block;
      create_blocks t i.next block

    | Lbranch lbl ->
      let successors = [(Always,lbl)] in
      assert (has_label i.next);
      add_terminator t (Branch successors) i block;
      create_blocks t i.next block

    | Lcondbranch(cond,lbl) ->
      let fallthrough = get_or_make_label t i.next in
      let successors = [(Test cond,lbl);
                        (Test (invert_test cond),fallthrough.label)] in
      add_terminator t (Branch successors) i block;
      create_blocks t fallthrough.insn block

    | Lcondbranch3(lbl0,lbl1,lbl2) ->
      let fallthrough = get_or_make_label t i.next in
      let get_dest = function
        | None -> fallthrough.label
        | Some lbl -> lbl
      in
      let s0 = (Test(Iinttest_imm(Iunsigned Clt, 1)), get_dest lbl0) in
      let s1 = (Test(Iinttest_imm(Iunsigned Ceq, 1)), get_dest lbl1) in
      let s2 = (Test(Iinttest_imm(Isigned   Cgt, 1)), get_dest lbl2) in
      add_terminator t (Branch [s0;s1;s2]) i block;
      create_blocks t fallthrough.insn block

    | Lswitch labels ->
      add_terminator t (Switch labels) i block;
      assert (has_label i.next);
      create_blocks t i.next block

    | d ->
      let desc = begin match d with
        | Lpushtrap { lbl_handler } ->
          mark_trap_label t ~lbl_handler ~lbl_pushtrap_block:block.start;
          Pushtrap { lbl_handler }
        | Lentertrap -> Entertrap
        | Ladjust_trap_depth { delta_traps } ->
          Adjust_trap_depth { delta_traps }
        | Lpoptrap -> Poptrap
        | Lreloadretaddr -> Reloadretaddr
        | Lop(op) -> begin match op with
          | Icall_ind { label_after} -> Call(F(Indirect {label_after}))
          | Icall_imm {func; label_after} ->
            Call(F(Immediate {func;label_after}))
          | Iextcall {func; alloc; label_after} ->
            Call(P(External {func; alloc; label_after}))
          | Iintop(op) -> begin match op with
            | Icheckbound {label_after_error; spacetime_index} ->
              Call(P(Checkbound{ immediate = None;
                                 label_after_error;
                                 spacetime_index}))
            | _ -> Op(Intop(op))
          end
          | Iintop_imm(op,i) -> begin match op with
            | Icheckbound {label_after_error; spacetime_index} ->
              Call(P(Checkbound ({immediate = Some i;
                                label_after_error;
                                spacetime_index; })))
            |_ -> Op(Intop_imm(op, i))
          end
          | Ialloc {words;label_after_call_gc;spacetime_index} ->
            Call(P(Alloc {words;label_after_call_gc;spacetime_index}))
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
          | Iname_for_debugger {ident;
                                which_parameter;
                                provenance;
                                is_assignment;} ->
            Op(Name_for_debugger {ident;
                                  which_parameter;
                                  provenance;
                                  is_assignment;})
          | Itailcall_ind _ | Itailcall_imm _ -> assert (false)
        end
        | Lend| Lreturn| Llabel _
        | Lbranch _| Lcondbranch (_, _)| Lcondbranch3 (_, _, _)
        | Lswitch _| Lraise _ -> assert (false)
      end
      in
      block.body <- (create_instr desc i)::block.body;
      create_blocks t i.next block

let make_empty_cfg (f : Linearize.fundecl) =
  {
    fun_name = f.fun_name;
    blocks = (Hashtbl.create 31 : (label, block) Hashtbl.t);
    trap_depths = Linear_invariants.compute_trap_depths f;
    trap_labels = (Hashtbl.create 7 : (label, label) Hashtbl.t);
    new_labels = LabelSet.empty;
    split_labels = (Hashtbl.create 7 : (label, Layout.t) Hashtbl.t);
    entry_label = 0;
    layout = [];
  }

let from_linear (f : Linearize.fundecl) =
  let t = make_empty_cfg f in
  (* CR gyorsh: label of the function entry must not conflict with existing
     labels. Relies on the invariant: Cmm.new_label() is int > 99.
     An alternative is to create a new type for label here,
     but it is less efficient because label is used as a key to Hashtble. *)
  let entry_block = create_empty_block t t.entry_label in
  create_blocks t f.fun_body entry_block;
  (* Register predecessors now rather than during cfg construction,
     because of forward jumps: the blocks do not exist when the jump
     that reference them is processed.
     CR gyorsh: combine with dead block elimination. *)
  register_predecessors t;
  register_split_labels t;
  eliminate_dead_blocks t;
  (* Layout was constructed in reverse, fix it now: *)
  t.layout <- List.rev t.layout;
  (t, t.layout)

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

let linearize_terminator terminator next =
  let desc_list =
    match terminator.desc with
    | Return -> [Lreturn]
    | Raise kind -> [Lraise kind]
    | Tailcall(Indirect {label_after}) ->
      [Lop(Itailcall_ind {label_after})]
    | Tailcall(Immediate {func;label_after}) ->
      [Lop(Itailcall_imm {func;label_after})]
    | Switch labels -> [Lswitch labels]
    | Branch successors ->
      match successors with
      | [] -> Misc.fatal_error ("Branch without successors")
      | [(Always,label)] ->
        if next.label = label then []
        else [Lbranch(label)]
      | [(Test _, _)] -> Misc.fatal_error ("Successors not exhastive");
      | [(Test cond_p,label_p); (Test cond_q,label_q)] ->
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
          [Lcondbranch(cond_p,label_p); Lcondbranch(cond_q,label_q)]
        else if label_p = next.label then
          [Lcondbranch(cond_q,label_q)]
        else if label_q = next.label then
          [Lcondbranch(cond_p,label_p)]
        else assert false
      | [(Test(Iinttest_imm(Iunsigned Clt, 1)),label0);
         (Test(Iinttest_imm(Iunsigned Ceq, 1)),label1);
         (Test(Iinttest_imm(Isigned   Cgt, 1)),label2)] ->
        let find_label l =
          if next.label = l then None
          else Some l
        in
        [Lcondbranch3(find_label label0,
                      find_label label1,
                      find_label label2)]
      | _ -> assert (false)
  in
  List.fold_right (to_linear_instr ~i:terminator) desc_list next.insn

let to_linear t layout =
  let layout = Array.of_list layout in
  let len = Array.length layout in
  let next = ref { label = no_label; insn = end_instr; } in
  for i = len - 1 downto 0 do
    let label = layout.(i) in
    let block = Hashtbl.find t.blocks label in
    let terminator = linearize_terminator block.terminator !next  in
    let body = List.fold_right basic_to_linear block.body terminator in
    if i = 0 then begin (* First block, don't add label. *)
      next := { label; insn=body; }
    end
    else begin
      let pred = layout.(i-1) in
      if block.predecessors = LabelSet.singleton pred then begin
        if is_live_trap_handler t block.start then
          Misc.fatal_errorf "Fallthrough from %d to trap handler %d\n"
            pred block.start;
        (* Single predecessor is immediately prior to this block,
           no need for the label. *)
        (* CR gyorsh: is this correct with label_after for calls? *)
        next := { label; insn=body; }
      end
      else begin
        next := { label;
                  insn = make_simple_linear (Llabel label) body;
                }
      end
    end
  done;
  !next.insn
