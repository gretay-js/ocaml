[@@@ocaml.warning "+a-4-30-40-41-42"]

open Linearize

module Int = Numbers.Int

type label = Linearize.label

type call_operation =
  | Indirect of { label_after : label; }
  | Immediate of { func : string; label_after : label; }
  | External of { func : string; alloc : bool; label_after : label; }
  | Alloc of { words : int; label_after_call_gc : label option;
               spacetime_index : int; }

type operation =
    Move
  | Spill
  | Reload
  | Const_int of nativeint
  | Const_float of int64
  | Const_symbol of string
  | Stackoffset of int
  | Load of Cmm.memory_chunk * Arch.addressing_mode
  | Store of Cmm.memory_chunk * Arch.addressing_mode * bool
  | Intop of integer_operation
  | Intop_imm of integer_operation * int
  | Negf | Absf | Addf | Subf | Mulf | Divf
  | Floatofint | Intoffloat
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
  | Op of Mach.operation
  | Reloadretaddr
  | Entertrap
  | Adjust_trap_depth of { delta_traps : int }
  | Pushtrap of { lbl_handler : label }
  | Poptrap
  | Call of call_operation

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
  layout : label list;
}

(* Original layout *)
let layout = ref []

(* Collect used labels *)
let used_labels = Int.Set.empty ()

let mark_used_label lbl = Int.Set.add lbl used_labels

let create_empty_instruction desc =
  { desc; arg = [||]; res = [||]; Debuginfo.none; live = Reg.Set.empty; }

let create_empty_block start terminator =
  let terminator = create_empty_instruction terminator in
  let block = { start; body = []; terminator; } in
  if Hashtbl.mem blocks lbl then
    Misc.fatal_error("Cannot add block, label exists: " ^ lbl);
  Hashtbl.add blocks lbl block;
  block

let create_instr desc i =
  {
    desc = desc;
    arg = i.arg;
    res = i.res;
    dbg = i.dbg
    live = i.live;
  }

(* Ensure that terminator is followed by a new block. *)
let check_terminator i =
  match i.next.desc with
  | Lend | Llabel -> ();
  | _ -> Misc.fata_error("Unexpected instruction after terminator");

let rec from_linear i layout =
  let block = List.hd !layout in
    match i.desc with
    | Lend -> ()
    | Llabel start ->
      let new_block = create_empty_block start  in
      layout := new_block::!layout;
      from_linear i.next layout
    | Lop(op) -> begin
        match op with
        | Itailcall_imm label_after
        | Itailcall_ind(func; label_after)
          ->
          check_terminator i;
          let desc = { action = Call op; [] } in
          block.terminator <- create_instr desc i;
          from_linear i.next layout

        | Icall_ind label_after
        | Icall_imm(func; label_after)
        | Iextcall(func; alloc; label_after)
        | Iintop(Iinto_imm(
            Icheckbound of { label_after_error : label option;
                             spacetime_index : int; }

        | Iextcall of { func : string; alloc : bool; label_after : label; }
        | Istackoffset of int
        | Iload of Cmm.memory_chunk * Arch.addressing_mode
        | Istore of Cmm.memory_chunk * Arch.addressing_mode * bool
        (* false = initialization, true = assignment *)
        | Ialloc of { words : int; label_after_call_gc : label option;
                      spacetime_index : int; }

        | _ ->
          block.body <- (create_instr Op(op) i)::block.body;
      end;
      from_linear i.next layout

    | Lreloadretaddr ->
      block.body <- (create_instr Reloadretaddr i)::block.body;
      from_linear i.next layout

    | Lentertrap ->
      block.body <- (create_instr Entertrap  i)::block.body;
      from_linear i.next layout

    | Ladjust_trap_depth delta_traps ->
      cons_instr Adjust_trap_depth(delta_traps) i block;
      from_linear i.next layout

    | Lpushtrap lbl_handler->
      Int.Set.add lbl_hanlder used_labels;
      cons_basic_instr Pushtrap(lbl_handler) i block;
      record_trap_depth_at_label ~label:handler ;
      let new_trap_depth = trap_depth + 1 in
      from_linear i.next layout :new_trap_depth

    | Lpoptrap ->
      cons_basic_instr Poptrap i block;
      from_linear i.next layout

    | Lreturn ->
      check_terminator i;
      let desc = { action = Return; successors = [] } in
      block.terminator <- create_instr desc i;
      from_linear i.next layout

    | Lbranch lbl ->
      check_terminator i;
      let desc = { action = Nothing; [(Always,lbl)] } in
      block.terminator <- create_instr desc i;
      from_linear i.next layout

    | Lcondbranch(cond,lbl) ->
      let desc = { action = Nothing; [(Always,lbl)] } in
      block.terminator <- create_instr desc i;
      from_linear i.next layout

    | Lcondbranch3 of
        label option * label option * label option

    | Lswitch label array ->
      Array.fold_left (fun state label ->
        record_trap_depth_at_label ~state ~insn ~label)
        state
        labels

    | Lraise(raise_kind) ->


let reset () =
  Hashtbl.reset blocks;
  Hashtbl.reset trap_depths;
  Int.Set.reset used_labels;
  ()

let from_linear i =
  blocks = (Hashtbl.create 31 : (label, block) Hashtbl.t)

  (* CR gyorsh: label of the function entry must not conflict with existing
     labels. Relies on the invariant: Cmm.new_label() is int > 99.
     An alternative is to create a new type for label here,
     but it is less efficient because label is used as a key to Hashtble. *)
  let func_start_lbl = 0 in
  let entry_block = create_empty_block func_start_lbl in
  mark_used_label func_start_lbl;
  let layout = ref [ entry_block ] in
  from_linear_block i layout;
  layout := List.rev !layout;

  (* check that all labels are used. *)
  let unused_labels = Int.Set.diff (Hashtble.keys blocks) used_labels in
  if not (Int.Set.is_empty unused_labels) then begin
    (* CR gyorsh: remove unused labels and unreachable blocks transitively. *)
    Misc.fatal_error("Found "^
                     Int.Set.cardinal unusued_labels
                     ^" unused labels in function " ^ f.func_name)
  end;
  { blocks; layout; }

let to_linear_block { blocks; layout } =
  Lend

let to_linear layout =
  List.fold_left to_linear_block Linear.end_insr layout

(* CR gyorsh: some parameters to determine new order *)
let reorder { blocks; layout }  =
  let new_layout =
  { blocks; layout }
