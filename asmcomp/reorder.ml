[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Block reordering within a function *)

open Linearize

module Int = Numbers.Int

(* Representation of a function as a Control Flow Graph. *)

type cond =
  | Always
  | Never
  | Test of Mach.test

(* basic block *)
type block = {
  start : label option; (* CR gyorsh: can be more than one? list? *)
  mutable body : basic instruction list;
  mutable terminator : terminator instruction;
  mutable successors : ...;
  mutable trap_depth : int option; (* CR gyorsh: compute in trap_analysis from pr1482 *)
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
  | Pushtrap of { lbl_handler : Mach.lbl }
  | Poptrap

and action =
  | Nothing
  | Return
  | Call of Mach.operation
  | Raise of Cmm.raise_kind

and terminator = {
  action : action;
  mutable successors : (cond * Mach.lbl) list;
}


(* Original layout *)
let layout = ref []

(* Map labels to blocks *)
let blocks = (Hashtbl.create 31 : (label, block) Hashtbl.t)

(* Map labels to trap_depths *)
let trap_depths = (Hashtbl.create 31 : (label, int) Hashtbl.t)

let record_trap_depth_at_label ~label ~trap_depth =
  match Hashtble.find label trap_depths with
  | exception Not_found ->
    Hashtble.add label trap_depth trap_depths
  | existing_trap_depth ->
    if not (trap_depth = existing_trap_depth) then
      Misc.fatal_errorf "Conflicting trap depths for label %d (already have \
          %d but the following instruction has depth %d):@;%a"
        label
        existing_trap_depth
        state.trap_depth

let record_trap_depth_at_label_opt ~label ~trap_depth =
  match label with
  | None -> state
  | Some label -> record_trap_depth_at_label ~state ~label

(* Collect used labels *)
let used_labels = Int.Set.empty ()

let mark_used_label lbl = Int.Set.add lbl used_labels

let create_empty_instruction desc =
  { desc; arg = [||]; res = [||]; Debuginfo.none; live = Reg.Set.empty; }

let create_empty_block start ~trap_depth =
  record_trap_depth_at_label ~label:start ~trap_depth;
  let terminator = create_empty_instruction
                     { action = Nothing; successors = [] } in
  let block = { start; body = []; terminator; trap_depth; } in
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

let rec from_linear i layout ~trap_depth =
  assert (trap_depth >= 0);
  let block = List.hd !layout in
    match i.desc with
    | Lend -> ()
    | Llabel start ->
      let new_block = create_empty_block start ~trap_depth in
      layout := new_block::!layout;
      from_linear i.next layout ~trap_depth
    | Lop(op) -> begin
        match op with
        | Itailcall_imm label_after
        | Itailcall_ind(func; label_after)
          ->
          check_terminator i;
          let desc = { action = Call op; [] } in
          block.terminator <- create_instr desc i;
          from_linear i.next layout ~trap_depth

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
      from_linear i.next layout ~trap_depth

    | Lreloadretaddr ->
      block.body <- (create_instr Reloadretaddr i)::block.body;
      from_linear i.next layout ~trap_depth

    | Lentertrap ->
      block.body <- (create_instr Entertrap  i)::block.body;
      from_linear i.next layout ~trap_depth

    | Ladjust_trap_depth delta_traps ->
      record_trap_depth_at_label ~label ~trap_depth;
      cons_instr Adjust_trap_depth(delta_traps) i block;
      let new_trap_depth = trap_depth + delta_traps in
      if new_trap_depth < 0 then begin
        Misc.fatal_errorf "Ladjust_trap_depth %d moves the trap depth %d \
                           below zero"
          delta
          trap_depth
      end;
      from_linear i.next layout ~trap_depth:new_trap_depth

    | Lpushtrap lbl_handler->
      Int.Set.add lbl_hanlder used_labels;
      cons_basic_instr Pushtrap(lbl_handler) i block;
      record_trap_depth_at_label ~label:handler ~trap_depth;
      let new_trap_depth = trap_depth + 1 in
      from_linear i.next layout ~trap_depth:new_trap_depth

    | Lpoptrap ->
      cons_basic_instr Poptrap i block;
      let new_trap_depth = trap_depth - 1 in
      if trap_depth < 0 then begin
        Misc.fatal_errorf "Lpoptrap moves the trap depth below zero"
      end;
      from_linear i.next layout ~trap_depth:new_trap_depth

    | Lreturn ->
      if trap_depth <> 0 then begin
        Misc.fatal_error "Trap depth must be zero at Lreturn"
      end;
      check_terminator i;
      let desc = { action = Return; successors = [] } in
      block.terminator <- create_instr desc i;
      from_linear i.next layout ~trap_depth

    | Lbranch lbl ->
      record_trap_depth_at_label ~label ~trap_depth;
      check_terminator i;
      let desc = { action = Nothing; [(Always,lbl)] } in
      block.terminator <- create_instr desc i;
      from_linear i.next layout ~trap_depth

    | Lcondbranch of test * label
    | Lcondbranch3 of label option * label option * label option
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
  (* CR gyorsh: label of the function entry must not conflict with existing
     labels. Relies on the invariant: Cmm.new_label() is int > 99.
     An alternative is to create a new type for label here,
     but it is less efficient because label is used as a key to Hashtble. *)
  let func_start_lbl = 0 in
  let entry_block = create_empty_block func_start_lbl ~trap_depth:0 in
  mark_used_label func_start_lbl;
  ref [ entry_block ]


let fundecl f =
  if f.fun_fast then begin
    let out_layout = reset () in
    from_linear f.fun_body old_layout ~trap_depth:0;
    old_layout := List.rev !old_layout;
    (* check that all labels are used. *)
    let unused_labels = Int.Set.diff (Hashtble.keys blocks) used_labels in
    if not (Int.Set.is_empty unused_labels) then begin
      (* CR gyorsh: remove unused labels and unreachable blocks transitively. *)
      Misc.fatal_error("Found "^
                       Int.Set.cardinal unusued_labels
                       ^" unused labels in function " ^ f.func_name)
    end;
    let new_layout = reorder out_layout in
    let new_body = to_linear new_layout in
    {f with fun_body = new_body.i}
  end
  else
    f
