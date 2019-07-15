(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016 OCamlPro SAS                                          *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module LabelMap = Map.Make(
struct
  type t = Linearize.label
  let compare (x:t) y = compare x y
end)

type state = {
  trap_depth : int;
  trap_depth_at_labels : int LabelMap.t;
}

let record_trap_depth_at_label ~state ~insn ~label =
  match LabelMap.find label state.trap_depth_at_labels with
  | exception Not_found ->
    let trap_depth_at_labels =
      LabelMap.add label state.trap_depth state.trap_depth_at_labels
    in
    { state with trap_depth_at_labels; }
  | existing_trap_depth ->
    if state.trap_depth = existing_trap_depth then
      state
    else
      Misc.fatal_errorf "Conflicting trap depths for label %d (already have \
          %d but the following instruction has depth %d):@;%a"
        label
        existing_trap_depth
        state.trap_depth
        Printlinear.instr insn

let record_trap_depth_at_label_opt ~state ~insn ~label =
  match label with
  | None -> state
  | Some label -> record_trap_depth_at_label ~state ~insn ~label

let check_instruction (insn : Linearize.instruction) ~state =
  assert (state.trap_depth >= 0);
  (* CR gyorsh: enable the following check after trap_analysis is merged. *)
  (* if state.trap_depth <> insn.trap_depth then begin
   *   Misc.fatal_errorf "Trap depth %d expected on instruction:@;%a"
   *     state.trap_depth
   *     Printlinear.instr insn
   * end; *)
  match insn.desc with
    | Lend | Lop _ | Lreloadretaddr | Lraise _ | Lentertrap -> state
    | Lprologue -> state
    | Lreturn ->
      if state.trap_depth <> 0 then begin
        Misc.fatal_error "Trap depth must be zero at Lreturn"
      end;
      state
    | Llabel label | Lbranch label | Lcondbranch (_, label) ->
    record_trap_depth_at_label ~state ~insn ~label
    | Lcondbranch3 (label1, label2, label3) ->
    let state = record_trap_depth_at_label_opt ~state ~insn ~label:label1 in
    let state = record_trap_depth_at_label_opt ~state ~insn ~label:label2 in
    record_trap_depth_at_label_opt ~state ~insn ~label:label3
  | Lswitch labels ->
    Array.fold_left (fun state label ->
      record_trap_depth_at_label ~state ~insn ~label)
      state
      labels
  | Ladjust_trap_depth {delta_traps} ->
    let trap_depth = state.trap_depth + delta_traps in
    if trap_depth >= 0 then
      { state with trap_depth; }
    else
      Misc.fatal_errorf "Ladjust_trap_depth %d moves the trap depth %d \
                         below zero"
        delta_traps
        state.trap_depth
  | Lpushtrap { lbl_handler; } ->
    let state = record_trap_depth_at_label ~state ~insn ~label:lbl_handler in
    let trap_depth = state.trap_depth + 1 in
    { state with trap_depth; }
  | Lpoptrap ->
    let trap_depth = state.trap_depth - 1 in
    if trap_depth >= 0 then
      { state with trap_depth; }
    else
      Misc.fatal_errorf "Lpoptrap moves the trap depth below zero"

let rec check_instructions (insn : Linearize.instruction) ~state =
  let state = check_instruction insn ~state in
  if not (insn.next == insn) then begin
    check_instructions insn.next ~state
  end
  else
    state

let compute_trap_depths (fundecl : Linearize.fundecl) =
  let state =
    { trap_depth = 0;
      trap_depth_at_labels = LabelMap.empty;
    }
  in
  let state = check_instructions fundecl.fun_body ~state in
  state.trap_depth_at_labels

let check (fundecl : Linearize.fundecl) =
  ignore (compute_trap_depths fundecl);
  fundecl
