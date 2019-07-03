(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Dead code elimination: remove pure instructions whose results are
   not used. *)

open Mach

module Int = Identifiable.Make (Numbers.Int)

type d = {
  i : instruction;   (* optimized instruction *)
  regs : Reg.Set.t;  (* a set of registers live "before" instruction [i] *)
  exits : Int.Set.t;  (* indexes of Iexit instructions "live before" [i] *)
}

let rec deadcode i =
  let arg =
    if Config.spacetime
      && Mach.spacetime_node_hole_pointer_is_live_before i
    then Array.append i.arg [| Proc.loc_spacetime_node_hole |]
    else i.arg
  in
  match i.desc with
  | Iend | Ireturn | Iop(Itailcall_ind _) | Iop(Itailcall_imm _) | Iraise _ ->
      let regs = Reg.add_set_array i.live arg in
      { i; regs; exits = Int.Set.empty; }
  | Iop op ->
      let s = deadcode i.next in
      if Proc.op_is_pure op                     (* no side effects *)
      && Reg.disjoint_set_array s.regs i.res   (* results are not used after *)
      && not (Proc.regs_are_volatile arg)      (* no stack-like hard reg *)
      && not (Proc.regs_are_volatile i.res)    (*            is involved *)
      then begin
        assert (Array.length i.res > 0);  (* sanity check *)
        s
      end else begin
        { i = {i with next = s.i};
          regs = Reg.add_set_array i.live arg;
          exits = s.exits;
        }
      end
  | Iifthenelse(test, ifso, ifnot) ->
      let ifso' = deadcode ifso in
      let ifnot' = deadcode ifnot in
      let s = deadcode i.next in
      { i = {i with desc = Iifthenelse(test, ifso'.i, ifnot'.i); next = s.i};
        regs = Reg.add_set_array i.live arg;
        exits = Int.Set.union s.exits
                  (Int.Set.union ifso'.exits ifnot'.exits);
      }
  | Iswitch(index, cases) ->
      let dc = Array.map deadcode cases in
      let cases' = Array.map (fun c -> c.i) dc in
      let s = deadcode i.next in
      { i = {i with desc = Iswitch(index, cases'); next = s.i};
        regs = Reg.add_set_array i.live arg;
        exits = Array.fold_left
                  (fun acc c -> Int.Set.union acc c.exits) s.exits dc;
      }
  | Icatch(rec_flag, handlers, body) ->
    let body' = deadcode body in
    let s = deadcode i.next in
    let handlers' = Int.Map.map deadcode (Int.Map.of_list handlers) in
    (* Previous passes guarantee that indexes of handlers are unique
       across the entire function and Iexit instructions refer
       to the correctly scoped handlers. *)
    let rec add_live nfail (live_exits, used_handlers) =
      if Int.Set.mem nfail live_exits then
        (live_exits, used_handlers)
      else
        let live_exits = Int.Set.add nfail live_exits in
        match Int.Map.find_opt nfail handlers' with
        | None -> (live_exits, used_handlers)
        | Some handler ->
          let used_handlers = (nfail, handler.i) :: used_handlers in
          Int.Set.fold add_live handler.exits (live_exits, used_handlers)
    in
    let live_exits, used_handlers =
      Int.Set.fold add_live body'.exits (s.exits, [])
    in
    { i = {i with desc = Icatch(rec_flag, used_handlers, body'.i); next = s.i};
      regs = i.live;
      exits = live_exits;
    }
  | Iexit nfail ->
      { i;  regs = i.live; exits = Int.Set.singleton nfail; }
  | Itrywith(body, handler) ->
      let body' = deadcode body in
      let handler' = deadcode handler in
      let s = deadcode i.next in
      { i = {i with desc = Itrywith(body'.i, handler'.i); next = s.i};
        regs = i.live;
        exits = Int.Set.union s.exits
                  (Int.Set.union body'.exits handler'.exits);
      }

(* Invariants checking for ICatch *)
type counters = { handlers : Int.Set.t; exits : Int.Set.t; }

(* Check handlers are unique and collect all exits that are used regardless of
   scope to check that they are all defined at the end. *)
let rec check_unique_handlers msg present i =
  match i.desc with
  | Iend -> present
  | Icatch(_, handlers, body) ->
    let present = check_unique_handlers msg present body in
    let present =
      List.fold_left (fun p (n,h) ->
        assert (not (Int.Set.mem n p.handlers));
        let p = { p with handlers = Int.Set.add n p.handlers } in
        check_unique_handlers msg p h)
        present
        handlers in
    check_unique_handlers msg present i.next
   | Itrywith(body, handler) ->
     let p = check_unique_handlers msg present body in
     let p = check_unique_handlers msg p handler in
     check_unique_handlers msg p i.next
   | Iswitch(_, cases) ->
     let p = Array.fold_left (check_unique_handlers msg) present cases in
     check_unique_handlers msg p i.next
   | Iifthenelse(_, ifso, ifnot) ->
     let p = check_unique_handlers msg present ifso in
     let p = check_unique_handlers msg p ifnot in
     check_unique_handlers msg p i.next
   | Ireturn | Iop(Itailcall_ind _) | Iop(Itailcall_imm _)
   | Iraise _  | Iop _ |  Iexit _ ->
     check_unique_handlers msg present i.next


(* Check that handler indexes are unique and that Iexit instructions
   are correctly scoped. *)
let rec check_invariants msg present i =
  match i.desc with
  | Iend -> present
  | Iexit nfail ->
    (* Scope check: exit refers to an enclosing handler,
       and the order of traversal of Icatch guarantees
       that we haven't added it to handlers yet. *)
    assert (not (Int.Set.mem nfail present.handlers));
    let present =
      { present with exits = Int.Set.add nfail present.exits } in
    check_invariants msg present i.next
  | Icatch(rec_flag, handlers, body) ->
    (* New scope entry *)
    let exits_before = present.exits in
    let present = { present with exits = Int.Set.empty } in
    let present = check_invariants msg present body in
    let check_and_add_handlers p =
       let nh =
          List.fold_left (fun p (n,_) ->
            assert (not (Int.Set.mem n p));
            Int.Set.add n p)
            p.handlers
            handlers in
       { p with handlers = nh } in
    let present =
      match rec_flag with
      | Cmm.Nonrecursive ->
        List.fold_left (fun p (_n,h) -> check_invariants msg p h)
          (check_and_add_handlers present)
          handlers
      | Cmm.Recursive ->
        (* First, recursively check all handlers.*)
        let present =
          List.fold_left (fun p (_n,h) -> check_invariants msg p h)
            present
            handlers in
        (* Then, add them. *)
        check_and_add_handlers present
    in
    (* Scope check: all handlers are live. *)
    (* Scope exit: remove exits that are captured by this catch *)
    let present =
      List.fold_left (fun p (n,_h) ->
        (* exits before are not in scope of this catch,
           must not refer to handlers in this patch. *)
        assert (not (Int.Set.mem n exits_before));
        if not (Int.Set.mem n p.exits) then begin
          Printf.printf "%s: Dead handler %d\n" msg n; p
        end else
          { p with exits = Int.Set.remove n p.exits })
        present
        handlers in
    let present =
      { present with exits = Int.Set.union present.exits exits_before } in
    check_invariants msg present i.next
   | Itrywith(body, handler) ->
     let p = check_invariants msg present body in
     let p = check_invariants msg p handler in
     check_invariants msg p i.next
   | Iswitch(_, cases) ->
     let p = Array.fold_left (check_invariants msg) present cases in
     check_invariants msg p i.next
   | Iifthenelse(_, ifso, ifnot) ->
     let p = check_invariants msg present ifso in
     let p = check_invariants msg p ifnot in
     check_invariants msg p i.next
   | Ireturn | Iop(Itailcall_ind _) | Iop(Itailcall_imm _)
   | Iraise _  | Iop _ -> check_invariants msg present i.next

let cmm_invariants f msg =
  let emp = { handlers = Int.Set.empty; exits = Int.Set.empty; } in
  let _res = check_unique_handlers msg emp f.fun_body in
  let res = check_invariants msg emp f.fun_body in
  if (not (Int.Set.is_empty res.exits)) then begin
    Printf.printf "%s: exits is not empty:\n" msg;
    Int.Set.iter (fun n -> Printf.printf "%d\n" n) res.exits;
    (* assert false; *)
  end;
  ()

let fundecl f =
  cmm_invariants f "before";
  let new_body = deadcode f.fun_body in
  let newf = {f with fun_body = new_body.i } in
  cmm_invariants newf "after";
  newf
