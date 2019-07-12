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

open Mach

type label = Cmm.label

type instruction =
  { mutable desc: instruction_desc;
    mutable next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    dbg: Debuginfo.t;
    live: Reg.Set.t;
    id: int;
  }

and instruction_desc =
  | Lprologue
  | Lend
  | Lop of operation
  | Lreloadretaddr
  | Lreturn
  | Llabel of label
  | Lbranch of label
  | Lcondbranch of test * label
  | Lcondbranch3 of label option * label option * label option
  | Lswitch of label array
  | Lentertrap
  | Ladjust_trap_depth of { delta_traps : int }
  | Lpushtrap of {lbl_handler:label}
  | Lpoptrap
  | Lraise of Cmm.raise_kind

let has_fallthrough = function
  | Lreturn | Lbranch _ | Lswitch _ | Lraise _
  | Lop Itailcall_ind _ | Lop (Itailcall_imm _) -> false
  | _ -> true

type fundecl =
  { fun_name: string;
    fun_body: instruction;
    fun_fast: bool;
    fun_dbg : Debuginfo.t;
    fun_spacetime_shape : Mach.spacetime_shape option;
  }

(* Invert a test *)

let invert_integer_test = function
    Isigned cmp -> Isigned(Cmm.negate_integer_comparison cmp)
  | Iunsigned cmp -> Iunsigned(Cmm.negate_integer_comparison cmp)

let invert_test = function
    Itruetest -> Ifalsetest
  | Ifalsetest -> Itruetest
  | Iinttest(cmp) -> Iinttest(invert_integer_test cmp)
  | Iinttest_imm(cmp, n) -> Iinttest_imm(invert_integer_test cmp, n)
  | Ifloattest(cmp) -> Ifloattest(Cmm.negate_float_comparison cmp)
  | Ieventest -> Ioddtest
  | Ioddtest -> Ieventest

(* The "end" instruction *)

let rec end_instr =
  { desc = Lend;
    next = end_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty;
    id = 0;
  }


(* Cons an instruction (live, debug empty) *)

let instr_cons d a r n =
  { desc = d; next = n; arg = a; res = r;
    dbg = Debuginfo.none; live = Reg.Set.empty; id = 0; }

(* marshal and unmashal of compilation unit in linear format *)

type linear =
  | Func of { decl : fundecl;
              contains_calls : bool;
              num_stack_slots : int array;
            }
  | Data of Cmm.data_item list

type linear_program =
  {
    last_label : Cmm.label;
    items : linear list;
  }

let write filename linear_program =
  let ch = open_out_bin filename in
  Misc.try_finally (fun () ->
    output_string ch Config.linear_magic_number;
    output_value ch linear_program
  )
    ~always:(fun () -> close_out ch)
    ~exceptionally:(fun () ->
      Misc.fatal_errorf "Failed to marshal IR to file %s" filename)

let read filename =
  let ic = open_in_bin filename in
  Misc.try_finally
    (fun () ->
       let magic = Config.linear_magic_number in
       let buffer = really_input_string ic (String.length magic) in
       if buffer = magic then
         (input_value ic : linear_program)
       else if String.sub buffer 0 9 = String.sub magic 0 9 then
         Misc.fatal_errorf "Ocaml and %s have incompatible versions"
           filename ()
       else
         Misc.fatal_errorf "Expected linear file in %s" filename ()
    )
    ~always:(fun () -> close_in ic)
