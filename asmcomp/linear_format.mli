(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                    Greta Yorsh, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Format of .cmir-linear files *)

(* Compiler can optionally save Linear representation of a compilation unit,
   along with other information required to emit assembly. *)
type linear_item_info =
  | Func of { decl : Linear.fundecl;
              contains_calls : bool;
              num_stack_slots : int array;
            }
  | Data of Cmm.data_item list

(* Reset unit info *)
val reset : unit -> unit

(* Add items to the current unit info *)
val add_fun : Linear.fundecl -> unit
val add_data : Cmm.data_item list -> unit

(* Marshal and unmashal a compilation unit in linear format.
   Save and restores global state required for Emit *)
val save : string -> unit
val restore : string -> linear_item_info list

(* Hack to restore per-function state, until we move it from Proc to Linear. *)
val restore_item : linear_item_info -> unit
