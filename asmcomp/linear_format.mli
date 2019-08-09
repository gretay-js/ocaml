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
open Linear

type linear_item_info =
  | Func of { decl : fundecl;
              contains_calls : bool;
              num_stack_slots : int array;
            }
  | Data of Cmm.data_item list

type linear_unit_info =
  {
    last_label : Cmm.label;
    items : linear_item_info list;
  }

(* marshal and unmashal a compilation unit in linear format *)
val write : string -> linear_unit_info -> unit
val read : string -> linear_unit_info
