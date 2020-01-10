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

(* Format of .cmir-* files *)

(* Compiler can optionally save intermediate representation
   of a compilation unit, along with other information
   required to emit assembly. *)
type 'a item_info =
  | Func of 'a
  | Data of Cmm.data_item list

type 'a unit_info =
  {
    mutable unit_name : string;
    mutable items : 'a item_info list;
  }

(* Marshal and unmashal a compilation unit in Linear format.
   Save and restores global state required for Emit. *)
val save : filename:string -> magic:string -> 'a unit_info -> unit
val restore : filename:string -> magic:string -> 'a unit_info * Digest.t
