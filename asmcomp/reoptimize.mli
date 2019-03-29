(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                based on                                *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Machine specific optimizations. *)

(** Registers function [f] as the transformer to be applied
    when this pass executes. *)
val setup: f:(Linearize.fundecl -> Linearize.fundecl) -> unit

(** Apply the transformer *)
val fundecl: Linearize.fundecl -> Linearize.fundecl
