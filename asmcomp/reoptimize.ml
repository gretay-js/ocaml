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

[@@@ocaml.warning "+a-4-30-40-41-42"]
open Linear

(* Transformation is an identity function by default *)
let transform = ref (fun fundecl -> fundecl)
(* Change transformation *)
let setup ~f = transform := f

let fundecl f =
  if f.fun_fast && !Clflags.extended_debug then begin
    let f = !transform f in
    f
  end
  else
    f
