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

(* marshal and unmashal a compilation unit in linear format *)
type linear_item_info =
  | Func of Linear.fundecl
  | Data of Cmm.data_item list

type linear_unit_info =
  {
    last_label : Cmm.label;
    items : linear_item_info list;
  }

let write filename linear_unit_info =
  let ch = open_out_bin filename in
  Misc.try_finally (fun () ->
    output_string ch Config.linear_magic_number;
    output_value ch linear_unit_info
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
         (input_value ic : linear_unit_info)
       else if String.sub buffer 0 9 = String.sub magic 0 9 then
         Misc.fatal_errorf "Ocaml and %s have incompatible versions"
           filename ()
       else
         Misc.fatal_errorf "Expected linear file in %s" filename ()
    )
    ~always:(fun () -> close_in ic)

let save filename linear_items =
  let linear_unit_info =
    { last_label = Cmm.cur_label ();
      items = List.rev linear_items;
    } in
  write filename linear_unit_info

let restore filename =
  let linear_unit_info = read filename in
  Cmm.reset_label ();
  Cmm.set_label linear_unit_info.last_label;
  linear_unit_info.items
