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

type error =
    Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

let write filename linear_unit_info =
  let ch = open_out_bin filename in
  Misc.try_finally (fun () ->
    output_string ch Config.linear_magic_number;
    output_value ch linear_unit_info
  )
    ~always:(fun () -> close_out ch)
    ~exceptionally:(fun () -> raise(Error(Marshal_failed(filename))))

let read filename =
  let ic = open_in_bin filename in
  Misc.try_finally
    (fun () ->
       let magic = Config.linear_magic_number in
       let buffer = really_input_string ic (String.length magic) in
       if buffer = magic then begin
         try
           (input_value ic : linear_unit_info)
         with End_of_file | Failure _ ->
              raise(Error(Corrupted(filename)))
            | Error e -> raise (Error e)
       end
       else if String.sub buffer 0 9 = String.sub magic 0 9 then
         raise(Error(Wrong_version(filename)))
       else
         raise(Error(Wrong_format(filename)))
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


(* Error report *)

open Format

let report_error ppf = function
  | Wrong_format filename ->
      fprintf ppf "Expected Linear format. Incompatible file %a@"
        Location.print_filename filename
  | Wrong_version filename ->
      fprintf ppf
        "%a@ is not compatible with this version of OCaml"
        Location.print_filename filename
  | Corrupted filename ->
      fprintf ppf "Corrupted format@ %a"
        Location.print_filename filename
  | Marshal_failed filename ->
      fprintf ppf "Failed to marshal Linear to file@ %a"
        Location.print_filename filename

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
