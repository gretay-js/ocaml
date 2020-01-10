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

(* Marshal and unmashal a compilation unit in intermediate representation.
   Currently supports: linear and mach *)
type 'a item_info =
  | Func of 'a
  | Data of Cmm.data_item list

type 'a unit_info =
  {
    mutable unit_name : string;
    mutable items : 'a item_info list;
  }

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

let save ~filename ~magic unit_info =
  let ch = open_out_bin filename in
  Misc.try_finally (fun () ->
    output_string ch magic;
    output_value ch unit_info;
    (* Saved because linearize and emit depend on cmm.label. *)
    output_value ch (Cmm.cur_label ());
    (* Compute digest of the contents and append it to the file. *)
    flush ch;
    let crc = Digest.file filename in
    output_value ch crc
  )
    ~always:(fun () -> close_out ch)
    ~exceptionally:(fun () -> raise(Error(Marshal_failed(filename))))

let restore ~filename ~magic =
  let ic = open_in_bin filename in
  Misc.try_finally
    (fun () ->
       let buffer = really_input_string ic (String.length magic) in
       if buffer = magic then begin
         try
           let unit_info = (input_value ic : 'a unit_info) in
           let last_label = (input_value ic : Cmm.label) in
           Cmm.reset ();
           Cmm.set_label last_label;
           let crc = (input_value ic : Digest.t) in
           (unit_info, crc)
         with End_of_file | Failure _ -> raise(Error(Corrupted(filename)))
            | Error e -> raise (Error e)
       end
       else if String.sub buffer 0 9 = String.sub magic 0 9 then
         raise(Error(Wrong_version(filename)))
       else
         raise(Error(Wrong_format(filename)))
    )
    ~always:(fun () -> close_in ic)

(* Error report *)

open Format

let report_error ppf = function
  | Wrong_format filename ->
      fprintf ppf "Expected format is incompatible with file %a"
        Location.print_filename filename
  | Wrong_version filename ->
      fprintf ppf
        "%a@ is not compatible with this version of OCaml"
        Location.print_filename filename
  | Corrupted filename ->
      fprintf ppf "Corrupted format@ %a"
        Location.print_filename filename
  | Marshal_failed filename ->
      fprintf ppf "Failed to marshal IR to file@ %a"
        Location.print_filename filename

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
