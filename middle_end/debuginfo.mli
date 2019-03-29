(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2006 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type item = private {
  dinfo_file: string;
  dinfo_line: int;
  dinfo_char_start: int;
  dinfo_char_end: int;
  dinfo_start_bol: int;
  dinfo_end_bol: int;
  dinfo_end_line: int;
  dinfo_discriminator: int;
}

type t = item list

val none : t

val is_none : t -> bool

val to_string : t -> string

val from_location : Location.t -> t

val to_location : t -> Location.t

val concat: t -> t -> t

val inline: Location.t -> t -> t

val compare : t -> t -> int

val hash : t -> int

val print_compact : Format.formatter -> t -> unit


(* CR gyorsh: used for creating debug info for low level IR,
   but we should have a better way to pass in this information. *)
val make: file:string -> line:int -> discriminator:int -> t
