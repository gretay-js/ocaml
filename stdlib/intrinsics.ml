(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2020 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external rdtsc : unit -> (int64 [@unboxed])  =
  "caml_rdtsc" "caml_rdtsc_unboxed" [@@noalloc]

external rdpmc : int32 -> int64  =
  "caml_rdpmc" "caml_rdpmc_unboxed" [@@unboxed] [@@noalloc]

external bsr : int -> (int [@untagged]) =
  "caml_int_bsr" "caml_int_bsr_untagged" [@@noalloc]

external bsr2 : int -> int =
  "caml_int_bsr" "caml_untagged_int_bsr" [@@untagged] [@@noalloc]

external int64_bsr : (int64 [@unboxed]) -> (int [@untagged]) =
    "caml_int64_bsr" "caml_int64_bsr_unboxed" [@@noalloc]

external lzcnt : int -> (int [@untagged]) =
  "caml_int_lzcnt" "caml_int_lzcnt_untagged" [@@noalloc]

external lzcnt2 : int -> int =
  "caml_int_lzcnt" "caml_untagged_int_lzcnt" [@@untagged] [@@noalloc]

