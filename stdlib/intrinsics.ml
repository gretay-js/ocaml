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
  "caml__rdtsc" "caml__rdtsc_unboxed" [@@noalloc]

external rdpmc : int32 -> int64  =
  "caml__rdpmc" "caml__rdpmc_unboxed" [@@unboxed] [@@noalloc]
