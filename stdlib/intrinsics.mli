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

(** Performance monitoring instructions that read hadware counters,
    available on some x86 targets. Returns 0 on unsupported targets.  *)

external rdtsc : unit -> int64  =
  "caml__rdtsc" "caml__rdtsc_unboxed" [@@unboxed] [@@noalloc]

external rdpmc : int32 -> int64  =
  "caml__rdpmc" "caml__rdpmc_unboxed" [@@unboxed] [@@noalloc]
