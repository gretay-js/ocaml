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

external rdtsc : unit -> (int64 [@unboxed])  =
  "caml_rdtsc" "caml_rdtsc_unboxed" [@@noalloc]

external rdpmc : int32 -> int64  =
  "caml_rdpmc" "caml_rdpmc_unboxed" [@@unboxed] [@@noalloc]
