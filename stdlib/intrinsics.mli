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

(** Bit-scan-reverse. Emit bsr instruction on amd64. *)
external bsr : int -> (int [@untagged]) =
  "caml_int_bsr" "caml_int_bsr_untagged" [@@noalloc]

external int64_bsr : (int64 [@unboxed]) -> (int [@untagged]) =
    "caml_bsr" "caml_bsr_int64_unboxed" [@@noalloc]

(** Count leading zeros. Emit lzcnt instruction on amd64 target.

    LZCNT instruction is not available on Intel Architectures prior to Haswell.

    Important: lzcnt assembles to bsr on architectures prior to Haswell.
    Code that uses lzcnt will run on older Intels and
    silently produce wrong results.

    The user is responsible for checking cpu performance
    counter configuration to avoid this situation.

    Two version are provide for experimenting with performance.
*)
external lzcnt : int -> (int [@untagged]) =
  "caml_int_lzcnt" "caml_int_lzcnt_untagged" [@@noalloc]

external lzcnt2 : int -> int =
  "caml_int_lzcnt" "caml_untagged_int_lzcnt" [@@untagged] [@@noalloc]
