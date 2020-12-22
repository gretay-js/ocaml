(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* XCR mshinwell: How do we determine the defaults for these values?
   It seems like maybe a configure script test is needed.

   gyorsh: I had it all set to true by default, because the support is available
   in all but the most ancient CPUs. Also, I wanted to avoid
   target-specific variables in Config and architecture specific checks in
   configure. The best way to do it at configure time is to check
   for CPUID feature flags. I've updated configure scripts to do it,
   and made changes to Config and the initial values below.

   gyorsh: After we discussed it, I'm planning to revert the changes in
   Config. I do not think it is a good idea for configure to fail without
   recovery if the default settings are not supported, (a) because
   flambda backend might not build upstream on some older x86_64 targets,
   and (b) users can set compile-time flags to override the defaults.

   Ideally, we probably need a way to configure per-target.

   In the meantime: configure can still be used to check the defaults and fail,
   but with another configure-time command-line option to ignore this failure.
   The command-line options below can be used to control code generation
   if the defaults are not applicable. When configure fails, it can
   print a message to the user about the "ignore" configure option
   and point to the correct compile-time options to use.
   It is the user's responsibility to pass these options everywhere if
   the defaults are inapplicable and configure failure was "ignored".

   The risk is that the compiler itself might use one of the intrinsics
   on a platform where it is not supported. To handle it,
   the compile time flags that disable it can be added to OPTCOMPFLAGS
   in Makefile.common.in.
*)

(* XCR mshinwell: This potentially major caveat should probably go in the help
   text of the relevant option below.

   gyorsh: done.
*)

(* LZCNT instruction is not available on Intel Architectures prior to Haswell.

   Important: lzcnt assembles to bsr on architectures prior to Haswell.  Code
   that uses lzcnt will run on older Intels and silently produce wrong
   results. *)
let lzcnt_support = ref Config.lzcnt_support

(* XCR mshinwell: Likewise, I would put something to this effect in the help
   text below. *)
(* POPCNT instruction is not available prior to Nehalem. *)
let popcnt_support = ref Config.popcnt_support

(* PREFETCHW instruction is not available on processors
   based on Haswell or earlier microarchitectures. *)
let prefetchw_support = ref Config.prefetchw_support

(* PREFETCHWT1 is Intel Xeon Phi only. *)
let prefetchwt1_support = ref Config.prefetchwt1_support

(* CRC32 requires SSE 4.2 support *)
let crc32_support = ref Config.sse42_support

(* Machine-specific command-line options *)

let command_line_options =
  [ "-fPIC", Arg.Set Clflags.pic_code,
      " Generate position-independent machine code (default)";
    "-fno-PIC", Arg.Clear Clflags.pic_code,
      " Generate position-dependent machine code";
    "-flzcnt", Arg.Set lzcnt_support,
      " Use LZCNT instruction to count leading zeros. \n\
LZCNT instruction is not available on Intel Architectures prior to Haswell.\n\
Important: code that uses LZCNT will run on older Intels and silently produce\n\
wrong results, because LZCNT assembles to BSR.\n";
    "-fno-lzcnt", Arg.Clear lzcnt_support,
      " Do not use LZCNT instruction to count leading zeros";
    "-fpopcnt", Arg.Set popcnt_support,
      " Use POPCNT instruction to count the number of bits set.\n\
POPCNT instruction is not available prior to Nehalem.";
    "-fno-popcnt", Arg.Clear popcnt_support,
      " Do not use POPCNT instruction to count the number of bits set";
    "-fprefetchw", Arg.Set prefetchw_support,
      " Use PREFETCHW and PREFETCHWT1 instructions.\n\
PREFETCHW instruction is not available on processors based on Haswell \n\
or earlier microarchitectures. PREFETCHWT1 is Intel Xeon Phi only.";
    "-fno-prefetchw", Arg.Clear prefetchw_support,
      " Do not use PREFETCHW and PREFETCHWT1 instructions";
    "-fcrc32", Arg.Set crc32_support,
      " Use CRC32 instructions (requires SSE4.2 support)";
    "-fno-crc32", Arg.Clear crc32_support,
      " Do not emit CRC32 instructions";
  ]

(* Specific operations for the AMD64 processor *)

open Format

type prefetch_temporal_locality_hint = Nonlocal | Low | Moderate | High

let prefetch_temporal_locality_hint = function
  | Nonlocal -> "none" (* CR mshinwell: same comment as in the Cmm part *)
  | Low -> "low"
  | Moderate -> "moderate"
  | High -> "high"

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)
  | Iindexed2 of int                    (* reg + reg + displ *)
  | Iscaled of int * int                (* reg * scale + displ *)
  | Iindexed2scaled of int * int        (* reg + reg * scale + displ *)

(* XCR mshinwell: rename to prefetch_locality_hint or something?  (I left a
   similar CR elsewhere; it would be worth ensuring the names match.) *)
type prefetch_info = {
  is_write: bool;
  locality: prefetch_temporal_locality_hint;
  addr: addressing_mode;
}

type specific_operation =
    Ilea of addressing_mode             (* "lea" gives scaled adds *)
  | Istore_int of nativeint * addressing_mode * bool
                                        (* Store an integer constant *)
  | Ioffset_loc of int * addressing_mode (* Add a constant to a location *)
  | Ifloatarithmem of float_operation * addressing_mode
                                       (* Float arith operation with memory *)
  | Ibswap of int                      (* endianness conversion *)
  | Isqrtf                             (* Float square root *)
  | Ifloatsqrtf of addressing_mode     (* Float square root from memory *)
  | Isextend32                         (* 32 to 64 bit conversion with sign
                                          extension *)
  | Izextend32                         (* 32 to 64 bit conversion with zero
                                          extension *)
  | Ilzcnt                             (* count leading zeros instruction *)
  (* XCR mshinwell: [non_zero] isn't very descriptive.  Maybe
     "arg_is_definitely_non_zero" or something (assuming that's what it
     means)? *)
  | Ibsr of { arg_is_non_zero : bool } (* bit scan reverse instruction *)
  | Ibsf of { arg_is_non_zero : bool } (* bit scan forward instruction *)
  | Irdtsc                             (* read timestamp *)
  | Irdpmc                             (* read performance counter *)
  | Icrc32q                            (* compute crc *)
  | Iprefetch of prefetch_info         (* memory prefetching hint *)

and float_operation =
    Ifloatadd | Ifloatsub | Ifloatmul | Ifloatdiv

let spacetime_node_hole_pointer_is_live_before _specific_op = false

(* Sizes, endianness *)

let big_endian = false

let size_addr = 8
let size_int = 8
let size_float = 8

let allow_unaligned_access = true

(* Behavior of division *)

let division_crashes_on_overflow = true

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Ibased(s, n) -> Ibased(s, n + delta)
  | Iindexed n -> Iindexed(n + delta)
  | Iindexed2 n -> Iindexed2(n + delta)
  | Iscaled(scale, n) -> Iscaled(scale, n + delta)
  | Iindexed2scaled(scale, n) -> Iindexed2scaled(scale, n + delta)

let num_args_addressing = function
    Ibased _ -> 0
  | Iindexed _ -> 1
  | Iindexed2 _ -> 2
  | Iscaled _ -> 1
  | Iindexed2scaled _ -> 2

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
  | Ibased(s, 0) ->
      fprintf ppf "\"%s\"" s
  | Ibased(s, n) ->
      fprintf ppf "\"%s\" + %i" s n
  | Iindexed n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a%s" printreg arg.(0) idx
  | Iindexed2 n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a + %a%s" printreg arg.(0) printreg arg.(1) idx
  | Iscaled(scale, n) ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a  * %i%s" printreg arg.(0) scale idx
  | Iindexed2scaled(scale, n) ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a + %a * %i%s" printreg arg.(0) printreg arg.(1) scale idx

let print_specific_operation printreg op ppf arg =
  match op with
  | Ilea addr -> print_addressing printreg addr ppf arg
  | Istore_int(n, addr, is_assign) ->
      fprintf ppf "[%a] := %nd %s"
         (print_addressing printreg addr) arg n
         (if is_assign then "(assign)" else "(init)")
  | Ioffset_loc(n, addr) ->
      fprintf ppf "[%a] +:= %i" (print_addressing printreg addr) arg n
  | Isqrtf ->
      fprintf ppf "sqrtf %a" printreg arg.(0)
  | Ifloatsqrtf addr ->
     fprintf ppf "sqrtf float64[%a]"
             (print_addressing printreg addr) [|arg.(0)|]
  | Ifloatarithmem(op, addr) ->
      let op_name = function
      | Ifloatadd -> "+f"
      | Ifloatsub -> "-f"
      | Ifloatmul -> "*f"
      | Ifloatdiv -> "/f" in
      fprintf ppf "%a %s float64[%a]" printreg arg.(0) (op_name op)
                   (print_addressing printreg addr)
                   (Array.sub arg 1 (Array.length arg - 1))
  | Ibswap i ->
      fprintf ppf "bswap_%i %a" i printreg arg.(0)
  | Isextend32 ->
      fprintf ppf "sextend32 %a" printreg arg.(0)
  | Izextend32 ->
      fprintf ppf "zextend32 %a" printreg arg.(0)
  | Ilzcnt ->
      fprintf ppf "lzcnt %a" printreg arg.(0)
  | Ibsr { arg_is_non_zero; } ->
      fprintf ppf "bsr arg_is_non_zero=%b %a" arg_is_non_zero printreg arg.(0)
  | Ibsf { arg_is_non_zero; } ->
      fprintf ppf "bsf arg_is_non_zero=%b %a" arg_is_non_zero printreg arg.(0)
  | Irdtsc ->
      fprintf ppf "rdtsc"
  | Irdpmc ->
      fprintf ppf "rdpmc %a" printreg arg.(0)
  | Icrc32q ->
      fprintf ppf "crc32 %a %a" printreg arg.(0) printreg arg.(1)
  | Iprefetch { is_write; locality; } ->
      fprintf ppf "prefetch is_write=%b temporal_locality=%s %a" is_write
        (prefetch_temporal_locality_hint locality) printreg arg.(0)

let win64 =
  match Config.system with
  | "win64" | "mingw64" | "cygwin" -> true
  | _                   -> false
