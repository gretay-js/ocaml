(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translation from closed lambda to C-- *)

[@@@ocaml.warning "-40"]

open Misc
open Arch
open Asttypes
open Primitive
open Types
open Lambda
open Clambda
open Clambda_primitives
open Cmm
open Cmx_format
open Cmxs_format

module String = Misc.Stdlib.String
module V = Backend_var
module VP = Backend_var.With_provenance

(* Environments used for translation to Cmm. *)

type boxed_number =
  | Boxed_float of Debuginfo.t
  | Boxed_integer of boxed_integer * Debuginfo.t

type env = {
  unboxed_ids : (V.t * boxed_number) V.tbl;
  environment_param : V.t option;
}

let empty_env =
  {
    unboxed_ids =V.empty;
    environment_param = None;
  }

let create_env ~environment_param =
  { unboxed_ids = V.empty;
    environment_param;
  }

let is_unboxed_id id env =
  try Some (V.find_same id env.unboxed_ids)
  with Not_found -> None

let add_unboxed_id id unboxed_id bn env =
  { env with
    unboxed_ids = V.add id (unboxed_id, bn) env.unboxed_ids;
  }

(* Local binding of complex expressions *)

let bind name arg fn =
  match arg with
    Cvar _ | Cconst_int _ | Cconst_natint _ | Cconst_symbol _
  | Cconst_pointer _ | Cconst_natpointer _
  | Cblockheader _ -> fn arg
  | _ -> let id = V.create_local name in Clet(VP.create id, arg, fn (Cvar id))

let bind_load name arg fn =
  match arg with
  | Cop(Cload _, [Cvar _], _) -> fn arg
  | _ -> bind name arg fn

let bind_nonvar name arg fn =
  match arg with
    Cconst_int _ | Cconst_natint _ | Cconst_symbol _
  | Cconst_pointer _ | Cconst_natpointer _
  | Cblockheader _ -> fn arg
  | _ -> let id = V.create_local name in Clet(VP.create id, arg, fn (Cvar id))

let caml_black = Nativeint.shift_left (Nativeint.of_int 3) 8
    (* cf. runtime/caml/gc.h *)

(* Block headers. Meaning of the tag field: see stdlib/obj.ml *)

let floatarray_tag dbg = Cconst_int (Obj.double_array_tag, dbg)

let block_header tag sz =
  Nativeint.add (Nativeint.shift_left (Nativeint.of_int sz) 10)
                (Nativeint.of_int tag)
(* Static data corresponding to "value"s must be marked black in case we are
   in no-naked-pointers mode.  See [caml_darken] and the code below that emits
   structured constants and static module definitions. *)
let black_block_header tag sz = Nativeint.logor (block_header tag sz) caml_black
let white_closure_header sz = block_header Obj.closure_tag sz
let black_closure_header sz = black_block_header Obj.closure_tag sz
let infix_header ofs = block_header Obj.infix_tag ofs
let float_header = block_header Obj.double_tag (size_float / size_addr)
let floatarray_header len =
  (* Zero-sized float arrays have tag zero for consistency with
     [caml_alloc_float_array]. *)
  assert (len >= 0);
  if len = 0 then block_header 0 0
  else block_header Obj.double_array_tag (len * size_float / size_addr)
let string_header len =
      block_header Obj.string_tag ((len + size_addr) / size_addr)
let boxedint32_header = block_header Obj.custom_tag 2
let boxedint64_header = block_header Obj.custom_tag (1 + 8 / size_addr)
let boxedintnat_header = block_header Obj.custom_tag 2
let caml_nativeint_ops = "caml_nativeint_ops"
let caml_int32_ops = "caml_int32_ops"
let caml_int64_ops = "caml_int64_ops"


let alloc_float_header dbg = Cblockheader (float_header, dbg)
let alloc_floatarray_header len dbg = Cblockheader (floatarray_header len, dbg)
let alloc_closure_header sz dbg = Cblockheader (white_closure_header sz, dbg)
let alloc_infix_header ofs dbg = Cblockheader (infix_header ofs, dbg)
let alloc_boxedint32_header dbg = Cblockheader (boxedint32_header, dbg)
let alloc_boxedint64_header dbg = Cblockheader (boxedint64_header, dbg)
let alloc_boxedintnat_header dbg = Cblockheader (boxedintnat_header, dbg)

(* Integers *)

let max_repr_int = max_int asr 1
let min_repr_int = min_int asr 1

let int_const dbg n =
  if n <= max_repr_int && n >= min_repr_int
  then Cconst_int((n lsl 1) + 1, dbg)
  else Cconst_natint
          (Nativeint.add (Nativeint.shift_left (Nativeint.of_int n) 1) 1n, dbg)

let natint_const_untagged dbg n =
  if n > Nativeint.of_int max_int
  || n < Nativeint.of_int min_int
  then Cconst_natint (n,dbg)
  else Cconst_int (Nativeint.to_int n, dbg)

let cint_const n =
  Cint(Nativeint.add (Nativeint.shift_left (Nativeint.of_int n) 1) 1n)

let targetint_const n =
  Targetint.add (Targetint.shift_left (Targetint.of_int n) 1)
    Targetint.one

let add_no_overflow n x c dbg =
  let d = n + x in
  if d = 0 then c else Cop(Caddi, [c; Cconst_int (d, dbg)], dbg)

let rec add_const c n dbg =
  if n = 0 then c
  else match c with
  | Cconst_int (x, _) when no_overflow_add x n -> Cconst_int (x + n, dbg)
  | Cop(Caddi, [Cconst_int (x, _); c], _)
    when no_overflow_add n x ->
      add_no_overflow n x c dbg
  | Cop(Caddi, [c; Cconst_int (x, _)], _)
    when no_overflow_add n x ->
      add_no_overflow n x c dbg
  | Cop(Csubi, [Cconst_int (x, _); c], _) when no_overflow_add n x ->
      Cop(Csubi, [Cconst_int (n + x, dbg); c], dbg)
  | Cop(Csubi, [c; Cconst_int (x, _)], _) when no_overflow_sub n x ->
      add_const c (n - x) dbg
  | c -> Cop(Caddi, [c; Cconst_int (n, dbg)], dbg)

let incr_int c dbg = add_const c 1 dbg
let decr_int c dbg = add_const c (-1) dbg

let rec add_int c1 c2 dbg =
  match (c1, c2) with
  | (Cconst_int (n, _), c) | (c, Cconst_int (n, _)) ->
      add_const c n dbg
  | (Cop(Caddi, [c1; Cconst_int (n1, _)], _), c2) ->
      add_const (add_int c1 c2 dbg) n1 dbg
  | (c1, Cop(Caddi, [c2; Cconst_int (n2, _)], _)) ->
      add_const (add_int c1 c2 dbg) n2 dbg
  | (_, _) ->
      Cop(Caddi, [c1; c2], dbg)

let rec sub_int c1 c2 dbg =
  match (c1, c2) with
  | (c1, Cconst_int (n2, _)) when n2 <> min_int ->
      add_const c1 (-n2) dbg
  | (c1, Cop(Caddi, [c2; Cconst_int (n2, _)], _)) when n2 <> min_int ->
      add_const (sub_int c1 c2 dbg) (-n2) dbg
  | (Cop(Caddi, [c1; Cconst_int (n1, _)], _), c2) ->
      add_const (sub_int c1 c2 dbg) n1 dbg
  | (c1, c2) ->
      Cop(Csubi, [c1; c2], dbg)

let rec lsl_int c1 c2 dbg =
  match (c1, c2) with
  | (Cop(Clsl, [c; Cconst_int (n1, _)], _), Cconst_int (n2, _))
    when n1 > 0 && n2 > 0 && n1 + n2 < size_int * 8 ->
      Cop(Clsl, [c; Cconst_int (n1 + n2, dbg)], dbg)
  | (Cop(Caddi, [c1; Cconst_int (n1, _)], _), Cconst_int (n2, _))
    when no_overflow_lsl n1 n2 ->
      add_const (lsl_int c1 c2 dbg) (n1 lsl n2) dbg
  | (_, _) ->
      Cop(Clsl, [c1; c2], dbg)

let is_power2 n = n = 1 lsl Misc.log2 n

and mult_power2 c n dbg = lsl_int c (Cconst_int (Misc.log2 n, dbg)) dbg

let rec mul_int c1 c2 dbg =
  match (c1, c2) with
  | (c, Cconst_int (0, _)) | (Cconst_int (0, _), c) ->
      Csequence (c, Cconst_int (0, dbg))
  | (c, Cconst_int (1, _)) | (Cconst_int (1, _), c) ->
      c
  | (c, Cconst_int(-1, _)) | (Cconst_int(-1, _), c) ->
      sub_int (Cconst_int (0, dbg)) c dbg
  | (c, Cconst_int (n, _)) when is_power2 n -> mult_power2 c n dbg
  | (Cconst_int (n, _), c) when is_power2 n -> mult_power2 c n dbg
  | (Cop(Caddi, [c; Cconst_int (n, _)], _), Cconst_int (k, _)) |
    (Cconst_int (k, _), Cop(Caddi, [c; Cconst_int (n, _)], _))
    when no_overflow_mul n k ->
      add_const (mul_int c (Cconst_int (k, dbg)) dbg) (n * k) dbg
  | (c1, c2) ->
      Cop(Cmuli, [c1; c2], dbg)


let ignore_low_bit_int = function
    Cop(Caddi,
        [(Cop(Clsl, [_; Cconst_int (n, _)], _) as c); Cconst_int (1, _)], _)
      when n > 0
      -> c
  | Cop(Cor, [c; Cconst_int (1, _)], _) -> c
  | c -> c

(* removes the 1-bit sign-extension left by untag_int (tag_int c) *)
let ignore_high_bit_int = function
    Cop(Casr,
        [Cop(Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], _) -> c
  | c -> c

let lsr_int c1 c2 dbg =
  match c2 with
    Cconst_int (0, _) ->
      c1
  | Cconst_int (n, _) when n > 0 ->
      Cop(Clsr, [ignore_low_bit_int c1; c2], dbg)
  | _ ->
      Cop(Clsr, [c1; c2], dbg)

let asr_int c1 c2 dbg =
  match c2 with
    Cconst_int (0, _) ->
      c1
  | Cconst_int (n, _) when n > 0 ->
      Cop(Casr, [ignore_low_bit_int c1; c2], dbg)
  | _ ->
      Cop(Casr, [c1; c2], dbg)

let tag_int i dbg =
  match i with
  | Cconst_int (n, _) ->
      int_const dbg n
  | Cop(Casr, [c; Cconst_int (n, _)], _) when n > 0 ->
      Cop(Cor,
        [asr_int c (Cconst_int (n - 1, dbg)) dbg; Cconst_int (1, dbg)],
        dbg)
  | c ->
      incr_int (lsl_int c (Cconst_int (1, dbg)) dbg) dbg

let untag_int i dbg =
  match i with
    Cconst_int (n, _) -> Cconst_int(n asr 1, dbg)
  | Cop(Cor, [Cop(Casr, [c; Cconst_int (n, _)], _); Cconst_int (1, _)], _)
    when n > 0 && n < size_int * 8 ->
      Cop(Casr, [c; Cconst_int (n+1, dbg)], dbg)
  | Cop(Cor, [Cop(Clsr, [c; Cconst_int (n, _)], _); Cconst_int (1, _)], _)
    when n > 0 && n < size_int * 8 ->
      Cop(Clsr, [c; Cconst_int (n+1, dbg)], dbg)
  | c -> asr_int c (Cconst_int (1, dbg)) dbg

(* Description of the "then" and "else" continuations in [transl_if]. If
   the "then" continuation is true and the "else" continuation is false then
   we can use the condition directly as the result. Similarly, if the "then"
   continuation is false and the "else" continuation is true then we can use
   the negation of the condition directly as the result. *)
type then_else =
  | Then_true_else_false
  | Then_false_else_true
  | Unknown

let invert_then_else = function
  | Then_true_else_false -> Then_false_else_true
  | Then_false_else_true -> Then_true_else_false
  | Unknown -> Unknown

let mk_if_then_else dbg cond ifso_dbg ifso ifnot_dbg ifnot =
  match cond with
  | Cconst_int (0, _) -> ifnot
  | Cconst_int (1, _) -> ifso
  | _ ->
    Cifthenelse(cond, ifso_dbg, ifso, ifnot_dbg, ifnot, dbg)

let mk_not dbg cmm =
  match cmm with
  | Cop(Caddi,
        [Cop(Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], dbg') ->
    begin
      match c with
      | Cop(Ccmpi cmp, [c1; c2], dbg'') ->
          tag_int
            (Cop(Ccmpi (negate_integer_comparison cmp), [c1; c2], dbg'')) dbg'
      | Cop(Ccmpa cmp, [c1; c2], dbg'') ->
          tag_int
            (Cop(Ccmpa (negate_integer_comparison cmp), [c1; c2], dbg'')) dbg'
      | Cop(Ccmpf cmp, [c1; c2], dbg'') ->
          tag_int
            (Cop(Ccmpf (negate_float_comparison cmp), [c1; c2], dbg'')) dbg'
      | _ ->
        (* 0 -> 3, 1 -> 1 *)
        Cop(Csubi,
          [Cconst_int (3, dbg); Cop(Clsl, [c; Cconst_int (1, dbg)], dbg)], dbg)
    end
  | Cconst_int (3, _) -> Cconst_int (1, dbg)
  | Cconst_int (1, _) -> Cconst_int (3, dbg)
  | c ->
      (* 1 -> 3, 3 -> 1 *)
      Cop(Csubi, [Cconst_int (4, dbg); c], dbg)


let create_loop body dbg =
  let cont = next_raise_count () in
  let call_cont = Cexit (cont, []) in
  let body = Csequence (body, call_cont) in
  Ccatch (Recursive, [cont, [], body, dbg], call_cont)

(* Turning integer divisions into multiply-high then shift.
   The [division_parameters] function is used in module Emit for
   those target platforms that support this optimization. *)

(* Unsigned comparison between native integers. *)

let ucompare x y = Nativeint.(compare (add x min_int) (add y min_int))

(* Unsigned division and modulus at type nativeint.
   Algorithm: Hacker's Delight section 9.3 *)

let udivmod n d = Nativeint.(
  if d < 0n then
    if ucompare n d < 0 then (0n, n) else (1n, sub n d)
  else begin
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if ucompare r d >= 0 then (succ q, sub r d) else (q, r)
  end)

(* Compute division parameters.
   Algorithm: Hacker's Delight chapter 10, fig 10-1. *)

let divimm_parameters d = Nativeint.(
  assert (d > 0n);
  let twopsm1 = min_int in (* 2^31 for 32-bit archs, 2^63 for 64-bit archs *)
  let nc = sub (pred twopsm1) (snd (udivmod twopsm1 d)) in
  let rec loop p (q1, r1) (q2, r2) =
    let p = p + 1 in
    let q1 = shift_left q1 1 and r1 = shift_left r1 1 in
    let (q1, r1) =
      if ucompare r1 nc >= 0 then (succ q1, sub r1 nc) else (q1, r1) in
    let q2 = shift_left q2 1 and r2 = shift_left r2 1 in
    let (q2, r2) =
      if ucompare r2 d >= 0 then (succ q2, sub r2 d) else (q2, r2) in
    let delta = sub d r2 in
    if ucompare q1 delta < 0 || (q1 = delta && r1 = 0n)
    then loop p (q1, r1) (q2, r2)
    else (succ q2, p - size)
  in loop (size - 1) (udivmod twopsm1 nc) (udivmod twopsm1 d))

(* The result [(m, p)] of [divimm_parameters d] satisfies the following
   inequality:

      2^(wordsize + p) < m * d <= 2^(wordsize + p) + 2^(p + 1)    (i)

   from which it follows that

      floor(n / d) = floor(n * m / 2^(wordsize+p))
                              if 0 <= n < 2^(wordsize-1)
      ceil(n / d) = floor(n * m / 2^(wordsize+p)) + 1
                              if -2^(wordsize-1) <= n < 0

   The correctness condition (i) above can be checked by the code below.
   It was exhaustively tested for values of d from 2 to 10^9 in the
   wordsize = 64 case.

let add2 (xh, xl) (yh, yl) =
  let zl = add xl yl and zh = add xh yh in
  ((if ucompare zl xl < 0 then succ zh else zh), zl)

let shl2 (xh, xl) n =
  assert (0 < n && n < size + size);
  if n < size
  then (logor (shift_left xh n) (shift_right_logical xl (size - n)),
        shift_left xl n)
  else (shift_left xl (n - size), 0n)

let mul2 x y =
  let halfsize = size / 2 in
  let halfmask = pred (shift_left 1n halfsize) in
  let xl = logand x halfmask and xh = shift_right_logical x halfsize in
  let yl = logand y halfmask and yh = shift_right_logical y halfsize in
  add2 (mul xh yh, 0n)
    (add2 (shl2 (0n, mul xl yh) halfsize)
       (add2 (shl2 (0n, mul xh yl) halfsize)
          (0n, mul xl yl)))

let ucompare2 (xh, xl) (yh, yl) =
  let c = ucompare xh yh in if c = 0 then ucompare xl yl else c

let validate d m p =
  let md = mul2 m d in
  let one2 = (0n, 1n) in
  let twoszp = shl2 one2 (size + p) in
  let twop1 = shl2 one2 (p + 1) in
  ucompare2 twoszp md < 0 && ucompare2 md (add2 twoszp twop1) <= 0
*)

let raise_regular dbg exc =
  Csequence(
    Cop(Cstore (Thirtytwo_signed, Assignment),
        [(Cconst_symbol ("caml_backtrace_pos", dbg));
         Cconst_int (0, dbg)], dbg),
      Cop(Craise Raise_withtrace,[exc], dbg))

let raise_symbol dbg symb =
  raise_regular dbg (Cconst_symbol (symb, dbg))

let rec div_int c1 c2 is_safe dbg =
  match (c1, c2) with
    (c1, Cconst_int (0, _)) ->
      Csequence(c1, raise_symbol dbg "caml_exn_Division_by_zero")
  | (c1, Cconst_int (1, _)) ->
      c1
  | (Cconst_int (n1, _), Cconst_int (n2, _)) ->
      Cconst_int (n1 / n2, dbg)
  | (c1, Cconst_int (n, _)) when n <> min_int ->
      let l = Misc.log2 n in
      if n = 1 lsl l then
        (* Algorithm:
              t = shift-right-signed(c1, l - 1)
              t = shift-right(t, W - l)
              t = c1 + t
              res = shift-right-signed(c1 + t, l)
        *)
        Cop(Casr, [bind "dividend" c1 (fun c1 ->
                     let t = asr_int c1 (Cconst_int (l - 1, dbg)) dbg in
                     let t =
                       lsr_int t (Cconst_int (Nativeint.size - l, dbg)) dbg
                     in
                     add_int c1 t dbg);
                   Cconst_int (l, dbg)], dbg)
      else if n < 0 then
        sub_int (Cconst_int (0, dbg))
          (div_int c1 (Cconst_int (-n, dbg)) is_safe dbg)
          dbg
      else begin
        let (m, p) = divimm_parameters (Nativeint.of_int n) in
        (* Algorithm:
              t = multiply-high-signed(c1, m)
              if m < 0, t = t + c1
              if p > 0, t = shift-right-signed(t, p)
              res = t + sign-bit(c1)
        *)
        bind "dividend" c1 (fun c1 ->
          let t = Cop(Cmulhi, [c1; Cconst_natint (m, dbg)], dbg) in
          let t = if m < 0n then Cop(Caddi, [t; c1], dbg) else t in
          let t =
            if p > 0 then Cop(Casr, [t; Cconst_int (p, dbg)], dbg) else t
          in
          add_int t (lsr_int c1 (Cconst_int (Nativeint.size - 1, dbg)) dbg) dbg)
      end
  | (c1, c2) when !Clflags.unsafe || is_safe = Lambda.Unsafe ->
      Cop(Cdivi, [c1; c2], dbg)
  | (c1, c2) ->
      bind "divisor" c2 (fun c2 ->
        bind "dividend" c1 (fun c1 ->
          Cifthenelse(c2,
                      dbg,
                      Cop(Cdivi, [c1; c2], dbg),
                      dbg,
                      raise_symbol dbg "caml_exn_Division_by_zero",
                      dbg)))

let mod_int c1 c2 is_safe dbg =
  match (c1, c2) with
    (c1, Cconst_int (0, _)) ->
      Csequence(c1, raise_symbol dbg "caml_exn_Division_by_zero")
  | (c1, Cconst_int ((1 | (-1)), _)) ->
      Csequence(c1, Cconst_int (0, dbg))
  | (Cconst_int (n1, _), Cconst_int (n2, _)) ->
      Cconst_int (n1 mod n2, dbg)
  | (c1, (Cconst_int (n, _) as c2)) when n <> min_int ->
      let l = Misc.log2 n in
      if n = 1 lsl l then
        (* Algorithm:
              t = shift-right-signed(c1, l - 1)
              t = shift-right(t, W - l)
              t = c1 + t
              t = bit-and(t, -n)
              res = c1 - t
         *)
        bind "dividend" c1 (fun c1 ->
          let t = asr_int c1 (Cconst_int (l - 1, dbg)) dbg in
          let t = lsr_int t (Cconst_int (Nativeint.size - l, dbg)) dbg in
          let t = add_int c1 t dbg in
          let t = Cop(Cand, [t; Cconst_int (-n, dbg)], dbg) in
          sub_int c1 t dbg)
      else
        bind "dividend" c1 (fun c1 ->
          sub_int c1 (mul_int (div_int c1 c2 is_safe dbg) c2 dbg) dbg)
  | (c1, c2) when !Clflags.unsafe || is_safe = Lambda.Unsafe ->
      (* Flambda already generates that test *)
      Cop(Cmodi, [c1; c2], dbg)
  | (c1, c2) ->
      bind "divisor" c2 (fun c2 ->
        bind "dividend" c1 (fun c1 ->
          Cifthenelse(c2,
                      dbg,
                      Cop(Cmodi, [c1; c2], dbg),
                      dbg,
                      raise_symbol dbg "caml_exn_Division_by_zero",
                      dbg)))

(* Division or modulo on boxed integers.  The overflow case min_int / -1
   can occur, in which case we force x / -1 = -x and x mod -1 = 0. (PR#5513). *)

let is_different_from x = function
    Cconst_int (n, _) -> n <> x
  | Cconst_natint (n, _) -> n <> Nativeint.of_int x
  | _ -> false

let safe_divmod_bi mkop is_safe mkm1 c1 c2 bi dbg =
  bind "dividend" c1 (fun c1 ->
  bind "divisor" c2 (fun c2 ->
    let c = mkop c1 c2 is_safe dbg in
    if Arch.division_crashes_on_overflow
    && (size_int = 4 || bi <> Pint32)
    && not (is_different_from (-1) c2)
    then
      Cifthenelse(Cop(Ccmpi Cne, [c2; Cconst_int (-1, dbg)], dbg),
        dbg, c,
        dbg, mkm1 c1 dbg,
        dbg)
    else
      c))

let safe_div_bi is_safe =
  safe_divmod_bi div_int is_safe
    (fun c1 dbg -> Cop(Csubi, [Cconst_int (0, dbg); c1], dbg))

let safe_mod_bi is_safe =
  safe_divmod_bi mod_int is_safe (fun _ dbg -> Cconst_int (0, dbg))

(* Bool *)

let test_bool dbg cmm =
  match cmm with
  | Cop(Caddi, [Cop(Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], _) ->
      c
  | Cconst_int (n, dbg) ->
      if n = 1 then
        Cconst_int (0, dbg)
      else
        Cconst_int (1, dbg)
  | c -> Cop(Ccmpi Cne, [c; Cconst_int (1, dbg)], dbg)

(* Float *)

let box_float dbg c = Cop(Calloc, [alloc_float_header dbg; c], dbg)

let map_ccatch f rec_flag handlers body =
  let handlers = List.map
      (fun (n, ids, handler, dbg) -> (n, ids, f handler, dbg))
      handlers in
  Ccatch(rec_flag, handlers, f body)

let rec unbox_float dbg cmm =
  match cmm with
  | Cop(Calloc, [Cblockheader (header, _); c], _) when header = float_header ->
      c
  | Clet(id, exp, body) -> Clet(id, exp, unbox_float dbg body)
  | Cifthenelse(cond, ifso_dbg, e1, ifnot_dbg, e2, dbg) ->
      Cifthenelse(cond,
        ifso_dbg, unbox_float dbg e1,
        ifnot_dbg, unbox_float dbg e2,
        dbg)
  | Csequence(e1, e2) -> Csequence(e1, unbox_float dbg e2)
  | Cswitch(e, tbl, el, dbg') ->
    Cswitch(e, tbl,
      Array.map (fun (expr, dbg) -> unbox_float dbg expr, dbg) el, dbg')
  | Ccatch(rec_flag, handlers, body) ->
    map_ccatch (unbox_float dbg) rec_flag handlers body
  | Ctrywith(e1, id, e2, dbg) ->
      Ctrywith(unbox_float dbg e1, id, unbox_float dbg e2, dbg)
  | c -> Cop(Cload (Double_u, Immutable), [c], dbg)

(* Complex *)

let box_complex dbg c_re c_im =
  Cop(Calloc, [alloc_floatarray_header 2 dbg; c_re; c_im], dbg)

let complex_re c dbg = Cop(Cload (Double_u, Immutable), [c], dbg)
let complex_im c dbg = Cop(Cload (Double_u, Immutable),
                        [Cop(Cadda, [c; Cconst_int (size_float, dbg)], dbg)],
                        dbg)

(* Unit *)

let return_unit dbg c = Csequence(c, Cconst_pointer (1, dbg))

let rec remove_unit = function
    Cconst_pointer (1, _) -> Ctuple []
  | Csequence(c, Cconst_pointer (1, _)) -> c
  | Csequence(c1, c2) ->
      Csequence(c1, remove_unit c2)
  | Cifthenelse(cond, ifso_dbg, ifso, ifnot_dbg, ifnot, dbg) ->
      Cifthenelse(cond,
        ifso_dbg, remove_unit ifso,
        ifnot_dbg,
        remove_unit ifnot, dbg)
  | Cswitch(sel, index, cases, dbg) ->
      Cswitch(sel, index,
        Array.map (fun (case, dbg) -> remove_unit case, dbg) cases,
        dbg)
  | Ccatch(rec_flag, handlers, body) ->
      map_ccatch remove_unit rec_flag handlers body
  | Ctrywith(body, exn, handler, dbg) ->
      Ctrywith(remove_unit body, exn, remove_unit handler, dbg)
  | Clet(id, c1, c2) ->
      Clet(id, c1, remove_unit c2)
  | Cop(Capply _mty, args, dbg) ->
      Cop(Capply typ_void, args, dbg)
  | Cop(Cextcall(proc, _mty, alloc, label_after), args, dbg) ->
      Cop(Cextcall(proc, typ_void, alloc, label_after), args, dbg)
  | Cexit (_,_) as c -> c
  | Ctuple [] as c -> c
  | c -> Csequence(c, Ctuple [])

(* Access to block fields *)

let field_address ptr n dbg =
  if n = 0
  then ptr
  else Cop(Cadda, [ptr; Cconst_int(n * size_addr, dbg)], dbg)

let get_field env ptr n dbg =
  let mut =
    match env.environment_param with
    | None -> Mutable
    | Some environment_param ->
      match ptr with
      | Cvar ptr ->
        (* Loads from the current function's closure are immutable. *)
        if V.same environment_param ptr then Immutable
        else Mutable
      | _ -> Mutable
  in
  Cop(Cload (Word_val, mut), [field_address ptr n dbg], dbg)

let set_field ptr n newval init dbg =
  Cop(Cstore (Word_val, init), [field_address ptr n dbg; newval], dbg)

let non_profinfo_mask =
  if Config.profinfo
  then (1 lsl (64 - Config.profinfo_width)) - 1
  else 0 (* [non_profinfo_mask] is unused in this case *)

let get_header ptr dbg =
  (* We cannot deem this as [Immutable] due to the presence of [Obj.truncate]
     and [Obj.set_tag]. *)
  Cop(Cload (Word_int, Mutable),
    [Cop(Cadda, [ptr; Cconst_int(-size_int, dbg)], dbg)], dbg)

let get_header_without_profinfo ptr dbg =
  if Config.profinfo then
    Cop(Cand, [get_header ptr dbg; Cconst_int (non_profinfo_mask, dbg)], dbg)
  else
    get_header ptr dbg

let tag_offset =
  if big_endian then -1 else -size_int

let get_tag ptr dbg =
  if Proc.word_addressed then           (* If byte loads are slow *)
    Cop(Cand, [get_header ptr dbg; Cconst_int (255, dbg)], dbg)
  else                                  (* If byte loads are efficient *)
    Cop(Cload (Byte_unsigned, Mutable), (* Same comment as [get_header] above *)
        [Cop(Cadda, [ptr; Cconst_int(tag_offset, dbg)], dbg)], dbg)

let get_size ptr dbg =
  Cop(Clsr, [get_header_without_profinfo ptr dbg; Cconst_int (10, dbg)], dbg)

(* Array indexing *)

let log2_size_addr = Misc.log2 size_addr
let log2_size_float = Misc.log2 size_float

let wordsize_shift = 9
let numfloat_shift = 9 + log2_size_float - log2_size_addr

let is_addr_array_hdr hdr dbg =
  Cop(Ccmpi Cne,
    [Cop(Cand, [hdr; Cconst_int (255, dbg)], dbg); floatarray_tag dbg],
    dbg)

let is_addr_array_ptr ptr dbg =
  Cop(Ccmpi Cne, [get_tag ptr dbg; floatarray_tag dbg], dbg)

let addr_array_length hdr dbg =
  Cop(Clsr, [hdr; Cconst_int (wordsize_shift, dbg)], dbg)
let float_array_length hdr dbg =
  Cop(Clsr, [hdr; Cconst_int (numfloat_shift, dbg)], dbg)

let lsl_const c n dbg =
  if n = 0 then c
  else Cop(Clsl, [c; Cconst_int (n, dbg)], dbg)

(* Produces a pointer to the element of the array [ptr] on the position [ofs]
   with the given element [log2size] log2 element size. [ofs] is given as a
   tagged int expression.
   The optional ?typ argument is the C-- type of the result.
   By default, it is Addr, meaning we are constructing a derived pointer
   into the heap.  If we know the pointer is outside the heap
   (this is the case for bigarray indexing), we give type Int instead. *)

let array_indexing ?typ log2size ptr ofs dbg =
  let add =
    match typ with
    | None | Some Addr -> Cadda
    | Some Int -> Caddi
    | _ -> assert false in
  match ofs with
  | Cconst_int (n, _) ->
      let i = n asr 1 in
      if i = 0 then ptr
      else Cop(add, [ptr; Cconst_int(i lsl log2size, dbg)], dbg)
  | Cop(Caddi,
        [Cop(Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], dbg') ->
      Cop(add, [ptr; lsl_const c log2size dbg], dbg')
  | Cop(Caddi, [c; Cconst_int (n, _)], dbg') when log2size = 0 ->
      Cop(add,
        [Cop(add, [ptr; untag_int c dbg], dbg); Cconst_int (n asr 1, dbg)],
        dbg')
  | Cop(Caddi, [c; Cconst_int (n, _)], _) ->
      Cop(add, [Cop(add, [ptr; lsl_const c (log2size - 1) dbg], dbg);
                    Cconst_int((n-1) lsl (log2size - 1), dbg)], dbg)
  | _ when log2size = 0 ->
      Cop(add, [ptr; untag_int ofs dbg], dbg)
  | _ ->
      Cop(add, [Cop(add, [ptr; lsl_const ofs (log2size - 1) dbg], dbg);
                    Cconst_int((-1) lsl (log2size - 1), dbg)], dbg)

let addr_array_ref arr ofs dbg =
  Cop(Cload (Word_val, Mutable),
    [array_indexing log2_size_addr arr ofs dbg], dbg)
let int_array_ref arr ofs dbg =
  Cop(Cload (Word_int, Mutable),
    [array_indexing log2_size_addr arr ofs dbg], dbg)
let unboxed_float_array_ref arr ofs dbg =
  Cop(Cload (Double_u, Mutable),
    [array_indexing log2_size_float arr ofs dbg], dbg)
let float_array_ref dbg arr ofs =
  box_float dbg (unboxed_float_array_ref arr ofs dbg)

let addr_array_set arr ofs newval dbg =
  Cop(Cextcall("caml_modify", typ_void, false, None),
      [array_indexing log2_size_addr arr ofs dbg; newval], dbg)
let addr_array_initialize arr ofs newval dbg =
  Cop(Cextcall("caml_initialize", typ_void, false, None),
      [array_indexing log2_size_addr arr ofs dbg; newval], dbg)
let int_array_set arr ofs newval dbg =
  Cop(Cstore (Word_int, Assignment),
    [array_indexing log2_size_addr arr ofs dbg; newval], dbg)
let float_array_set arr ofs newval dbg =
  Cop(Cstore (Double_u, Assignment),
    [array_indexing log2_size_float arr ofs dbg; newval], dbg)

(* String length *)

(* Length of string block *)

let string_length exp dbg =
  bind "str" exp (fun str ->
    let tmp_var = V.create_local "*tmp*" in
    Clet(VP.create tmp_var,
         Cop(Csubi,
             [Cop(Clsl,
                   [get_size str dbg;
                     Cconst_int (log2_size_addr, dbg)],
                   dbg);
              Cconst_int (1, dbg)],
             dbg),
         Cop(Csubi,
             [Cvar tmp_var;
               Cop(Cload (Byte_unsigned, Mutable),
                     [Cop(Cadda, [str; Cvar tmp_var], dbg)], dbg)], dbg)))

let bigstring_length ba dbg =
  Cop(Cload (Word_int, Mutable), [field_address ba 5 dbg], dbg)

(* Message sending *)

let lookup_tag obj tag dbg =
  bind "tag" tag (fun tag ->
    Cop(Cextcall("caml_get_public_method", typ_val, false, None),
        [obj; tag],
        dbg))

let lookup_label obj lab dbg =
  bind "lab" lab (fun lab ->
    let table = Cop (Cload (Word_val, Mutable), [obj], dbg) in
    addr_array_ref table lab dbg)

let call_cached_method obj tag cache pos args dbg =
  let arity = List.length args in
  let cache = array_indexing log2_size_addr cache pos dbg in
  Compilenv.need_send_fun arity;
  Cop(Capply typ_val,
      Cconst_symbol("caml_send" ^ Int.to_string arity, dbg) ::
        obj :: tag :: cache :: args,
      dbg)

(* Allocation *)

let make_alloc_generic set_fn dbg tag wordsize args =
  if wordsize <= Config.max_young_wosize then
    Cop(Calloc, Cblockheader(block_header tag wordsize, dbg) :: args, dbg)
  else begin
    let id = V.create_local "*alloc*" in
    let rec fill_fields idx = function
      [] -> Cvar id
    | e1::el -> Csequence(set_fn (Cvar id) (Cconst_int (idx, dbg)) e1 dbg,
                          fill_fields (idx + 2) el) in
    Clet(VP.create id,
         Cop(Cextcall("caml_alloc", typ_val, true, None),
                 [Cconst_int (wordsize, dbg); Cconst_int (tag, dbg)], dbg),
         fill_fields 1 args)
  end

let make_alloc dbg tag args =
  let addr_array_init arr ofs newval dbg =
    Cop(Cextcall("caml_initialize", typ_void, false, None),
        [array_indexing log2_size_addr arr ofs dbg; newval], dbg)
  in
  make_alloc_generic addr_array_init dbg tag (List.length args) args

let make_float_alloc dbg tag args =
  make_alloc_generic float_array_set dbg tag
                     (List.length args * size_float / size_addr) args

(* Bounds checking *)

let make_checkbound dbg = function
  | [Cop(Clsr, [a1; Cconst_int (n, _)], _); Cconst_int (m, _)]
    when (m lsl n) > n ->
      Cop(Ccheckbound, [a1; Cconst_int(m lsl n + 1 lsl n - 1, dbg)], dbg)
  | args ->
      Cop(Ccheckbound, args, dbg)

(* To compile "let rec" over values *)

let fundecls_size fundecls =
  let sz = ref (-1) in
  List.iter
    (fun f ->
       let indirect_call_code_pointer_size =
         match f.arity with
         | 0 | 1 -> 0
           (* arity 1 does not need an indirect call handler.
              arity 0 cannot be indirect called *)
         | _ -> 1
           (* For other arities there is an indirect call handler.
              if arity >= 2 it is caml_curry...
              if arity < 0 it is caml_tuplify... *)
       in
       sz := !sz + 1 + 2 + indirect_call_code_pointer_size)
    fundecls;
  !sz

type rhs_kind =
  | RHS_block of int
  | RHS_infix of { blocksize : int; offset : int }
  | RHS_floatblock of int
  | RHS_nonrec
;;
let rec expr_size env = function
  | Uvar id ->
      begin try V.find_same id env with Not_found -> RHS_nonrec end
  | Uclosure(fundecls, clos_vars) ->
      RHS_block (fundecls_size fundecls + List.length clos_vars)
  | Ulet(_str, _kind, id, exp, body) ->
      expr_size (V.add (VP.var id) (expr_size env exp) env) body
  | Uletrec(bindings, body) ->
      let env =
        List.fold_right
          (fun (id, exp) env -> V.add (VP.var id) (expr_size env exp) env)
          bindings env
      in
      expr_size env body
  | Uprim(Pmakeblock _, args, _) ->
      RHS_block (List.length args)
  | Uprim(Pmakearray((Paddrarray | Pintarray), _), args, _) ->
      RHS_block (List.length args)
  | Uprim(Pmakearray(Pfloatarray, _), args, _) ->
      RHS_floatblock (List.length args)
  | Uprim(Pmakearray(Pgenarray, _), _, _) ->
     (* Pgenarray is excluded from recursive bindings by the
        check in Translcore.check_recursive_lambda *)
     RHS_nonrec
  | Uprim (Pduprecord ((Record_regular | Record_inlined _), sz), _, _) ->
      RHS_block sz
  | Uprim (Pduprecord (Record_unboxed _, _), _, _) ->
      assert false
  | Uprim (Pduprecord (Record_extension _, sz), _, _) ->
      RHS_block (sz + 1)
  | Uprim (Pduprecord (Record_float, sz), _, _) ->
      RHS_floatblock sz
  | Uprim (Pccall { prim_name; _ }, closure::_, _)
        when prim_name = "caml_check_value_is_closure" ->
      (* Used for "-clambda-checks". *)
      expr_size env closure
  | Usequence(_exp, exp') ->
      expr_size env exp'
  | Uoffset (exp, offset) ->
      (match expr_size env exp with
      | RHS_block blocksize -> RHS_infix { blocksize; offset }
      | RHS_nonrec -> RHS_nonrec
      | _ -> assert false)
  | _ -> RHS_nonrec

(* Record application and currying functions *)

let apply_function n =
  Compilenv.need_apply_fun n; "caml_apply" ^ Int.to_string n
let curry_function n =
  Compilenv.need_curry_fun n;
  if n >= 0
  then "caml_curry" ^ Int.to_string n
  else "caml_tuplify" ^ Int.to_string (-n)

(* Comparisons *)

let transl_int_comparison cmp = cmp

let transl_float_comparison cmp = cmp

(* Translate structured constants to Cmm data items *)

let cdata items = Cdata { section = None; items; }

let transl_constant dbg = function
  | Uconst_int n ->
      int_const dbg n
  | Uconst_ptr n ->
      if n <= max_repr_int && n >= min_repr_int
      then Cconst_pointer((n lsl 1) + 1, dbg)
      else Cconst_natpointer
              (Nativeint.add (Nativeint.shift_left (Nativeint.of_int n) 1) 1n,
               dbg)
  | Uconst_ref (label, _) ->
      Cconst_symbol (label, dbg)

let cdefine_symbol (symb, (global : Cmmgen_state.is_global)) =
  match global with
  | Global -> [Cglobal_symbol symb; Cdefine_symbol symb]
  | Local -> [Cdefine_symbol symb]

let emit_block symb is_global white_header cont =
  (* Headers for structured constants must be marked black in case we
     are in no-naked-pointers mode.  See [caml_darken]. *)
  let black_header = Nativeint.logor white_header caml_black in
  Cint black_header :: cdefine_symbol (symb, is_global) @ cont

let rec emit_structured_constant (sym, is_global) cst cont =
  match cst with
  | Uconst_float s ->
      emit_block sym is_global float_header (Cdouble s :: cont)
  | Uconst_string s ->
      emit_block sym is_global (string_header (String.length s))
        (emit_string_constant s cont)
  | Uconst_int32 n ->
      emit_block sym is_global boxedint32_header
        (emit_boxed_int32_constant n cont)
  | Uconst_int64 n ->
      emit_block sym is_global boxedint64_header
        (emit_boxed_int64_constant n cont)
  | Uconst_nativeint n ->
      emit_block sym is_global boxedintnat_header
        (emit_boxed_nativeint_constant n cont)
  | Uconst_block (tag, csts) ->
      let cont = List.fold_right emit_constant csts cont in
      emit_block sym is_global (block_header tag (List.length csts)) cont
  | Uconst_float_array fields ->
      emit_block sym is_global (floatarray_header (List.length fields))
        (Misc.map_end (fun f -> Cdouble f) fields cont)
  | Uconst_closure(fundecls, lbl, fv) ->
      Cmmgen_state.add_constant lbl (Const_closure (is_global, fundecls, fv));
      List.iter (fun f -> Cmmgen_state.add_function f) fundecls;
      cont

and emit_constant cst cont =
  match cst with
  | Uconst_int n | Uconst_ptr n ->
      cint_const n
      :: cont
  | Uconst_ref (sym, _) ->
      Csymbol_address sym :: cont

and emit_string_constant s cont =
  let n = size_int - 1 - (String.length s) mod size_int in
  Cstring s :: Cskip n :: Cint8 n :: cont

and emit_boxed_int32_constant n cont =
  let n = Nativeint.of_int32 n in
  if size_int = 8 then
    Csymbol_address caml_int32_ops :: Cint32 n :: Cint32 0n :: cont
  else
    Csymbol_address caml_int32_ops :: Cint n :: cont

and emit_boxed_nativeint_constant n cont =
  Csymbol_address caml_nativeint_ops :: Cint n :: cont

and emit_boxed_int64_constant n cont =
  let lo = Int64.to_nativeint n in
  if size_int = 8 then
    Csymbol_address caml_int64_ops :: Cint lo :: cont
  else begin
    let hi = Int64.to_nativeint (Int64.shift_right n 32) in
    if big_endian then
      Csymbol_address caml_int64_ops :: Cint hi :: Cint lo :: cont
    else
      Csymbol_address caml_int64_ops :: Cint lo :: Cint hi :: cont
  end

(* Boxed integers *)

let box_int_constant sym bi n =
  match bi with
    Pnativeint ->
      emit_block sym Local boxedintnat_header
        (emit_boxed_nativeint_constant n [])
  | Pint32 ->
      let n = Nativeint.to_int32 n in
      emit_block sym Local boxedint32_header
        (emit_boxed_int32_constant n [])
  | Pint64 ->
      let n = Int64.of_nativeint n in
      emit_block sym Local boxedint64_header
        (emit_boxed_int64_constant n [])

let operations_boxed_int bi =
  match bi with
    Pnativeint -> caml_nativeint_ops
  | Pint32 -> caml_int32_ops
  | Pint64 -> caml_int64_ops

let alloc_header_boxed_int bi =
  match bi with
    Pnativeint -> alloc_boxedintnat_header
  | Pint32 -> alloc_boxedint32_header
  | Pint64 -> alloc_boxedint64_header

let box_int dbg bi arg =
  match arg with
  | Cconst_int (n, _) ->
      let sym = Compilenv.new_const_symbol () in
      let data_items = box_int_constant sym bi (Nativeint.of_int n) in
      Cmmgen_state.add_data_items data_items;
      Cconst_symbol (sym, dbg)
  | Cconst_natint (n, _) ->
      let sym = Compilenv.new_const_symbol () in
      let data_items = box_int_constant sym bi n in
      Cmmgen_state.add_data_items data_items;
      Cconst_symbol (sym, dbg)
  | _ ->
      let arg' =
        if bi = Pint32 && size_int = 8 && big_endian
        then Cop(Clsl, [arg; Cconst_int (32, dbg)], dbg)
        else arg in
      Cop(Calloc, [alloc_header_boxed_int bi dbg;
                   Cconst_symbol(operations_boxed_int bi, dbg);
                   arg'], dbg)

let split_int64_for_32bit_target arg dbg =
  bind "split_int64" arg (fun arg ->
    let first = Cop (Cadda, [Cconst_int (size_int, dbg); arg], dbg) in
    let second = Cop (Cadda, [Cconst_int (2 * size_int, dbg); arg], dbg) in
    Ctuple [Cop (Cload (Thirtytwo_unsigned, Mutable), [first], dbg);
            Cop (Cload (Thirtytwo_unsigned, Mutable), [second], dbg)])

let alloc_matches_boxed_int bi ~hdr ~ops =
  match bi, hdr, ops with
  | Pnativeint, Cblockheader (hdr, _dbg), Cconst_symbol (sym, _) ->
      Nativeint.equal hdr boxedintnat_header
        && String.equal sym caml_nativeint_ops
  | Pint32, Cblockheader (hdr, _dbg), Cconst_symbol (sym, _) ->
      Nativeint.equal hdr boxedint32_header
        && String.equal sym caml_int32_ops
  | Pint64, Cblockheader (hdr, _dbg), Cconst_symbol (sym, _) ->
      Nativeint.equal hdr boxedint64_header
        && String.equal sym caml_int64_ops
  | (Pnativeint | Pint32 | Pint64), _, _ -> false

let rec unbox_int bi arg dbg =
  match arg with
    Cop(Calloc, [hdr; ops; Cop(Clsl, [contents; Cconst_int (32, _)], dbg')],
      _dbg)
    when bi = Pint32 && size_int = 8 && big_endian
      && alloc_matches_boxed_int bi ~hdr ~ops ->
      (* Force sign-extension of low 32 bits *)
      Cop(Casr, [Cop(Clsl, [contents; Cconst_int (32, dbg)], dbg');
        Cconst_int (32, dbg)],
        dbg)
  | Cop(Calloc, [hdr; ops; contents], _dbg)
    when bi = Pint32 && size_int = 8 && not big_endian
      && alloc_matches_boxed_int bi ~hdr ~ops ->
      (* Force sign-extension of low 32 bits *)
      Cop(Casr, [Cop(Clsl, [contents; Cconst_int (32, dbg)], dbg);
        Cconst_int (32, dbg)],
        dbg)
  | Cop(Calloc, [hdr; ops; contents], _dbg)
    when alloc_matches_boxed_int bi ~hdr ~ops ->
      contents
  | Clet(id, exp, body) -> Clet(id, exp, unbox_int bi body dbg)
  | Cifthenelse(cond, ifso_dbg, e1, ifnot_dbg, e2, dbg) ->
      Cifthenelse(cond,
        ifso_dbg, unbox_int bi e1 ifso_dbg,
        ifnot_dbg, unbox_int bi e2 ifnot_dbg,
        dbg)
  | Csequence(e1, e2) -> Csequence(e1, unbox_int bi e2 dbg)
  | Cswitch(e, tbl, el, dbg') ->
      Cswitch(e, tbl,
        Array.map (fun (e, dbg) -> unbox_int bi e dbg, dbg) el,
        dbg')
  | Ccatch(rec_flag, handlers, body) ->
      map_ccatch (fun e -> unbox_int bi e dbg) rec_flag handlers body
  | Ctrywith(e1, id, e2, handler_dbg) ->
      Ctrywith(unbox_int bi e1 dbg, id,
        unbox_int bi e2 handler_dbg, handler_dbg)
  | _ ->
      if size_int = 4 && bi = Pint64 then
        split_int64_for_32bit_target arg dbg
      else
        Cop(
          Cload((if bi = Pint32 then Thirtytwo_signed else Word_int), Mutable),
          [Cop(Cadda, [arg; Cconst_int (size_addr, dbg)], dbg)], dbg)

let make_unsigned_int bi arg dbg =
  if bi = Pint32 && size_int = 8
  then Cop(Cand, [arg; Cconst_natint (0xFFFFFFFFn, dbg)], dbg)
  else arg

(* Boxed numbers *)

let equal_unboxed_integer ui1 ui2 =
  match ui1, ui2 with
  | Pnativeint, Pnativeint -> true
  | Pint32, Pint32 -> true
  | Pint64, Pint64 -> true
  | _, _ -> false

let equal_boxed_number bn1 bn2 =
  match bn1, bn2 with
  | Boxed_float _, Boxed_float _ -> true
  | Boxed_integer(ui1, _), Boxed_integer(ui2, _) ->
    equal_unboxed_integer ui1 ui2
  | _, _ -> false

let box_number bn arg =
  match bn with
  | Boxed_float dbg -> box_float dbg arg
  | Boxed_integer (bi, dbg) -> box_int dbg bi arg

(* Big arrays *)

let bigarray_elt_size = function
    Pbigarray_unknown -> assert false
  | Pbigarray_float32 -> 4
  | Pbigarray_float64 -> 8
  | Pbigarray_sint8 -> 1
  | Pbigarray_uint8 -> 1
  | Pbigarray_sint16 -> 2
  | Pbigarray_uint16 -> 2
  | Pbigarray_int32 -> 4
  | Pbigarray_int64 -> 8
  | Pbigarray_caml_int -> size_int
  | Pbigarray_native_int -> size_int
  | Pbigarray_complex32 -> 8
  | Pbigarray_complex64 -> 16

(* Produces a pointer to the element of the bigarray [b] on the position
   [args].  [args] is given as a list of tagged int expressions, one per array
   dimension. *)
let bigarray_indexing unsafe elt_kind layout b args dbg =
  let check_ba_bound bound idx v =
    Csequence(make_checkbound dbg [bound;idx], v) in
  (* Validates the given multidimensional offset against the array bounds and
     transforms it into a one dimensional offset.  The offsets are expressions
     evaluating to tagged int. *)
  let rec ba_indexing dim_ofs delta_ofs = function
    [] -> assert false
  | [arg] ->
      if unsafe then arg
      else
        bind "idx" arg (fun idx ->
          (* Load the untagged int bound for the given dimension *)
          let bound =
            Cop(Cload (Word_int, Mutable),[field_address b dim_ofs dbg], dbg)
          in
          let idxn = untag_int idx dbg in
          check_ba_bound bound idxn idx)
  | arg1 :: argl ->
      (* The remainder of the list is transformed into a one dimensional offset
         *)
      let rem = ba_indexing (dim_ofs + delta_ofs) delta_ofs argl in
      (* Load the untagged int bound for the given dimension *)
      let bound =
        Cop(Cload (Word_int, Mutable), [field_address b dim_ofs dbg], dbg)
      in
      if unsafe then add_int (mul_int (decr_int rem dbg) bound dbg) arg1 dbg
      else
        bind "idx" arg1 (fun idx ->
          bind "bound" bound (fun bound ->
            let idxn = untag_int idx dbg in
            (* [offset = rem * (tag_int bound) + idx] *)
            let offset =
              add_int (mul_int (decr_int rem dbg) bound dbg) idx dbg
            in
            check_ba_bound bound idxn offset)) in
  (* The offset as an expression evaluating to int *)
  let offset =
    match layout with
      Pbigarray_unknown_layout ->
        assert false
    | Pbigarray_c_layout ->
        ba_indexing (4 + List.length args) (-1) (List.rev args)
    | Pbigarray_fortran_layout ->
        ba_indexing 5 1
          (List.map (fun idx -> sub_int idx (Cconst_int (2, dbg)) dbg) args)
  and elt_size =
    bigarray_elt_size elt_kind in
  (* [array_indexing] can simplify the given expressions *)
  array_indexing ~typ:Addr (log2 elt_size)
                 (Cop(Cload (Word_int, Mutable),
                    [field_address b 1 dbg], dbg)) offset dbg

let bigarray_word_kind = function
    Pbigarray_unknown -> assert false
  | Pbigarray_float32 -> Single
  | Pbigarray_float64 -> Double
  | Pbigarray_sint8 -> Byte_signed
  | Pbigarray_uint8 -> Byte_unsigned
  | Pbigarray_sint16 -> Sixteen_signed
  | Pbigarray_uint16 -> Sixteen_unsigned
  | Pbigarray_int32 -> Thirtytwo_signed
  | Pbigarray_int64 -> Word_int
  | Pbigarray_caml_int -> Word_int
  | Pbigarray_native_int -> Word_int
  | Pbigarray_complex32 -> Single
  | Pbigarray_complex64 -> Double

let bigarray_get unsafe elt_kind layout b args dbg =
  bind "ba" b (fun b ->
    match elt_kind with
      Pbigarray_complex32 | Pbigarray_complex64 ->
        let kind = bigarray_word_kind elt_kind in
        let sz = bigarray_elt_size elt_kind / 2 in
        bind "addr"
          (bigarray_indexing unsafe elt_kind layout b args dbg) (fun addr ->
            bind "reval"
              (Cop(Cload (kind, Mutable), [addr], dbg)) (fun reval ->
                bind "imval"
                  (Cop(Cload (kind, Mutable),
                       [Cop(Cadda, [addr; Cconst_int (sz, dbg)], dbg)], dbg))
                  (fun imval -> box_complex dbg reval imval)))
    | _ ->
        Cop(Cload (bigarray_word_kind elt_kind, Mutable),
            [bigarray_indexing unsafe elt_kind layout b args dbg],
            dbg))

let bigarray_set unsafe elt_kind layout b args newval dbg =
  bind "ba" b (fun b ->
    match elt_kind with
      Pbigarray_complex32 | Pbigarray_complex64 ->
        let kind = bigarray_word_kind elt_kind in
        let sz = bigarray_elt_size elt_kind / 2 in
        bind "newval" newval (fun newv ->
        bind "addr" (bigarray_indexing unsafe elt_kind layout b args dbg)
          (fun addr ->
          Csequence(
            Cop(Cstore (kind, Assignment), [addr; complex_re newv dbg], dbg),
            Cop(Cstore (kind, Assignment),
                [Cop(Cadda, [addr; Cconst_int (sz, dbg)], dbg);
                 complex_im newv dbg],
                dbg))))
    | _ ->
        Cop(Cstore (bigarray_word_kind elt_kind, Assignment),
            [bigarray_indexing unsafe elt_kind layout b args dbg; newval],
            dbg))

let unaligned_load_16 ptr idx dbg =
  if Arch.allow_unaligned_access
  then Cop(Cload (Sixteen_unsigned, Mutable), [add_int ptr idx dbg], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 = Cop(Cload (Byte_unsigned, Mutable), [add_int ptr idx dbg], dbg) in
    let v2 = Cop(Cload (Byte_unsigned, Mutable),
                 [add_int (add_int ptr idx dbg) (cconst_int 1) dbg], dbg)
    in
    let b1, b2 = if Arch.big_endian then v1, v2 else v2, v1 in
    Cop(Cor, [lsl_int b1 (cconst_int 8) dbg; b2], dbg)

let unaligned_set_16 ptr idx newval dbg =
  if Arch.allow_unaligned_access
  then
    Cop(Cstore (Sixteen_unsigned, Assignment),
      [add_int ptr idx dbg; newval], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int 8], dbg);
        cconst_int 0xFF], dbg)
    in
    let v2 = Cop(Cand, [newval; cconst_int 0xFF], dbg) in
    let b1, b2 = if Arch.big_endian then v1, v2 else v2, v1 in
    Csequence(
        Cop(Cstore (Byte_unsigned, Assignment), [add_int ptr idx dbg; b1], dbg),
        Cop(Cstore (Byte_unsigned, Assignment),
            [add_int (add_int ptr idx dbg) (cconst_int 1) dbg; b2], dbg))

let unaligned_load_32 ptr idx dbg =
  if Arch.allow_unaligned_access
  then Cop(Cload (Thirtytwo_unsigned, Mutable), [add_int ptr idx dbg], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 = Cop(Cload (Byte_unsigned, Mutable), [add_int ptr idx dbg], dbg) in
    let v2 = Cop(Cload (Byte_unsigned, Mutable),
                 [add_int (add_int ptr idx dbg) (cconst_int 1) dbg], dbg)
    in
    let v3 = Cop(Cload (Byte_unsigned, Mutable),
                 [add_int (add_int ptr idx dbg) (cconst_int 2) dbg], dbg)
    in
    let v4 = Cop(Cload (Byte_unsigned, Mutable),
                 [add_int (add_int ptr idx dbg) (cconst_int 3) dbg], dbg)
    in
    let b1, b2, b3, b4 =
      if Arch.big_endian
      then v1, v2, v3, v4
      else v4, v3, v2, v1 in
    Cop(Cor,
      [Cop(Cor, [lsl_int b1 (cconst_int 24) dbg;
         lsl_int b2 (cconst_int 16) dbg], dbg);
       Cop(Cor, [lsl_int b3 (cconst_int 8) dbg; b4], dbg)],
      dbg)

let unaligned_set_32 ptr idx newval dbg =
  if Arch.allow_unaligned_access
  then
    Cop(Cstore (Thirtytwo_unsigned, Assignment), [add_int ptr idx dbg; newval],
      dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int 24], dbg); cconst_int 0xFF], dbg)
    in
    let v2 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int 16], dbg); cconst_int 0xFF], dbg)
    in
    let v3 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int 8], dbg); cconst_int 0xFF], dbg)
    in
    let v4 = Cop(Cand, [newval; cconst_int 0xFF], dbg) in
    let b1, b2, b3, b4 =
      if Arch.big_endian
      then v1, v2, v3, v4
      else v4, v3, v2, v1 in
    Csequence(
        Csequence(
            Cop(Cstore (Byte_unsigned, Assignment),
                [add_int ptr idx dbg; b1], dbg),
            Cop(Cstore (Byte_unsigned, Assignment),
                [add_int (add_int ptr idx dbg) (cconst_int 1) dbg; b2],
                dbg)),
        Csequence(
            Cop(Cstore (Byte_unsigned, Assignment),
                [add_int (add_int ptr idx dbg) (cconst_int 2) dbg; b3],
                dbg),
            Cop(Cstore (Byte_unsigned, Assignment),
                [add_int (add_int ptr idx dbg) (cconst_int 3) dbg; b4],
                dbg)))

let unaligned_load_64 ptr idx dbg =
  assert(size_int = 8);
  if Arch.allow_unaligned_access
  then Cop(Cload (Word_int, Mutable), [add_int ptr idx dbg], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 = Cop(Cload (Byte_unsigned, Mutable), [add_int ptr idx dbg], dbg) in
    let v2 = Cop(Cload (Byte_unsigned, Mutable),
                 [add_int (add_int ptr idx dbg) (cconst_int 1) dbg], dbg)
    in
    let v3 = Cop(Cload (Byte_unsigned, Mutable),
                 [add_int (add_int ptr idx dbg) (cconst_int 2) dbg], dbg)
    in
    let v4 = Cop(Cload (Byte_unsigned, Mutable),
                 [add_int (add_int ptr idx dbg) (cconst_int 3) dbg], dbg)
    in
    let v5 = Cop(Cload (Byte_unsigned, Mutable),
                 [add_int (add_int ptr idx dbg) (cconst_int 4) dbg], dbg)
    in
    let v6 = Cop(Cload (Byte_unsigned, Mutable),
                 [add_int (add_int ptr idx dbg) (cconst_int 5) dbg], dbg)
    in
    let v7 = Cop(Cload (Byte_unsigned, Mutable),
                 [add_int (add_int ptr idx dbg) (cconst_int 6) dbg], dbg)
    in
    let v8 = Cop(Cload (Byte_unsigned, Mutable),
                 [add_int (add_int ptr idx dbg) (cconst_int 7) dbg], dbg)
    in
    let b1, b2, b3, b4, b5, b6, b7, b8 =
      if Arch.big_endian
      then v1, v2, v3, v4, v5, v6, v7, v8
      else v8, v7, v6, v5, v4, v3, v2, v1 in
    Cop(Cor,
        [Cop(Cor,
             [Cop(Cor, [lsl_int b1 (cconst_int (8*7)) dbg;
                        lsl_int b2 (cconst_int (8*6)) dbg], dbg);
              Cop(Cor, [lsl_int b3 (cconst_int (8*5)) dbg;
                        lsl_int b4 (cconst_int (8*4)) dbg], dbg)],
             dbg);
         Cop(Cor,
             [Cop(Cor, [lsl_int b5 (cconst_int (8*3)) dbg;
                        lsl_int b6 (cconst_int (8*2)) dbg], dbg);
              Cop(Cor, [lsl_int b7 (cconst_int 8) dbg;
                        b8], dbg)],
             dbg)], dbg)

let unaligned_set_64 ptr idx newval dbg =
  assert(size_int = 8);
  if Arch.allow_unaligned_access
  then Cop(Cstore (Word_int, Assignment), [add_int ptr idx dbg; newval], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int (8*7)], dbg); cconst_int 0xFF],
        dbg)
    in
    let v2 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int (8*6)], dbg); cconst_int 0xFF],
        dbg)
    in
    let v3 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int (8*5)], dbg); cconst_int 0xFF],
        dbg)
    in
    let v4 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int (8*4)], dbg); cconst_int 0xFF],
        dbg)
    in
    let v5 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int (8*3)], dbg); cconst_int 0xFF],
        dbg)
    in
    let v6 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int (8*2)], dbg); cconst_int 0xFF],
        dbg)
    in
    let v7 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int 8], dbg); cconst_int 0xFF],
        dbg)
    in
    let v8 = Cop(Cand, [newval; cconst_int 0xFF], dbg) in
    let b1, b2, b3, b4, b5, b6, b7, b8 =
      if Arch.big_endian
      then v1, v2, v3, v4, v5, v6, v7, v8
      else v8, v7, v6, v5, v4, v3, v2, v1 in
    Csequence(
        Csequence(
            Csequence(
                Cop(Cstore (Byte_unsigned, Assignment),
                    [add_int ptr idx dbg; b1],
                    dbg),
                Cop(Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 1) dbg; b2],
                    dbg)),
            Csequence(
                Cop(Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 2) dbg; b3],
                    dbg),
                Cop(Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 3) dbg; b4],
                    dbg))),
        Csequence(
            Csequence(
                Cop(Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 4) dbg; b5],
                    dbg),
                Cop(Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 5) dbg; b6],
                    dbg)),
            Csequence(
                Cop(Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 6) dbg; b7],
                    dbg),
                Cop(Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 7) dbg; b8],
                    dbg))))

let max_or_zero a dbg =
  bind "size" a (fun a ->
    (* equivalent to
       Cifthenelse(Cop(Ccmpi Cle, [a; cconst_int 0]), cconst_int 0, a)

       if a is positive, sign is 0 hence sign_negation is full of 1
                         so sign_negation&a = a
       if a is negative, sign is full of 1 hence sign_negation is 0
                         so sign_negation&a = 0 *)
    let sign = Cop(Casr, [a; Cconst_int (size_int * 8 - 1, dbg)], dbg) in
    let sign_negation = Cop(Cxor, [sign; Cconst_int (-1, dbg)], dbg) in
    Cop(Cand, [sign_negation; a], dbg))

let check_bound safety access_size dbg length a2 k =
  match safety with
  | Unsafe -> k
  | Safe ->
      let offset =
        match access_size with
        | Sixteen -> 1
        | Thirty_two -> 3
        | Sixty_four -> 7
      in
      let a1 =
        sub_int length (Cconst_int (offset, dbg)) dbg
      in
      Csequence(make_checkbound dbg [max_or_zero a1 dbg; a2], k)

let unaligned_set size ptr idx newval dbg =
  match size with
  | Sixteen -> unaligned_set_16 ptr idx newval dbg
  | Thirty_two -> unaligned_set_32 ptr idx newval dbg
  | Sixty_four -> unaligned_set_64 ptr idx newval dbg

let unaligned_load size ptr idx dbg =
  match size with
  | Sixteen -> unaligned_load_16 ptr idx dbg
  | Thirty_two -> unaligned_load_32 ptr idx dbg
  | Sixty_four -> unaligned_load_64 ptr idx dbg

let box_sized size dbg exp =
  match size with
  | Sixteen -> tag_int exp dbg
  | Thirty_two -> box_int dbg Pint32 exp
  | Sixty_four -> box_int dbg Pint64 exp

(* Simplification of some primitives into C calls *)

let default_prim name =
  Primitive.simple ~name ~arity:0(*ignored*) ~alloc:true

let int64_native_prim name arity ~alloc =
  let u64 = Unboxed_integer Pint64 in
  let rec make_args = function 0 -> [] | n -> u64 :: make_args (n - 1) in
  Primitive.make ~name ~native_name:(name ^ "_native")
    ~alloc
    ~native_repr_args:(make_args arity)
    ~native_repr_res:u64

let simplif_primitive_32bits = function
    Pbintofint Pint64 -> Pccall (default_prim "caml_int64_of_int")
  | Pintofbint Pint64 -> Pccall (default_prim "caml_int64_to_int")
  | Pcvtbint(Pint32, Pint64) -> Pccall (default_prim "caml_int64_of_int32")
  | Pcvtbint(Pint64, Pint32) -> Pccall (default_prim "caml_int64_to_int32")
  | Pcvtbint(Pnativeint, Pint64) ->
      Pccall (default_prim "caml_int64_of_nativeint")
  | Pcvtbint(Pint64, Pnativeint) ->
      Pccall (default_prim "caml_int64_to_nativeint")
  | Pnegbint Pint64 -> Pccall (int64_native_prim "caml_int64_neg" 1
                                 ~alloc:false)
  | Paddbint Pint64 -> Pccall (int64_native_prim "caml_int64_add" 2
                                 ~alloc:false)
  | Psubbint Pint64 -> Pccall (int64_native_prim "caml_int64_sub" 2
                                 ~alloc:false)
  | Pmulbint Pint64 -> Pccall (int64_native_prim "caml_int64_mul" 2
                                 ~alloc:false)
  | Pdivbint {size=Pint64} -> Pccall (int64_native_prim "caml_int64_div" 2
                                        ~alloc:true)
  | Pmodbint {size=Pint64} -> Pccall (int64_native_prim "caml_int64_mod" 2
                                        ~alloc:true)
  | Pandbint Pint64 -> Pccall (int64_native_prim "caml_int64_and" 2
                                 ~alloc:false)
  | Porbint Pint64 ->  Pccall (int64_native_prim "caml_int64_or" 2
                                 ~alloc:false)
  | Pxorbint Pint64 -> Pccall (int64_native_prim "caml_int64_xor" 2
                                 ~alloc:false)
  | Plslbint Pint64 -> Pccall (default_prim "caml_int64_shift_left")
  | Plsrbint Pint64 -> Pccall (default_prim "caml_int64_shift_right_unsigned")
  | Pasrbint Pint64 -> Pccall (default_prim "caml_int64_shift_right")
  | Pbintcomp(Pint64, Lambda.Ceq) -> Pccall (default_prim "caml_equal")
  | Pbintcomp(Pint64, Lambda.Cne) -> Pccall (default_prim "caml_notequal")
  | Pbintcomp(Pint64, Lambda.Clt) -> Pccall (default_prim "caml_lessthan")
  | Pbintcomp(Pint64, Lambda.Cgt) -> Pccall (default_prim "caml_greaterthan")
  | Pbintcomp(Pint64, Lambda.Cle) -> Pccall (default_prim "caml_lessequal")
  | Pbintcomp(Pint64, Lambda.Cge) -> Pccall (default_prim "caml_greaterequal")
  | Pbigarrayref(_unsafe, n, Pbigarray_int64, _layout) ->
      Pccall (default_prim ("caml_ba_get_" ^ Int.to_string n))
  | Pbigarrayset(_unsafe, n, Pbigarray_int64, _layout) ->
      Pccall (default_prim ("caml_ba_set_" ^ Int.to_string n))
  | Pstring_load(Sixty_four, _) -> Pccall (default_prim "caml_string_get64")
  | Pbytes_load(Sixty_four, _) -> Pccall (default_prim "caml_bytes_get64")
  | Pbytes_set(Sixty_four, _) -> Pccall (default_prim "caml_bytes_set64")
  | Pbigstring_load(Sixty_four,_) -> Pccall (default_prim "caml_ba_uint8_get64")
  | Pbigstring_set(Sixty_four,_) -> Pccall (default_prim "caml_ba_uint8_set64")
  | Pbbswap Pint64 -> Pccall (default_prim "caml_int64_bswap")
  | p -> p

let simplif_primitive p =
  match p with
  | Pduprecord _ ->
      Pccall (default_prim "caml_obj_dup")
  | Pbigarrayref(_unsafe, n, Pbigarray_unknown, _layout) ->
      Pccall (default_prim ("caml_ba_get_" ^ Int.to_string n))
  | Pbigarrayset(_unsafe, n, Pbigarray_unknown, _layout) ->
      Pccall (default_prim ("caml_ba_set_" ^ Int.to_string n))
  | Pbigarrayref(_unsafe, n, _kind, Pbigarray_unknown_layout) ->
      Pccall (default_prim ("caml_ba_get_" ^ Int.to_string n))
  | Pbigarrayset(_unsafe, n, _kind, Pbigarray_unknown_layout) ->
      Pccall (default_prim ("caml_ba_set_" ^ Int.to_string n))
  | p ->
      if size_int = 8 then p else simplif_primitive_32bits p

(* Build switchers both for constants and blocks *)

let transl_isout h arg dbg = tag_int (Cop(Ccmpa Clt, [h ; arg], dbg)) dbg

(* Build an actual switch (ie jump table) *)

let make_switch arg cases actions dbg =
  let extract_uconstant =
    function
    (* Constant integers loaded from a table should end in 1,
       so that Cload never produces untagged integers *)
    | Cconst_int     (n, _), _dbg
    | Cconst_pointer (n, _), _dbg when (n land 1) = 1 ->
        Some (Cint (Nativeint.of_int n))
    | Cconst_natint     (n, _), _dbg
    | Cconst_natpointer (n, _), _dbg
      when Nativeint.(to_int (logand n one) = 1) ->
        Some (Cint n)
    | Cconst_symbol (s,_), _dbg ->
        Some (Csymbol_address s)
    | _ -> None
  in
  let extract_affine ~cases ~const_actions =
    let length = Array.length cases in
    if length >= 2
    then begin
      match const_actions.(cases.(0)), const_actions.(cases.(1)) with
      | Cint v0, Cint v1 ->
          let slope = Nativeint.sub v1 v0 in
          let check i = function
            | Cint v -> v = Nativeint.(add (mul (of_int i) slope) v0)
            | _ -> false
          in
          if Misc.Stdlib.Array.for_alli
              (fun i idx -> check i const_actions.(idx)) cases
          then Some (v0, slope)
          else None
      | _, _ ->
          None
    end
    else None
  in
  let make_table_lookup ~cases ~const_actions arg dbg =
    let table = Compilenv.new_const_symbol () in
    Cmmgen_state.add_constant table (Const_table (Local,
        Array.to_list (Array.map (fun act ->
          const_actions.(act)) cases)));
    addr_array_ref (Cconst_symbol (table, dbg)) (tag_int arg dbg) dbg
  in
  let make_affine_computation ~offset ~slope arg dbg =
    (* In case the resulting integers are an affine function of the index, we
       don't emit a table, and just compute the result directly *)
    add_int
      (mul_int arg (natint_const_untagged dbg slope) dbg)
      (natint_const_untagged dbg offset)
      dbg
  in
  match Misc.Stdlib.Array.all_somes (Array.map extract_uconstant actions) with
  | None ->
      Cswitch (arg,cases,actions,dbg)
  | Some const_actions ->
      match extract_affine ~cases ~const_actions with
      | Some (offset, slope) ->
          make_affine_computation ~offset ~slope arg dbg
      | None -> make_table_lookup ~cases ~const_actions arg dbg

module SArgBlocks =
struct
  type primitive = operation

  let eqint = Ccmpi Ceq
  let neint = Ccmpi Cne
  let leint = Ccmpi Cle
  let ltint = Ccmpi Clt
  let geint = Ccmpi Cge
  let gtint = Ccmpi Cgt

  type act = expression

  (* CR mshinwell: GPR#2294 will fix the Debuginfo here *)

  let make_const i =  Cconst_int (i, Debuginfo.none)
  let make_prim p args = Cop (p,args, Debuginfo.none)
  let make_offset arg n = add_const arg n Debuginfo.none
  let make_isout h arg = Cop (Ccmpa Clt, [h ; arg], Debuginfo.none)
  let make_isin h arg = Cop (Ccmpa Cge, [h ; arg], Debuginfo.none)
  let make_if cond ifso ifnot =
    Cifthenelse (cond, Debuginfo.none, ifso, Debuginfo.none, ifnot,
      Debuginfo.none)
  let make_switch loc arg cases actions =
    let dbg = Debuginfo.from_location loc in
    let actions = Array.map (fun expr -> expr, dbg) actions in
    make_switch arg cases actions dbg
  let bind arg body = bind "switcher" arg body

  let make_catch handler =
  match handler with
  | Cexit (i,[]) -> i,fun e -> e
  | _ ->
      let dbg = Debuginfo.none in
      let i = next_raise_count () in
(*
      Printf.eprintf  "SHARE CMM: %i\n" i ;
      Printcmm.expression Format.str_formatter handler ;
      Printf.eprintf "%s\n" (Format.flush_str_formatter ()) ;
*)
      i,
      (fun body -> match body with
      | Cexit (j,_) ->
          if i=j then handler
          else body
      | _ ->  ccatch (i,[],body,handler, dbg))

  let make_exit i = Cexit (i,[])

end

(* cmm store, as sharing as normally been detected in previous
   phases, we only share exits *)
(* Some specific patterns can lead to switches where several cases
   point to the same action, but this action is not an exit (see GPR#1370).
   The addition of the index in the action array as context allows
   sharing them correctly without duplication. *)
module StoreExpForSwitch =
  Switch.CtxStore
    (struct
      type t = expression
      type key = int option * int
      type context = int
      let make_key index expr =
        let continuation =
          match expr with
          | Cexit (i,[]) -> Some i
          | _ -> None
        in
        Some (continuation, index)
      let compare_key (cont, index) (cont', index') =
        match cont, cont' with
        | Some i, Some i' when i = i' -> 0
        | _, _ -> Stdlib.compare index index'
    end)

(* For string switches, we can use a generic store *)
module StoreExp =
  Switch.Store
    (struct
      type t = expression
      type key = int
      let make_key = function
        | Cexit (i,[]) -> Some i
        | _ -> None
      let compare_key = Stdlib.compare
    end)

module SwitcherBlocks = Switch.Make(SArgBlocks)

(* Int switcher, arg in [low..high],
   cases is list of individual cases, and is sorted by first component *)

let transl_int_switch loc arg low high cases default = match cases with
| [] -> assert false
| _::_ ->
    let store = StoreExp.mk_store () in
    assert (store.Switch.act_store () default = 0) ;
    let cases =
      List.map
        (fun (i,act) -> i,store.Switch.act_store () act)
        cases in
    let rec inters plow phigh pact = function
      | [] ->
          if phigh = high then [plow,phigh,pact]
          else [(plow,phigh,pact); (phigh+1,high,0) ]
      | (i,act)::rem ->
          if i = phigh+1 then
            if pact = act then
              inters plow i pact rem
            else
              (plow,phigh,pact)::inters i i act rem
          else (* insert default *)
            if pact = 0 then
              if act = 0 then
                inters plow i 0 rem
              else
                (plow,i-1,pact)::
                inters i i act rem
            else (* pact <> 0 *)
              (plow,phigh,pact)::
              begin
                if act = 0 then inters (phigh+1) i 0 rem
                else (phigh+1,i-1,0)::inters i i act rem
              end in
    let inters = match cases with
    | [] -> assert false
    | (k0,act0)::rem ->
        if k0 = low then inters k0 k0 act0 rem
        else inters low (k0-1) 0 cases in
    bind "switcher" arg
      (fun a ->
        SwitcherBlocks.zyva
          loc
          (low,high)
          a
          (Array.of_list inters) store)


(* Auxiliary functions for optimizing "let" of boxed numbers (floats and
   boxed integers *)

type unboxed_number_kind =
    No_unboxing
  | Boxed of boxed_number * bool (* true: boxed form available at no cost *)
  | No_result (* expression never returns a result *)

let unboxed_number_kind_of_unbox dbg = function
  | Same_as_ocaml_repr -> No_unboxing
  | Unboxed_float -> Boxed (Boxed_float dbg, false)
  | Unboxed_integer bi -> Boxed (Boxed_integer (bi, dbg), false)
  | Untagged_int -> No_unboxing

let rec is_unboxed_number ~strict env e =
  (* Given unboxed_number_kind from two branches of the code, returns the
     resulting unboxed_number_kind.

     If [strict=false], one knows that the type of the expression
     is an unboxable number, and we decide to return an unboxed value
     if this indeed eliminates at least one allocation.

     If [strict=true], we need to ensure that all possible branches
     return an unboxable number (of the same kind).  This could not
     be the case in presence of GADTs.
 *)
  let join k1 e =
    match k1, is_unboxed_number ~strict env e with
    | Boxed (b1, c1), Boxed (b2, c2) when equal_boxed_number b1 b2 ->
        Boxed (b1, c1 && c2)
    | No_result, k | k, No_result ->
        k (* if a branch never returns, it is safe to unbox it *)
    | No_unboxing, k | k, No_unboxing when not strict ->
        k
    | _, _ -> No_unboxing
  in
  match e with
  | Uvar id ->
      begin match is_unboxed_id id env with
      | None -> No_unboxing
      | Some (_, bn) -> Boxed (bn, false)
      end

  (* CR mshinwell: Changes to [Clambda] will provide the [Debuginfo] here *)
  | Uconst(Uconst_ref(_, Some (Uconst_float _))) ->
      let dbg = Debuginfo.none in
      Boxed (Boxed_float dbg, true)
  | Uconst(Uconst_ref(_, Some (Uconst_int32 _))) ->
      let dbg = Debuginfo.none in
      Boxed (Boxed_integer (Pint32, dbg), true)
  | Uconst(Uconst_ref(_, Some (Uconst_int64 _))) ->
      let dbg = Debuginfo.none in
      Boxed (Boxed_integer (Pint64, dbg), true)
  | Uconst(Uconst_ref(_, Some (Uconst_nativeint _))) ->
      let dbg = Debuginfo.none in
      Boxed (Boxed_integer (Pnativeint, dbg), true)
  | Uprim(p, _, dbg) ->
      begin match simplif_primitive p with
        | Pccall p -> unboxed_number_kind_of_unbox dbg p.prim_native_repr_res
        | Pfloatfield _
        | Pfloatofint
        | Pnegfloat
        | Pabsfloat
        | Paddfloat
        | Psubfloat
        | Pmulfloat
        | Pdivfloat
        | Parrayrefu Pfloatarray
        | Parrayrefs Pfloatarray -> Boxed (Boxed_float dbg, false)
        | Pbintofint bi
        | Pcvtbint(_, bi)
        | Pnegbint bi
        | Paddbint bi
        | Psubbint bi
        | Pmulbint bi
        | Pdivbint {size=bi}
        | Pmodbint {size=bi}
        | Pandbint bi
        | Porbint bi
        | Pxorbint bi
        | Plslbint bi
        | Plsrbint bi
        | Pasrbint bi
        | Pbbswap bi -> Boxed (Boxed_integer (bi, dbg), false)
        | Pbigarrayref(_, _, (Pbigarray_float32 | Pbigarray_float64), _) ->
            Boxed (Boxed_float dbg, false)
        | Pbigarrayref(_, _, Pbigarray_int32, _) ->
            Boxed (Boxed_integer (Pint32, dbg), false)
        | Pbigarrayref(_, _, Pbigarray_int64, _) ->
            Boxed (Boxed_integer (Pint64, dbg), false)
        | Pbigarrayref(_, _, Pbigarray_native_int,_) ->
            Boxed (Boxed_integer (Pnativeint, dbg), false)
        | Pstring_load(Thirty_two,_)
        | Pbytes_load(Thirty_two,_) ->
            Boxed (Boxed_integer (Pint32, dbg), false)
        | Pstring_load(Sixty_four,_)
        | Pbytes_load(Sixty_four,_) ->
            Boxed (Boxed_integer (Pint64, dbg), false)
        | Pbigstring_load(Thirty_two,_) ->
            Boxed (Boxed_integer (Pint32, dbg), false)
        | Pbigstring_load(Sixty_four,_) ->
            Boxed (Boxed_integer (Pint64, dbg), false)
        | Praise _ -> No_result
        | _ -> No_unboxing
      end
  | Ulet (_, _, _, _, e) | Uletrec (_, e) | Usequence (_, e) ->
      is_unboxed_number ~strict env e
  | Uswitch (_, switch, _dbg) ->
      let k = Array.fold_left join No_result switch.us_actions_consts in
      Array.fold_left join k switch.us_actions_blocks
  | Ustringswitch (_, actions, default_opt) ->
      let k = List.fold_left (fun k (_, e) -> join k e) No_result actions in
      begin match default_opt with
        None -> k
      | Some default -> join k default
      end
  | Ustaticfail _ -> No_result
  | Uifthenelse (_, e1, e2) | Ucatch (_, _, e1, e2) | Utrywith (e1, _, e2) ->
      join (is_unboxed_number ~strict env e1) e2
  | _ -> No_unboxing

(* Helper for compilation of initialization and assignment operations *)

type assignment_kind = Caml_modify | Caml_initialize | Simple

let assignment_kind ptr init =
  match init, ptr with
  | Assignment, Pointer -> Caml_modify
  | Heap_initialization, Pointer -> Caml_initialize
  | Assignment, Immediate
  | Heap_initialization, Immediate
  | Root_initialization, (Immediate | Pointer) -> Simple

(* Translate an expression *)

let strmatch_compile =
  let module S =
    Strmatch.Make
      (struct
        let string_block_length ptr = get_size ptr Debuginfo.none
        let transl_switch = transl_int_switch
      end) in
  S.compile

let rec transl env e =
  match e with
    Uvar id ->
      begin match is_unboxed_id id env with
      | None -> Cvar id
      | Some (unboxed_id, bn) -> box_number bn (Cvar unboxed_id)
      end
  | Uconst sc ->
      transl_constant Debuginfo.none sc
  | Uclosure(fundecls, []) ->
      let sym = Compilenv.new_const_symbol() in
      Cmmgen_state.add_constant sym (Const_closure (Local, fundecls, []));
      List.iter (fun f -> Cmmgen_state.add_function f) fundecls;
      let dbg =
        match fundecls with
        | [] -> Debuginfo.none
        | fundecl::_ -> fundecl.dbg
      in
      Cconst_symbol (sym, dbg)
  | Uclosure(fundecls, clos_vars) ->
      let rec transl_fundecls pos = function
          [] ->
            List.map (transl env) clos_vars
        | f :: rem ->
            Cmmgen_state.add_function f;
            let dbg = f.dbg in
            let without_header =
              if f.arity = 1 || f.arity = 0 then
                Cconst_symbol (f.label, dbg) ::
                int_const dbg f.arity ::
                transl_fundecls (pos + 3) rem
              else
                Cconst_symbol (curry_function f.arity, dbg) ::
                int_const dbg f.arity ::
                Cconst_symbol (f.label, dbg) ::
                transl_fundecls (pos + 4) rem
            in
            if pos = 0 then without_header
            else (alloc_infix_header pos f.dbg) :: without_header
      in
      let dbg =
        match fundecls with
        | [] -> Debuginfo.none
        | fundecl::_ -> fundecl.dbg
      in
      make_alloc dbg Obj.closure_tag (transl_fundecls 0 fundecls)
  | Uoffset(arg, offset) ->
      (* produces a valid Caml value, pointing just after an infix header *)
      let ptr = transl env arg in
      let dbg = Debuginfo.none in
      if offset = 0
      then ptr
      else Cop(Caddv, [ptr; Cconst_int(offset * size_addr, dbg)], dbg)
  | Udirect_apply(lbl, args, dbg) ->
      Cop(Capply typ_val,
        Cconst_symbol (lbl, dbg) :: List.map (transl env) args,
        dbg)
  | Ugeneric_apply(clos, [arg], dbg) ->
      bind "fun" (transl env clos) (fun clos ->
        Cop(Capply typ_val,
          [get_field env clos 0 dbg; transl env arg; clos],
          dbg))
  | Ugeneric_apply(clos, args, dbg) ->
      let arity = List.length args in
      let cargs = Cconst_symbol(apply_function arity, dbg) ::
        List.map (transl env) (args @ [clos]) in
      Cop(Capply typ_val, cargs, dbg)
  | Usend(kind, met, obj, args, dbg) ->
      let call_met obj args clos =
        if args = [] then
          Cop(Capply typ_val,
            [get_field env clos 0 dbg; obj; clos], dbg)
        else
          let arity = List.length args + 1 in
          let cargs = Cconst_symbol(apply_function arity, dbg) :: obj ::
            (List.map (transl env) args) @ [clos] in
          Cop(Capply typ_val, cargs, dbg)
      in
      bind "obj" (transl env obj) (fun obj ->
        match kind, args with
          Self, _ ->
            bind "met" (lookup_label obj (transl env met) dbg)
              (call_met obj args)
        | Cached, cache :: pos :: args ->
            call_cached_method obj
              (transl env met) (transl env cache) (transl env pos)
              (List.map (transl env) args) dbg
        | _ ->
            bind "met" (lookup_tag obj (transl env met) dbg)
              (call_met obj args))
  | Ulet(str, kind, id, exp, body) ->
      transl_let env str kind id exp body
  | Uphantom_let (var, defining_expr, body) ->
      let defining_expr =
        match defining_expr with
        | None -> None
        | Some defining_expr ->
          let defining_expr =
            match defining_expr with
            | Uphantom_const (Uconst_ref (sym, _defining_expr)) ->
              Cphantom_const_symbol sym
            | Uphantom_read_symbol_field { sym; field; } ->
              Cphantom_read_symbol_field { sym; field; }
            | Uphantom_const (Uconst_int i) | Uphantom_const (Uconst_ptr i) ->
              Cphantom_const_int (targetint_const i)
            | Uphantom_var var -> Cphantom_var var
            | Uphantom_read_field { var; field; } ->
              Cphantom_read_field { var; field; }
            | Uphantom_offset_var { var; offset_in_words; } ->
              Cphantom_offset_var { var; offset_in_words; }
            | Uphantom_block { tag; fields; } ->
              Cphantom_block { tag; fields; }
          in
          Some defining_expr
      in
      Cphantom_let (var, defining_expr, transl env body)
  | Uletrec(bindings, body) ->
      transl_letrec env bindings (transl env body)

  (* Primitives *)
  | Uprim(prim, args, dbg) ->
      begin match (simplif_primitive prim, args) with
      | (Pread_symbol sym, []) ->
          Cconst_symbol (sym, dbg)
      | (Pmakeblock _, []) ->
          assert false
      | (Pmakeblock(tag, _mut, _kind), args) ->
          make_alloc dbg tag (List.map (transl env) args)
      | (Pccall prim, args) ->
          transl_ccall env prim args dbg
      | (Pduparray (kind, _), [Uprim (Pmakearray (kind', _), args, _dbg)]) ->
          (* We arrive here in two cases:
             1. When using Closure, all the time.
             2. When using Flambda, if a float array longer than
             [Translcore.use_dup_for_constant_arrays_bigger_than] turns out
             to be non-constant.
             If for some reason Flambda fails to lift a constant array we
             could in theory also end up here.
             Note that [kind] above is unconstrained, but with the current
             state of [Translcore], we will in fact only get here with
             [Pfloatarray]s. *)
          assert (kind = kind');
          transl_make_array dbg env kind args
      | (Pduparray _, [arg]) ->
          let prim_obj_dup =
            Primitive.simple ~name:"caml_obj_dup" ~arity:1 ~alloc:true
          in
          transl_ccall env prim_obj_dup [arg] dbg
      | (Pmakearray _, []) ->
          Misc.fatal_error "Pmakearray is not allowed for an empty array"
      | (Pmakearray (kind, _), args) -> transl_make_array dbg env kind args
      | (Pbigarrayref(unsafe, _num_dims, elt_kind, layout), arg1 :: argl) ->
          let elt =
            bigarray_get unsafe elt_kind layout
              (transl env arg1) (List.map (transl env) argl) dbg in
          begin match elt_kind with
            Pbigarray_float32 | Pbigarray_float64 -> box_float dbg elt
          | Pbigarray_complex32 | Pbigarray_complex64 -> elt
          | Pbigarray_int32 -> box_int dbg Pint32 elt
          | Pbigarray_int64 -> box_int dbg Pint64 elt
          | Pbigarray_native_int -> box_int dbg Pnativeint elt
          | Pbigarray_caml_int -> tag_int elt dbg
          | Pbigarray_sint8 | Pbigarray_uint8
          | Pbigarray_sint16 | Pbigarray_uint16 -> tag_int elt dbg
          | Pbigarray_unknown -> assert false
          end
      | (Pbigarrayset(unsafe, _num_dims, elt_kind, layout), arg1 :: argl) ->
          let (argidx, argnewval) = split_last argl in
          return_unit dbg (bigarray_set unsafe elt_kind layout
            (transl env arg1)
            (List.map (transl env) argidx)
            (match elt_kind with
              Pbigarray_float32 | Pbigarray_float64 ->
                transl_unbox_float dbg env argnewval
            | Pbigarray_complex32 | Pbigarray_complex64 -> transl env argnewval
            | Pbigarray_int32 -> transl_unbox_int dbg env Pint32 argnewval
            | Pbigarray_int64 -> transl_unbox_int dbg env Pint64 argnewval
            | Pbigarray_native_int ->
                transl_unbox_int dbg env Pnativeint argnewval
            | Pbigarray_caml_int ->
                untag_int (transl env argnewval) dbg
            | Pbigarray_sint8 | Pbigarray_uint8
            | Pbigarray_sint16 | Pbigarray_uint16 ->
                ignore_high_bit_int (untag_int (transl env argnewval) dbg)
            | Pbigarray_unknown -> assert false)
            dbg)
      | (Pbigarraydim(n), [b]) ->
          let dim_ofs = 4 + n in
          tag_int (Cop(Cload (Word_int, Mutable),
            [field_address (transl env b) dim_ofs dbg],
            dbg)) dbg
      | (p, [arg]) ->
          transl_prim_1 env p arg dbg
      | (p, [arg1; arg2]) ->
          transl_prim_2 env p arg1 arg2 dbg
      | (p, [arg1; arg2; arg3]) ->
          transl_prim_3 env p arg1 arg2 arg3 dbg
      | (Pread_symbol _, _::_::_::_::_)
      | (Pbigarrayset (_, _, _, _), [])
      | (Pbigarrayref (_, _, _, _), [])
      | ((Pbigarraydim _ | Pduparray (_, _)), ([] | _::_::_::_::_))
        ->
          fatal_error "Cmmgen.transl:prim, wrong arity"
      | ((Pfield_computed|Psequand
         | Psequor | Pnot | Pnegint | Paddint | Psubint
         | Pmulint | Pandint | Porint | Pxorint | Plslint
         | Plsrint | Pasrint | Pintoffloat | Pfloatofint
         | Pnegfloat | Pabsfloat | Paddfloat | Psubfloat
         | Pmulfloat | Pdivfloat | Pstringlength | Pstringrefu
         | Pstringrefs | Pbyteslength | Pbytesrefu | Pbytessetu
         | Pbytesrefs | Pbytessets | Pisint | Pisout
         | Pbswap16 | Pint_as_pointer | Popaque | Pfield _
         | Psetfield (_, _, _) | Psetfield_computed (_, _)
         | Pfloatfield _ | Psetfloatfield (_, _) | Pduprecord (_, _)
         | Praise _ | Pdivint _ | Pmodint _ | Pintcomp _ | Poffsetint _
         | Poffsetref _ | Pfloatcomp _ | Parraylength _
         | Parrayrefu _ | Parraysetu _ | Parrayrefs _ | Parraysets _
         | Pbintofint _ | Pintofbint _ | Pcvtbint (_, _) | Pnegbint _
         | Paddbint _ | Psubbint _ | Pmulbint _ | Pdivbint _ | Pmodbint _
         | Pandbint _ | Porbint _ | Pxorbint _ | Plslbint _ | Plsrbint _
         | Pasrbint _ | Pbintcomp (_, _) | Pstring_load _ | Pbytes_load _
         | Pbytes_set _ | Pbigstring_load _ | Pbigstring_set _
         | Pbbswap _), _)
        ->
          fatal_error "Cmmgen.transl:prim"
      end

  (* Control structures *)
  | Uswitch(arg, s, dbg) ->
      let loc = Debuginfo.to_location dbg in
      (* As in the bytecode interpreter, only matching against constants
         can be checked *)
      if Array.length s.us_index_blocks = 0 then
        make_switch
          (untag_int (transl env arg) dbg)
          s.us_index_consts
          (Array.map (fun expr -> transl env expr, dbg) s.us_actions_consts)
          dbg
      else if Array.length s.us_index_consts = 0 then
        bind "switch" (transl env arg) (fun arg ->
          transl_switch loc env (get_tag arg dbg)
            s.us_index_blocks s.us_actions_blocks)
      else
        bind "switch" (transl env arg) (fun arg ->
          Cifthenelse(
          Cop(Cand, [arg; Cconst_int (1, dbg)], dbg),
          dbg,
          transl_switch loc env
            (untag_int arg dbg) s.us_index_consts s.us_actions_consts,
          dbg,
          transl_switch loc env
            (get_tag arg dbg) s.us_index_blocks s.us_actions_blocks,
          dbg))
  | Ustringswitch(arg,sw,d) ->
      let dbg = Debuginfo.none in
      bind "switch" (transl env arg)
        (fun arg ->
          strmatch_compile dbg arg (Misc.may_map (transl env) d)
            (List.map (fun (s,act) -> s,transl env act) sw))
  | Ustaticfail (nfail, args) ->
      Cexit (nfail, List.map (transl env) args)
  | Ucatch(nfail, [], body, handler) ->
      let dbg = Debuginfo.none in
      make_catch nfail (transl env body) (transl env handler) dbg
  | Ucatch(nfail, ids, body, handler) ->
      let dbg = Debuginfo.none in
      (* CR-someday mshinwell: consider how we can do better than
         [typ_val] when appropriate. *)
      let ids_with_types =
        List.map (fun (i, _) -> (i, Cmm.typ_val)) ids in
      ccatch(nfail, ids_with_types, transl env body, transl env handler, dbg)
  | Utrywith(body, exn, handler) ->
      let dbg = Debuginfo.none in
      Ctrywith(transl env body, exn, transl env handler, dbg)
  | Uifthenelse(cond, ifso, ifnot) ->
      let ifso_dbg = Debuginfo.none in
      let ifnot_dbg = Debuginfo.none in
      let dbg = Debuginfo.none in
      transl_if env Unknown dbg cond
        ifso_dbg (transl env ifso) ifnot_dbg (transl env ifnot)
  | Usequence(exp1, exp2) ->
      Csequence(remove_unit(transl env exp1), transl env exp2)
  | Uwhile(cond, body) ->
      let dbg = Debuginfo.none in
      let raise_num = next_raise_count () in
      return_unit dbg
        (ccatch
           (raise_num, [],
            create_loop(transl_if env Unknown dbg cond
                    dbg (remove_unit(transl env body))
                    dbg (Cexit (raise_num,[])))
              dbg,
            Ctuple [],
            dbg))
  | Ufor(id, low, high, dir, body) ->
      let dbg = Debuginfo.none in
      let tst = match dir with Upto -> Cgt   | Downto -> Clt in
      let inc = match dir with Upto -> Caddi | Downto -> Csubi in
      let raise_num = next_raise_count () in
      let id_prev = VP.create (V.create_local "*id_prev*") in
      return_unit dbg
        (Clet
           (id, transl env low,
            bind_nonvar "bound" (transl env high) (fun high ->
              ccatch
                (raise_num, [],
                 Cifthenelse
                   (Cop(Ccmpi tst, [Cvar (VP.var id); high], dbg),
                    dbg,
                    Cexit (raise_num, []),
                    dbg,
                    create_loop
                      (Csequence
                         (remove_unit(transl env body),
                         Clet(id_prev, Cvar (VP.var id),
                          Csequence
                            (Cassign(VP.var id,
                               Cop(inc, [Cvar (VP.var id); Cconst_int (2, dbg)],
                                 dbg)),
                             Cifthenelse
                               (Cop(Ccmpi Ceq, [Cvar (VP.var id_prev); high],
                                  dbg),
                                dbg, Cexit (raise_num,[]),
                                dbg, Ctuple [],
                                dbg)))))
                      dbg,
                   dbg),
                 Ctuple [],
                 dbg))))
  | Uassign(id, exp) ->
      let dbg = Debuginfo.none in
      begin match is_unboxed_id id env with
      | None ->
          return_unit dbg (Cassign(id, transl env exp))
      | Some (unboxed_id, bn) ->
          return_unit dbg (Cassign(unboxed_id,
            transl_unbox_number dbg env bn exp))
      end
  | Uunreachable ->
      let dbg = Debuginfo.none in
      Cop(Cload (Word_int, Mutable), [Cconst_int (0, dbg)], dbg)

and transl_make_array dbg env kind args =
  match kind with
  | Pgenarray ->
      Cop(Cextcall("caml_make_array", typ_val, true, None),
          [make_alloc dbg 0 (List.map (transl env) args)], dbg)
  | Paddrarray | Pintarray ->
      make_alloc dbg 0 (List.map (transl env) args)
  | Pfloatarray ->
      make_float_alloc dbg Obj.double_array_tag
                      (List.map (transl_unbox_float dbg env) args)

and transl_ccall env prim args dbg =
  let transl_arg native_repr arg =
    match native_repr with
    | Same_as_ocaml_repr -> transl env arg
    | Unboxed_float -> transl_unbox_float dbg env arg
    | Unboxed_integer bi -> transl_unbox_int dbg env bi arg
    | Untagged_int -> untag_int (transl env arg) dbg
  in
  let rec transl_args native_repr_args args =
    match native_repr_args, args with
    | [], args ->
        (* We don't require the two lists to be of the same length as
           [default_prim] always sets the arity to [0]. *)
        List.map (transl env) args
    | _, [] -> assert false
    | native_repr :: native_repr_args, arg :: args ->
        transl_arg native_repr arg :: transl_args native_repr_args args
  in
  let typ_res, wrap_result =
    match prim.prim_native_repr_res with
    | Same_as_ocaml_repr -> (typ_val, fun x -> x)
    | Unboxed_float -> (typ_float, box_float dbg)
    | Unboxed_integer Pint64 when size_int = 4 ->
        ([|Int; Int|], box_int dbg Pint64)
    | Unboxed_integer bi -> (typ_int, box_int dbg bi)
    | Untagged_int -> (typ_int, (fun i -> tag_int i dbg))
  in
  let args = transl_args prim.prim_native_repr_args args in
  wrap_result
    (Cop(Cextcall(Primitive.native_name prim,
                  typ_res, prim.prim_alloc, None), args, dbg))

and transl_prim_1 env p arg dbg =
  match p with
  (* Generic operations *)
    Popaque ->
      transl env arg
  (* Heap operations *)
  | Pfield n ->
      get_field env (transl env arg) n dbg
  | Pfloatfield n ->
      let ptr = transl env arg in
      box_float dbg (
        Cop(Cload (Double_u, Mutable),
            [if n = 0
             then ptr
             else Cop(Cadda, [ptr; Cconst_int(n * size_float, dbg)], dbg)],
            dbg))
  | Pint_as_pointer ->
     Cop(Caddi, [transl env arg; Cconst_int (-1, dbg)], dbg)
     (* always a pointer outside the heap *)
  (* Exceptions *)
  | Praise _ when not (!Clflags.debug) ->
      Cop(Craise Cmm.Raise_notrace, [transl env arg], dbg)
  | Praise Lambda.Raise_notrace ->
      Cop(Craise Cmm.Raise_notrace, [transl env arg], dbg)
  | Praise Lambda.Raise_reraise ->
      Cop(Craise Cmm.Raise_withtrace, [transl env arg], dbg)
  | Praise Lambda.Raise_regular ->
      raise_regular dbg (transl env arg)
  (* Integer operations *)
  | Pnegint ->
      Cop(Csubi, [Cconst_int (2, dbg); transl env arg], dbg)
  | Poffsetint n ->
      if no_overflow_lsl n 1 then
        add_const (transl env arg) (n lsl 1) dbg
      else
        transl_prim_2 env Paddint arg (Uconst (Uconst_int n)) dbg
  | Poffsetref n ->
      return_unit dbg
        (bind "ref" (transl env arg) (fun arg ->
          Cop(Cstore (Word_int, Assignment),
              [arg;
               add_const (Cop(Cload (Word_int, Mutable), [arg], dbg))
                 (n lsl 1) dbg],
              dbg)))
  (* Floating-point operations *)
  | Pfloatofint ->
      box_float dbg (Cop(Cfloatofint, [untag_int(transl env arg) dbg], dbg))
  | Pintoffloat ->
     tag_int(Cop(Cintoffloat, [transl_unbox_float dbg env arg], dbg)) dbg
  | Pnegfloat ->
      box_float dbg (Cop(Cnegf, [transl_unbox_float dbg env arg], dbg))
  | Pabsfloat ->
      box_float dbg (Cop(Cabsf, [transl_unbox_float dbg env arg], dbg))
  (* String operations *)
  | Pstringlength | Pbyteslength ->
      tag_int(string_length (transl env arg) dbg) dbg
  (* Array operations *)
  | Parraylength kind ->
      let hdr = get_header_without_profinfo (transl env arg) dbg in
      begin match kind with
        Pgenarray ->
          let len =
            if wordsize_shift = numfloat_shift then
              Cop(Clsr, [hdr; Cconst_int (wordsize_shift, dbg)], dbg)
            else
              bind "header" hdr (fun hdr ->
                Cifthenelse(is_addr_array_hdr hdr dbg,
                            dbg,
                            Cop(Clsr,
                              [hdr; Cconst_int (wordsize_shift, dbg)], dbg),
                            dbg,
                            Cop(Clsr,
                              [hdr; Cconst_int (numfloat_shift, dbg)], dbg),
                            dbg))
          in
          Cop(Cor, [len; Cconst_int (1, dbg)], dbg)
      | Paddrarray | Pintarray ->
          Cop(Cor, [addr_array_length hdr dbg; Cconst_int (1, dbg)], dbg)
      | Pfloatarray ->
          Cop(Cor, [float_array_length hdr dbg; Cconst_int (1, dbg)], dbg)
      end
  (* Boolean operations *)
  | Pnot ->
      transl_if env Then_false_else_true
        dbg arg
        dbg (Cconst_pointer (1, dbg))
        dbg (Cconst_pointer (3, dbg))
  (* Test integer/block *)
  | Pisint ->
      tag_int(Cop(Cand, [transl env arg; Cconst_int (1, dbg)], dbg)) dbg
  (* Boxed integers *)
  | Pbintofint bi ->
      box_int dbg bi (untag_int (transl env arg) dbg)
  | Pintofbint bi ->
      tag_int (transl_unbox_int dbg env bi arg) dbg
  | Pcvtbint(bi1, bi2) ->
      box_int dbg bi2 (transl_unbox_int dbg env bi1 arg)
  | Pnegbint bi ->
      box_int dbg bi
        (Cop(Csubi, [Cconst_int (0, dbg); transl_unbox_int dbg env bi arg],
          dbg))
  | Pbbswap bi ->
      let prim = match bi with
        | Pnativeint -> "nativeint"
        | Pint32 -> "int32"
        | Pint64 -> "int64" in
      box_int dbg bi (Cop(Cextcall(Printf.sprintf "caml_%s_direct_bswap" prim,
                               typ_int, false, None),
                      [transl_unbox_int dbg env bi arg],
                      dbg))
  | Pbswap16 ->
      tag_int (Cop(Cextcall("caml_bswap16_direct", typ_int, false, None),
                   [ignore_high_bit_int (untag_int (transl env arg) dbg)],
                   dbg))
        dbg
  | (Pfield_computed | Psequand | Psequor
    | Paddint | Psubint | Pmulint | Pandint
    | Porint | Pxorint | Plslint | Plsrint | Pasrint
    | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
    | Pstringrefu | Pstringrefs | Pbytesrefu | Pbytessetu
    | Pbytesrefs | Pbytessets | Pisout | Pread_symbol _
    | Pmakeblock (_, _, _) | Psetfield (_, _, _) | Psetfield_computed (_, _)
    | Psetfloatfield (_, _) | Pduprecord (_, _) | Pccall _ | Pdivint _
    | Pmodint _ | Pintcomp _ | Pfloatcomp _ | Pmakearray (_, _)
    | Pduparray (_, _) | Parrayrefu _ | Parraysetu _
    | Parrayrefs _ | Parraysets _ | Paddbint _ | Psubbint _ | Pmulbint _
    | Pdivbint _ | Pmodbint _ | Pandbint _ | Porbint _ | Pxorbint _
    | Plslbint _ | Plsrbint _ | Pasrbint _ | Pbintcomp (_, _)
    | Pbigarrayref (_, _, _, _) | Pbigarrayset (_, _, _, _)
    | Pbigarraydim _ | Pstring_load _ | Pbytes_load _ | Pbytes_set _
    | Pbigstring_load _ | Pbigstring_set _)
    ->
      fatal_errorf "Cmmgen.transl_prim_1: %a"
        Printclambda_primitives.primitive p

and transl_prim_2 env p arg1 arg2 dbg =
  match p with
  (* Heap operations *)
  | Pfield_computed ->
      addr_array_ref (transl env arg1) (transl env arg2) dbg
  | Psetfield(n, ptr, init) ->
      begin match assignment_kind ptr init with
      | Caml_modify ->
        return_unit dbg (Cop(Cextcall("caml_modify", typ_void, false, None),
                        [field_address (transl env arg1) n dbg;
                         transl env arg2],
                        dbg))
      | Caml_initialize ->
        return_unit dbg (Cop(Cextcall("caml_initialize", typ_void, false, None),
                        [field_address (transl env arg1) n dbg;
                         transl env arg2],
                        dbg))
      | Simple ->
        return_unit dbg
          (set_field (transl env arg1) n (transl env arg2) init dbg)
      end
  | Psetfloatfield (n, init) ->
      let ptr = transl env arg1 in
      return_unit dbg (
        Cop(Cstore (Double_u, init),
            [if n = 0 then ptr
                      else
                        Cop(Cadda, [ptr; Cconst_int(n * size_float, dbg)], dbg);
             transl_unbox_float dbg env arg2], dbg))

  (* Boolean operations *)
  | Psequand ->
      let dbg' = Debuginfo.none in
      transl_sequand env Then_true_else_false
        dbg arg1
        dbg' arg2
        dbg (Cconst_pointer (3, dbg))
        dbg' (Cconst_pointer (1, dbg))
      (* let id = V.create_local "res1" in
      Clet(id, transl env arg1,
           Cifthenelse(test_bool dbg (Cvar id), transl env arg2, Cvar id)) *)
  | Psequor ->
      let dbg' = Debuginfo.none in
      transl_sequor env Then_true_else_false
        dbg arg1
        dbg' arg2
        dbg (Cconst_pointer (3, dbg))
        dbg' (Cconst_pointer (1, dbg))
  (* Integer operations *)
  | Paddint ->
      decr_int(add_int (transl env arg1) (transl env arg2) dbg) dbg
  | Psubint ->
      incr_int(sub_int (transl env arg1) (transl env arg2) dbg) dbg
  | Pmulint ->
     begin
       (* decrementing the non-constant part helps when the multiplication is
          followed by an addition;
          for example, using this trick compiles (100 * a + 7) into
            (+ ( * a 100) -85)
          rather than
            (+ ( * 200 (>>s a 1)) 15)
        *)
       match transl env arg1, transl env arg2 with
       | Cconst_int _ as c1, c2 ->
         incr_int (mul_int (untag_int c1 dbg) (decr_int c2 dbg) dbg) dbg
       | c1, c2 ->
         incr_int (mul_int (decr_int c1 dbg) (untag_int c2 dbg) dbg) dbg
     end
  | Pdivint is_safe ->
      tag_int(div_int (untag_int(transl env arg1) dbg)
        (untag_int(transl env arg2) dbg) is_safe dbg) dbg
  | Pmodint is_safe ->
      tag_int(mod_int (untag_int(transl env arg1) dbg)
        (untag_int(transl env arg2) dbg) is_safe dbg) dbg
  | Pandint ->
      Cop(Cand, [transl env arg1; transl env arg2], dbg)
  | Porint ->
      Cop(Cor, [transl env arg1; transl env arg2], dbg)
  | Pxorint ->
      Cop(Cor, [Cop(Cxor, [ignore_low_bit_int(transl env arg1);
                           ignore_low_bit_int(transl env arg2)], dbg);
                Cconst_int (1, dbg)], dbg)
  | Plslint ->
      incr_int(lsl_int (decr_int(transl env arg1) dbg)
        (untag_int(transl env arg2) dbg) dbg) dbg
  | Plsrint ->
      Cop(Cor, [lsr_int (transl env arg1) (untag_int(transl env arg2) dbg) dbg;
                Cconst_int (1, dbg)], dbg)
  | Pasrint ->
      Cop(Cor, [asr_int (transl env arg1) (untag_int(transl env arg2) dbg) dbg;
                Cconst_int (1, dbg)], dbg)
  | Pintcomp cmp ->
      tag_int(Cop(Ccmpi(transl_int_comparison cmp),
                  [transl env arg1; transl env arg2], dbg)) dbg
  | Pisout ->
      transl_isout (transl env arg1) (transl env arg2) dbg
  (* Float operations *)
  | Paddfloat ->
      box_float dbg (Cop(Caddf,
                    [transl_unbox_float dbg env arg1;
                     transl_unbox_float dbg env arg2],
                    dbg))
  | Psubfloat ->
      box_float dbg (Cop(Csubf,
                    [transl_unbox_float dbg env arg1;
                     transl_unbox_float dbg env arg2],
                    dbg))
  | Pmulfloat ->
      box_float dbg (Cop(Cmulf,
                    [transl_unbox_float dbg env arg1;
                     transl_unbox_float dbg env arg2],
                    dbg))
  | Pdivfloat ->
      box_float dbg (Cop(Cdivf,
                    [transl_unbox_float dbg env arg1;
                     transl_unbox_float dbg env arg2],
                    dbg))
  | Pfloatcomp cmp ->
      tag_int(Cop(Ccmpf(transl_float_comparison cmp),
                  [transl_unbox_float dbg env arg1;
                   transl_unbox_float dbg env arg2],
                  dbg)) dbg

  (* String operations *)
  | Pstringrefu | Pbytesrefu ->
      tag_int(Cop(Cload (Byte_unsigned, Mutable),
                  [add_int (transl env arg1) (untag_int(transl env arg2) dbg)
                    dbg],
                  dbg)) dbg
  | Pstringrefs | Pbytesrefs ->
      tag_int
        (bind "str" (transl env arg1) (fun str ->
          bind "index" (untag_int (transl env arg2) dbg) (fun idx ->
            Csequence(
              make_checkbound dbg [string_length str dbg; idx],
              Cop(Cload (Byte_unsigned, Mutable),
                [add_int str idx dbg], dbg))))) dbg

  | Pstring_load(size, unsafe) | Pbytes_load(size, unsafe) ->
     box_sized size dbg
       (bind "str" (transl env arg1) (fun str ->
        bind "index" (untag_int (transl env arg2) dbg) (fun idx ->
          check_bound unsafe size dbg
             (string_length str dbg)
             idx (unaligned_load size str idx dbg))))

  | Pbigstring_load(size, unsafe) ->
      box_sized size dbg
       (bind "ba" (transl env arg1) (fun ba ->
        bind "index" (untag_int (transl env arg2) dbg) (fun idx ->
        bind "ba_data"
         (Cop(Cload (Word_int, Mutable), [field_address ba 1 dbg], dbg))
         (fun ba_data ->
            check_bound unsafe size dbg
              (bigstring_length ba dbg)
              idx
              (unaligned_load size ba_data idx dbg)))))

  (* Array operations *)
  | Parrayrefu kind ->
      begin match kind with
        Pgenarray ->
          bind "arr" (transl env arg1) (fun arr ->
            bind "index" (transl env arg2) (fun idx ->
              Cifthenelse(is_addr_array_ptr arr dbg,
                          dbg,
                          addr_array_ref arr idx dbg,
                          dbg,
                          float_array_ref dbg arr idx,
                          dbg)))
      | Paddrarray ->
          addr_array_ref (transl env arg1) (transl env arg2) dbg
      | Pintarray ->
          (* CR mshinwell: for int/addr_array_ref move "dbg" to first arg *)
          int_array_ref (transl env arg1) (transl env arg2) dbg
      | Pfloatarray ->
          float_array_ref dbg (transl env arg1) (transl env arg2)
      end
  | Parrayrefs kind ->
      begin match kind with
      | Pgenarray ->
          bind "index" (transl env arg2) (fun idx ->
          bind "arr" (transl env arg1) (fun arr ->
          bind "header" (get_header_without_profinfo arr dbg) (fun hdr ->
            if wordsize_shift = numfloat_shift then
              Csequence(make_checkbound dbg [addr_array_length hdr dbg; idx],
                        Cifthenelse(is_addr_array_hdr hdr dbg,
                                    dbg,
                                    addr_array_ref arr idx dbg,
                                    dbg,
                                    float_array_ref dbg arr idx,
                                    dbg))
            else
              Cifthenelse(is_addr_array_hdr hdr dbg,
                dbg,
                Csequence(make_checkbound dbg [addr_array_length hdr dbg; idx],
                          addr_array_ref arr idx dbg),
                dbg,
                Csequence(make_checkbound dbg [float_array_length hdr dbg; idx],
                          float_array_ref dbg arr idx),
                dbg))))
      | Paddrarray ->
          bind "index" (transl env arg2) (fun idx ->
          bind "arr" (transl env arg1) (fun arr ->
            Csequence(make_checkbound dbg [
              addr_array_length(get_header_without_profinfo arr dbg) dbg; idx],
                      addr_array_ref arr idx dbg)))
      | Pintarray ->
          bind "index" (transl env arg2) (fun idx ->
          bind "arr" (transl env arg1) (fun arr ->
            Csequence(make_checkbound dbg [
              addr_array_length(get_header_without_profinfo arr dbg) dbg; idx],
                      int_array_ref arr idx dbg)))
      | Pfloatarray ->
          box_float dbg (
            bind "index" (transl env arg2) (fun idx ->
            bind "arr" (transl env arg1) (fun arr ->
              Csequence(make_checkbound dbg
                [float_array_length(get_header_without_profinfo arr dbg) dbg;
                  idx],
                unboxed_float_array_ref arr idx dbg))))
      end

  (* Boxed integers *)
  | Paddbint bi ->
      box_int dbg bi (Cop(Caddi,
                      [transl_unbox_int dbg env bi arg1;
                       transl_unbox_int dbg env bi arg2], dbg))
  | Psubbint bi ->
      box_int dbg bi (Cop(Csubi,
                      [transl_unbox_int dbg env bi arg1;
                       transl_unbox_int dbg env bi arg2], dbg))
  | Pmulbint bi ->
      box_int dbg bi (Cop(Cmuli,
                      [transl_unbox_int dbg env bi arg1;
                       transl_unbox_int dbg env bi arg2], dbg))
  | Pdivbint { size = bi; is_safe } ->
      box_int dbg bi (safe_div_bi is_safe
                      (transl_unbox_int dbg env bi arg1)
                      (transl_unbox_int dbg env bi arg2)
                      bi dbg)
  | Pmodbint { size = bi; is_safe } ->
      box_int dbg bi (safe_mod_bi is_safe
                      (transl_unbox_int dbg env bi arg1)
                      (transl_unbox_int dbg env bi arg2)
                      bi dbg)
  | Pandbint bi ->
      box_int dbg bi (Cop(Cand,
                     [transl_unbox_int dbg env bi arg1;
                      transl_unbox_int dbg env bi arg2], dbg))
  | Porbint bi ->
      box_int dbg bi (Cop(Cor,
                     [transl_unbox_int dbg env bi arg1;
                      transl_unbox_int dbg env bi arg2], dbg))
  | Pxorbint bi ->
      box_int dbg bi (Cop(Cxor,
                     [transl_unbox_int dbg env bi arg1;
                      transl_unbox_int dbg env bi arg2], dbg))
  | Plslbint bi ->
      box_int dbg bi (Cop(Clsl,
                     [transl_unbox_int dbg env bi arg1;
                      untag_int(transl env arg2) dbg], dbg))
  | Plsrbint bi ->
      box_int dbg bi (Cop(Clsr,
                     [make_unsigned_int bi (transl_unbox_int dbg env bi arg1)
                                        dbg;
                      untag_int(transl env arg2) dbg], dbg))
  | Pasrbint bi ->
      box_int dbg bi (Cop(Casr,
                     [transl_unbox_int dbg env bi arg1;
                      untag_int(transl env arg2) dbg], dbg))
  | Pbintcomp(bi, cmp) ->
      tag_int (Cop(Ccmpi(transl_int_comparison cmp),
                     [transl_unbox_int dbg env bi arg1;
                      transl_unbox_int dbg env bi arg2], dbg)) dbg
  | Pnot | Pnegint | Pintoffloat | Pfloatofint | Pnegfloat
  | Pabsfloat | Pstringlength | Pbyteslength | Pbytessetu | Pbytessets
  | Pisint | Pbswap16 | Pint_as_pointer | Popaque | Pread_symbol _
  | Pmakeblock (_, _, _) | Pfield _ | Psetfield_computed (_, _) | Pfloatfield _
  | Pduprecord (_, _) | Pccall _ | Praise _ | Poffsetint _ | Poffsetref _
  | Pmakearray (_, _) | Pduparray (_, _) | Parraylength _ | Parraysetu _
  | Parraysets _ | Pbintofint _ | Pintofbint _ | Pcvtbint (_, _)
  | Pnegbint _ | Pbigarrayref (_, _, _, _) | Pbigarrayset (_, _, _, _)
  | Pbigarraydim _ | Pbytes_set _ | Pbigstring_set _ | Pbbswap _
    ->
      fatal_errorf "Cmmgen.transl_prim_2: %a"
        Printclambda_primitives.primitive p

and transl_prim_3 env p arg1 arg2 arg3 dbg =
  match p with
  (* Heap operations *)
  | Psetfield_computed(ptr, init) ->
      begin match assignment_kind ptr init with
      | Caml_modify ->
        return_unit dbg (
          addr_array_set (transl env arg1) (transl env arg2) (transl env arg3)
            dbg)
      | Caml_initialize ->
        return_unit dbg (
          addr_array_initialize (transl env arg1) (transl env arg2)
            (transl env arg3) dbg)
      | Simple ->
        return_unit dbg (
          int_array_set (transl env arg1) (transl env arg2) (transl env arg3)
            dbg)
      end
  (* String operations *)
  | Pbytessetu ->
      return_unit dbg (Cop(Cstore (Byte_unsigned, Assignment),
                      [add_int (transl env arg1)
                          (untag_int(transl env arg2) dbg)
                          dbg;
                       ignore_high_bit_int (untag_int(transl env arg3) dbg)],
                           dbg))
  | Pbytessets ->
      return_unit dbg
        (bind "str" (transl env arg1) (fun str ->
          bind "index" (untag_int (transl env arg2) dbg) (fun idx ->
            Csequence(
              make_checkbound dbg [string_length str dbg; idx],
              Cop(Cstore (Byte_unsigned, Assignment),
                  [add_int str idx dbg;
                   ignore_high_bit_int (untag_int(transl env arg3) dbg)],
                  dbg)))))

  (* Array operations *)
  | Parraysetu kind ->
      return_unit dbg (begin match kind with
        Pgenarray ->
          bind "newval" (transl env arg3) (fun newval ->
            bind "index" (transl env arg2) (fun index ->
              bind "arr" (transl env arg1) (fun arr ->
                Cifthenelse(is_addr_array_ptr arr dbg,
                            dbg,
                            addr_array_set arr index newval dbg,
                            dbg,
                            float_array_set arr index (unbox_float dbg newval)
                              dbg,
                            dbg))))
      | Paddrarray ->
          addr_array_set (transl env arg1) (transl env arg2) (transl env arg3)
            dbg
      | Pintarray ->
          int_array_set (transl env arg1) (transl env arg2) (transl env arg3)
            dbg
      | Pfloatarray ->
          float_array_set (transl env arg1) (transl env arg2)
            (transl_unbox_float dbg env arg3)
            dbg
      end)
  | Parraysets kind ->
      return_unit dbg (begin match kind with
      | Pgenarray ->
          bind "newval" (transl env arg3) (fun newval ->
          bind "index" (transl env arg2) (fun idx ->
          bind "arr" (transl env arg1) (fun arr ->
          bind "header" (get_header_without_profinfo arr dbg) (fun hdr ->
            if wordsize_shift = numfloat_shift then
              Csequence(make_checkbound dbg [addr_array_length hdr dbg; idx],
                        Cifthenelse(is_addr_array_hdr hdr dbg,
                                    dbg,
                                    addr_array_set arr idx newval dbg,
                                    dbg,
                                    float_array_set arr idx
                                                    (unbox_float dbg newval)
                                                    dbg,
                                    dbg))
            else
              Cifthenelse(is_addr_array_hdr hdr dbg,
                dbg,
                Csequence(make_checkbound dbg [addr_array_length hdr dbg; idx],
                          addr_array_set arr idx newval dbg),
                dbg,
                Csequence(make_checkbound dbg [float_array_length hdr dbg; idx],
                          float_array_set arr idx
                                          (unbox_float dbg newval) dbg),
                dbg)))))
      | Paddrarray ->
          bind "newval" (transl env arg3) (fun newval ->
          bind "index" (transl env arg2) (fun idx ->
          bind "arr" (transl env arg1) (fun arr ->
            Csequence(make_checkbound dbg [
              addr_array_length(get_header_without_profinfo arr dbg) dbg; idx],
                      addr_array_set arr idx newval dbg))))
      | Pintarray ->
          bind "newval" (transl env arg3) (fun newval ->
          bind "index" (transl env arg2) (fun idx ->
          bind "arr" (transl env arg1) (fun arr ->
            Csequence(make_checkbound dbg [
              addr_array_length(get_header_without_profinfo arr dbg) dbg; idx],
                      int_array_set arr idx newval dbg))))
      | Pfloatarray ->
          bind_load "newval" (transl_unbox_float dbg env arg3) (fun newval ->
          bind "index" (transl env arg2) (fun idx ->
          bind "arr" (transl env arg1) (fun arr ->
            Csequence(make_checkbound dbg [
              float_array_length (get_header_without_profinfo arr dbg) dbg;idx],
                      float_array_set arr idx newval dbg))))
      end)

  | Pbytes_set(size, unsafe) ->
     return_unit dbg
       (bind "str" (transl env arg1) (fun str ->
        bind "index" (untag_int (transl env arg2) dbg) (fun idx ->
        bind "newval" (transl_unbox_sized size dbg env arg3) (fun newval ->
          check_bound unsafe size dbg (string_length str dbg)
                      idx (unaligned_set size str idx newval dbg)))))

  | Pbigstring_set(size, unsafe) ->
     return_unit dbg
       (bind "ba" (transl env arg1) (fun ba ->
        bind "index" (untag_int (transl env arg2) dbg) (fun idx ->
        bind "newval" (transl_unbox_sized size dbg env arg3) (fun newval ->
        bind "ba_data"
             (Cop(Cload (Word_int, Mutable), [field_address ba 1 dbg], dbg))
             (fun ba_data ->
                check_bound unsafe size dbg (bigstring_length ba dbg)
                  idx (unaligned_set size ba_data idx newval dbg))))))

  | Pfield_computed | Psequand | Psequor | Pnot | Pnegint | Paddint
  | Psubint | Pmulint | Pandint | Porint | Pxorint | Plslint | Plsrint | Pasrint
  | Pintoffloat | Pfloatofint | Pnegfloat | Pabsfloat | Paddfloat | Psubfloat
  | Pmulfloat | Pdivfloat | Pstringlength | Pstringrefu | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytesrefs | Pisint | Pisout
  | Pbswap16 | Pint_as_pointer | Popaque | Pread_symbol _ | Pmakeblock (_, _, _)
  | Pfield _ | Psetfield (_, _, _) | Pfloatfield _ | Psetfloatfield (_, _)
  | Pduprecord (_, _) | Pccall _ | Praise _ | Pdivint _ | Pmodint _ | Pintcomp _
  | Poffsetint _ | Poffsetref _ | Pfloatcomp _ | Pmakearray (_, _)
  | Pduparray (_, _) | Parraylength _ | Parrayrefu _ | Parrayrefs _
  | Pbintofint _ | Pintofbint _ | Pcvtbint (_, _) | Pnegbint _ | Paddbint _
  | Psubbint _ | Pmulbint _ | Pdivbint _ | Pmodbint _ | Pandbint _ | Porbint _
  | Pxorbint _ | Plslbint _ | Plsrbint _ | Pasrbint _ | Pbintcomp (_, _)
  | Pbigarrayref (_, _, _, _) | Pbigarrayset (_, _, _, _) | Pbigarraydim _
  | Pstring_load _ | Pbytes_load _ | Pbigstring_load _ | Pbbswap _
    ->
      fatal_errorf "Cmmgen.transl_prim_3: %a"
        Printclambda_primitives.primitive p

and transl_unbox_float dbg env = function
    Uconst(Uconst_ref(_, Some (Uconst_float f))) -> Cconst_float (f, dbg)
  | exp -> unbox_float dbg (transl env exp)

and transl_unbox_int dbg env bi = function
    Uconst(Uconst_ref(_, Some (Uconst_int32 n))) ->
      Cconst_natint (Nativeint.of_int32 n, dbg)
  | Uconst(Uconst_ref(_, Some (Uconst_nativeint n))) ->
      Cconst_natint (n, dbg)
  | Uconst(Uconst_ref(_, Some (Uconst_int64 n))) ->
      if size_int = 8 then
        Cconst_natint (Int64.to_nativeint n, dbg)
      else begin
        let low = Int64.to_nativeint n in
        let high = Int64.to_nativeint (Int64.shift_right_logical n 32) in
        if big_endian then
          Ctuple [Cconst_natint (high, dbg); Cconst_natint (low, dbg)]
        else
          Ctuple [Cconst_natint (low, dbg); Cconst_natint (high, dbg)]
      end
  | Uprim(Pbintofint bi',[Uconst(Uconst_int i)],_) when bi = bi' ->
      Cconst_int (i, dbg)
  | exp -> unbox_int bi (transl env exp) dbg

and transl_unbox_number dbg env bn arg =
  match bn with
  | Boxed_float _ -> transl_unbox_float dbg env arg
  | Boxed_integer (bi, _) -> transl_unbox_int dbg env bi arg

and transl_unbox_sized size dbg env exp =
  match size with
  | Sixteen ->
     ignore_high_bit_int (untag_int (transl env exp) dbg)
  | Thirty_two -> transl_unbox_int dbg env Pint32 exp
  | Sixty_four -> transl_unbox_int dbg env Pint64 exp

and transl_let env str kind id exp body =
  let dbg = Debuginfo.none in
  let unboxing =
    (* If [id] is a mutable variable (introduced to eliminate a local
       reference) and it contains a type of unboxable numbers, then
       force unboxing.  Indeed, if not boxed, each assignment to the variable
       might require some boxing, but such local references are often
       used in loops and we really want to avoid repeated boxing. *)
    match str, kind with
    | Mutable, Pfloatval ->
        Boxed (Boxed_float dbg, false)
    | Mutable, Pboxedintval bi ->
        Boxed (Boxed_integer (bi, dbg), false)
    | _, (Pfloatval | Pboxedintval _) ->
        (* It would be safe to always unbox in this case, but
           we do it only if this indeed allows us to get rid of
           some allocations in the bound expression. *)
        is_unboxed_number ~strict:false env exp
    | _, Pgenval ->
        (* Here we don't know statically that the bound expression
           evaluates to an unboxable number type.  We need to be stricter
           and ensure that all possible branches in the expression
           return a boxed value (of the same kind).  Indeed, with GADTs,
           different branches could return different types. *)
        is_unboxed_number ~strict:true env exp
    | _, Pintval ->
        No_unboxing
  in
  match unboxing with
  | No_unboxing | Boxed (_, true) | No_result ->
      (* N.B. [body] must still be traversed even if [exp] will never return:
         there may be constant closures inside that need lifting out. *)
      Clet(id, transl env exp, transl env body)
  | Boxed (boxed_number, _false) ->
      let unboxed_id = V.create_local (VP.name id) in
      Clet(VP.create unboxed_id, transl_unbox_number dbg env boxed_number exp,
           transl (add_unboxed_id (VP.var id) unboxed_id boxed_number env) body)

and make_catch ncatch body handler dbg = match body with
| Cexit (nexit,[]) when nexit=ncatch -> handler
| _ ->  ccatch (ncatch, [], body, handler, dbg)

and is_shareable_cont exp =
  match exp with
  | Cexit (_,[]) -> true
  | _ -> false

and make_shareable_cont dbg mk exp =
  if is_shareable_cont exp then mk exp
  else begin
    let nfail = next_raise_count () in
    make_catch
      nfail
      (mk (Cexit (nfail,[])))
      exp
      dbg
  end

and transl_if env (approx : then_else)
      (dbg : Debuginfo.t) cond
      (then_dbg : Debuginfo.t) then_
      (else_dbg : Debuginfo.t) else_ =
  match cond with
  | Uconst (Uconst_ptr 0) -> else_
  | Uconst (Uconst_ptr 1) -> then_
  | Uifthenelse (arg1, arg2, Uconst (Uconst_ptr 0)) ->
      (* CR mshinwell: These Debuginfos will flow through from Clambda *)
      let inner_dbg = Debuginfo.none in
      let ifso_dbg = Debuginfo.none in
      transl_sequand env approx
        inner_dbg arg1
        ifso_dbg arg2
        then_dbg then_
        else_dbg else_
  | Uprim (Psequand, [arg1; arg2], inner_dbg) ->
      transl_sequand env approx
        inner_dbg arg1
        inner_dbg arg2
        then_dbg then_
        else_dbg else_
  | Uifthenelse (arg1, Uconst (Uconst_ptr 1), arg2) ->
      let inner_dbg = Debuginfo.none in
      let ifnot_dbg = Debuginfo.none in
      transl_sequor env approx
        inner_dbg arg1
        ifnot_dbg arg2
        then_dbg then_
        else_dbg else_
  | Uprim (Psequor, [arg1; arg2], inner_dbg) ->
      transl_sequor env approx
        inner_dbg arg1
        inner_dbg arg2
        then_dbg then_
        else_dbg else_
  | Uprim (Pnot, [arg], _dbg) ->
      transl_if env (invert_then_else approx)
        dbg arg
        else_dbg else_
        then_dbg then_
  | Uifthenelse (Uconst (Uconst_ptr 1), ifso, _) ->
      let ifso_dbg = Debuginfo.none in
      transl_if env approx
        ifso_dbg ifso
        then_dbg then_
        else_dbg else_
  | Uifthenelse (Uconst (Uconst_ptr 0), _, ifnot) ->
      let ifnot_dbg = Debuginfo.none in
      transl_if env approx
        ifnot_dbg ifnot
        then_dbg then_
        else_dbg else_
  | Uifthenelse (cond, ifso, ifnot) ->
      let inner_dbg = Debuginfo.none in
      let ifso_dbg = Debuginfo.none in
      let ifnot_dbg = Debuginfo.none in
      make_shareable_cont then_dbg
        (fun shareable_then ->
           make_shareable_cont else_dbg
             (fun shareable_else ->
                mk_if_then_else
                  inner_dbg (test_bool inner_dbg (transl env cond))
                  ifso_dbg (transl_if env approx
                    ifso_dbg ifso
                    then_dbg shareable_then
                    else_dbg shareable_else)
                  ifnot_dbg (transl_if env approx
                    ifnot_dbg ifnot
                    then_dbg shareable_then
                    else_dbg shareable_else))
             else_)
        then_
  | _ -> begin
      match approx with
      | Then_true_else_false ->
          transl env cond
      | Then_false_else_true ->
          mk_not dbg (transl env cond)
      | Unknown ->
          mk_if_then_else
            dbg (test_bool dbg (transl env cond))
            then_dbg then_
            else_dbg else_
    end

and transl_sequand env (approx : then_else)
      (arg1_dbg : Debuginfo.t) arg1
      (arg2_dbg : Debuginfo.t) arg2
      (then_dbg : Debuginfo.t) then_
      (else_dbg : Debuginfo.t) else_ =
  make_shareable_cont else_dbg
    (fun shareable_else ->
       transl_if env Unknown
         arg1_dbg arg1
         arg2_dbg (transl_if env approx
           arg2_dbg arg2
           then_dbg then_
           else_dbg shareable_else)
         else_dbg shareable_else)
    else_

and transl_sequor env (approx : then_else)
      (arg1_dbg : Debuginfo.t) arg1
      (arg2_dbg : Debuginfo.t) arg2
      (then_dbg : Debuginfo.t) then_
      (else_dbg : Debuginfo.t) else_ =
  make_shareable_cont then_dbg
    (fun shareable_then ->
       transl_if env Unknown
         arg1_dbg arg1
         then_dbg shareable_then
         arg2_dbg (transl_if env approx
           arg2_dbg arg2
           then_dbg shareable_then
           else_dbg else_))
    then_

(* This assumes that [arg] can be safely discarded if it is not used. *)
and transl_switch loc env arg index cases = match Array.length cases with
| 0 -> fatal_error "Cmmgen.transl_switch"
| 1 -> transl env cases.(0)
| _ ->
    let cases = Array.map (transl env) cases in
    let store = StoreExpForSwitch.mk_store () in
    let index =
      Array.map
        (fun j -> store.Switch.act_store j cases.(j))
        index in
    let n_index = Array.length index in
    let inters = ref []
    and this_high = ref (n_index-1)
    and this_low = ref (n_index-1)
    and this_act = ref index.(n_index-1) in
    for i = n_index-2 downto 0 do
      let act = index.(i) in
      if act = !this_act then
        decr this_low
      else begin
        inters := (!this_low, !this_high, !this_act) :: !inters ;
        this_high := i ;
        this_low := i ;
        this_act := act
      end
    done ;
    inters := (0, !this_high, !this_act) :: !inters ;
    match !inters with
    | [_] -> cases.(0)
    | inters ->
        bind "switcher" arg
          (fun a ->
            SwitcherBlocks.zyva
              loc
              (0,n_index-1)
              a
              (Array.of_list inters) store)

and transl_letrec env bindings cont =
  let dbg = Debuginfo.none in
  let bsz =
    List.map (fun (id, exp) -> (id, exp, expr_size V.empty exp))
      bindings
  in
  let op_alloc prim args =
    Cop(Cextcall(prim, typ_val, true, None), args, dbg) in
  let rec init_blocks = function
    | [] -> fill_nonrec bsz
    | (id, _exp, RHS_block sz) :: rem ->
        Clet(id, op_alloc "caml_alloc_dummy" [int_const dbg sz],
          init_blocks rem)
    | (id, _exp, RHS_infix { blocksize; offset}) :: rem ->
        Clet(id, op_alloc "caml_alloc_dummy_infix"
             [int_const dbg blocksize; int_const dbg offset],
             init_blocks rem)
    | (id, _exp, RHS_floatblock sz) :: rem ->
        Clet(id, op_alloc "caml_alloc_dummy_float" [int_const dbg sz],
          init_blocks rem)
    | (id, _exp, RHS_nonrec) :: rem ->
        Clet (id, Cconst_int (0, dbg), init_blocks rem)
  and fill_nonrec = function
    | [] -> fill_blocks bsz
    | (_id, _exp,
       (RHS_block _ | RHS_infix _ | RHS_floatblock _)) :: rem ->
        fill_nonrec rem
    | (id, exp, RHS_nonrec) :: rem ->
        Clet(id, transl env exp, fill_nonrec rem)
  and fill_blocks = function
    | [] -> cont
    | (id, exp, (RHS_block _ | RHS_infix _ | RHS_floatblock _)) :: rem ->
        let op =
          Cop(Cextcall("caml_update_dummy", typ_void, false, None),
              [Cvar (VP.var id); transl env exp], dbg) in
        Csequence(op, fill_blocks rem)
    | (_id, _exp, RHS_nonrec) :: rem ->
        fill_blocks rem
  in init_blocks bsz

(* Translate a function definition *)

let transl_function ~ppf_dump f =
  let body =
    if Config.flambda then
      Un_anf.apply ~ppf_dump f.body ~what:f.label
    else
      f.body
  in
  let cmm_body =
    let env = create_env ~environment_param:f.env in
    if !Clflags.afl_instrument then
      Afl_instrument.instrument_function (transl env body) f.dbg
    else
      transl env body in
  let fun_codegen_options =
    if !Clflags.optimize_for_speed then
      []
    else
      [ Reduce_code_size ]
  in
  Cfunction {fun_name = f.label;
             fun_args = List.map (fun (id, _) -> (id, typ_val)) f.params;
             fun_body = cmm_body;
             fun_codegen_options;
             fun_dbg  = f.dbg}

(* Translate all function definitions *)

let rec transl_all_functions ~ppf_dump already_translated cont =
  match Cmmgen_state.next_function () with
  | None -> cont, already_translated
  | Some f ->
    let sym = f.label in
    if String.Set.mem sym already_translated then
      transl_all_functions ~ppf_dump already_translated cont
    else begin
      transl_all_functions ~ppf_dump
        (String.Set.add sym already_translated)
        ((f.dbg, transl_function ~ppf_dump f) :: cont)
    end

(* Emit constant closures *)

let emit_constant_closure ((_, global_symb) as symb) fundecls clos_vars cont =
  let closure_symbol f =
    if Config.flambda then
      cdefine_symbol (f.label ^ "_closure", global_symb)
    else
      []
  in
  match fundecls with
    [] ->
      (* This should probably not happen: dead code has normally been
         eliminated and a closure cannot be accessed without going through
         a [Project_closure], which depends on the function. *)
      assert (clos_vars = []);
      cdefine_symbol symb @
        List.fold_right emit_constant clos_vars cont
  | f1 :: remainder ->
      let rec emit_others pos = function
          [] ->
            List.fold_right emit_constant clos_vars cont
      | f2 :: rem ->
          if f2.arity = 1 || f2.arity = 0 then
            Cint(infix_header pos) ::
            (closure_symbol f2) @
            Csymbol_address f2.label ::
            cint_const f2.arity ::
            emit_others (pos + 3) rem
          else
            Cint(infix_header pos) ::
            (closure_symbol f2) @
            Csymbol_address(curry_function f2.arity) ::
            cint_const f2.arity ::
            Csymbol_address f2.label ::
            emit_others (pos + 4) rem in
      Cint(black_closure_header (fundecls_size fundecls
                                 + List.length clos_vars)) ::
      cdefine_symbol symb @
      (closure_symbol f1) @
      if f1.arity = 1 || f1.arity = 0 then
        Csymbol_address f1.label ::
        cint_const f1.arity ::
        emit_others 3 remainder
      else
        Csymbol_address(curry_function f1.arity) ::
        cint_const f1.arity ::
        Csymbol_address f1.label ::
        emit_others 4 remainder

(* Emit constant blocks *)

let emit_constant_table symb elems =
  cdefine_symbol symb @
  elems

(* Emit all structured constants *)

let transl_clambda_constants (constants : Clambda.preallocated_constant list)
      cont =
  let c = ref cont in
  let emit_clambda_constant symbol global cst =
     let cst = emit_structured_constant (symbol, global) cst [] in
     c := (cdata cst) :: !c
  in
  List.iter
    (fun { symbol; exported; definition = cst; provenance = _; } ->
       let global : Cmmgen_state.is_global =
         if exported then Global else Local
       in
       emit_clambda_constant symbol global cst)
    constants;
  !c

let emit_cmm_data_items_for_constants cont =
  let c = ref cont in
  String.Map.iter (fun symbol (cst : Cmmgen_state.constant) ->
      match cst with
      | Const_closure (global, fundecls, clos_vars) ->
          let cmm =
            emit_constant_closure (symbol, global) fundecls clos_vars []
          in
          c := (cdata cmm) :: !c
      | Const_table (global, elems) ->
          c := (cdata (emit_constant_table (symbol, global) elems)) :: !c)
    (Cmmgen_state.constants ());
  cdata (Cmmgen_state.data_items ()) :: !c

let transl_all_functions ~ppf_dump cont =
  let rec aux already_translated cont translated_functions =
    if Cmmgen_state.no_more_functions ()
    then cont, translated_functions
    else
      let translated_functions, already_translated =
        transl_all_functions ~ppf_dump already_translated translated_functions
      in
      aux already_translated cont translated_functions
  in
  let cont, translated_functions =
    aux String.Set.empty cont []
  in
  let translated_functions =
    (* Sort functions according to source position *)
    List.map snd
      (List.sort (fun (dbg1, _) (dbg2, _) ->
           Debuginfo.compare dbg1 dbg2) translated_functions)
  in
  translated_functions @ cont

(* Build the NULL terminated array of gc roots *)

let emit_gc_roots_table ~symbols cont =
  let table_symbol = Compilenv.make_symbol (Some "gc_roots") in
  cdata(Cglobal_symbol table_symbol ::
        Cdefine_symbol table_symbol ::
        List.map (fun s -> Csymbol_address s) symbols @
        [Cint 0n])
  :: cont

(* Build preallocated blocks (used for Flambda [Initialize_symbol]
   constructs, and Clambda global module) *)

let preallocate_block cont { Clambda.symbol; exported; tag; fields } =
  let space =
    (* These words will be registered as roots and as such must contain
       valid values, in case we are in no-naked-pointers mode.  Likewise
       the block header must be black, below (see [caml_darken]), since
       the overall record may be referenced. *)
    List.map (fun field ->
        match field with
        | None ->
            Cint (Nativeint.of_int 1 (* Val_unit *))
        | Some (Uconst_field_int n) ->
            cint_const n
        | Some (Uconst_field_ref label) ->
            Csymbol_address label)
      fields
  in
  let data =
    Cint(black_block_header tag (List.length fields)) ::
    if exported then
      Cglobal_symbol symbol ::
      Cdefine_symbol symbol :: space
    else
      Cdefine_symbol symbol :: space
  in
  let section =
    if !Clflags.data_sections then
      Some { name = symbol; flags = Default; args = Default_args }
    else None
  in
  Cdata { section; items=data } :: cont

let emit_preallocated_blocks preallocated_blocks cont =
  let symbols =
    List.map (fun ({ Clambda.symbol }:Clambda.preallocated_block) -> symbol)
      preallocated_blocks
  in
  let c1 = emit_gc_roots_table ~symbols cont in
  List.fold_left preallocate_block c1 preallocated_blocks

(* Translate a compilation unit *)

let compunit ~ppf_dump (ulam, preallocated_blocks, constants) =
  let dbg = Debuginfo.none in
  let init_code =
    if !Clflags.afl_instrument then
      Afl_instrument.instrument_initialiser (transl empty_env ulam)
        (fun () -> dbg)
    else
      transl empty_env ulam in
  let c1 = [Cfunction {fun_name = Compilenv.make_symbol (Some "entry");
                       fun_args = [];
                       fun_body = init_code;
                       (* This function is often large and run only once.
                          Compilation time matter more than runtime.
                          See MPR#7630 *)
                       fun_codegen_options =
                         if Config.flambda then [
                           Reduce_code_size;
                           No_CSE;
                         ]
                         else [ Reduce_code_size ];
                       fun_dbg  = Debuginfo.none }] in
  let c2 = transl_clambda_constants constants c1 in
  let c3 = transl_all_functions ~ppf_dump c2 in
  let c4 = emit_preallocated_blocks preallocated_blocks c3 in
  emit_cmm_data_items_for_constants c4

(*
CAMLprim value caml_cache_public_method (value meths, value tag, value *cache)
{
  int li = 3, hi = Field(meths,0), mi;
  while (li < hi) { // no need to check the 1st time
    mi = ((li+hi) >> 1) | 1;
    if (tag < Field(meths,mi)) hi = mi-2;
    else li = mi;
  }
  *cache = (li-3)*sizeof(value)+1;
  return Field (meths, li-1);
}
*)

let cache_public_method meths tag cache dbg =
  let raise_num = next_raise_count () in
  let cconst_int i = Cconst_int (i, dbg) in
  let li = V.create_local "*li*" and hi = V.create_local "*hi*"
  and mi = V.create_local "*mi*" and tagged = V.create_local "*tagged*" in
  Clet (
  VP.create li, cconst_int 3,
  Clet (
  VP.create hi, Cop(Cload (Word_int, Mutable), [meths], dbg),
  Csequence(
  ccatch
    (raise_num, [],
     create_loop
       (Clet(
        VP.create mi,
        Cop(Cor,
            [Cop(Clsr, [Cop(Caddi, [Cvar li; Cvar hi], dbg); cconst_int 1],
               dbg);
             cconst_int 1],
            dbg),
        Csequence(
        Cifthenelse
          (Cop (Ccmpi Clt,
                [tag;
                 Cop(Cload (Word_int, Mutable),
                     [Cop(Cadda,
                          [meths; lsl_const (Cvar mi) log2_size_addr dbg],
                          dbg)],
                     dbg)], dbg),
          dbg, Cassign(hi, Cop(Csubi, [Cvar mi; cconst_int 2], dbg)),
          dbg, Cassign(li, Cvar mi),
          dbg),
        Cifthenelse
          (Cop(Ccmpi Cge, [Cvar li; Cvar hi], dbg),
           dbg, Cexit (raise_num, []),
           dbg, Ctuple [],
           dbg))))
       dbg,
     Ctuple [],
     dbg),
  Clet (
    VP.create tagged,
      Cop(Cadda, [lsl_const (Cvar li) log2_size_addr dbg;
        cconst_int(1 - 3 * size_addr)], dbg),
    Csequence(Cop (Cstore (Word_int, Assignment), [cache; Cvar tagged], dbg),
              Cvar tagged)))))

(* CR mshinwell: These will be filled in by later pull requests. *)
let placeholder_dbg () = Debuginfo.none
let placeholder_fun_dbg ~human_name:_ = Debuginfo.none

(* Generate an application function:
     (defun caml_applyN (a1 ... aN clos)
       (if (= clos.arity N)
         (app clos.direct a1 ... aN clos)
         (let (clos1 (app clos.code a1 clos)
               clos2 (app clos1.code a2 clos)
               ...
               closN-1 (app closN-2.code aN-1 closN-2))
           (app closN-1.code aN closN-1))))
*)

let apply_function_body arity =
  let dbg = placeholder_dbg in
  let arg = Array.make arity (V.create_local "arg") in
  for i = 1 to arity - 1 do arg.(i) <- V.create_local "arg" done;
  let clos = V.create_local "clos" in
  let env = empty_env in
  let rec app_fun clos n =
    if n = arity-1 then
      Cop(Capply typ_val,
          [get_field env (Cvar clos) 0 (dbg ()); Cvar arg.(n); Cvar clos],
          dbg ())
    else begin
      let newclos = V.create_local "clos" in
      Clet(VP.create newclos,
           Cop(Capply typ_val,
               [get_field env (Cvar clos) 0 (dbg ()); Cvar arg.(n); Cvar clos],
               dbg ()),
           app_fun newclos (n+1))
    end in
  let args = Array.to_list arg in
  let all_args = args @ [clos] in
  (args, clos,
   if arity = 1 then app_fun clos 0 else
   Cifthenelse(
   Cop(Ccmpi Ceq,
     [get_field env (Cvar clos) 1 (dbg ()); int_const (dbg ()) arity], dbg ()),
   dbg (),
   Cop(Capply typ_val,
       get_field env (Cvar clos) 2 (dbg ())
         :: List.map (fun s -> Cvar s) all_args,
       dbg ()),
   dbg (),
   app_fun clos 0,
   dbg ()))

let send_function arity =
  let dbg = placeholder_dbg in
  let cconst_int i = Cconst_int (i, dbg ()) in
  let (args, clos', body) = apply_function_body (1+arity) in
  let cache = V.create_local "cache"
  and obj = List.hd args
  and tag = V.create_local "tag" in
  let env = empty_env in
  let clos =
    let cache = Cvar cache and obj = Cvar obj and tag = Cvar tag in
    let meths = V.create_local "meths" and cached = V.create_local "cached" in
    let real = V.create_local "real" in
    let mask = get_field env (Cvar meths) 1 (dbg ()) in
    let cached_pos = Cvar cached in
    let tag_pos = Cop(Cadda, [Cop (Cadda, [cached_pos; Cvar meths], dbg ());
                              cconst_int(3*size_addr-1)], dbg ()) in
    let tag' = Cop(Cload (Word_int, Mutable), [tag_pos], dbg ()) in
    Clet (
    VP.create meths, Cop(Cload (Word_val, Mutable), [obj], dbg ()),
    Clet (
    VP.create cached,
      Cop(Cand, [Cop(Cload (Word_int, Mutable), [cache], dbg ()); mask],
          dbg ()),
    Clet (
    VP.create real,
    Cifthenelse(Cop(Ccmpa Cne, [tag'; tag], dbg ()),
                dbg (),
                cache_public_method (Cvar meths) tag cache (dbg ()),
                dbg (),
                cached_pos,
                dbg ()),
    Cop(Cload (Word_val, Mutable),
      [Cop(Cadda, [Cop (Cadda, [Cvar real; Cvar meths], dbg ());
       cconst_int(2*size_addr-1)], dbg ())], dbg ()))))

  in
  let body = Clet(VP.create clos', clos, body) in
  let cache = cache in
  let fun_name = "caml_send" ^ Int.to_string arity in
  let fun_args =
    [obj, typ_val; tag, typ_int; cache, typ_val]
    @ List.map (fun id -> (id, typ_val)) (List.tl args) in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction
   {fun_name;
    fun_args = List.map (fun (arg, ty) -> VP.create arg, ty) fun_args;
    fun_body = body;
    fun_codegen_options = [];
    fun_dbg;
   }

let apply_function arity =
  let (args, clos, body) = apply_function_body arity in
  let all_args = args @ [clos] in
  let fun_name = "caml_apply" ^ Int.to_string arity in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction
   {fun_name;
    fun_args = List.map (fun arg -> (VP.create arg, typ_val)) all_args;
    fun_body = body;
    fun_codegen_options = [];
    fun_dbg;
   }

(* Generate tuplifying functions:
      (defun caml_tuplifyN (arg clos)
        (app clos.direct #0(arg) ... #N-1(arg) clos)) *)

let tuplify_function arity =
  let dbg = placeholder_dbg in
  let arg = V.create_local "arg" in
  let clos = V.create_local "clos" in
  let env = empty_env in
  let rec access_components i =
    if i >= arity
    then []
    else get_field env (Cvar arg) i (dbg ()) :: access_components(i+1) in
  let fun_name = "caml_tuplify" ^ Int.to_string arity in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction
   {fun_name;
    fun_args = [VP.create arg, typ_val; VP.create clos, typ_val];
    fun_body =
      Cop(Capply typ_val,
          get_field env (Cvar clos) 2 (dbg ())
            :: access_components 0 @ [Cvar clos],
          dbg ());
    fun_codegen_options = [];
    fun_dbg;
   }

(* Generate currying functions:
      (defun caml_curryN (arg clos)
         (alloc HDR caml_curryN_1 <arity (N-1)> caml_curry_N_1_app arg clos))
      (defun caml_curryN_1 (arg clos)
         (alloc HDR caml_curryN_2 <arity (N-2)> caml_curry_N_2_app arg clos))
      ...
      (defun caml_curryN_N-1 (arg clos)
         (let (closN-2 clos.vars[1]
               closN-3 closN-2.vars[1]
               ...
               clos1 clos2.vars[1]
               clos clos1.vars[1])
           (app clos.direct
                clos1.vars[0] ... closN-2.vars[0] clos.vars[0] arg clos)))

    Special "shortcut" functions are also generated to handle the
    case where a partially applied function is applied to all remaining
    arguments in one go.  For instance:
      (defun caml_curry_N_1_app (arg2 ... argN clos)
        (let clos' clos.vars[1]
           (app clos'.direct clos.vars[0] arg2 ... argN clos')))

    Those shortcuts may lead to a quadratic number of application
    primitives being generated in the worst case, which resulted in
    linking time blowup in practice (PR#5933), so we only generate and
    use them when below a fixed arity 'max_arity_optimized'.
*)

let max_arity_optimized = 15
let final_curry_function arity =
  let dbg = placeholder_dbg in
  let last_arg = V.create_local "arg" in
  let last_clos = V.create_local "clos" in
  let env = empty_env in
  let rec curry_fun args clos n =
    if n = 0 then
      Cop(Capply typ_val,
          get_field env (Cvar clos) 2 (dbg ()) ::
            args @ [Cvar last_arg; Cvar clos],
          dbg ())
    else
      if n = arity - 1 || arity > max_arity_optimized then
        begin
      let newclos = V.create_local "clos" in
      Clet(VP.create newclos,
           get_field env (Cvar clos) 3 (dbg ()),
           curry_fun (get_field env (Cvar clos) 2 (dbg ()) :: args)
             newclos (n-1))
        end else
        begin
          let newclos = V.create_local "clos" in
          Clet(VP.create newclos,
               get_field env (Cvar clos) 4 (dbg ()),
               curry_fun (get_field env (Cvar clos) 3 (dbg ()) :: args)
                         newclos (n-1))
    end in
  let fun_name =
    "caml_curry" ^ Int.to_string arity ^ "_" ^ Int.to_string (arity-1)
  in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction
   {fun_name;
    fun_args = [VP.create last_arg, typ_val; VP.create last_clos, typ_val];
    fun_body = curry_fun [] last_clos (arity-1);
    fun_codegen_options = [];
    fun_dbg;
   }

let rec intermediate_curry_functions arity num =
  let dbg = placeholder_dbg in
  let env = empty_env in
  if num = arity - 1 then
    [final_curry_function arity]
  else begin
    let name1 = "caml_curry" ^ Int.to_string arity in
    let name2 = if num = 0 then name1 else name1 ^ "_" ^ Int.to_string num in
    let arg = V.create_local "arg" and clos = V.create_local "clos" in
    let fun_dbg = placeholder_fun_dbg ~human_name:name2 in
    Cfunction
     {fun_name = name2;
      fun_args = [VP.create arg, typ_val; VP.create clos, typ_val];
      fun_body =
         if arity - num > 2 && arity <= max_arity_optimized then
           Cop(Calloc,
               [alloc_closure_header 5 Debuginfo.none;
                Cconst_symbol(name1 ^ "_" ^ Int.to_string (num+1), dbg ());
                int_const (dbg ()) (arity - num - 1);
                Cconst_symbol(name1 ^ "_" ^ Int.to_string (num+1) ^ "_app",
                  dbg ());
                Cvar arg; Cvar clos],
               dbg ())
         else
           Cop(Calloc,
                [alloc_closure_header 4 (dbg ());
                 Cconst_symbol(name1 ^ "_" ^ Int.to_string (num+1), dbg ());
                 int_const (dbg ()) 1; Cvar arg; Cvar clos],
                dbg ());
      fun_codegen_options = [];
      fun_dbg;
     }
    ::
      (if arity <= max_arity_optimized && arity - num > 2 then
          let rec iter i =
            if i <= arity then
              let arg = V.create_local (Printf.sprintf "arg%d" i) in
              (arg, typ_val) :: iter (i+1)
            else []
          in
          let direct_args = iter (num+2) in
          let rec iter i args clos =
            if i = 0 then
              Cop(Capply typ_val,
                  (get_field env (Cvar clos) 2 (dbg ())) :: args @ [Cvar clos],
                  dbg ())
            else
              let newclos = V.create_local "clos" in
              Clet(VP.create newclos,
                   get_field env (Cvar clos) 4 (dbg ()),
                   iter (i-1) (get_field env (Cvar clos) 3 (dbg ()) :: args)
                     newclos)
          in
          let fun_args =
            List.map (fun (arg, ty) -> VP.create arg, ty)
              (direct_args @ [clos, typ_val])
          in
          let fun_name = name1 ^ "_" ^ Int.to_string (num+1) ^ "_app" in
          let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
          let cf =
            Cfunction
              {fun_name;
               fun_args;
               fun_body = iter (num+1)
                  (List.map (fun (arg,_) -> Cvar arg) direct_args) clos;
               fun_codegen_options = [];
               fun_dbg;
              }
          in
          cf :: intermediate_curry_functions arity (num+1)
       else
          intermediate_curry_functions arity (num+1))
  end

let curry_function arity =
  assert(arity <> 0);
  (* Functions with arity = 0 does not have a curry_function *)
  if arity > 0
  then intermediate_curry_functions arity 0
  else [tuplify_function (-arity)]

module Int = Numbers.Int

let default_apply = Int.Set.add 2 (Int.Set.add 3 Int.Set.empty)
  (* These apply funs are always present in the main program because
     the run-time system needs them (cf. runtime/<arch>.S) . *)

let generic_functions shared units =
  let (apply,send,curry) =
    List.fold_left
      (fun (apply,send,curry) ui ->
         List.fold_right Int.Set.add ui.ui_apply_fun apply,
         List.fold_right Int.Set.add ui.ui_send_fun send,
         List.fold_right Int.Set.add ui.ui_curry_fun curry)
      (Int.Set.empty,Int.Set.empty,Int.Set.empty)
      units in
  let apply = if shared then apply else Int.Set.union apply default_apply in
  let accu = Int.Set.fold (fun n accu -> apply_function n :: accu) apply [] in
  let accu = Int.Set.fold (fun n accu -> send_function n :: accu) send accu in
  Int.Set.fold (fun n accu -> curry_function n @ accu) curry accu

(* Generate the entry point *)

let entry_point namelist =
  let dbg = placeholder_dbg in
  let cconst_int i = Cconst_int (i, dbg ()) in
  let cconst_symbol sym = Cconst_symbol (sym, dbg ()) in
  let incr_global_inited () =
    Cop(Cstore (Word_int, Assignment),
        [cconst_symbol "caml_globals_inited";
         Cop(Caddi, [Cop(Cload (Word_int, Mutable),
                       [cconst_symbol "caml_globals_inited"], dbg ());
                     cconst_int 1], dbg ())], dbg ()) in
  let body =
    List.fold_right
      (fun name next ->
        let entry_sym = Compilenv.make_symbol ~unitname:name (Some "entry") in
        Csequence(Cop(Capply typ_void,
                         [cconst_symbol entry_sym], dbg ()),
                  Csequence(incr_global_inited (), next)))
      namelist (cconst_int 1) in
  let fun_name = "caml_program" in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction {fun_name;
             fun_args = [];
             fun_body = body;
             fun_codegen_options = [Reduce_code_size];
             fun_dbg;
            }

(* Generate the table of globals *)

let cint_zero = Cint 0n

let global_table namelist =
  let mksym name =
    Csymbol_address (Compilenv.make_symbol ~unitname:name (Some "gc_roots"))
  in
  cdata(Cglobal_symbol "caml_globals" ::
        Cdefine_symbol "caml_globals" ::
        List.map mksym namelist @
        [cint_zero])

let reference_symbols namelist =
  let mksym name = Csymbol_address name in
  cdata(List.map mksym namelist)

let global_data name v =
  cdata(emit_structured_constant (name, Global)
          (Uconst_string (Marshal.to_string v [])) [])

let globals_map v = global_data "caml_globals_map" v

(* Generate the master table of frame descriptors *)

let frame_table namelist =
  let mksym name =
    Csymbol_address (Compilenv.make_symbol ~unitname:name (Some "frametable"))
  in
  cdata(Cglobal_symbol "caml_frametable" ::
        Cdefine_symbol "caml_frametable" ::
        List.map mksym namelist
        @ [cint_zero])

(* Generate the master table of Spacetime shapes *)

let spacetime_shapes namelist =
  let mksym name =
    Csymbol_address (
      Compilenv.make_symbol ~unitname:name (Some "spacetime_shapes"))
  in
  cdata(Cglobal_symbol "caml_spacetime_shapes" ::
        Cdefine_symbol "caml_spacetime_shapes" ::
        List.map mksym namelist
        @ [cint_zero])

(* Generate the table of module data and code segments *)

let segment_table namelist symbol begname endname =
  let addsyms name lst =
    Csymbol_address (Compilenv.make_symbol ~unitname:name (Some begname)) ::
    Csymbol_address (Compilenv.make_symbol ~unitname:name (Some endname)) ::
    lst
  in
  cdata(Cglobal_symbol symbol ::
        Cdefine_symbol symbol ::
        List.fold_right addsyms namelist [cint_zero])

let data_segment_table namelist =
  segment_table namelist "caml_data_segments" "data_begin" "data_end"

let code_segment_table namelist =
  segment_table namelist "caml_code_segments" "code_begin" "code_end"

(* Initialize a predefined exception *)

let predef_exception i name =
  let name_sym = Compilenv.new_const_symbol () in
  let data_items =
    emit_block name_sym Local (string_header (String.length name))
      (emit_string_constant name [])
  in
  let exn_sym = "caml_exn_" ^ name in
  let tag = Obj.object_tag in
  let size = 2 in
  let fields =
    (Csymbol_address name_sym)
      :: (cint_const (-i - 1))
      :: data_items
  in
  let data_items = emit_block exn_sym Global (block_header tag size) fields in
  cdata data_items

(* Header for a plugin *)

let plugin_header units =
  let mk (ui,crc) =
    { dynu_name = ui.ui_name;
      dynu_crc = crc;
      dynu_imports_cmi = ui.ui_imports_cmi;
      dynu_imports_cmx = ui.ui_imports_cmx;
      dynu_defines = ui.ui_defines
    } in
  global_data "caml_plugin_header"
    { dynu_magic = Config.cmxs_magic_number; dynu_units = List.map mk units }

let reset () =
  Cmmgen_state.reset ()
