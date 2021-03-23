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

(* Second intermediate language (machine independent) *)

type machtype_component =
  | Val
  | Addr
  | Int
  | Float

(* - [Val] denotes a valid OCaml value: either a pointer to the beginning
     of a heap block, an infix pointer if it is preceded by the correct
     infix header, or a 2n+1 encoded integer.
   - [Int] is for integers (not necessarily 2n+1 encoded) and for
     pointers outside the heap.
   - [Addr] denotes pointers that are neither [Val] nor [Int], i.e.
     pointers into the heap that point in the middle of a heap block.
     Such derived pointers are produced by e.g. array indexing.
   - [Float] is for unboxed floating-point numbers.

The purpose of these types is twofold.  First, they guide register
allocation: type [Float] goes in FP registers, the other types go
into integer registers.  Second, they determine how local variables are
tracked by the GC:
   - Variables of type [Val] are GC roots.  If they are pointers, the
     GC will not deallocate the addressed heap block, and will update
     the local variable if the heap block moves.
   - Variables of type [Int] and [Float] are ignored by the GC.
     The GC does not change their values.
   - Variables of type [Addr] must never be live across an allocation
     point or function call.  They cannot be given as roots to the GC
     because they don't point after a well-formed block header of the
     kind that the GC needs.  However, the GC may move the block pointed
     into, invalidating the value of the [Addr] variable.
*)

type machtype = machtype_component array

val typ_void: machtype
val typ_val: machtype
val typ_addr: machtype
val typ_int: machtype
val typ_float: machtype

(** Least upper bound of two [machtype_component]s. *)
val lub_component
   : machtype_component
  -> machtype_component
  -> machtype_component

(** Returns [true] iff the first supplied [machtype_component] is greater than
    or equal to the second under the relation used by [lub_component]. *)
val ge_component
   : machtype_component
  -> machtype_component
  -> bool

type integer_comparison = Lambda.integer_comparison =
  | Ceq | Cne | Clt | Cgt | Cle | Cge

val negate_integer_comparison: integer_comparison -> integer_comparison
val swap_integer_comparison: integer_comparison -> integer_comparison

type float_comparison = Lambda.float_comparison =
  | CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

val negate_float_comparison: float_comparison -> float_comparison
val swap_float_comparison: float_comparison -> float_comparison

type label = int
val new_label: unit -> label
val set_label: label -> unit
val cur_label: unit -> label

type rec_flag = Nonrecursive | Recursive

(* XCR mshinwell: Rename to [prefetch_temporal_locality] so it's clear what
   this refers to. *)
type prefetch_temporal_locality_hint = Not_at_all | Low | Moderate | High

type phantom_defining_expr =
  (* CR-soon mshinwell: Convert this to [Targetint.OCaml.t] (or whatever the
     representation of "target-width OCaml integers of type [int]"
     becomes when merged). *)
  | Cphantom_const_int of Targetint.t
  (** The phantom-let-bound variable is a constant integer.
      The argument must be the tagged representation of an integer within
      the range of type [int] on the target.  (Analogously to [Cconst_int].) *)
  | Cphantom_const_symbol of string
  (** The phantom-let-bound variable is an alias for a symbol. *)
  | Cphantom_var of Backend_var.t
  (** The phantom-let-bound variable is an alias for another variable.  The
      aliased variable must not be a bound by a phantom let. *)
  | Cphantom_offset_var of { var : Backend_var.t; offset_in_words : int; }
  (** The phantom-let-bound-variable's value is defined by adding the given
      number of words to the pointer contained in the given identifier. *)
  | Cphantom_read_field of { var : Backend_var.t; field : int; }
  (** The phantom-let-bound-variable's value is found by adding the given
      number of words to the pointer contained in the given identifier, then
      dereferencing. *)
  | Cphantom_read_symbol_field of { sym : string; field : int; }
  (** As for [Uphantom_read_var_field], but with the pointer specified by
      a symbol. *)
  | Cphantom_block of { tag : int; fields : Backend_var.t list; }
  (** The phantom-let-bound variable points at a block with the given
      structure. *)

(* CR gyorsh for mshinwell: not sure this halfway house.
   On one hand, Cmm.effects does not have any use for
   Primitive.effects.Only_generative_effects which
   is abstracted in cmmgen to Cmm.effects.Arbitrary_effects.
   On the other hand, Cmm.effects does not express the more refined effects
   available in Selectgen.Effect.Raise and Selectgen.Coeffect.Read_mutable.
   An obvious solution is to copy Primitive.effects over as is, but it
   will be confusing to have it around and not used.
*)
type effects = No_effects | Arbitrary_effects
type coeffects = No_coeffects | Has_coeffects
(* XCR mshinwell: Please pull out the change to use a record for Cextcall
   into a separate patch.  (Also, just to check, I presume there's some
   reason why this can't be an inline record?)

   gyorsh: I didn't make an inline record because it is long.
   Should I change it to an inline record?
   ok, realized that inline record saves one dereference.
   fixed. I'll make a separate patch for this change when rebasing the whole lot
   onto flambda-backend.
*)
type memory_chunk =
    Byte_unsigned
  | Byte_signed
  | Sixteen_unsigned
  | Sixteen_signed
  | Thirtytwo_unsigned
  | Thirtytwo_signed
  | Word_int                           (* integer or pointer outside heap *)
  | Word_val                           (* pointer inside heap or encoded int *)
  | Single
  | Double                             (* 64-bit-aligned 64-bit float *)
  | Double_u                           (* word-aligned 64-bit float *)

type bswap_width_in_bits =
  | Sixteen
  | Thirtytwo
  | Sixtyfour

and operation =
    Capply of machtype
  | Cextcall of
      { name: string;
        ret: machtype;
        alloc: bool;
        builtin: bool;
        effects: effects;
        coeffects: coeffects;
        label_after: label option;
        (** If specified, the given label will be placed immediately after the
            call (at the same place as any frame descriptor would reference). *)
      }
  | Cload of memory_chunk * Asttypes.mutable_flag
  | Calloc
  | Cstore of memory_chunk * Lambda.initialization_or_assignment
  | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  (* CR mshinwell: Why is count leading zeros here but count trailing zeros
     is not?  Moving it here will probably simplify selection.ml, which is
     harder to get right than the Cmm stages.

     gyorsh: I don't have implementation of ctz for all other architectures,
     whereas clz was from the previous patch that has been tested and reviewed
     on all architectures before, and so was popcnt. Since then, we introduced
     Proc.operation_supported so now I can put ctz in cmm too. The downside is
     that we are making Cmm.operation bigger. If that's not an issue, we should
     add sqrt and bswap.
  *)
  | Cclz of { arg_is_non_zero: bool; }
  | Cctz of { arg_is_non_zero: bool; }
  | Cpopcnt
  | Cprefetch of { is_write: bool; locality: prefetch_temporal_locality_hint; }
  | Csqrt
  | Cbswap of bswap_width_in_bits
  | Ccmpi of integer_comparison
  | Caddv (* pointer addition that produces a [Val] (well-formed Caml value) *)
  | Cadda (* pointer addition that produces a [Addr] (derived heap pointer) *)
  | Ccmpa of integer_comparison
  | Cnegf | Cabsf
  | Caddf | Csubf | Cmulf | Cdivf
  | Cfloatofint | Cintoffloat
  | Ccmpf of float_comparison
  | Craise of Lambda.raise_kind
  | Ccheckbound (* Takes two arguments : first the bound to check against,
                   then the index.
                   It results in a bounds error if the index is greater than
                   or equal to the bound. *)
  | Cprobe of { name: string; handler_code_sym: string; }
  | Cprobe_is_enabled of { name: string }

(** Every basic block should have a corresponding [Debuginfo.t] for its
    beginning. *)
and expression =
    Cconst_int of int * Debuginfo.t
  | Cconst_natint of nativeint * Debuginfo.t
  | Cconst_float of float * Debuginfo.t
  | Cconst_symbol of string * Debuginfo.t
  | Cconst_pointer of int * Debuginfo.t
  | Cconst_natpointer of nativeint * Debuginfo.t
  | Cblockheader of nativeint * Debuginfo.t
  | Cvar of Backend_var.t
  | Clet of Backend_var.With_provenance.t * expression * expression
  | Clet_mut of Backend_var.With_provenance.t * machtype
                * expression * expression
  | Cphantom_let of Backend_var.With_provenance.t
      * phantom_defining_expr option * expression
  (* Cassign must refer to a variable bound by Clet_mut *)
  | Cassign of Backend_var.t * expression
  | Ctuple of expression list
  | Cop of operation * expression list * Debuginfo.t
  | Csequence of expression * expression
  | Cifthenelse of expression * Debuginfo.t * expression
      * Debuginfo.t * expression * Debuginfo.t
  | Cswitch of expression * int array * (expression * Debuginfo.t) array
      * Debuginfo.t
  | Ccatch of
      rec_flag
        * (int * (Backend_var.With_provenance.t * machtype) list
          * expression * Debuginfo.t) list
        * expression
  | Cexit of int * expression list
  | Ctrywith of expression * Backend_var.With_provenance.t * expression
      * Debuginfo.t

type codegen_option =
  | Reduce_code_size
  | No_CSE

type fundecl =
  { fun_name: string;
    fun_args: (Backend_var.With_provenance.t * machtype) list;
    fun_body: expression;
    fun_codegen_options : codegen_option list;
    fun_dbg : Debuginfo.t;
  }

type data_item =
    Cdefine_symbol of string
  | Cglobal_symbol of string
  | Cint8 of int
  | Cint16 of int
  | Cint32 of nativeint
  | Cint of nativeint
  | Csingle of float
  | Cdouble of float
  | Csymbol_address of string
  | Cstring of string
  | Cskip of int
  | Calign of int

type phrase =
    Cfunction of fundecl
  | Cdata of data_item list

val ccatch :
     int * (Backend_var.With_provenance.t * machtype) list
       * expression * expression * Debuginfo.t
  -> expression

val reset : unit -> unit

val iter_shallow_tail: (expression -> unit) -> expression -> bool
  (** Either apply the callback to all immediate sub-expressions that
      can produce the final result for the expression and return
      [true], or do nothing and return [false].  Note that the notion
      of "tail" sub-expression used here does not match the one used
      to trigger tail calls; in particular, try...with handlers are
      considered to be in tail position (because their result become
      the final result for the expression).  *)

val map_tail: (expression -> expression) -> expression -> expression
  (** Apply the transformation to an expression, trying to push it
      to all inner sub-expressions that can produce the final result.
      Same disclaimer as for [iter_shallow_tail] about the notion
      of "tail" sub-expression. *)

val map_shallow: (expression -> expression) -> expression -> expression
  (** Apply the transformation to each immediate sub-expression. *)
