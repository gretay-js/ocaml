(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Saving of intermediate representations to files. *)

module Language : sig
  type lambda =
    | Transl
    | Simplif

  type clambda =
    | Closure
    | Flambda_to_clambda
    | Un_anf

  type flambda =
    | Closure_conversion
    | Lift_constants
    | Share_constants
    | Remove_unused_program_constructs
    | Lift_let_to_initialize_symbol
    | Lift_code
    | Inline_and_simplify
    | Remove_unused_closure_vars
    | Ref_to_variables
    | Initialize_symbol_to_let_symbol

  type mach =
    | Selection
    | Comballoc
    | CSE
    | Liveness_1
    | Deadcode_1
    | Deadcode_2
    | Spill
    | Liveness_2
    | Split
    | Liveness_3
    | Regalloc
    | Reload
    | Liveness_during_regalloc
    | Available_regs

  type linear =
    | Linearize
    | Linear_invariants
    | Scheduling
    | Reoptimize

  type 'a pass =
    | After_all_passes
    | After of 'a
    | Before of 'a

  type t =
    | Parsetree
    | Typedtree
    | Lambda of lambda pass
    | Clambda of clambda pass
    | Flambda of flambda pass
    | Cmm
    | Mach of mach pass
    | Linear of linear pass
    | Bytecode

  val all : t list

  (** Names of passes. *)
  val to_string : t -> string

  (** Human-readable descriptions of passes. *)
  val to_string_hum : t -> string

  val extension : t -> string
  (** Filename extension for the language. *)

end

(** Mark that the given language should be saved at a particular stage. *)
val should_save : string -> unit

(** Mark that all languages should be saved prior to them being converted
    into the next language. *)
val should_save_all : unit -> unit

(** Mark that all languages should be saved after each pass. *)
val should_save_all_after_each_pass : unit -> unit

(** Textual names of all intermediate representations. *)
val all_languages : string list

(** If the given intermediate representation is to be saved, create a
    formatter pointing at the appropriate file, ready for the supplied
    function to do the pretty-printing. *)
val save
   : Language.t
  -> ?output_prefix:string
  -> (Format.formatter -> 'a -> unit)
  -> 'a
  -> 'a

val set_output_prefix : string -> unit

val get_output_prefix : unit -> string
