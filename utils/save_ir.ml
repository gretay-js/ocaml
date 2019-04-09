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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Language = struct
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
    | Deadcode
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
    | Block_reorder

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

  let mach_pass_name_human (m:mach) =
    match m with
    | Selection -> "instruction selection"
    | Comballoc -> "allocation combining"
    | CSE -> "CSE"
    | Liveness_1 -> "liveness analysis (1)"
    | Deadcode -> "dead code elimination"
    | Spill -> "insertion of suggested spills and reloads"
    | Liveness_2 -> "liveness analysis (2)"
    | Split -> "live range splitting"
    | Liveness_3 -> "liveness analysis (3)"
    | Regalloc -> "register allocation"
    | Liveness_during_regalloc -> "liveness analysis (during reg. alloc)"
    | Reload -> "insertion of remaining spills and reloads"
    | Available_regs -> "available_regs"

  let lambda_pass_name (l:lambda) =
    match l with
    | Transl -> "transl"
    | Simplif -> "simplif"

  let clambda_pass_name (cl:clambda) =
    match cl with
    | Closure -> "closure"
    | Flambda_to_clambda -> "flambda_to_clambda"
    | Un_anf -> "un_anf"

  let flambda_pass_name (fl:flambda) =
    match fl with
    | Closure_conversion -> "closure_conversion"
    | Lift_constants -> "lift_constants"
    | Share_constants -> "share_constants"
    | Remove_unused_program_constructs -> "remove_unused_program_constructs"
    | Lift_let_to_initialize_symbol -> "lift_let_to_initialize_symbol"
    | Lift_code -> "lift_code"
    | Inline_and_simplify -> "inline_and_simplify"
    | Remove_unused_closure_vars -> "remove_unused_closure_vars"
    | Ref_to_variables -> "ref_to_variables"
    | Initialize_symbol_to_let_symbol -> "initialize_symbol_to_let_symbol"

  let mach_pass_name (m:mach) =
    match m with
    | Selection -> "selection"
    | Comballoc -> "comballoc"
    | CSE -> "cse"
    | Liveness_1 -> "liveness_1"
    | Deadcode -> "deadcode"
    | Spill -> "spill"
    | Liveness_2 -> "Liveness_2"
    | Split -> "split"
    | Liveness_3 -> "liveness_3"
    | Regalloc -> "regalloc"
    | Reload -> "reload"
    | Liveness_during_regalloc -> "liveness_during_regalloc"
    | Available_regs -> "available_regs"

  let linear_pass_name (lin:linear) =
    match lin with
    | Linearize -> "linearize"
    | Linear_invariants -> "linear_invariants"
    | Scheduling -> "scheduling"
    | Block_reorder -> "block_reorder"

  let to_string_pass pass_name = function
    | After_all_passes -> "_all_passes"
    | After p -> "_after_" ^ (pass_name p)
    | Before p -> "_before_" ^ (pass_name p)

  let to_string = function
    | Parsetree -> "parsetree"
    | Typedtree -> "typedtree"
    | Lambda pass -> "lambda_" ^ (to_string_pass lambda_pass_name pass)
    | Clambda pass -> "clambda_" ^ (to_string_pass clambda_pass_name pass)
    | Flambda pass -> "flambda_" ^ (to_string_pass flambda_pass_name pass)
    | Cmm -> "cmm"
    | Mach pass -> "mach_" ^ (to_string_pass mach_pass_name pass)
    | Linear pass -> "linear" ^ (to_string_pass linear_pass_name pass)
    | Bytecode -> "bytecode"

  let extension = function
    | Parsetree -> "parsetree"
    | Typedtree -> "typedtree"
    | Lambda _ -> "lambda"
    | Clambda _ -> "clambda"
    | Flambda _ -> "flambda"
    | Cmm -> "cmm"
    | Mach _ -> "mach"
    | Linear  _ -> "linear"
    | Bytecode -> "bytecode"

  let to_string_hum = function
    | Mach (After pass) -> mach_pass_name_human pass
    | pass -> to_string pass

  include Identifiable.Make (struct
      type nonrec t = t
      let to_string t = to_string t
      let compare = Pervasives.compare
      let equal t1 t2 = (compare t1 t2 = 0)
      let hash = Hashtbl.hash
      let output oc t = Printf.fprintf oc "%s"  (to_string t)
      let print ppf t = Format.fprintf ppf "%s" (to_string t)
    end)

  let all = [
    Parsetree;
    Typedtree;
    Lambda (After Transl);
    Lambda (After Simplif);
    Clambda (After Closure);
    Clambda (After Flambda_to_clambda);
    Clambda (After Un_anf);
    Flambda (After Closure_conversion);
    Flambda (After Lift_constants);
    Flambda (After Share_constants);
    Flambda (After Remove_unused_program_constructs);
    Flambda (After Lift_let_to_initialize_symbol);
    Flambda (After Lift_code);
    Flambda (After Inline_and_simplify);
    Flambda (After Remove_unused_closure_vars);
    Flambda (After Ref_to_variables);
    Flambda (After Initialize_symbol_to_let_symbol);
    Cmm;
    Mach (After Selection);
    Mach (After Comballoc);
    Mach (After CSE);
    Mach (After Liveness_1);
    Mach (After Deadcode);
    Mach (After Spill);
    Mach (After Liveness_2);
    Mach (After Split);
    Mach (After Liveness_3);
    Mach (After Regalloc);
    Mach (After Reload);
    Mach (After Liveness_during_regalloc);
    Linear (After Linearize);
    Linear (After Scheduling);
    Linear (After Block_reorder);
  ]

  let conversions = [
    Parsetree;
    Typedtree;
    Lambda (After Simplif);
    Clambda (After Un_anf);
    Flambda (After Initialize_symbol_to_let_symbol);
    Cmm;
    Mach (After Liveness_during_regalloc);
    Linear (After Block_reorder);
  ]

  let all_numbered =
    let _next_count, all_numbered =
      List.fold_left (fun (next_count, all_numbered) pass ->
        let all_numbered = Map.add pass next_count all_numbered in
        next_count + 1, all_numbered)
        (0, Map.empty)
        all
    in
    all_numbered

  let number_of_pass pass =
    match Map.find pass all_numbered with
    | exception Not_found -> assert false  (* see above *)
    | number -> number

  let find name =
    match List.find_opt (fun lang -> to_string lang = name) all with
    | None -> Misc.fatal_errorf "Unknown language as argument for -save-ir"
    | Some lang -> lang
end

let shoud_save_languages = ref Language.Set.empty

let should_save name =
  let lang = Language.find name in
  shoud_save_languages := Language.Set.add lang !shoud_save_languages

(** Mark that all languages should be saved prior to them being converted
    into the next language. *)
let should_save_all () =
  shoud_save_languages := Language.Set.of_seq (List.to_seq Language.conversions)


let should_save_all_after_each_pass () =
  shoud_save_languages := Language.Set.of_seq (List.to_seq Language.all)

let all_languages =
  List.map Language.to_string Language.all

let save lang ~output_prefix f term =
  if Language.Set.mem lang !shoud_save_languages then begin
    let filename =
      Printf.sprintf "%s.%03d.%s.%s"
        output_prefix
        (Language.number_of_pass lang)
        (Language.to_string lang)
        (Language.extension lang)
    in
    match open_out filename with
    | exception exn ->
      Printf.eprintf "Cannot open intermediate language file \
          for writing: %s (%s)\n"
        filename (Printexc.to_string exn);
      exit 1
    | chan ->
      let formatter = Format.formatter_of_out_channel chan in
      f formatter term;
      close_out chan
  end;
  term

let passes_finished lang f term =
  if Language.Set.mem lang !shoud_save_languages then begin
    let output_prefix  = "" in
    let filename =
      Printf.sprintf "%s.%s"
        output_prefix
        (Language.extension lang)
    in
    match open_out filename with
    | exception exn ->
      Printf.eprintf "Cannot open intermediate language file \
                      for writing: %s (%s)\n"
        filename (Printexc.to_string exn);
      exit 1
    | chan ->
      let formatter = Format.formatter_of_out_channel chan in
      f formatter term;
      close_out chan
  end;
  term
