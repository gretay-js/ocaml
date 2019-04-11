
(* let pass_dump_if ppf flag message phrase =
 *   if !flag then Printmach.phase message ppf phrase;
 *   phrase
 *
 * let pass_dump_linear_if ppf flag message phrase =
 *   if !flag then fprintf ppf "*** %s@.%a@." message Printlinear.fundecl phrase;
 *   phrase
 *
 * let run_pass : 'a 'b.
 *   output_prefix:string
 *   -> ppf:Format.formatter
 *   -> ?dump_if:bool ref
 *   -> L.t
 *   -> print:(Format.formatter -> 'b -> unit)
 *   -> pass_dump_if:(Format.formatter -> bool ref -> string -> 'b -> 'b)
 *   -> ('a -> 'b)
 *   -> 'a
 *   -> 'b
 *   =
 *   fun ~output_prefix ~ppf ?dump_if lang ~print ~pass_dump_if f term ->
 *     let name = L.to_string lang in
 *     let term = Profile.record ~accumulate:true name f term in
 *     let _ = Save_ir.save lang ~output_prefix print term in
 *     match dump_if with
 *     | None -> term
 *     | Some flag ->
 *     let name = L.to_string_hum lang in
 *     pass_dump_if ppf flag name term
 *
 * let to_linear_pass : 'a.
 *   output_prefix:string
 *   -> ppf: Format.formatter
 *   -> ?dump_if:bool ref
 *   -> L.linear
 *   -> ('a -> Linearize.fundecl)
 *   -> 'a
 *   -> Linearize.fundecl
 *   =
 *   fun ~output_prefix ~ppf ?dump_if pass f term ->
 *   run_pass ~output_prefix ~ppf ?dump_if (Linear (After pass))
 *     ~print:Printlinear.fundecl
 *     ~pass_dump_if:pass_dump_linear_if
 *     f term
 *
 * let to_mach_pass : 'a .
 *   output_prefix:string
 *   -> ppf:Format.formatter
 *   -> ?dump_if:bool ref
 *   -> L.mach
 *   -> ('a -> Mach.fundecl)
 *   -> 'a
 *   -> Mach.fundecl =
 *   fun ~output_prefix ~ppf ?dump_if pass f term ->
 *   run_pass ~output_prefix ~ppf ?dump_if (Mach (After pass))
 *     ~print:Printmach.fundecl
 *     ~pass_dump_if:pass_dump_if
 *     f term *)

(* pass that returns IR of type 'a *)
type 'a p = {
  (* dump_if : bool;    (* dump IR after this pass? *)
   * print : Format.formatter -> 'a -> unit; (* printter for this IR *) *)
  name : string; (* name of the pass, does not need to be unique. *)
  mutable round : int; (* incremented every time this pass executes *)
  f : 'b . Format.formatter -> 'b -> 'a; (* transformation *)
}

let make_pass ~name ~f =
  [  { name; f; round = 0; } ]

let combine passes =
  List.concat passes

(* CR gyorsh: input/output types of passes in the pipeline must match
   to ensure that function composition won't fail when the passes execute.
   How to check it statically? Can we enforce it using the type system
   without case analysis and without assuming a fixed set of IRs and passes? *)
let pipeline = ref []
let current_pass = ref None

let current_pass_round () =
  match !current_pass with
  | None -> None
  | Some p -> p.round

let current_pass_name () =
  match !current_pass with
  | None -> None
  | Some p -> p.name

let schedule new_passes =
  pipeline := !pipeline @ (List.concat new_passes)

let schedule_next pass =
  pipeline := pass @ !pipeline

let schedule_before name new_passes =
  let len = List.length
              (List.filter (fun p -> p.name = name) !pipeline) in
  if len <> 1 then begin
    Misc.fatal_errorf
      "Failed to register pass %s before %s. \
       Expected 1 occurrence of pass %s, but found %d."
      p.name name len
  end;
  pipeline := List.fold_right
                (fun p acc ->
                   let nl =
                     if p.name = name then List.concat new_passes
                     else []
                   in nl@(p::acc))
                !pipeline
                []

let run ~ppf term =
  let run_pass p term =
    p.round <- p.round + 1;
    current_pass := Some p;
    Profile.record ~accumulate:true p.name (p.f ppf) term
  in
  let rec loop term =
    match !pipeline with
    | [] -> term
    | p::tail ->
      pipeline := tail;
      let res = run_pass p term in
      loop res
  in
  let res = loop term in
  current_pass := None;
  res
