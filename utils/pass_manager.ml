
let pass_dump_if ppf flag message phrase =
  if !flag then Printmach.phase message ppf phrase;
  phrase

let pass_dump_linear_if ppf flag message phrase =
  if !flag then fprintf ppf "*** %s@.%a@." message Printlinear.fundecl phrase;
  phrase

let run_pass : 'a 'b.
  output_prefix:string
  -> ppf:Format.formatter
  -> ?dump_if:bool ref
  -> L.t
  -> print:(Format.formatter -> 'b -> unit)
  -> pass_dump_if:(Format.formatter -> bool ref -> string -> 'b -> 'b)
  -> ('a -> 'b)
  -> 'a
  -> 'b
  =
  fun ~output_prefix ~ppf ?dump_if lang ~print ~pass_dump_if f term ->
    let name = L.to_string lang in
    let term = Profile.record ~accumulate:true name f term in
    let _ = Save_ir.save lang ~output_prefix print term in
    match dump_if with
    | None -> term
    | Some flag ->
    let name = L.to_string_hum lang in
    pass_dump_if ppf flag name term

let to_linear_pass : 'a.
  output_prefix:string
  -> ppf: Format.formatter
  -> ?dump_if:bool ref
  -> L.linear
  -> ('a -> Linearize.fundecl)
  -> 'a
  -> Linearize.fundecl
  =
  fun ~output_prefix ~ppf ?dump_if pass f term ->
  run_pass ~output_prefix ~ppf ?dump_if (Linear (After pass))
    ~print:Printlinear.fundecl
    ~pass_dump_if:pass_dump_linear_if
    f term

let to_mach_pass : 'a .
  output_prefix:string
  -> ppf:Format.formatter
  -> ?dump_if:bool ref
  -> L.mach
  -> ('a -> Mach.fundecl)
  -> 'a
  -> Mach.fundecl =
  fun ~output_prefix ~ppf ?dump_if pass f term ->
  run_pass ~output_prefix ~ppf ?dump_if (Mach (After pass))
    ~print:Printmach.fundecl
    ~pass_dump_if:pass_dump_if
    f term

(* pass that returns IR of type 'a *)
type 'a pass = {
  dump_if : bool;    (* dump IR after this pass? *)
  print : Format.formatter -> 'a -> unit; (* printter for this IR *)
  name : string; (* name of the pass, does not need to be unique. *)
  id : int; (* unique id for passes with the same name *)
}

type t = {
  output_prefix : string; (* useful for dumping to separate files *)
  ppf_dump : Formatter.format; (* *)
}

let make ~ppf_dump ?output_prefix =
  let output_prefix =
  match output_prefix with
  | None -> ""
  | Some s -> s
  in
  { ~output_prefix; ~ppf_dump; }

let run_pass t term p =

  let output_prefix = t.output_prefix in
  ~ppf ?dump_if lang ~print ~pass_dump_if f term ->

    let res = Profile.record ~accumulate:true p.name p.f term in
    let _ = Save_ir.save p.lang ~output_prefix print term in
    match dump_if with
    | None -> term
    | Some flag ->
    let name = L.to_string_hum lang in
    pass_dump_if ppf flag name term

let run t passes term =
  List.fold_left t run_pass term
