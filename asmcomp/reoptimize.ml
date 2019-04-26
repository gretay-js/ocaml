[@@@ocaml.warning "+a-4-30-40-41-42"]
(* Annotate linear IR with debug information that ties generated assembly and
   binaries back to linear IR for profile-guided optimizations.
   Optionally, apply intra-procedural optimizations, such as block reordering
   and peephole optimizations. *)
open Linearize

(* Transformation is an identity function by default *)
let transform = ref (fun fundecl -> fundecl)

(* Change transformation *)
let setup ~f = transform := f

(* All labels have id 0 because cfg operations can create new labels,
   whereas ids of basic block instructions do not change. *)
let label_id = 0
let prolog_id = 1
(* From 4.08, LPrologue is added to fun_body, so there is
   no need to make an id for fun_dbg, and prolog_id instead
   of entry_id in add_linear_discriminators. *)
let entry_id = 1

let rec add_linear_id i d =
  match i.desc with
  | Lend -> { i with next = i.next }
  | Llabel _ | Ladjust_trap_depth _
    -> { i with next = add_linear_id i.next d }
  | _ -> { i with next = add_linear_id i.next (d + 1); id = d }

let add_linear_ids f =
  { f with fun_body = add_linear_id f.fun_body entry_id }

let add_discriminator dbg file d =
  Debuginfo.make ~file ~line:d ~discriminator:d
  |> Debuginfo.concat dbg

let rec add_linear_discriminator i file d =
  assert (i.id = label_id || i.id = d);
  match i.desc with
  | Lend -> { i with next = i.next }
  | Llabel _ | Ladjust_trap_depth _
    -> { i with next = add_linear_discriminator i.next file d }
  | _ -> begin
      { i with dbg = add_discriminator i.dbg file d;
               next = add_linear_discriminator i.next file (d+1);
      }
    end

(* CR gyorsh: This is the only machine dependent part. *)
let to_symbol name =
  let symbol_prefix =
    if X86_proc.system = X86_proc.S_macosx then "_" else ""
  in
  X86_proc.string_of_symbol symbol_prefix name

let add_linear_discriminators f =
  (* Best guess for filename based on compilation unit name,
     because dwarf format (and assembler) require it,
     but only the line number or discriminator really matter,
     and it is per function. *)
  let file = (to_symbol f.fun_name)^".linear" in
  { f with fun_dbg = add_discriminator f.fun_dbg file prolog_id;
           fun_body = add_linear_discriminator f.fun_body file entry_id
  }

let fundecl f =
  if f.fun_fast && !Clflags.extended_debug then begin
    let f = add_linear_ids f in
    let f = add_linear_discriminators f in
    !transform f
  end
  else
    f
