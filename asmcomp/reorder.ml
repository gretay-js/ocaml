[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Construct control flow graph of the function from linear representation.
   Annotate linear IR with debug information that ties generated assembly and
   binaries back to it.
   Optionally, apply intra-procedural optimizations, such as block reordering.
   Convert back to linear. *)

open Linearize

let verbose = ref false

(* Turns on cfg construction. *)
let build_cfg = ref true

(* Transformation is identity function by default *)
let transform = ref (fun cfg -> (cfg, cfg.get_layout()))

(* Change reorder algorithm *)
let set_transform f =  transform := f

let rec equal i1 i2 =
  (* Format.kasprintf prerr_endline "@;%a" Printlinear.instr i1;
   * Format.kasprintf prerr_endline "@;%a" Printlinear.instr i2; *)
  if i1 == i2 then true
  else
  if i1.desc = i2.desc &&
     i1.id = i2.id &&
     Reg.array_equal i1.arg i2.arg &&
     Reg.array_equal i1.res i2.res &&
     Reg.Set.equal i1.live i2.live &&
     (Debuginfo.compare i1.dbg i2.dbg) = 0
  then begin
    if i1.desc = Lend then true
    else equal i1.next i2.next
  end
  else begin
    Format.kasprintf prerr_endline "Equality failed on:@;%a@;%a"
      Printlinear.instr i1
      Printlinear.instr i2;
    false
  end

let rec add_linear_id i d =
  match i.desc with
  | Lend -> { i with next = i.next }
  | Llabel _ | Ladjust_trap_depth _
    -> { i with next = add_linear_id i.next d }
  | _ -> { i with next = add_linear_id i.next (d + 1); id = d }

let add_linear_ids f =
  { f with fun_body = add_linear_id f.fun_body 2 }

let add_discriminator dbg file d =
  Debuginfo.make ~file ~line:d ~discriminator:d
  |> Debuginfo.concat dbg

let rec add_linear_discriminator i file d =
  assert (i.id = 0 || i.id = d);
  match i.desc with
  | Lend -> { i with next = i.next }
  | Llabel _ | Ladjust_trap_depth _
    -> { i with next = add_linear_discriminator i.next file d }
  | _ -> begin
      { i with dbg = add_discriminator i.dbg file d;
               next = add_linear_discriminator i.next file (d+1);
      }
    end

let add_linear_discriminators f =
  (* Best guess for filename based on compilation unit name,
     because dwarf format (and assembler) require it,
     but only the discriminator really matters,
     and it is per function. *)
  let open Save_ir.Language in
  let file = Printf.sprintf "%s.%s"
               f.fun_name
               (extension (Linear After_all_passes)) in
  { f with fun_dbg = add_discriminator f.fun_dbg file 1;
           fun_body = add_linear_discriminator f.fun_body file 2
  }

let fundecl f =
  if !build_cfg && f.fun_fast then begin
    if !verbose then begin
      Printf.printf "Processing %s\n" f.fun_name;
      Format.kasprintf prerr_endline "Before:@;%a" Printlinear.fundecl f
    end;
    let f = add_linear_ids f in
    let f = add_linear_discriminators f in
    let cfg = Cfg.from_linear f in
    let (new_layout, cfg) = !tranform cfg in
    let new_body = Cfg.to_linear cfg new_layout in
    if !verbose then
      Format.kasprintf prerr_endline "\nAfter:@;%a"
        Printlinear.fundecl {f with fun_body = new_body};
    if not (equal f.fun_body new_body) then begin
      Format.kasprintf prerr_endline "Before:@;%a" Printlinear.fundecl f;
      Format.kasprintf prerr_endline "\nAfter:@;%a"
        Printlinear.fundecl {f with fun_body = new_body};
      Misc.fatal_errorf "Conversion from linear to cfg and back to linear \
                         is not an indentity function.\n"
    end;
    {f with fun_body = new_body}
  end
  else
    f
