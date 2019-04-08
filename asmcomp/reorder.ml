[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Block reordering within a function *)

open Linearize

let reorder = ref true

let verbose = ref false

let rec equal i1 i2 =
  (* Format.kasprintf prerr_endline "@;%a" Printlinear.instr i1;
   * Format.kasprintf prerr_endline "@;%a" Printlinear.instr i2; *)
  if i1 == i2 then true
  else
  if i1.desc = i2.desc &&
     Reg.array_equal i1.arg i2.arg &&
     Reg.array_equal i1.res i2.res &&
     (Debuginfo.compare i1.dbg i2.dbg) = 0 &&
     Reg.Set.equal i1.live i2.live
  then begin
    if i1.desc = Lend then true
    else equal i1.next i2.next
  end
  else
    false

let rec add_discriminator i d =
  let next =
    match i.desc with
    | Lend -> i.next
    | _ -> add_discriminators i.next (d + 1)
  in { i with next and discriminator = d }

let add_discriminators f =
  { f with fun_body = add_discriminators f.fun_body 1 }

let fundecl f =
  if !reorder && f.fun_fast then begin
    if !verbose then begin
      Printf.printf "Processing %s\n" f.fun_name;
      Format.kasprintf prerr_endline "Before:@;%a" Printlinear.fundecl f
    end;
    let f = add_discriminators f in
    let cfg = Cfg.from_linear f in
    (* Cfg.eliminate_dead_blocks cfg; *)
    let old_layout = Cfg.layout cfg in
    let new_layout = Cfg.Layout.reorder old_layout in
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
