[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Block reordering within a function *)

open Linearize

let fundecl f =
  if f.fun_fast then begin
    let cfg, old_layout = Cfg.from_linear f in
    let new_layout = Cfg.Layout.reorder old_layout in
    let new_body = Cfg.to_linear cfg new_layout in
    if f.fun_body <> new_body then begin
      Misc.fatal_errorf "Conversion from linear to cfg and back to linear \
                         is not an indentity function.\n\
                         \nBefore:@;%a\
                         \nAfter:@;%a"
        Printlinear.fundecl f
        Printlinear.fundecl {f with fun_body = new_body};
    end;
    {f with fun_body = new_body}
  end
  else
    f
