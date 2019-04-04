[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Block reordering within a function *)

open Linearize

module Layout : sig
  type t = label list

  val from_linear : Linearize.instruction -> t
  val reorder : t -> t
end = struct
  type t = label list

  let rec from_linear i =
    match i.desc with
    | Lend -> []
    | Llabel start -> start :: (from_linear i.next)
    | _ -> from_linear i.next

  let from_linear i =
    from_linear i

  (* CR gyorsh: missing cfg and parameters to determine new order *)
  let reorder t  = t
end

let fundecl f =
  if f.fun_fast then begin
    let cfg = Cfg.from_linear f.fun_body in
    let old_layout = Layout.from_linear f.fun_body in
    let new_layout = Layout.reorder old_layout in
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

