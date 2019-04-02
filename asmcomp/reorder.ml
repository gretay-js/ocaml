[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Block reordering within a function *)

open Linearize

module Layout  = sig
  type t
  val from_linear : Linear.instruction -> t
  val reorder : t -> t
end
struct
  type t = label list

  let rec from_linear i =
    match i with
    | Lend -> []
    | Llabel start -> start :: (from_linear i.next)
    | _ -> from_linear i.next

  (* CR gyorsh: missing cfg and parameters to determine new order *)
  let reorder t  = t
end

let fundecl f =
  if f.fun_fast then begin
    CFG.reset ();
    let cfg = CFG.from_linear f.fun_body in
    let old_layout = Layout.from_linear f.fun_body in
    let new_layout = Layout.reorder old_layout in
    let new_body = CFG.to_linear cfg new_layout in
    {f with fun_body = new_body.i}
  end
  else
    f

