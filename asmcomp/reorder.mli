(* Basic block reordering. *)

val fundecl: Linearize.fundecl -> Linearize.fundecl

(* Contains a permutation of the original linear ids. *)
type fun_layout = (int, int) Hashtbl.t
(* Maps functions to layout of the function. Spares, i.e., only contains
functions whose layout changed. *)
type layout = (string, fun_layout) Hashtbl.t

type algo =
  | Identity
  | Random
  | External of layout
  | CachePlus

val set_layout : layout -> unit
