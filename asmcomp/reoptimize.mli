(* Basic block reordering. *)

val fundecl: Linearize.fundecl -> Linearize.fundecl

(** Registersd function [f] as the transformer to be applied to the cfg,
    when this pass executes.
    Enables cfg from linear construction, linear ids, and
    special dwarf debug info for linear ids
    (even if [f] is identity function).
    [f] might modify the input cfg instead of copying it. *)
val set_transform: f:(Cfg.t -> Cfg.t) -> unit
