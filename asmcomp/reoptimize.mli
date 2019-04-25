(* Machine specific optimizations. *)

(** Registers function [f] as the transformer to be applied
    when this pass executes. *)
val setup: f:(Linearize.fundecl -> Linearize.fundecl) -> unit

(** Apply the transformer *)
val fundecl: Linearize.fundecl -> Linearize.fundecl
