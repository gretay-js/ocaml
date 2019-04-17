(* Basic block reordering. *)

val fundecl: Linearize.fundecl -> Linearize.fundecl

(* might modify the input cfg instead of copying it *)
val set_transform: (Cfg.t -> Cfg.t) -> unit
