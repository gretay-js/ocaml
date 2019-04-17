(* Basic block reordering. *)

val fundecl: Linearize.fundecl -> Linearize.fundecl

val set_transform: (Cfg.t -> Cfg.t * Cfg.Layout.t) -> unit
