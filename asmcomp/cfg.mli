(* Control Flow Graph of a function. *)
type t

val from_linear : Linearize.fundecl -> t
val to_linear : t -> Linearize.instruction

(* Mutates t *)
val eliminate_dead_blocks : t -> unit
