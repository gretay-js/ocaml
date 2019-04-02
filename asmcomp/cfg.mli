(* Control Flow Graph of a function. *)
type t
val from_linear : Linearize.instruction -> t
val to_linear : t -> Linearize.instruction
