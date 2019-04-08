

module Layout : sig
  type t
  val reorder : t -> t
end

(* Control Flow Graph of a function. *)
type t

val from_linear : Linearize.fundecl -> t
val to_linear : t -> Layout.t -> Linearize.instruction

(* Mutates t *)
val eliminate_dead_blocks : t -> unit
val layout : t -> Layout.t
