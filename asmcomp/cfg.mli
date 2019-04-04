(* Control Flow Graph of a function. *)

module Layout : sig
  type t
  val reorder : t -> t
end

type t

val from_linear : Linearize.instruction -> t * Layout.t
val to_linear : t -> Layout.t -> Linearize.instruction
