module Layout : sig
  type t
end

(* Control Flow Graph of a function. *)
type t

val from_linear : Linearize.fundecl -> t
val to_linear : t -> Layout.t -> Linearize.instruction


val get_layout : t -> Layout.t

(* Mutates t *)
val eliminate_dead_blocks : t -> unit
