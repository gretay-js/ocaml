
type ('a,'b) pass
(** Describes a transformation from 'a to 'b
    that can be applied multiple times. *)

val make_pass
  : ~name:string
  -> (Formatter.format -> 'a -> 'b)
  -> ('a, 'b) pass
(** Creates a pass called [name] for applying transformation [f]. *)

val combine : ('a, 'b) pass list -> ('a, 'b) pass
(** Creates an aggregate pass to represent a sequence of
    individual passes for convenience.
    Only individual passes have names.  *)

val current_pass_name : unit -> string option
(** Returns the name of the current pass, if there is one in flight. *)

val current_pass_round : unit -> int option
(** Returns the round of the current pass, if there is one in flight.
    "round" is the number of times this pass executed so far.
    It can be used to limit the number of times a pass executes.
    If the number of rounds is not known statically, i.e., depends
    on the output of the pass, then it is useful to . *)

val schedule : ('a ,'b) pass list -> unit
(** [Pass_manager.schedule passes]
    registers passes for execution in the order provided in the list,
    after previously registered passes.
    A pass may appear multiple times in the list argument,
    and may be registered multiple times separately. *)

val schedule_next : ('a ,'b) pass -> unit
(** Same as {!Pass_manager.schedule} but places the pass at the beginning
    to be executed before all previously registered passes.
    It can be used to register a pass dynamically based on the result of
    the previously executed pass. *)

val schedule_before : string -> ('a ,'b) pass list -> unit
(** [Pass_manager.schedule_before name new_passes] registers [new_passes]
    to be executed right before the pass called [name],
    which must have already been scheduled exactly once. *)

(** Executes register passes in order. *)
val run : Formatter.format -> 'a -> 'b
