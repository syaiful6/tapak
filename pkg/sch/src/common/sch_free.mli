(** Free applicative functor over an effect ['f].

    [apply f x] passes every effect in [f] to the natural transformation before
    those in [x], then gives [app.apply] the two results in the same order.
    Applicatives that accumulate values should therefore append [x]'s values to
    [f]'s. *)

type ('f, 'a) t

val pure : 'a -> ('f, 'a) t
val map : ('a -> 'b) -> ('f, 'a) t -> ('f, 'b) t
val apply : ('f, 'a -> 'b) t -> ('f, 'a) t -> ('f, 'b) t
val lift : ('a, 'f) Sch_sig.app -> ('f, 'a) t
val hoist : ('f, 'g) Sch_sig.nat -> ('f, 'a) t -> ('g, 'a) t
val retract : 'f Sch_sig.applicative -> ('f, 'a) t -> ('a, 'f) Sch_sig.app

val run :
   'g Sch_sig.applicative
  -> ('f, 'g) Sch_sig.nat
  -> ('f, 'a) t
  -> ('a, 'g) Sch_sig.app
(** [run app nat free] interprets [free] in one pass. It is not tail recursive;
    stack depth matches the nesting depth of [free], usually the number of
    fields. *)

module Syntax : sig
  val ( <*> ) : ('a, 'b -> 'c) t -> ('a, 'b) t -> ('a, 'c) t
  val ( let+ ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  val ( and+ ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
end
