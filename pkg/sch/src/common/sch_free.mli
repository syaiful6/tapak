type (_, _) aseq =
  | ANil : ('f, unit) aseq
  | ACons : ('a, 'f) Sch_sig.app * ('f, 'u) aseq -> ('f, 'a * 'u) aseq

type ('f, 'y, 'z) continue = { cont : 'x. ('x -> 'y) -> ('f, 'x) aseq -> 'z }

val reduce_aseq :
   'f Sch_sig.applicative
  -> ('f, 'u) aseq
  -> ('u, 'f) Sch_sig.app

val hoist_aseq : ('f, 'g) Sch_sig.nat -> ('f, 'a) aseq -> ('g, 'a) aseq

val rebase_aseq :
   ('f, 'u) aseq
  -> ('f, 'y, 'z) continue
  -> ('v -> 'u -> 'y)
  -> ('f, 'v) aseq
  -> 'z

type ('f, 'a) t =
  { fold :
      'u 'y 'z. ('f, 'y, 'z) continue -> ('u -> 'a -> 'y) -> ('f, 'u) aseq -> 'z
  }

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

module Syntax : sig
  val ( <*> ) : ('a, 'b -> 'c) t -> ('a, 'b) t -> ('a, 'c) t
  val ( let+ ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  val ( and+ ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
end
