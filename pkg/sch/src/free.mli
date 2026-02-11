type (_, _) aseq =
  | ANil : ('f, unit) aseq
  | ACons : ('a, 'f) Sig.app * ('f, 'u) aseq -> ('f, 'a * 'u) aseq

type ('f, 'y, 'z) continue = { cont : 'x. ('x -> 'y) -> ('f, 'x) aseq -> 'z }

val reduce_aseq : 'f Sig.applicative -> ('f, 'u) aseq -> ('u, 'f) Sig.app
val hoist_aseq : ('f, 'g) Sig.nat -> ('f, 'a) aseq -> ('g, 'a) aseq

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
val lift : ('a, 'f) Sig.app -> ('f, 'a) t
val hoist : ('f, 'g) Sig.nat -> ('f, 'a) t -> ('g, 'a) t
val retract : 'f Sig.applicative -> ('f, 'a) t -> ('a, 'f) Sig.app

val run :
   'g Sig.applicative
  -> ('f, 'g) Sig.nat
  -> ('f, 'a) t
  -> ('a, 'g) Sig.app

module Syntax : sig
  val ( <*> ) : ('a, 'b -> 'c) t -> ('a, 'b) t -> ('a, 'c) t
  val ( let+ ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  val ( and+ ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
end
