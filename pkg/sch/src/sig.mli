module type FUNCTOR = sig
  type 'a t
end

type ('a, 't) app

type 'f applicative =
  { pure : 'a. 'a -> ('a, 'f) app
  ; map : 'a 'b. ('a -> 'b) -> ('a, 'f) app -> ('b, 'f) app
  ; apply : 'a 'b. ('a -> 'b, 'f) app -> ('a, 'f) app -> ('b, 'f) app
  }

type ('f, 'g) nat = { run : 'a. ('a, 'f) app -> ('a, 'g) app }

module Common : sig
  type t

  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

module Make : functor (T : FUNCTOR) -> sig
  type 'a s = 'a T.t
  type t = Common.t

  external inj : 'a s -> ('a, t) app = "%identity"
  external prj : ('a, t) app -> 'a s = "%identity"
end
