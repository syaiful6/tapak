module type FUNCTOR = sig
  type 'a t
end

type ('a, 't) app
(** LKHT style encoding of higher-kinded types. ['t] is the type constructor,
    and ['a] is the type parameter. *)

type 'f applicative =
  { pure : 'a. 'a -> ('a, 'f) app
  ; map : 'a 'b. ('a -> 'b) -> ('a, 'f) app -> ('b, 'f) app
  ; apply : 'a 'b. ('a -> 'b, 'f) app -> ('a, 'f) app -> ('b, 'f) app
  }
(** applicative functor structure for the type constructor ['f] *)

type ('f, 'g) nat = { run : 'a. ('a, 'f) app -> ('a, 'g) app }
(** natural transformation from ['f] to ['g] *)

(* useful constructor for ap *)

module Common = struct
  type t

  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

module Make (T : FUNCTOR) = struct
  type 'a s = 'a T.t

  include Common
end
