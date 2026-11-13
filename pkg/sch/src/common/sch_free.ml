(** A free applicative represented as a GADT. [Apply (f, x)] interprets the
    effects in [f] before those in [x], preserving declaration order. *)

type ('f, 'a) t =
  | Pure : 'a -> ('f, 'a) t
  | Map : ('a -> 'b) * ('f, 'a) t -> ('f, 'b) t
  | Apply : ('f, 'a -> 'b) t * ('f, 'a) t -> ('f, 'b) t
  | Lift : ('a, 'f) Sch_sig.app -> ('f, 'a) t

let pure a = Pure a
let lift a = Lift a
let map f x = Map (f, x)
let apply f x = Apply (f, x)

let rec hoist : type f g a. (f, g) Sch_sig.nat -> (f, a) t -> (g, a) t =
 fun nat -> function
  | Pure a -> Pure a
  | Map (f, x) -> Map (f, hoist nat x)
  | Apply (f, x) ->
    let f = hoist nat f in
    Apply (f, hoist nat x)
  | Lift x -> Lift (nat.run x)

(* OCaml evaluates function arguments right to left. Bind both results so
   side-effecting natural transformations, such as the JSON writer, process [f]
   before [x]. *)
let rec run : type f g a.
  g Sch_sig.applicative -> (f, g) Sch_sig.nat -> (f, a) t -> (a, g) Sch_sig.app
  =
 fun app nat -> function
  | Pure a -> app.pure a
  | Map (f, x) -> app.map f (run app nat x)
  | Apply (f, x) ->
    let f = run app nat f in
    let x = run app nat x in
    app.apply f x
  | Lift x -> nat.run x

let retract app free = run app { Sch_sig.run = Fun.id } free

module Syntax = struct
  let ( <*> ) f x = apply f x
  let ( let+ ) ap_a f = map f ap_a
  let pair a b = a, b
  let ( and+ ) fa fb = apply (map pair fa) fb
end
