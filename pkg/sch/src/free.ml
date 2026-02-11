(** This module implement free applicative structure *)

(** The free applicative sequence, you can think this a 
    sequence of effects, and a pure function to apply
    that sequence to. *)
type (_, _) aseq =
  | ANil : ('f, unit) aseq
  | ACons : ('a, 'f) Sig.app * ('f, 'u) aseq -> ('f, 'a * 'u) aseq

type ('f, 'y, 'z) continue = { cont : 'x. ('x -> 'y) -> ('f, 'x) aseq -> 'z }
(** the continuation for the free applicative structure *)

let rec reduce_aseq : type f u.
  f Sig.applicative -> (f, u) aseq -> (u, f) Sig.app
  =
 fun app aseq ->
  match aseq with
  | ANil -> app.pure ()
  | ACons (fa, rest) ->
    app.apply (app.map (fun u a -> a, u) (reduce_aseq app rest)) fa

(** Hoist a natural transformation over the applicative sequence *)
let[@tail_mod_cons] rec hoist_aseq : type f g a.
  (f, g) Sig.nat -> (f, a) aseq -> (g, a) aseq
  =
 fun nat aseq ->
  match aseq with
  | ANil -> ANil
  | ACons (fa, rest) -> ACons (nat.run fa, hoist_aseq nat rest)

let rec rebase_aseq : type f u y z v.
  (f, u) aseq -> (f, y, z) continue -> (v -> u -> y) -> (f, v) aseq -> z
  =
 fun aseq k f aseq2 ->
  match aseq with
  | ANil -> k.cont (fun v -> f v ()) aseq2
  | ACons (fx, rest) ->
    rebase_aseq
      rest
      { cont = (fun g s -> k.cont (fun (a, u) -> g u a) (ACons (fx, s))) }
      (fun v u a -> f v (a, u))
      aseq2

type ('f, 'a) t =
  { fold :
      'u 'y 'z. ('f, 'y, 'z) continue -> ('u -> 'a -> 'y) -> ('f, 'u) aseq -> 'z
  }
(** The free applicative structure, the type of the function is a continuation,
    accumulator function, accumulated sequence and return the value
    of the continuation *)

let pure : type f a. a -> (f, a) t =
 fun a -> { fold = (fun k f -> k.cont (Fun.flip f a)) }

let map : type f a b. (a -> b) -> (f, a) t -> (f, b) t =
 fun f ap_a ->
  { fold = (fun k g aseq -> ap_a.fold k (fun cont a -> g cont (f a)) aseq) }

let apply : type f a b. (f, a -> b) t -> (f, a) t -> (f, b) t =
 fun ap_f ap_a ->
  { fold =
      (fun k f ->
        ap_a.fold
          { cont = (fun ka seq -> ap_f.fold k ka seq) }
          (fun s a g -> f s (g a)))
  }

(** Lift a single effect into the free applicative structure *)
let lift : type f a. (a, f) Sig.app -> (f, a) t =
 fun a ->
  { fold = (fun k f s -> k.cont (fun (a', s') -> f s' a') (ACons (a, s))) }

let hoist : type f g a. (f, g) Sig.nat -> (f, a) t -> (g, a) t =
 fun nat xa ->
  { fold =
      (fun k f s ->
        xa.fold
          { cont =
              (fun f' s' ->
                rebase_aseq (hoist_aseq nat s') k (fun v u -> f v (f' u)) s)
          }
          (fun _ d -> d)
          ANil)
  }

let retract : type f a. f Sig.applicative -> (f, a) t -> (a, f) Sig.app =
 fun app xa ->
  xa.fold
    { cont = (fun f s -> reduce_aseq app s |> app.map f) }
    (fun _ d -> d)
    ANil

let run : type f g a.
  g Sig.applicative -> (f, g) Sig.nat -> (f, a) t -> (a, g) Sig.app
  =
 fun app nat xa -> retract app (hoist nat xa)

module Syntax = struct
  let ( <*> ) f x = apply f x
  let ( let+ ) ap_a f = map f ap_a
  let pair a b = a, b
  let ( and+ ) fa fb = apply (map pair fa) fb
end
