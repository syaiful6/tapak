(** Immutable string-keyed map that can be searched by a slice of a larger
    string, so a path segment is looked up without being copied.

    Keys are ordered by length, then byte by byte. [iter], [fold] and
    [bindings] follow that order, which is not [String.compare]'s. *)

type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val cardinal : 'a t -> int

val insert : string -> 'a -> 'a t -> 'a t
(** [insert key value t] binds [key] to [value], replacing any previous
    binding. *)

val find_span : string -> off:int -> len:int -> 'a t -> 'a option
(** [find_span str ~off ~len t] is the value bound to [String.sub str off len],
    found without allocating.

    @raise Invalid_argument if the slice is not within [str]. *)

val find : string -> 'a t -> 'a option
val iter : (string -> 'a -> unit) -> 'a t -> unit
val fold : (string -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
val bindings : 'a t -> (string * 'a) list
