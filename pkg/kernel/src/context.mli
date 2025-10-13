type 'a field_metadata =
  { name : string option
  ; show : ('a -> string) option
  }

type 'a key

module Key : sig
  type 'a info = 'a field_metadata

  val create : 'a info -> 'a key
  val info : 'a key -> 'a info

  type t

  val hide_type : 'a key -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

type t

val empty : t
val is_empty : t -> bool
val mem : 'a key -> t -> bool
val add : 'a key -> 'a -> t -> t
val singleton : 'a key -> 'a -> t
val rem : 'a key -> t -> t
val find : 'a key -> t -> 'a option
val get : 'a key -> t -> 'a

type binding = B : 'a key * 'a -> binding

val iter : (binding -> unit) -> t -> unit
val fold : (binding -> 'a -> 'a) -> t -> 'a -> 'a
val for_all : (binding -> bool) -> t -> bool
val exists : (binding -> bool) -> t -> bool
val filter : (binding -> bool) -> t -> t
val cardinal : t -> int
val any_binding : t -> binding option
val get_any_binding : t -> binding
