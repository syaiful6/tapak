type message =
  { topic : string
  ; event : string
  ; payload : Jsont.json
  ; origin : string option
  }

type t

module type S = sig
  type t
  type subscription

  val start : sw:Eio.Switch.t -> t
  val subscribe : t -> string -> (message -> unit) -> subscription
  val unsubscribe : t -> subscription -> unit
  val broadcast : t -> message -> unit
  val node_name : t -> string
end

val create : (module S with type subscription = 's and type t = 'a) -> 'a -> t
val subscribe : t -> string -> (message -> unit) -> int
val unsubscribe : t -> int -> unit
val broadcast : t -> message -> unit
val node_name : t -> string

module Local : sig
  type t

  include S with type t := t and type subscription = int
end

val local : sw:Eio.Switch.t -> t
