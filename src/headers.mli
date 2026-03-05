type t = Http.Header.t

val empty : t

include module type of Http.Header with type t := t

module type MESSAGE = sig
  type t

  val with_headers : (Http.Header.t -> Http.Header.t) -> t -> t
  val headers : t -> Http.Header.t
end

module type S = sig
  type t

  val headers : t -> Http.Header.t
  val header : string -> t -> string option
  val add_header : string -> string -> t -> t
  val add_header_or_replace : string -> string -> t -> t
  val add_header_unless_exists : string -> string -> t -> t
  val add_headers : (string * string) list -> t -> t
  val remove_header : string -> t -> t
end

module Make : functor (M : MESSAGE) -> S with type t := M.t
