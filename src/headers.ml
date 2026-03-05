include Http.Header

let empty = init ()

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

(* helpers for working with headers in request and response types *)
module Make (M : MESSAGE) = struct
  let headers = M.headers
  let header name t = Http.Header.get (M.headers t) name

  let add_header name value =
    M.with_headers (fun h -> Http.Header.add h name value)

  let add_header_or_replace name value =
    M.with_headers (fun h ->
      if Http.Header.mem h name
      then Http.Header.replace h name value
      else Http.Header.add h name value)

  let add_header_unless_exists name value =
    M.with_headers (fun h -> Http.Header.add_unless_exists h name value)

  let add_headers hs = M.with_headers (fun h -> Http.Header.add_list h hs)
  let remove_header name = M.with_headers (fun h -> Http.Header.remove h name)
end
