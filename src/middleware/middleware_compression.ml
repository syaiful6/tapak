open Header_parser

type encoder = Accept.encoding -> Bytesrw.Bytes.Reader.filter option
type predicate = Request.t -> Response.t -> bool

type t =
  { encoder : encoder
  ; predicate : predicate
  ; preferred_encodings : Accept.encoding list
  }

let encoding_to_string = function
  | `Gzip -> "gzip"
  | `Deflate -> "deflate"
  | `Br -> "br"
  | `Zstd -> "zstd"
  | `Identity -> "identity"
  | `Star -> "*"
  | `Other s -> s

let args ~encoder ~predicate ~preferred_encodings =
  { encoder; predicate; preferred_encodings }

(** TODO: handle actual compression *)
let call { encoder = _; predicate; preferred_encodings } next request =
  let response = next request in

  if not (predicate request response)
  then response
  else
    let accept_encoding_header = Request.header "Accept-Encoding" request in
    match
      Content_negotiation.negotiate_encoding
        accept_encoding_header
        preferred_encodings
    with
    | None -> response
    | Some `Identity -> response
    | Some _encoding -> response (* TODO: actually compress the response body *)
