open Header_parser

type encoder = Accept.encoding -> Bytesrw.Bytes.Writer.filter option
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

let call { encoder; predicate; preferred_encodings } next request =
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
    | Some encoding ->
      (match
         encoder encoding, Body.to_streaming_fn (Response.body response)
       with
      | Some filter, Some streaming_fn ->
        let filter_writer writer flush =
          let w = filter ~eod:true writer in
          (* TODO: should we set eod to true here? *)
          streaming_fn w flush
        in
        let encoding_name = encoding_to_string encoding in
        response
        |> Response.with_ ~body:(Body.writer filter_writer)
        |> Response.add_header "Vary" "Accept-Encoding"
        |> Response.remove_header "Content-Length"
        |> Response.add_header_or_replace "Content-Encoding" encoding_name
      | _, _ -> response)
