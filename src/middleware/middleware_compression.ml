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
      (match encoder encoding with
      | None -> response
      | Some filter ->
        let compressed_stream =
          Response.body response
          |> Body.to_stream
          |> Bytesrw_util.reader_of_stream
          |> filter
          |> Bytesrw_util.stream_of_reader
        in

        let encoding_name = encoding_to_string encoding in
        let headers =
          Headers.add_to_list_header
            (Response.headers response)
            "Vary"
            "Accept-Encoding"
        in
        response
        |> Response.with_ ~body:(Body.of_stream compressed_stream) ~headers
        |> Response.remove_header "Content-Length"
        |> Response.add_header_or_replace ("Content-Encoding", encoding_name))
