open Header_parser

module type S = sig
  val compress :
     string Piaf.Stream.t
    -> (string Piaf.Stream.t, [> Piaf.Error.t ]) result
end

type encoder = Accept.encoding -> (module S) option
type predicate = Request.t -> Response.t -> bool

type args =
  { encoder : encoder
  ; predicate : predicate
  ; preferred_encodings : Accept.encoding list
  }

type state = args

let encoding_to_string = function
  | `Gzip -> "gzip"
  | `Deflate -> "deflate"
  | `Br -> "br"
  | `Zstd -> "zstd"
  | `Identity -> "identity"
  | `Star -> "*"
  | `Other s -> s

external init : args -> state = "%identity"

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
      | Some (module E : S) ->
        let body_stream = Response.body response |> Body.to_stream in
        let string_stream =
          Piaf.Stream.from ~f:(fun () ->
            match Piaf.Stream.take body_stream with
            | None -> None
            | Some { Piaf.IOVec.buffer; off; len } ->
              Some (Bigstringaf.substring buffer ~off ~len))
        in

        (match E.compress string_stream with
        | Error _ -> response
        | Ok compressed_stream ->
          let encoding_name = encoding_to_string encoding in
          let headers =
            Headers.add_to_list_header
              (Response.headers response)
              "Vary"
              "Accept-Encoding"
          in
          response
          |> Response.with_
               ~body:(Body.of_string_stream compressed_stream)
               ~headers
          |> Response.remove_header "Content-Length"
          |> Response.add_header_or_replace ("Content-Encoding", encoding_name)))
