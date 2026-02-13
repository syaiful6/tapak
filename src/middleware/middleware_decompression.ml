open Imports
open Header_parser
open Bytesrw

type decoder = Accept.encoding -> Bytes.Reader.filter option
type t = decoder

let encoding_of_string = function
  | "gzip" -> Some `Gzip
  | "deflate" -> Some `Deflate
  | "br" -> Some `Br
  | "zstd" -> Some `Zstd
  | "identity" -> Some `Identity
  | _ -> None

let content_encodings req =
  match Request.header "Content-Encoding" req with
  | None -> None
  | Some s ->
    String.split_on_char ~sep:',' s
    |> List.map (fun x -> x |> String.trim |> String.lowercase_ascii)
    |> List.filter_map encoding_of_string
    |> fun xs -> if List.is_empty xs then None else Some xs

let call (decoder : decoder) next request =
  match content_encodings request with
  | None -> next request
  | Some encodings ->
    let reader =
      Request.body request |> Body.to_stream |> Bytesrw_util.reader_of_stream
    in
    let rec aux algorithms cr =
      match algorithms with
      | [] -> Ok cr
      | algo :: rest ->
        (match algo with
        | `Identity -> aux rest cr
        | _ ->
          (match decoder algo with
          | None -> Error `Unsupported_encoding
          | Some filter -> aux rest (filter cr)))
    in
    (match aux encodings reader with
    | Ok reader ->
      let new_request =
        request
        |> Request.with_
             ~body:(Bytesrw_util.stream_of_reader reader |> Body.of_stream)
        |> Request.remove_header "Content-Encoding"
        |> Request.remove_header "Content-Length"
      in
      next new_request
    | Error `Unsupported_encoding -> Response.create `Unsupported_media_type
    | Error (`Decompression_error _) -> Response.create `Bad_request)
