open Imports
open Header_parser

module type S = sig
  val decompress : Body.t -> (Body.t, [> Piaf.Error.t ]) result
end

module Identity : S = struct
  let decompress body = Ok body
end

type decoder = Accept.encoding -> (module S) option
type args = decoder
type state = args

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

external init : args -> state = "%identity"

let call (decoder : decoder) next request =
  match content_encodings request with
  | None -> next request
  | Some encodings ->
    let body = Request.body request in
    let rec aux algorithms body =
      match algorithms with
      | [] -> Ok body
      | algo :: rest ->
        (match decoder algo with
        | None -> Error `Unsupported_encoding
        | Some (module D : S) ->
          (match D.decompress body with
          | Error e -> Error (`Decompression_error e)
          | Ok decompressed_body -> aux rest decompressed_body))
    in
    (match aux encodings body with
    | Ok decompressed_body ->
      let new_request =
        request
        |> Request.with_ ~body:decompressed_body
        |> Request.remove_header "Content-Encoding"
        |> Request.remove_header "Content-Length"
      in
      next new_request
    | Error `Unsupported_encoding -> Response.create `Unsupported_media_type
    | Error (`Decompression_error _) -> Response.create `Bad_request)
