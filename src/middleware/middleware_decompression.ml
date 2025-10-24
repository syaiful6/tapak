open Imports
open Header_parser

module type S = sig
  val decompress :
     Bigstringaf.t Piaf.IOVec.t Piaf.Stream.t
    -> (string Piaf.Stream.t, [> Piaf.Error.t ]) result
end

type decoder = Accept.encoding -> (module S) option
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
    let stream = Request.body request |> Body.to_stream in
    let rec aux algorithms current_stream =
      match algorithms with
      | [] -> Ok current_stream
      | algo :: rest ->
        (match decoder algo with
        | None -> Error `Unsupported_encoding
        | Some (module D : S) ->
          (match D.decompress current_stream with
          | Error e -> Error (`Decompression_error e)
          | Ok string_stream ->
            let iovec_stream =
              Piaf.Stream.from ~f:(fun () ->
                match Piaf.Stream.take string_stream with
                | None -> None
                | Some str ->
                  let len = String.length str in
                  let buf = Bigstringaf.of_string ~off:0 ~len str in
                  Some { Piaf.IOVec.buffer = buf; off = 0; len })
            in
            aux rest iovec_stream))
    in
    (match aux encodings stream with
    | Ok decompressed_stream ->
      let string_stream =
        Piaf.Stream.from ~f:(fun () ->
          match Piaf.Stream.take decompressed_stream with
          | None -> None
          | Some { Piaf.IOVec.buffer; off; len } ->
            Some (Bigstringaf.substring buffer ~off ~len))
      in
      let new_request =
        request
        |> Request.with_ ~body:(string_stream |> Body.of_string_stream)
        |> Request.remove_header "Content-Encoding"
        |> Request.remove_header "Content-Length"
      in
      next new_request
    | Error `Unsupported_encoding -> Response.create `Unsupported_media_type
    | Error (`Decompression_error _) -> Response.create `Bad_request)
