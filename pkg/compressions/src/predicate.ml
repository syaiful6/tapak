type t = Tapak.Request.t -> Tapak.Response.t -> bool

let ( && ) p1 p2 req resp = p1 req resp && p2 req resp
let ( || ) p1 p2 req resp = p1 req resp || p2 req resp
let not p req resp = not (p req resp)
let always _req _resp = true
let never _req _resp = false

let min_size threshold _req resp =
  match Tapak.Response.header "content-length" resp with
  | None -> true
  | Some length_str ->
    (match int_of_string_opt length_str with
    | None -> true (* invalid, default to compress *)
    | Some len -> len >= threshold)

let content_type_matches types _req resp =
  match Tapak.Response.header "content-type" resp with
  | None -> false
  | Some ct ->
    let media_type =
      match String.split_on_char ';' ct with
      | [] -> ""
      | hd :: _ -> String.trim hd |> String.lowercase_ascii
    in
    List.exists
      (fun ty -> String.equal media_type (String.lowercase_ascii ty))
      types

let compressible_content_type =
  content_type_matches
    [ "text/html"
    ; "text/plain"
    ; "text/css"
    ; "text/javascript"
    ; "text/xml"
    ; "application/json"
    ; "application/javascript"
    ; "application/xml"
    ; "application/xhtml+xml"
    ; "application/atom+xml"
    ; "application/rss+xml"
    ; "image/svg+xml"
    ]

let not_already_compressed =
  not
    (content_type_matches
       [ "application/gzip"
       ; "application/zip"
       ; "application/x-rar-compressed"
       ; "image/jpeg"
       ; "image/png"
       ; "image/gif"
       ; "image/webp"
       ; "video/mp4"
       ; "video/webm"
       ; "audio/mpeg"
       ; "audio/ogg"
       ])

let not_already_encoded _req resp =
  match Tapak.Response.header "content-encoding" resp with
  | None -> true
  | Some _ -> false

let respect_no_transform _req resp =
  match Tapak.Response.header "cache-control" resp with
  | None -> true
  | Some cc ->
    let has_no_transform =
      String.split_on_char ',' cc
      |> List.exists (fun directive ->
        let trimmed = String.trim directive |> String.lowercase_ascii in
        String.equal trimmed "no-transform")
    in
    Bool.not has_no_transform

let has_body _req resp =
  match Tapak.Response.status resp with
  | `Continue | `Switching_protocols | `No_content | `Not_modified -> false
  | _ -> true

let default_predicate =
  min_size 32
  && compressible_content_type
  && not_already_encoded
  && respect_no_transform
  && has_body
