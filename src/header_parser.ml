open Angstrom

module Accept = struct
  type encoding =
    [ `Gzip
    | `Deflate
    | `Br
    | `Zstd
    | `Identity
    | `Star
    | `Other of string
    ]

  type p = string * string
  type q = int
  type 'a qlist = (q * 'a) list

  let is_space = function ' ' | '\t' -> true | _ -> false

  let is_token = function
    | '\000' .. '\031'
    | '\127' | ')' | '(' | '<' | '>' | '@' | ',' | ';' | ':' | '"' | '/' | '['
    | ']' | '?' | '=' | '{' | '}' | ' ' ->
      false
    | _ -> true

  let ows = skip is_space <|> return ()
  let token = take_while1 is_token

  let sep_by1_comma value_parser =
    sep_by1 (char ',') value_parser <* end_of_input

  let eval_parser parser default_value = function
    | None -> [ 1000, default_value ]
    | Some str ->
      (match parse_string ~consume:All parser str with
      | Ok lst -> lst
      | Error msg -> failwith msg)

  type param =
    | Q of int
    | Kv of p
    (** Parser for header parameters like defined in rfc
      https://tools.ietf.org/html/rfc7231#section-5.3.2 *)

  let q_of_string s = truncate (1000. *. float_of_string s)
  let qs = char '"' *> token <* char '"'

  let param : param t =
    ows
    *> char ';'
    *> ows
    *> (lift2
          (fun n v -> if n = "q" then Q (q_of_string v) else Kv (n, v))
          token
          (char '=' *> token)
       <|> lift2 (fun n v -> Kv (n, v)) token (char '=' *> qs))

  let params = many param

  let rec get_q params =
    match params with [] -> 1000 | Q q :: _ -> q | Kv _ :: rest -> get_q rest

  let encoding_value_parser =
    ows
    *> (char '*' *> return `Star
       <|> lift
             (fun s ->
                match String.lowercase_ascii s with
                | "gzip" -> `Gzip
                | "deflate" -> `Deflate
                | "br" -> `Br
                | "zstd" -> `Zstd
                | "identity" -> `Identity
                | other -> `Other other)
             token)

  let encoding_parser =
    lift2 (fun value q -> q, value) encoding_value_parser (lift get_q params)

  let encodings_parser = sep_by1_comma encoding_parser
  let encodings = eval_parser encodings_parser `Star

  let rec string_of_pl = function
    | [] -> ""
    | (k, v) :: r ->
      let e = Stringext.quote v in
      if v = e
      then Format.sprintf ";%s=%s%s" k v (string_of_pl r)
      else Format.sprintf ";%s=\"%s\"%s" k e (string_of_pl r)
end

module Media_type = struct
  type media_range =
    | Any
    | Type_wildcard of string
    | Concrete of string * string
    | Extension of string

  type media_type =
    { range : media_range
    ; params : (string * string) list
    }

  type accept_item =
    { media : media_type
    ; q : int
    }

  let is_space = function ' ' | '\t' -> true | _ -> false
  let is_token = Accept.is_token
  let ows = skip is_space <|> return ()
  let token = take_while1 is_token

  let media_range_parser =
    ows
    *> (lift2
          (fun typ subtyp ->
             match typ, subtyp with
             | "*", "*" -> Any
             | t, "*" -> Type_wildcard t
             | t, s ->
               Concrete (String.lowercase_ascii t, String.lowercase_ascii s))
          token
          (char '/' *> token)
       <|> lift (fun ext -> Extension ext) token)

  let media_param : (string * string) t =
    ows
    *> char ';'
    *> ows
    *> lift2
         (fun n v -> n, v)
         token
         (char '=' *> (token <|> (char '"' *> token <* char '"')))

  let accept_item_parser =
    lift2
      (fun range params ->
         let q_value =
           List.find_map
             (fun (k, v) ->
                if k = "q" then Some (Accept.q_of_string v) else None)
             params
         in
         let q = Option.value q_value ~default:1000 in
         let media_params =
           List.filter_map
             (fun (k, v) -> if k = "q" then None else Some (k, v))
             params
         in
         { media = { range; params = media_params }; q })
      media_range_parser
      (many media_param)

  let accept_parser = sep_by1 (char ',') accept_item_parser <* end_of_input

  let parse_accept accept_header =
    match accept_header with
    | None -> [ { media = { range = Any; params = [] }; q = 1000 } ]
    | Some str ->
      (match parse_string ~consume:All accept_parser str with
      | Ok lst -> lst
      | Error msg -> failwith msg)

  let matches_range (typ, subtyp) range =
    match range with
    | Any -> true
    | Type_wildcard t -> String.equal (String.lowercase_ascii typ) t
    | Concrete (t, s) ->
      String.equal (String.lowercase_ascii typ) t
      && String.equal (String.lowercase_ascii subtyp) s
    | Extension _ -> false

  let find_best_match content_type accept_items =
    let typ, subtyp =
      match String.split_on_char '/' content_type with
      | [ t; s ] -> String.lowercase_ascii t, String.lowercase_ascii s
      | [ t ] -> String.lowercase_ascii t, "*"
      | _ -> "*", "*"
    in
    let matches =
      List.filter
        (fun { media = { range; _ }; _ } -> matches_range (typ, subtyp) range)
        accept_items
      |> List.sort (fun a b -> compare b.q a.q)
    in
    match matches with [] -> None | x :: _ -> Some x

  let is_acceptable content_type accept_items =
    Option.is_some (find_best_match content_type accept_items)

  let json = "application/json"
  let html = "text/html"
  let xml = "application/xml"
  let text = "text/plain"
  let form = "application/x-www-form-urlencoded"
  let multipart = "multipart/form-data"
end

module Content_negotiation = struct
  type format =
    | Json
    | Html
    | Xml
    | Text
    | Other of string

  let format_to_media_type = function
    | Json -> Media_type.json
    | Html -> Media_type.html
    | Xml -> Media_type.xml
    | Text -> Media_type.text
    | Other s -> s

  let media_type_to_format = function
    | "application/json" -> Json
    | "text/html" -> Html
    | "application/xml" -> Xml
    | "text/xml" -> Xml
    | "text/plain" -> Text
    | other -> Other other

  let negotiate_format accept_header available_formats =
    let accept_items = Media_type.parse_accept accept_header in
    List.filter_map
      (fun format ->
         let media_type = format_to_media_type format in
         match Media_type.find_best_match media_type accept_items with
         | Some { q; _ } when q > 0 -> Some (q, format)
         | _ -> None)
      available_formats
    |> List.sort (fun (q1, _) (q2, _) -> compare q2 q1)
    |> function
    | [] -> None
    | (_, format) :: _ -> Some format

  let preferred_format ?(default = Html) accept_header available_formats =
    match negotiate_format accept_header available_formats with
    | Some fmt -> fmt
    | None -> default

  let accepts format accept_header =
    let media_type = format_to_media_type format in
    let accept_items = Media_type.parse_accept accept_header in
    Media_type.is_acceptable media_type accept_items

  let negotiate_encoding accept_encoding_header available_encodings =
    let encoding_items = Accept.encodings accept_encoding_header in
    List.find_map
      (fun encoding ->
         match
           List.find_opt
             (fun (q, enc) ->
                q > 0
                &&
                match enc, encoding with
                | `Gzip, `Gzip -> true
                | `Deflate, `Deflate -> true
                | `Br, `Br -> true
                | `Zstd, `Zstd -> true
                | `Identity, `Identity -> true
                | `Star, _ -> true
                | _ -> false)
             encoding_items
         with
         | Some _ -> Some encoding
         | None -> None)
      available_encodings

  let preferred_encoding
        ?(default = `Identity)
        accept_encoding_header
        available_encodings
    =
    match negotiate_encoding accept_encoding_header available_encodings with
    | Some enc -> enc
    | None -> default
end
