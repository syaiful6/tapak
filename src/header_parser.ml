open Imports
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

  let rec _string_of_pl = function
    | [] -> ""
    | (k, v) :: r ->
      let e = Stringext.quote v in
      if v = e
      then Format.sprintf ";%s=%s%s" k v (_string_of_pl r)
      else Format.sprintf ";%s=\"%s\"%s" k e (_string_of_pl r)
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
      match String.split_on_char ~sep:'/' content_type with
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

module Range = struct
  type t =
    | From of int64
    | From_to of int64 * int64
    | Suffix of int64

  let to_string = function
    | From start -> Format.sprintf "%Ld-" start
    | From_to (start, end_) -> Format.sprintf "%Ld-%Ld" start end_
    | Suffix len -> Format.sprintf "-%Ld" len

  let render xs =
    Format.sprintf "bytes=%s" (String.concat ~sep:"," (List.map to_string xs))

  let is_space = function ' ' | '\t' -> true | _ -> false
  let ows = skip is_space <|> return ()

  let int64_parser =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int64.of_string

  let range_parser =
    ows
    *> (lift2
          (fun start end_opt ->
             match end_opt with
             | Some end_ -> From_to (start, end_)
             | None -> From start)
          int64_parser
          (char '-' *> option None (int64_parser >>| Option.some))
       <|> char '-' *> lift (fun len -> Suffix len) int64_parser)

  let ranges_parser =
    string "bytes=" *> sep_by1 (ows *> char ',' *> ows) range_parser
    <* end_of_input

  let parse header_value =
    match header_value with
    | None -> Ok []
    | Some str ->
      (match parse_string ~consume:All ranges_parser str with
      | Ok lst -> Ok lst
      | Error msg -> Error msg)
end

module Content_negotiation = struct
  type format =
    [ `Json
    | `Html
    | `Xml
    | `Text
    | `Other of string
    ]

  let format_to_media_type = function
    | `Json -> Media_type.json
    | `Html -> Media_type.html
    | `Xml -> Media_type.xml
    | `Text -> Media_type.text
    | `Other s -> s

  let media_type_to_format = function
    | "application/json" -> `Json
    | "text/html" -> `Html
    | "application/xml" -> `Xml
    | "text/xml" -> `Xml
    | "text/plain" -> `Text
    | other -> `Other other

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

  let preferred_format ?(default = `Html) accept_header available_formats =
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

module Date = struct
  open Angstrom

  type month =
    [ `Jan
    | `Feb
    | `Mar
    | `Apr
    | `May
    | `Jun
    | `Jul
    | `Aug
    | `Sep
    | `Oct
    | `Nov
    | `Dec
    ]

  type year = int
  type day = int

  type weekday =
    [ `Mon
    | `Tue
    | `Wed
    | `Thu
    | `Fri
    | `Sat
    | `Sun
    ]

  type t =
    { year : year
    ; month : month
    ; day : day
    ; hour : int
    ; minute : int
    ; second : int
    ; weekday : weekday
    }

  type date =
    { year : year
    ; month : month
    ; day : day
    }

  let month_to_string = function
    | `Jan -> "Jan"
    | `Feb -> "Feb"
    | `Mar -> "Mar"
    | `Apr -> "Apr"
    | `May -> "May"
    | `Jun -> "Jun"
    | `Jul -> "Jul"
    | `Aug -> "Aug"
    | `Sep -> "Sep"
    | `Oct -> "Oct"
    | `Nov -> "Nov"
    | `Dec -> "Dec"

  let weekday_to_string = function
    | `Mon -> "Mon"
    | `Tue -> "Tue"
    | `Wed -> "Wed"
    | `Thu -> "Thu"
    | `Fri -> "Fri"
    | `Sat -> "Sat"
    | `Sun -> "Sun"

  let pp formatter { year; month; day; hour; minute; second; weekday } =
    Format.fprintf
      formatter
      "%s, %02d %s %04d %02d:%02d:%02d GMT"
      (weekday_to_string weekday)
      day
      (month_to_string month)
      year
      hour
      minute
      second

  let ( <$ ) a p = p >>| fun _ -> a
  (* map to constant value *)

  let parse_week_day =
    `Mon
    <$ string "Mon"
    <|> (`Tue <$ string "Tue")
    <|> (`Wed <$ string "Wed")
    <|> (`Thu <$ string "Thu")
    <|> (`Fri <$ string "Fri")
    <|> (`Sat <$ string "Sat")
    <|> (`Sun <$ string "Sun")

  let parse_month =
    `Jan
    <$ string "Jan"
    <|> (`Feb <$ string "Feb")
    <|> (`Mar <$ string "Mar")
    <|> (`Apr <$ string "Apr")
    <|> (`May <$ string "May")
    <|> (`Jun <$ string "Jun")
    <|> (`Jul <$ string "Jul")
    <|> (`Aug <$ string "Aug")
    <|> (`Sep <$ string "Sep")
    <|> (`Oct <$ string "Oct")
    <|> (`Nov <$ string "Nov")
    <|> (`Dec <$ string "Dec")

  let sp = () <$ char ' '
  let any_digit = satisfy (function '0' .. '9' -> true | _ -> false)
  let char_to_int s = Char.code s - Char.code '0'

  let digit2 =
    let format2 x y = (x * 10) + y in
    format2 <$> (char_to_int <$> any_digit) <*> (char_to_int <$> any_digit)

  let digit4 =
    let format4 w x y z = (w * 1000) + (x * 100) + (y * 10) + z in
    format4
    <$> (char_to_int <$> any_digit)
    <*> (char_to_int <$> any_digit)
    <*> (char_to_int <$> any_digit)
    <*> (char_to_int <$> any_digit)

  let fail_if_nothing msg parser =
    parser >>= function Some v -> return v | None -> fail msg

  let valid_days d = if d >= 1 && d <= 31 then Some d else None

  let parse_date =
    lift3
      (fun day month year -> { day; month; year })
      (fail_if_nothing "Invalid day" (digit2 <* sp >>| valid_days))
      (parse_month <* sp)
      (fail_if_nothing "Invalid year" (digit4 <* sp >>| fun year -> Some year))

  let parse_time =
    let valid_hour d = if d >= 0 && d <= 23 then Some d else None in
    let valid_minute_seconds d = if d >= 0 && d <= 59 then Some d else None in
    lift3
      (fun hour minute second -> hour, minute, second)
      (fail_if_nothing "Invalid hour" (digit2 <* char ':' >>| valid_hour))
      (fail_if_nothing
         "Invalid minute"
         (digit2 <* char ':' >>| valid_minute_seconds))
      (fail_if_nothing "Invalid second" (digit2 >>| valid_minute_seconds))

  let rfc1123_parser =
    lift4
      (fun weekday date (hour, minute, second) () ->
         { year = date.year
         ; month = date.month
         ; day = date.day
         ; hour
         ; minute
         ; second
         ; weekday
         })
      (parse_week_day <* string ", ")
      parse_date
      parse_time
      (* RFC 2616 defines GMT only but there are actually ill-formed ones such
         as "+0000" and "UTC" in the wild. *)
      (sp <* (string "GMT" <|> string "+0000" <|> string "UTC") <* end_of_input)

  let parse date_str = parse_string ~consume:All rfc1123_parser date_str

  let month_to_int = function
    | `Jan -> 1
    | `Feb -> 2
    | `Mar -> 3
    | `Apr -> 4
    | `May -> 5
    | `Jun -> 6
    | `Jul -> 7
    | `Aug -> 8
    | `Sep -> 9
    | `Oct -> 10
    | `Nov -> 11
    | `Dec -> 12

  let int_to_month = function
    | 1 -> Some `Jan
    | 2 -> Some `Feb
    | 3 -> Some `Mar
    | 4 -> Some `Apr
    | 5 -> Some `May
    | 6 -> Some `Jun
    | 7 -> Some `Jul
    | 8 -> Some `Aug
    | 9 -> Some `Sep
    | 10 -> Some `Oct
    | 11 -> Some `Nov
    | 12 -> Some `Dec
    | _ -> None

  let weekday_of_ptime_weekday (wd : Ptime.weekday) : weekday =
    match wd with
    | `Mon -> `Mon
    | `Tue -> `Tue
    | `Wed -> `Wed
    | `Thu -> `Thu
    | `Fri -> `Fri
    | `Sat -> `Sat
    | `Sun -> `Sun

  let to_ptime { year; month; day; hour; minute; second; weekday = _ } =
    match
      Ptime.of_date_time
        ((year, month_to_int month, day), ((hour, minute, second), 0))
    with
    | Some ptime -> Ok ptime
    | None -> Error "Invalid date/time values"

  let of_ptime ptime =
    (* Truncate to second precision to match HTTP date format.
       This ensures correct If-Modified-Since comparisons by dropping
       subsecond precision that filesystems may provide. *)
    let ptime_truncated = Ptime.truncate ~frac_s:0 ptime in
    let (year, month_int, day), ((hour, minute, second), _tz_offset_s) =
      Ptime.to_date_time ptime_truncated
    in
    let weekday = Ptime.weekday ptime_truncated |> weekday_of_ptime_weekday in
    match int_to_month month_int with
    | Some month -> Ok { year; month; day; hour; minute; second; weekday }
    | None -> Error "Invalid month"

  let equal : t -> t -> bool =
   fun d1 d2 ->
    d1.year = d2.year
    && d1.month = d2.month
    && d1.day = d2.day
    && d1.hour = d2.hour
    && d1.minute = d2.minute
    && d1.second = d2.second
end
