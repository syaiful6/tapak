let test_accept_encoding_parser () =
  let open Tapak.Header_parser.Accept in
  let encodings = encodings (Some "gzip, deflate;q=0.8, br;q=0.9") in
  Alcotest.(check int) "gzip quality" 1000 (fst (List.hd encodings));
  match snd (List.hd encodings) with
  | `Gzip -> ()
  | _ -> Alcotest.fail "Expected Gzip encoding"

let test_accept_media_type_parser () =
  let open Tapak.Header_parser.Media_type in
  let items =
    parse_accept
      (Some
         "text/html, application/json;q=0.9, application/xml;q=0.8, */*;q=0.1")
  in
  Alcotest.(check int) "first item quality" 1000 (List.hd items).q;
  match (List.hd items).media.range with
  | Concrete ("text", "html") -> ()
  | _ -> Alcotest.fail "Expected text/html"

let test_accept_wildcard () =
  let open Tapak.Header_parser.Media_type in
  let items = parse_accept (Some "*/*") in
  Alcotest.(check int) "wildcard quality" 1000 (List.hd items).q;
  match (List.hd items).media.range with
  | Any -> ()
  | _ -> Alcotest.fail "Expected Any wildcard"

let test_content_negotiation () =
  let open Tapak.Header_parser.Content_negotiation in
  let format =
    preferred_format
      (Some "text/html, application/json;q=0.9")
      [ `Json; `Html; `Xml ]
  in
  Alcotest.(check bool) "should prefer HTML" true (format = `Html)

let test_content_negotiation_json () =
  let open Tapak.Header_parser.Content_negotiation in
  let format =
    preferred_format
      (Some "application/json, text/html;q=0.8")
      [ `Json; `Html; `Xml ]
  in
  Alcotest.(check bool) "should prefer JSON" true (format = `Json)

let test_accepts_helper () =
  let open Tapak.Header_parser.Content_negotiation in
  let accept_header = Some "application/json, text/html" in
  Alcotest.(check bool) "accepts JSON" true (accepts `Json accept_header);
  Alcotest.(check bool) "accepts HTML" true (accepts `Html accept_header);
  Alcotest.(check bool) "doesn't accept XML" false (accepts `Xml accept_header)

let test_encoding_negotiation () =
  let open Tapak.Header_parser.Content_negotiation in
  let encoding =
    preferred_encoding
      (Some "gzip, deflate;q=0.8")
      [ `Gzip; `Deflate; `Identity ]
  in
  Alcotest.(check bool) "should prefer gzip" true (encoding = `Gzip)

let test_encoding_negotiation_zstd () =
  let open Tapak.Header_parser.Content_negotiation in
  let encoding =
    preferred_encoding
      (Some "zstd, gzip;q=0.9, deflate;q=0.8")
      [ `Zstd; `Gzip; `Deflate; `Identity ]
  in
  Alcotest.(check bool) "should prefer zstd" true (encoding = `Zstd)

let test_range_from () =
  let open Tapak.Header_parser.Range in
  let result = parse (Some "bytes=100-") in
  match result with
  | Ok [ From 100L ] -> ()
  | Ok _ -> Alcotest.fail "Expected From 100"
  | Error msg -> Alcotest.fail msg

let test_range_from_to () =
  let open Tapak.Header_parser.Range in
  let result = parse (Some "bytes=100-200") in
  match result with
  | Ok [ From_to (100L, 200L) ] -> ()
  | Ok _ -> Alcotest.fail "Expected From_to (100, 200)"
  | Error msg -> Alcotest.fail msg

let test_range_suffix () =
  let open Tapak.Header_parser.Range in
  let result = parse (Some "bytes=-500") in
  match result with
  | Ok [ Suffix 500L ] -> ()
  | Ok _ -> Alcotest.fail "Expected Suffix 500"
  | Error msg -> Alcotest.fail msg

let test_range_multiple () =
  let open Tapak.Header_parser.Range in
  let result = parse (Some "bytes=0-499, 1000-1499, -500") in
  match result with
  | Ok [ From_to (0L, 499L); From_to (1000L, 1499L); Suffix 500L ] -> ()
  | Ok _ -> Alcotest.fail "Expected multiple ranges"
  | Error msg -> Alcotest.fail msg

let test_range_with_spaces () =
  let open Tapak.Header_parser.Range in
  let result = parse (Some "bytes=100-200 , 300-400") in
  match result with
  | Ok [ From_to (100L, 200L); From_to (300L, 400L) ] -> ()
  | Ok _ -> Alcotest.fail "Expected ranges with spaces"
  | Error msg -> Alcotest.fail msg

let test_range_none () =
  let open Tapak.Header_parser.Range in
  let result = parse None in
  match result with
  | Ok [] -> ()
  | Ok _ -> Alcotest.fail "Expected empty list"
  | Error msg -> Alcotest.fail msg

let test_range_render () =
  let open Tapak.Header_parser.Range in
  let rendered = render [ From_to (0L, 499L); From 1000L; Suffix 500L ] in
  Alcotest.(check string) "rendered range" "bytes=0-499,1000-,-500" rendered

let test_date_parse_rfc1123 () =
  let open Tapak.Header_parser.Date in
  match parse "Mon, 27 Jul 2009 12:28:53 GMT" with
  | Error msg -> Alcotest.fail msg
  | Ok date ->
    Alcotest.(check int) "year" 2009 date.year;
    Alcotest.(check bool) "month is July" true (date.month = `Jul);
    Alcotest.(check int) "day" 27 date.day;
    Alcotest.(check int) "hour" 12 date.hour;
    Alcotest.(check int) "minute" 28 date.minute;
    Alcotest.(check int) "second" 53 date.second;
    Alcotest.(check bool) "weekday is Monday" true (date.weekday = `Mon)

let test_date_parse_utc () =
  let open Tapak.Header_parser.Date in
  (* Some servers use UTC instead of GMT *)
  match parse "Tue, 15 Nov 1994 08:12:31 UTC" with
  | Error msg -> Alcotest.fail msg
  | Ok date ->
    Alcotest.(check int) "year" 1994 date.year;
    Alcotest.(check bool) "month is November" true (date.month = `Nov);
    Alcotest.(check int) "day" 15 date.day;
    Alcotest.(check int) "hour" 8 date.hour;
    Alcotest.(check int) "minute" 12 date.minute;
    Alcotest.(check int) "second" 31 date.second;
    Alcotest.(check bool) "weekday is Tuesday" true (date.weekday = `Tue)

let test_date_parse_plus0000 () =
  let open Tapak.Header_parser.Date in
  (* Some servers use +0000 instead of GMT *)
  match parse "Wed, 01 Jan 2025 00:00:00 +0000" with
  | Error msg -> Alcotest.fail msg
  | Ok date ->
    Alcotest.(check int) "year" 2025 date.year;
    Alcotest.(check bool) "month is January" true (date.month = `Jan);
    Alcotest.(check int) "day" 1 date.day;
    Alcotest.(check int) "hour" 0 date.hour;
    Alcotest.(check int) "minute" 0 date.minute;
    Alcotest.(check int) "second" 0 date.second;
    Alcotest.(check bool) "weekday is Wednesday" true (date.weekday = `Wed)

let test_date_parse_invalid () =
  let open Tapak.Header_parser.Date in
  match parse "Invalid date string" with
  | Ok _ -> Alcotest.fail "Should have failed to parse invalid date"
  | Error _ -> ()

let test_date_parse_invalid_day () =
  let open Tapak.Header_parser.Date in
  (* Day 32 is invalid *)
  match parse "Mon, 32 Jul 2009 12:28:53 GMT" with
  | Ok _ -> Alcotest.fail "Should have failed to parse invalid day"
  | Error _ -> ()

let test_date_parse_invalid_hour () =
  let open Tapak.Header_parser.Date in
  (* Hour 24 is invalid *)
  match parse "Mon, 27 Jul 2009 24:28:53 GMT" with
  | Ok _ -> Alcotest.fail "Should have failed to parse invalid hour"
  | Error _ -> ()

let test_date_format () =
  let open Tapak.Header_parser.Date in
  let date =
    { year = 2009
    ; month = `Jul
    ; day = 27
    ; hour = 12
    ; minute = 28
    ; second = 53
    ; weekday = `Mon
    }
  in
  let formatted = Format.asprintf "%a" pp date in
  Alcotest.(check string)
    "formatted date"
    "Mon, 27 Jul 2009 12:28:53 GMT"
    formatted

let test_date_parse_format_roundtrip () =
  let open Tapak.Header_parser.Date in
  let original = "Fri, 25 Dec 2024 15:30:45 GMT" in
  match parse original with
  | Error msg -> Alcotest.fail msg
  | Ok date ->
    let formatted = Format.asprintf "%a" pp date in
    Alcotest.(check string) "roundtrip preserves format" original formatted

let test_date_to_ptime () =
  let open Tapak.Header_parser.Date in
  let date =
    { year = 2009
    ; month = `Jul
    ; day = 27
    ; hour = 12
    ; minute = 28
    ; second = 53
    ; weekday = `Mon
    }
  in
  match to_ptime date with
  | Ok ptime ->
    let (year, month, day), ((hour, minute, second), _tz) =
      Ptime.to_date_time ptime
    in
    Alcotest.(check int) "year" 2009 year;
    Alcotest.(check int) "month" 7 month;
    Alcotest.(check int) "day" 27 day;
    Alcotest.(check int) "hour" 12 hour;
    Alcotest.(check int) "minute" 28 minute;
    Alcotest.(check int) "second" 53 second
  | Error msg -> Alcotest.fail msg

let test_date_of_ptime () =
  let open Tapak.Header_parser.Date in
  (* Create a Ptime for "Mon, 27 Jul 2009 12:28:53 GMT" *)
  match Ptime.of_date_time ((2009, 7, 27), ((12, 28, 53), 0)) with
  | None -> Alcotest.fail "Failed to create Ptime"
  | Some ptime ->
    (match of_ptime ptime with
    | Ok date ->
      Alcotest.(check int) "year" 2009 date.year;
      Alcotest.(check bool) "month is July" true (date.month = `Jul);
      Alcotest.(check int) "day" 27 date.day;
      Alcotest.(check int) "hour" 12 date.hour;
      Alcotest.(check int) "minute" 28 date.minute;
      Alcotest.(check int) "second" 53 date.second;
      Alcotest.(check bool) "weekday is Monday" true (date.weekday = `Mon)
    | Error msg -> Alcotest.fail msg)

let test_date_roundtrip () =
  let open Tapak.Header_parser.Date in
  let original =
    { year = 2024
    ; month = `Dec
    ; day = 25
    ; hour = 15
    ; minute = 30
    ; second = 45
    ; weekday = `Wed
    }
  in
  match to_ptime original with
  | Error msg -> Alcotest.fail ("to_ptime failed: " ^ msg)
  | Ok ptime ->
    (match of_ptime ptime with
    | Error msg -> Alcotest.fail ("of_ptime failed: " ^ msg)
    | Ok roundtrip ->
      Alcotest.(check int) "year" original.year roundtrip.year;
      Alcotest.(check bool) "month" true (original.month = roundtrip.month);
      Alcotest.(check int) "day" original.day roundtrip.day;
      Alcotest.(check int) "hour" original.hour roundtrip.hour;
      Alcotest.(check int) "minute" original.minute roundtrip.minute;
      Alcotest.(check int) "second" original.second roundtrip.second)

let test_date_truncates_subseconds () =
  let open Tapak.Header_parser.Date in
  (* Create a Ptime with subsecond precision (500ms = 0.5s) *)
  match Ptime.of_date_time ((2024, 12, 25), ((15, 30, 45), 0)) with
  | None -> Alcotest.fail "Failed to create base Ptime"
  | Some base_ptime ->
    (* Add 500ms to the Ptime *)
    let ptime_with_subsec =
      match Ptime.Span.of_float_s 0.5 with
      | None -> Alcotest.fail "Failed to create span"
      | Some span ->
        (match Ptime.add_span base_ptime span with
        | None -> Alcotest.fail "Failed to add subseconds"
        | Some t -> t)
    in
    (* Convert to Date.t - should truncate subseconds *)
    (match of_ptime ptime_with_subsec with
    | Error msg -> Alcotest.fail ("of_ptime failed: " ^ msg)
    | Ok date ->
      (* The date should have second=45, not 46 (truncated, not rounded) *)
      Alcotest.(check int) "second (truncated)" 45 date.second;
      (* Convert back to Ptime and verify it matches the truncated time *)
      (match to_ptime date with
      | Error msg -> Alcotest.fail ("to_ptime failed: " ^ msg)
      | Ok ptime_result ->
        (* The result should equal the base time (without subseconds) *)
        Alcotest.(check bool)
          "subseconds dropped"
          true
          (Ptime.equal ptime_result base_ptime)))

let tests =
  [ ( "Header Parser"
    , [ Alcotest.test_case
          "Accept-Encoding parser"
          `Quick
          test_accept_encoding_parser
      ; Alcotest.test_case
          "Accept media type parser"
          `Quick
          test_accept_media_type_parser
      ; Alcotest.test_case "Accept wildcard" `Quick test_accept_wildcard
      ; Alcotest.test_case
          "Content negotiation prefers HTML"
          `Quick
          test_content_negotiation
      ; Alcotest.test_case
          "Content negotiation prefers JSON"
          `Quick
          test_content_negotiation_json
      ; Alcotest.test_case "Accepts helper function" `Quick test_accepts_helper
      ; Alcotest.test_case
          "Encoding negotiation"
          `Quick
          test_encoding_negotiation
      ; Alcotest.test_case
          "Encoding negotiation with zstd"
          `Quick
          test_encoding_negotiation_zstd
      ; Alcotest.test_case "Range header: from" `Quick test_range_from
      ; Alcotest.test_case "Range header: from-to" `Quick test_range_from_to
      ; Alcotest.test_case "Range header: suffix" `Quick test_range_suffix
      ; Alcotest.test_case "Range header: multiple" `Quick test_range_multiple
      ; Alcotest.test_case
          "Range header: with spaces"
          `Quick
          test_range_with_spaces
      ; Alcotest.test_case "Range header: none" `Quick test_range_none
      ; Alcotest.test_case "Range header: render" `Quick test_range_render
      ; Alcotest.test_case "Date: parse RFC 1123" `Quick test_date_parse_rfc1123
      ; Alcotest.test_case "Date: parse with UTC" `Quick test_date_parse_utc
      ; Alcotest.test_case
          "Date: parse with +0000"
          `Quick
          test_date_parse_plus0000
      ; Alcotest.test_case "Date: parse invalid" `Quick test_date_parse_invalid
      ; Alcotest.test_case
          "Date: parse invalid day"
          `Quick
          test_date_parse_invalid_day
      ; Alcotest.test_case
          "Date: parse invalid hour"
          `Quick
          test_date_parse_invalid_hour
      ; Alcotest.test_case "Date: format with pp_hum" `Quick test_date_format
      ; Alcotest.test_case
          "Date: parse/format roundtrip"
          `Quick
          test_date_parse_format_roundtrip
      ; Alcotest.test_case "Date: to_ptime conversion" `Quick test_date_to_ptime
      ; Alcotest.test_case "Date: of_ptime conversion" `Quick test_date_of_ptime
      ; Alcotest.test_case
          "Date: roundtrip conversion"
          `Quick
          test_date_roundtrip
      ; Alcotest.test_case
          "Date: truncates subseconds"
          `Quick
          test_date_truncates_subseconds
      ] )
  ]
