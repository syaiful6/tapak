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
      ] )
  ]
