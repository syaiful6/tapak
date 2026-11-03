module Range = Tapak.Private.Header_parser.Range
module Accept = Tapak.Private.Header_parser.Accept
module Content_negotiation = Tapak.Private.Header_parser.Content_negotiation

let range_testable =
  let pp fmt = function
    | Range.From start -> Format.fprintf fmt "From %Ld" start
    | Range.From_to (start, end_) ->
      Format.fprintf fmt "From_to (%Ld, %Ld)" start end_
    | Range.Suffix len -> Format.fprintf fmt "Suffix %Ld" len
  in
  Alcotest.testable pp ( = )

let ranges = Alcotest.(result (list range_testable) string)

let check_ok msg expected header =
  Alcotest.check ranges msg (Ok expected) (Range.parse (Some header))

let check_error msg header =
  match Range.parse (Some header) with
  | Error _ -> ()
  | Ok _ -> Alcotest.failf "%s: expected %S to be rejected" msg header

let test_range_none () =
  Alcotest.check
    ranges
    "missing header yields no ranges"
    (Ok [])
    (Range.parse None)

let test_range_from_to () =
  check_ok "first 500 bytes" [ Range.From_to (0L, 499L) ] "bytes=0-499";
  check_ok "second 500 bytes" [ Range.From_to (500L, 999L) ] "bytes=500-999";
  check_ok "single byte" [ Range.From_to (0L, 0L) ] "bytes=0-0"

let test_range_from () =
  check_ok "open-ended range" [ Range.From 9500L ] "bytes=9500-";
  check_ok "from zero" [ Range.From 0L ] "bytes=0-"

let test_range_suffix () =
  check_ok "last 500 bytes" [ Range.Suffix 500L ] "bytes=-500";
  check_ok "zero-length suffix" [ Range.Suffix 0L ] "bytes=-0"

let test_range_multiple () =
  check_ok
    "mixed range kinds"
    [ Range.From_to (0L, 499L); Range.Suffix 500L; Range.From 1000L ]
    "bytes=0-499,-500,1000-";
  check_ok
    "RFC 7233 example: first and last byte"
    [ Range.From_to (0L, 0L); Range.Suffix 1L ]
    "bytes=0-0,-1";
  check_ok
    "RFC 7233 example: non-canonical overlapping ranges"
    [ Range.From_to (500L, 600L); Range.From_to (601L, 999L) ]
    "bytes=500-600,601-999"

let test_range_whitespace () =
  check_ok
    "space after comma"
    [ Range.From_to (0L, 499L); Range.Suffix 500L ]
    "bytes=0-499, -500";
  check_ok
    "spaces and tabs around comma"
    [ Range.From_to (0L, 1L); Range.From_to (5L, 6L) ]
    "bytes=0-1 \t, \t5-6";
  check_ok "space after unit" [ Range.From_to (0L, 1L) ] "bytes= 0-1";
  check_ok
    "multiple spaces before comma"
    [ Range.From_to (0L, 1L); Range.From_to (5L, 6L) ]
    "bytes=0-1  ,5-6";
  check_ok "multiple spaces after unit" [ Range.From_to (0L, 1L) ] "bytes=  0-1"

let test_range_large_values () =
  check_ok
    "offsets beyond 32 bits"
    [ Range.From_to (4294967296L, 8589934592L) ]
    "bytes=4294967296-8589934592";
  check_ok
    "max int64"
    [ Range.From Int64.max_int ]
    (Printf.sprintf "bytes=%Ld-" Int64.max_int)

let test_range_leading_zeros () =
  check_ok
    "leading zeros are decimal"
    [ Range.From_to (7L, 10L) ]
    "bytes=007-010"

let test_range_overflow () =
  (* Must surface as an Error rather than an escaping exception. *)
  check_error "start overflows int64" "bytes=99999999999999999999-";
  check_error "suffix overflows int64" "bytes=-99999999999999999999";
  check_error "end overflows int64" "bytes=0-99999999999999999999";
  check_error "max int64 + 1" "bytes=9223372036854775808-"

let test_range_end_before_start () =
  check_error "end precedes start" "bytes=500-100";
  check_error "end precedes start in a list" "bytes=0-1,10-9"

let test_range_invalid_syntax () =
  check_error "empty value" "";
  check_error "unit only" "bytes=";
  check_error "missing unit" "0-499";
  check_error "missing equals" "bytes 0-499";
  check_error "unsupported unit" "items=0-10";
  check_error "lone dash" "bytes=-";
  check_error "non-numeric" "bytes=abc-def";
  check_error "negative start" "bytes=--5";
  check_error "trailing comma" "bytes=0-1,";
  check_error "leading comma" "bytes=,0-1";
  check_error "empty element" "bytes=0-1,,2-3";
  check_error "double dash" "bytes=0--1";
  check_error "trailing garbage" "bytes=0-1x";
  check_error "decimal value" "bytes=1.5-2"

let test_range_render () =
  Alcotest.(check string)
    "renders each kind"
    "bytes=0-499,1000-,-500"
    (Range.render
       [ Range.From_to (0L, 499L); Range.From 1000L; Range.Suffix 500L ])

let test_range_roundtrip () =
  let cases =
    [ [ Range.From_to (0L, 499L) ]
    ; [ Range.From 0L ]
    ; [ Range.Suffix 1L ]
    ; [ Range.From_to (0L, 0L); Range.Suffix 1L; Range.From 1024L ]
    ; [ Range.From Int64.max_int ]
    ]
  in
  List.iter
    (fun rs ->
       Alcotest.check
         ranges
         "parse (render r) = r"
         (Ok rs)
         (Range.parse (Some (Range.render rs))))
    cases

let encoding_testable : Accept.encoding Alcotest.testable =
  let pp fmt = function
    | `Gzip -> Format.pp_print_string fmt "gzip"
    | `Deflate -> Format.pp_print_string fmt "deflate"
    | `Br -> Format.pp_print_string fmt "br"
    | `Zstd -> Format.pp_print_string fmt "zstd"
    | `Identity -> Format.pp_print_string fmt "identity"
    | `Star -> Format.pp_print_string fmt "*"
    | `Other s -> Format.fprintf fmt "Other %S" s
  in
  Alcotest.testable pp ( = )

let test_accept_encodings () =
  Alcotest.(check (list (pair int encoding_testable)))
    "sorted by quality"
    [ 1000, `Gzip; 800, `Deflate ]
    (Accept.encodings (Some "gzip, deflate;q=0.8"))

let test_negotiate_encoding () =
  let check msg expected header avail =
    Alcotest.(check (option encoding_testable))
      msg
      expected
      (Content_negotiation.negotiate_encoding header avail)
  in
  check
    "picks gzip"
    (Some `Gzip)
    (Some "gzip, deflate;q=0.8")
    [ `Gzip; `Deflate ];
  check "q=0 is not acceptable" None (Some "gzip;q=0") [ `Gzip ];
  check "star matches anything" (Some `Br) (Some "*") [ `Br ]

let test_accept_encodings_whitespace () =
  Alcotest.(check (list (pair int encoding_testable)))
    "multiple spaces around separators"
    [ 1000, `Br; 500, `Gzip ]
    (Accept.encodings (Some "br,   gzip  ;  q=0.5"))

let test_negotiate_format () =
  let negotiate h avail = Content_negotiation.negotiate_format h avail in
  Alcotest.(check bool)
    "highest q wins"
    true
    (negotiate (Some "text/html, application/json;q=0.9") [ `Json; `Html ]
    = Some `Html);
  Alcotest.(check bool)
    "wildcard subtype"
    true
    (negotiate (Some "application/*") [ `Html; `Json ] = Some `Json);
  Alcotest.(check bool)
    "no acceptable format"
    true
    (negotiate (Some "image/png") [ `Json; `Html ] = None)

let tests =
  [ ( "Header_parser.Range"
    , [ Alcotest.test_case "missing header" `Quick test_range_none
      ; Alcotest.test_case "from-to" `Quick test_range_from_to
      ; Alcotest.test_case "from" `Quick test_range_from
      ; Alcotest.test_case "suffix" `Quick test_range_suffix
      ; Alcotest.test_case "multiple ranges" `Quick test_range_multiple
      ; Alcotest.test_case "whitespace" `Quick test_range_whitespace
      ; Alcotest.test_case "large values" `Quick test_range_large_values
      ; Alcotest.test_case "leading zeros" `Quick test_range_leading_zeros
      ; Alcotest.test_case "int64 overflow" `Quick test_range_overflow
      ; Alcotest.test_case "end before start" `Quick test_range_end_before_start
      ; Alcotest.test_case "invalid syntax" `Quick test_range_invalid_syntax
      ; Alcotest.test_case "render" `Quick test_range_render
      ; Alcotest.test_case "render/parse roundtrip" `Quick test_range_roundtrip
      ] )
  ; ( "Header_parser.Content_negotiation"
    , [ Alcotest.test_case "accept-encoding" `Quick test_accept_encodings
      ; Alcotest.test_case
          "accept-encoding whitespace"
          `Quick
          test_accept_encodings_whitespace
      ; Alcotest.test_case "negotiate encoding" `Quick test_negotiate_encoding
      ; Alcotest.test_case "negotiate format" `Quick test_negotiate_format
      ] )
  ]
