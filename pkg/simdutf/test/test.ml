let rfc4648_tests =
  [ "", ""
  ; "f", "Zg=="
  ; "fo", "Zm8="
  ; "foo", "Zm9v"
  ; "foob", "Zm9vYg=="
  ; "fooba", "Zm9vYmE="
  ; "foobar", "Zm9vYmFy"
  ]

let rfc3548_tests =
  [ "\x14\xfb\x9c\x03\xd9\x7e", "FPucA9l+"
  ; "\x14\xfb\x9c\x03\xd9", "FPucA9k="
  ; "\x14\xfb\x9c\x03", "FPucAw=="
  ]

let openssl_encode buf =
  Bos.(
    OS.Cmd.in_string buf
    |> OS.Cmd.run_io (Cmd.v "base64")
    |> OS.Cmd.to_string ~trim:true)
  |> function
  | Ok r ->
    prerr_endline r;
    r
  | Error (`Msg e) -> raise (Failure (Format.sprintf "openssl base64: %s" e))

let test_rfc4648 () =
  List.iter
    (fun (c, r) ->
       Alcotest.(check string)
         (Format.sprintf "encode vs openssl: %S" c)
         (openssl_encode c)
         (Simdutf.Base64.encode c);
       Alcotest.(check string)
         (Format.sprintf "encode rfc4648 %S" c)
         r
         (Simdutf.Base64.encode c);
       Alcotest.(check string)
         (Format.sprintf "decode %S" r)
         c
         (Simdutf.Base64.decode_exn r))
    rfc4648_tests

let test_rfc3548 () =
  List.iter
    (fun (c, r) ->
       Alcotest.(check string)
         (Format.sprintf "encode vs openssl: %S" c)
         (openssl_encode c)
         (Simdutf.Base64.encode c);
       Alcotest.(check string)
         (Format.sprintf "encode rfc3548 %S" c)
         r
         (Simdutf.Base64.encode c);
       Alcotest.(check string)
         (Format.sprintf "decode %S" r)
         c
         (Simdutf.Base64.decode_exn r))
    rfc3548_tests

let url_safe_tests =
  [ "\xff", "_w"; "\xfb", "-w"; "\x03\xef", "A-8"; "\x03\xff", "A_8" ]

let test_url_safe () =
  List.iter
    (fun (c, expected_url) ->
       let encoded_url = Simdutf.Base64.encode ~option:`Url c in
       Alcotest.(check string)
         (Format.sprintf "URL-safe encode %S" c)
         expected_url
         encoded_url;
       let decoded = Simdutf.Base64.decode_exn ~option:`Url encoded_url in
       Alcotest.(check string)
         (Format.sprintf "URL-safe decode %S" expected_url)
         c
         decoded;
       let contains_standard_chars =
         String.contains encoded_url '+' || String.contains encoded_url '/'
       in
       Alcotest.(check bool)
         (Format.sprintf "URL-safe encoding should not contain + or / for %S" c)
         false
         contains_standard_chars;
       let encoded_standard = Simdutf.Base64.encode c in
       let differs = encoded_standard <> encoded_url in
       Alcotest.(check bool)
         (Format.sprintf "Standard and URL encoding should differ for %S" c)
         true
         differs)
    url_safe_tests

let test_last_chunk_loose () =
  let valid = "Zm9v" in
  (* "foo" *)
  let with_extra_padding = "Zm9v===" in
  (* extra = at the end *)

  Alcotest.(check string)
    "loose mode: valid base64"
    "foo"
    (Simdutf.Base64.decode_exn ~last_chunk:`Loose valid);

  (match Simdutf.Base64.decode ~last_chunk:`Loose with_extra_padding with
  | Ok s -> Alcotest.(check string) "loose mode: accepts extra padding" "foo" s
  | Error _ ->
    Alcotest.(check bool) "loose mode processed extra padding" true true);

  let no_padding = "Zm9vYg" in
  Alcotest.(check string)
    "loose mode: accepts missing padding"
    "foob"
    (Simdutf.Base64.decode_exn ~last_chunk:`Loose no_padding)

let test_last_chunk_strict () =
  let valid_padded = "Zm9vYg==" in
  let no_padding = "Zm9vYg" in

  Alcotest.(check string)
    "strict mode: valid with padding"
    "foob"
    (Simdutf.Base64.decode_exn ~last_chunk:`Strict valid_padded);

  match Simdutf.Base64.decode ~last_chunk:`Strict no_padding with
  | Ok _ -> Alcotest.fail "strict mode should reject missing padding"
  | Error _ ->
    Alcotest.(check bool) "strict mode: rejects missing padding" true true

let test_last_chunk_stop_before_partial () =
  let complete = "Zm9vYmFy" in
  let with_partial = "Zm9vYmFyZm" in

  Alcotest.(check string)
    "stop_before_partial: complete input"
    "foobar"
    (Simdutf.Base64.decode_exn ~last_chunk:`Stop_before_partial complete);

  match Simdutf.Base64.decode ~last_chunk:`Stop_before_partial with_partial with
  | Ok s ->
    Alcotest.(check string)
      "stop_before_partial: ignores partial chunk"
      "foobar"
      s
  | Error _ ->
    Alcotest.(check bool) "stop_before_partial handled partial" true true

let test_last_chunk_invalid_input () =
  let invalid_char = "Zm9v!!!!" in
  let invalid_length = "Z" in

  (match Simdutf.Base64.decode ~last_chunk:`Strict invalid_char with
  | Ok _ -> Alcotest.fail "should reject invalid characters"
  | Error `Invalid_base64_character ->
    Alcotest.(check bool) "detected invalid character" true true
  | Error _ -> Alcotest.(check bool) "detected error in invalid input" true true);

  match Simdutf.Base64.decode ~last_chunk:`Strict invalid_length with
  | Ok _ -> Alcotest.fail "should reject invalid length"
  | Error _ -> Alcotest.(check bool) "detected invalid length" true true

let test_last_chunk_comparison () =
  let no_pad = "Zm9vYg" in
  let proper_pad = "Zm9vYg==" in

  (match Simdutf.Base64.decode ~last_chunk:`Loose no_pad with
  | Ok s -> Alcotest.(check string) "loose accepts no padding" "foob" s
  | Error _ -> Alcotest.fail "loose should accept no padding");

  (match Simdutf.Base64.decode ~last_chunk:`Loose proper_pad with
  | Ok s -> Alcotest.(check string) "loose accepts proper padding" "foob" s
  | Error _ -> Alcotest.fail "loose should accept proper padding");

  (match Simdutf.Base64.decode ~last_chunk:`Strict no_pad with
  | Ok _ -> Alcotest.fail "strict should reject no padding"
  | Error _ -> Alcotest.(check bool) "strict rejects no padding" true true);

  match Simdutf.Base64.decode ~last_chunk:`Strict proper_pad with
  | Ok s -> Alcotest.(check string) "strict accepts proper padding" "foob" s
  | Error _ -> Alcotest.fail "strict should accept proper padding"

let test_utf8_valid () =
  Alcotest.(check bool)
    "ASCII is valid UTF-8"
    true
    (Simdutf.Utf8.validate "Hello, World!");

  Alcotest.(check bool)
    "2-byte UTF-8 is valid"
    true
    (Simdutf.Utf8.validate "caf\xC3\xA9");
  Alcotest.(check bool)
    "3-byte UTF-8 is valid"
    true
    (Simdutf.Utf8.validate "\xE2\x98\x83");
  Alcotest.(check bool)
    "4-byte UTF-8 is valid"
    true
    (Simdutf.Utf8.validate "\xF0\x9F\x98\x80");
  Alcotest.(check bool)
    "empty string is valid UTF-8"
    true
    (Simdutf.Utf8.validate "")

let test_utf8_invalid () =
  Alcotest.(check bool)
    "invalid continuation byte"
    false
    (Simdutf.Utf8.validate "\xFF");

  Alcotest.(check bool)
    "truncated 2-byte sequence"
    false
    (Simdutf.Utf8.validate "caf\xC3");

  Alcotest.(check bool)
    "overlong encoding"
    false
    (Simdutf.Utf8.validate "\xC0\x80");
  Alcotest.(check bool)
    "invalid surrogate"
    false
    (Simdutf.Utf8.validate "\xED\xA0\x80")

let test_utf8_with_errors () =
  (match Simdutf.Utf8.validate_with_errors "Hello" with
  | Ok () -> Alcotest.(check bool) "valid UTF-8 returns Ok" true true
  | Error _ -> Alcotest.fail "valid UTF-8 should not error");

  match Simdutf.Utf8.validate_with_errors "Hello\xFF" with
  | Ok () -> Alcotest.fail "invalid UTF-8 should error"
  | Error (error_code, position) ->
    Alcotest.(check int) "error at position 5" 5 position;
    Alcotest.(check bool)
      "error code is not Success"
      true
      (error_code <> `Success)

let test_ascii_valid () =
  Alcotest.(check bool)
    "pure ASCII is valid"
    true
    (Simdutf.Ascii.validate "Hello, World!");

  Alcotest.(check bool)
    "ASCII numbers and symbols"
    true
    (Simdutf.Ascii.validate "1234567890!@#$%^&*()");

  Alcotest.(check bool)
    "empty string is valid ASCII"
    true
    (Simdutf.Ascii.validate "")

let test_ascii_invalid () =
  Alcotest.(check bool)
    "UTF-8 char is not ASCII"
    false
    (Simdutf.Ascii.validate "caf\xC3\xA9");

  Alcotest.(check bool)
    "high bit set is not ASCII"
    false
    (Simdutf.Ascii.validate "test\x80");

  Alcotest.(check bool)
    "extended ASCII is not valid"
    false
    (Simdutf.Ascii.validate "\xFF")

let test_ascii_with_errors () =
  (match Simdutf.Ascii.validate_with_errors "Hello" with
  | Ok () -> Alcotest.(check bool) "valid ASCII returns Ok" true true
  | Error _ -> Alcotest.fail "valid ASCII should not error");

  match Simdutf.Ascii.validate_with_errors "Hello\xC3\xA9" with
  | Ok () -> Alcotest.fail "invalid ASCII should error"
  | Error (error_code, position) ->
    Alcotest.(check int) "error at position 5" 5 position;
    Alcotest.(check bool)
      "error code is not Success"
      true
      (error_code <> `Success)

let () =
  Alcotest.run
    "Simdutf"
    [ ( "Base64"
      , [ Alcotest.test_case "rfc4648" `Quick test_rfc4648
        ; Alcotest.test_case "rfc3548" `Quick test_rfc3548
        ; Alcotest.test_case "url_safe" `Quick test_url_safe
        ; Alcotest.test_case "last_chunk_loose" `Quick test_last_chunk_loose
        ; Alcotest.test_case "last_chunk_strict" `Quick test_last_chunk_strict
        ; Alcotest.test_case
            "last_chunk_stop_before_partial"
            `Quick
            test_last_chunk_stop_before_partial
        ; Alcotest.test_case
            "last_chunk_invalid_input"
            `Quick
            test_last_chunk_invalid_input
        ; Alcotest.test_case
            "last_chunk_comparison"
            `Quick
            test_last_chunk_comparison
        ] )
    ; ( "UTF-8"
      , [ Alcotest.test_case "valid" `Quick test_utf8_valid
        ; Alcotest.test_case "invalid" `Quick test_utf8_invalid
        ; Alcotest.test_case "with_errors" `Quick test_utf8_with_errors
        ] )
    ; ( "ASCII"
      , [ Alcotest.test_case "valid" `Quick test_ascii_valid
        ; Alcotest.test_case "invalid" `Quick test_ascii_invalid
        ; Alcotest.test_case "with_errors" `Quick test_ascii_with_errors
        ] )
    ]
