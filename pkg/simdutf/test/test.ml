open Simdutf

let bigstring_of_string s =
  let len = String.length s in
  let bs = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
  for i = 0 to len - 1 do
    Bigarray.Array1.set bs i (String.get s i)
  done;
  bs

let string_of_bigstring bs =
  let len = Bigarray.Array1.dim bs in
  let bytes = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set bytes i (Bigarray.Array1.get bs i)
  done;
  Bytes.to_string bytes

let bigstring_testable =
  Alcotest.testable (Fmt.of_to_string string_of_bigstring) (fun a b ->
    String.equal (string_of_bigstring a) (string_of_bigstring b))

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

let url_safe_tests =
  [ "\xff", "_w"; "\xfb", "-w"; "\x03\xef", "A-8"; "\x03\xff", "A_8" ]

let test_rfc4648 () =
  List.iter
    (fun (decoded_str, encoded_str) ->
       let decoded_bs = bigstring_of_string decoded_str in
       let encoded_bs = bigstring_of_string encoded_str in

       Alcotest.(check bigstring_testable)
         (Format.sprintf "encode rfc4648 %S" decoded_str)
         encoded_bs
         (Base64.encode decoded_bs);

       Alcotest.(check bigstring_testable)
         (Format.sprintf "decode %S" encoded_str)
         decoded_bs
         (Base64.decode_exn encoded_bs))
    rfc4648_tests

let test_rfc3548 () =
  List.iter
    (fun (decoded_str, encoded_str) ->
       let decoded_bs = bigstring_of_string decoded_str in
       let encoded_bs = bigstring_of_string encoded_str in

       Alcotest.(check bigstring_testable)
         (Format.sprintf "encode rfc3548 %S" decoded_str)
         encoded_bs
         (Base64.encode decoded_bs);

       Alcotest.(check bigstring_testable)
         (Format.sprintf "decode %S" encoded_str)
         decoded_bs
         (Base64.decode_exn encoded_bs))
    rfc3548_tests

let test_url_safe () =
  List.iter
    (fun (decoded_str, expected_url) ->
       let decoded_bs = bigstring_of_string decoded_str in
       let expected_bs = bigstring_of_string expected_url in

       let encoded_url_bs = Base64.encode ~option:`Url decoded_bs in
       Alcotest.(check bigstring_testable)
         (Format.sprintf "URL-safe encode %S" decoded_str)
         expected_bs
         encoded_url_bs;

       let decoded_url_bs = Base64.decode_exn ~option:`Url encoded_url_bs in
       Alcotest.(check bigstring_testable)
         (Format.sprintf "URL-safe decode %S" expected_url)
         decoded_bs
         decoded_url_bs)
    url_safe_tests

let test_base64_slicing () =
  let original_str = "foobar" in
  let encoded_str = "Zm9vYmFy" in

  let padded_input_str = "---foobar---" in
  let padded_input_bs = bigstring_of_string padded_input_str in

  let encoded_slice_bs = Base64.encode ~offset:3 ~len:6 padded_input_bs in
  Alcotest.(check bigstring_testable)
    "encode slice"
    (bigstring_of_string encoded_str)
    encoded_slice_bs;

  let padded_encoded_str = "***Zm9vYmFy***" in
  let padded_encoded_bs = bigstring_of_string padded_encoded_str in

  let decoded_slice_bs = Base64.decode_exn ~offset:3 ~len:8 padded_encoded_bs in
  Alcotest.(check bigstring_testable)
    "decode slice"
    (bigstring_of_string original_str)
    decoded_slice_bs

let test_utf8_valid () =
  Alcotest.(check bool)
    "ASCII is valid UTF-8"
    true
    (Utf8.validate "Hello, World!");
  Alcotest.(check bool)
    "2-byte UTF-8 is valid"
    true
    (Utf8.validate "caf\xC3\xA9")

let test_utf8_invalid () =
  Alcotest.(check bool) "invalid continuation byte" false (Utf8.validate "\xFF")

let () =
  Alcotest.run
    "Simdutf"
    [ ( "Base64"
      , [ Alcotest.test_case "rfc4648" `Quick test_rfc4648
        ; Alcotest.test_case "rfc3548" `Quick test_rfc3548
        ; Alcotest.test_case "url_safe" `Quick test_url_safe
        ; Alcotest.test_case "slicing" `Quick test_base64_slicing
        ] )
    ; ( "UTF-8"
      , [ Alcotest.test_case "valid" `Quick test_utf8_valid
        ; Alcotest.test_case "invalid" `Quick test_utf8_invalid
        ] )
    ]
