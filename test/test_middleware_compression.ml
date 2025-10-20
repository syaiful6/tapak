open Tapak

module Mock_encoder : Middleware.Compression.Encoder = struct
  let compress stream = Ok stream
end

module Mock_failing_encoder : Middleware.Compression.Encoder = struct
  let compress _stream = Error (`Connect_error "Mock compression error")
end

let make_mock_encoder encoding : Middleware.Compression.encoder = function
  | enc when enc = encoding ->
    Some (module Mock_encoder : Middleware.Compression.Encoder)
  | _ -> None

let make_failing_encoder encoding : Middleware.Compression.encoder = function
  | enc when enc = encoding ->
    Some (module Mock_failing_encoder : Middleware.Compression.Encoder)
  | _ -> None

let make_request ?accept_encoding () =
  let headers =
    match accept_encoding with
    | None -> Headers.empty
    | Some ae -> Headers.add Headers.empty "Accept-Encoding" ae
  in
  Request.create
    ~scheme:`HTTP
    ~version:Versions.HTTP.HTTP_1_1
    ~headers
    ~meth:`GET
    ~body:Body.empty
    "/test"

let make_response ?vary ?content_length ?content_encoding body =
  let headers =
    let h = Headers.empty in
    let h = match vary with None -> h | Some v -> Headers.add h "Vary" v in
    let h =
      match content_length with
      | None -> h
      | Some len -> Headers.add h "Content-Length" (string_of_int len)
    in
    let h =
      match content_encoding with
      | None -> h
      | Some enc -> Headers.add h "Content-Encoding" enc
    in
    h
  in
  Response.create
    ~version:Versions.HTTP.HTTP_1_1
    ~headers
    ~body:(Body.of_string body)
    `OK

let get_header name response = Headers.get (Response.headers response) name
let always_compress _req _res = true
let never_compress _req _res = false

let test_no_accept_encoding () =
  let request = make_request () in
  let original_response = make_response "test body" in

  let compression_middleware =
    Middleware.Compression.middleware
      ~predicate:always_compress
      ~preferred_encodings:[ `Gzip ]
      (make_mock_encoder `Gzip)
  in

  let handler _req = original_response in
  let response = Middleware.apply compression_middleware handler request in

  Alcotest.(check (option string))
    "Content-Encoding is gzip"
    (Some "gzip")
    (get_header "Content-Encoding" response);

  Alcotest.(check (option string))
    "Vary header added"
    (Some "Accept-Encoding")
    (get_header "Vary" response)

let test_compression_with_gzip () =
  let request = make_request ~accept_encoding:"gzip, deflate" () in
  let original_response = make_response ~content_length:9 "test body" in

  let compression_middleware =
    Middleware.Compression.middleware
      ~predicate:always_compress
      ~preferred_encodings:[ `Gzip ]
      (make_mock_encoder `Gzip)
  in

  let handler _req = original_response in
  let response = Middleware.apply compression_middleware handler request in

  Alcotest.(check (option string))
    "Content-Encoding is gzip"
    (Some "gzip")
    (get_header "Content-Encoding" response);

  Alcotest.(check (option string))
    "Content-Length removed"
    None
    (get_header "Content-Length" response);

  Alcotest.(check (option string))
    "Vary header added"
    (Some "Accept-Encoding")
    (get_header "Vary" response)

let test_vary_header_merge () =
  let request = make_request ~accept_encoding:"gzip" () in
  let original_response =
    make_response ~vary:"Cookie, User-Agent" "test body"
  in

  let compression_middleware =
    Middleware.Compression.middleware
      ~predicate:always_compress
      ~preferred_encodings:[ `Gzip ]
      (make_mock_encoder `Gzip)
  in

  let handler _req = original_response in
  let response = Middleware.apply compression_middleware handler request in

  Alcotest.(check (option string))
    "Vary header merged"
    (Some "Cookie, User-Agent, Accept-Encoding")
    (get_header "Vary" response)

let test_vary_header_no_duplicate () =
  let request = make_request ~accept_encoding:"gzip" () in
  let original_response =
    make_response ~vary:"Accept-Encoding, Cookie" "test body"
  in

  let compression_middleware =
    Middleware.Compression.middleware
      ~predicate:always_compress
      ~preferred_encodings:[ `Gzip ]
      (make_mock_encoder `Gzip)
  in

  let handler _req = original_response in
  let response = Middleware.apply compression_middleware handler request in

  Alcotest.(check (option string))
    "Vary header not duplicated"
    (Some "Accept-Encoding, Cookie")
    (get_header "Vary" response)

let test_identity_encoding () =
  let request = make_request ~accept_encoding:"identity" () in
  let original_response = make_response "test body" in

  let compression_middleware =
    Middleware.Compression.middleware
      ~predicate:always_compress
      ~preferred_encodings:[ `Gzip ]
      (make_mock_encoder `Gzip)
  in

  let handler _req = original_response in
  let response = Middleware.apply compression_middleware handler request in

  Alcotest.(check (option string))
    "no Content-Encoding for identity"
    None
    (get_header "Content-Encoding" response)

let test_predicate_blocks_compression () =
  let request = make_request ~accept_encoding:"gzip" () in
  let original_response = make_response "test body" in

  let compression_middleware =
    Middleware.Compression.middleware
      ~predicate:never_compress
      ~preferred_encodings:[ `Gzip ]
      (make_mock_encoder `Gzip)
  in

  let handler _req = original_response in
  let response = Middleware.apply compression_middleware handler request in

  Alcotest.(check (option string))
    "predicate blocks compression"
    None
    (get_header "Content-Encoding" response)

let test_no_encoder_available () =
  let request = make_request ~accept_encoding:"br" () in
  let original_response = make_response "test body" in

  let compression_middleware =
    Middleware.Compression.middleware
      ~predicate:always_compress
      ~preferred_encodings:[ `Gzip; `Br ]
      (make_mock_encoder `Gzip)
  in

  let handler _req = original_response in
  let response = Middleware.apply compression_middleware handler request in

  Alcotest.(check (option string))
    "no compression without encoder"
    None
    (get_header "Content-Encoding" response)

let test_compression_failure () =
  let request = make_request ~accept_encoding:"gzip" () in
  let original_response = make_response ~content_length:9 "test body" in

  let compression_middleware =
    Middleware.Compression.middleware
      ~predicate:always_compress
      ~preferred_encodings:[ `Gzip ]
      (make_failing_encoder `Gzip)
  in

  let handler _req = original_response in
  let response = Middleware.apply compression_middleware handler request in

  Alcotest.(check (option string))
    "keeps Content-Length on failure"
    (Some "9")
    (get_header "Content-Length" response);

  Alcotest.(check (option string))
    "no Content-Encoding on failure"
    None
    (get_header "Content-Encoding" response)

let test_encoding_preference () =
  let request = make_request ~accept_encoding:"gzip, deflate, br" () in
  let original_response = make_response "test body" in

  let compression_middleware =
    Middleware.Compression.middleware
      ~predicate:always_compress
      ~preferred_encodings:[ `Gzip; `Deflate; `Br ]
      (make_mock_encoder `Gzip)
  in

  let handler _req = original_response in
  let response = Middleware.apply compression_middleware handler request in

  Alcotest.(check (option string))
    "prefers gzip"
    (Some "gzip")
    (get_header "Content-Encoding" response)

let test_replace_content_encoding () =
  let request = make_request ~accept_encoding:"gzip" () in
  let original_response =
    make_response ~content_encoding:"deflate" "test body"
  in

  let compression_middleware =
    Middleware.Compression.middleware
      ~predicate:always_compress
      ~preferred_encodings:[ `Gzip ]
      (make_mock_encoder `Gzip)
  in

  let handler _req = original_response in
  let response = Middleware.apply compression_middleware handler request in

  Alcotest.(check (option string))
    "Content-Encoding replaced"
    (Some "gzip")
    (get_header "Content-Encoding" response)

let tests =
  [ ( "Middleware Compression"
    , [ Alcotest.test_case
          "compression without Accept-Encoding (defaults to accept all)"
          `Quick
          test_no_accept_encoding
      ; Alcotest.test_case
          "compression with gzip"
          `Quick
          test_compression_with_gzip
      ; Alcotest.test_case "Vary header merge" `Quick test_vary_header_merge
      ; Alcotest.test_case
          "Vary header no duplicate"
          `Quick
          test_vary_header_no_duplicate
      ; Alcotest.test_case
          "identity encoding skips compression"
          `Quick
          test_identity_encoding
      ; Alcotest.test_case
          "predicate blocks compression"
          `Quick
          test_predicate_blocks_compression
      ; Alcotest.test_case
          "no encoder available"
          `Quick
          test_no_encoder_available
      ; Alcotest.test_case
          "compression failure returns original"
          `Quick
          test_compression_failure
      ; Alcotest.test_case "encoding preference" `Quick test_encoding_preference
      ; Alcotest.test_case
          "replace existing Content-Encoding"
          `Quick
          test_replace_content_encoding
      ] )
  ]
