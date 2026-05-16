let trusted_proxies = [ Ipaddr.Prefix.of_string_exn "10.0.1.0/24" ]

let test_is_secure_trusted_with_proto () =
  let request =
    Tapak.Request.make
      ~client_addr:"10.0.1.8"
      ~headers:(Http.Header.of_list [ "X-Forwarded-Proto", "https" ])
      "/admin"
  in
  Alcotest.(check bool)
    "trusted proxy with X-Forwarded-Proto: https should be secure"
    true
    (Tapak.Request.is_secure ~trusted_proxies request)

let test_is_secure_trusted_without_proto () =
  let request = Tapak.Request.make ~client_addr:"10.0.1.8" "/admin" in
  Alcotest.(check bool)
    "trusted proxy without X-Forwarded-Proto should not be secure"
    false
    (Tapak.Request.is_secure ~trusted_proxies request)

let test_is_secure_trusted_http_proto () =
  let request =
    Tapak.Request.make
      ~client_addr:"10.0.1.8"
      ~headers:(Http.Header.of_list [ "X-Forwarded-Proto", "http" ])
      "/admin"
  in
  Alcotest.(check bool)
    "trusted proxy with X-Forwarded-Proto: http should not be secure"
    false
    (Tapak.Request.is_secure ~trusted_proxies request)

let test_is_secure_untrusted_proxy () =
  let request =
    Tapak.Request.make
      ~client_addr:"192.168.1.1"
      ~headers:(Http.Header.of_list [ "X-Forwarded-Proto", "https" ])
      "/admin"
  in
  Alcotest.(check bool)
    "untrusted proxy with X-Forwarded-Proto: https should not be secure"
    false
    (Tapak.Request.is_secure ~trusted_proxies request)

let tests =
  [ ( "Request"
    , [ Alcotest.test_case
          "is_secure: trusted proxy with https proto"
          `Quick
          test_is_secure_trusted_with_proto
      ; Alcotest.test_case
          "is_secure: trusted proxy without proto header"
          `Quick
          test_is_secure_trusted_without_proto
      ; Alcotest.test_case
          "is_secure: trusted proxy with http proto"
          `Quick
          test_is_secure_trusted_http_proto
      ; Alcotest.test_case
          "is_secure: untrusted proxy with https proto"
          `Quick
          test_is_secure_untrusted_proxy
      ] )
  ]
