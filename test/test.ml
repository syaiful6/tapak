let () =
  Alcotest.run
    "Tapak Tests"
    (List.flatten
       [ Test_middleware_compression.tests
       ; Test_static.tests
       ; Test_sse.tests
       ; Test_openapi.tests
       ; Test_csrf.tests
       ; Test_middleware_cors.tests
       ])
