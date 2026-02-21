let () =
  Eio_main.run @@ fun _env ->
  Alcotest.run
    "Tapak Tests"
    (List.flatten
       [ Test_form.tests
       ; Test_router.tests
       ; Test_static.tests
       ; Test_sse.tests
       ; Test_csrf.tests
       ; Test_middleware_cors.tests
       ; Test_channel.tests
       ])
