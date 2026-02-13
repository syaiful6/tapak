let () =
  Alcotest.run
    "Tapak Compressions"
    [ "Brotli", Test_brotli.suite; "Bytesrw_brotli", Test_bytesrw_brotli.suite ]
