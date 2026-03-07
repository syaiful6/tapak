let () =
  Alcotest.run
    "Tapak Compressions"
    [ "Bytesrw_brotli", Test_bytesrw_brotli.suite ]
