(* Main test runner for compression library *)

let () =
  Alcotest.run
    "Tapak Compressions"
    [ "Zstd", Test_zstd.suite
    ; "Brotli", Test_brotli.suite
    ; "Gzip", Test_gzip.suite
    ]
