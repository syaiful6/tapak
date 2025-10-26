open Tapak_compressions

let test_zstd_basic_compression () =
  let original = "Hello, World! This is a test string for Zstd compression." in

  Zstd.Compress.with_cctx (fun cctx ->
    let status, consumed, compressed, is_finished =
      Zstd.Compress.compress_stream ~finish:false cctx original
    in

    Alcotest.(check bool)
      "compression should succeed"
      true
      (status = Zstd.Success);
    Alcotest.(check int)
      "should consume all input"
      (String.length original)
      consumed;
    Alcotest.(check bool) "should not be finished yet" false is_finished;

    let status2, _consumed2, compressed2, is_finished2 =
      Zstd.Compress.compress_stream ~finish:true cctx ""
    in

    Alcotest.(check bool) "finish should succeed" true (status2 = Zstd.Success);
    Alcotest.(check bool) "should be finished" true is_finished2;

    let full_compressed = compressed ^ compressed2 in

    Zstd.Decompress.with_dctx (fun dctx ->
      let rec decompress_all acc input =
        if String.length input = 0
        then acc
        else
          let status, consumed, output =
            Zstd.Decompress.decompress_stream dctx input
          in
          let remaining =
            String.sub input consumed (String.length input - consumed)
          in
          match status with
          | Zstd.Error -> Alcotest.fail "Decompression failed"
          | Zstd.Finished -> acc ^ output
          | Zstd.Needs_more_input -> decompress_all (acc ^ output) remaining
      in

      let decompressed = decompress_all "" full_compressed in
      Alcotest.(check string)
        "decompressed should match original"
        original
        decompressed))

let test_zstd_empty_string () =
  let original = "" in

  Zstd.Compress.with_cctx (fun cctx ->
    let status, consumed, compressed, is_finished =
      Zstd.Compress.compress_stream ~finish:true cctx original
    in

    Alcotest.(check bool)
      "compression should succeed"
      true
      (status = Zstd.Success);
    Alcotest.(check int) "should consume all input" 0 consumed;
    Alcotest.(check bool) "should be finished" true is_finished;

    Zstd.Decompress.with_dctx (fun dctx ->
      let rec decompress_all acc input =
        if String.length input = 0
        then acc
        else
          let status, consumed, output =
            Zstd.Decompress.decompress_stream dctx input
          in
          let remaining =
            String.sub input consumed (String.length input - consumed)
          in
          match status with
          | Zstd.Error -> Alcotest.fail "Decompression failed"
          | Zstd.Finished -> acc ^ output
          | Zstd.Needs_more_input -> decompress_all (acc ^ output) remaining
      in

      let decompressed = decompress_all "" compressed in
      Alcotest.(check string)
        "decompressed should be empty"
        original
        decompressed))

let test_zstd_large_data () =
  let chunk = "The quick brown fox jumps over the lazy dog. " in
  let original = String.concat "" (List.init 1000 (fun _ -> chunk)) in

  Zstd.Compress.with_cctx ~level:5 (fun cctx ->
    let status1, consumed1, compressed1, _is_finished1 =
      Zstd.Compress.compress_stream ~finish:false cctx original
    in

    Alcotest.(check bool)
      "compression should succeed"
      true
      (status1 = Zstd.Success);
    Alcotest.(check int)
      "should consume all input"
      (String.length original)
      consumed1;

    let status2, _consumed2, compressed2, is_finished2 =
      Zstd.Compress.compress_stream ~finish:true cctx ""
    in

    Alcotest.(check bool) "finish should succeed" true (status2 = Zstd.Success);
    Alcotest.(check bool) "should be finished" true is_finished2;

    let compressed = compressed1 ^ compressed2 in

    Alcotest.(check bool)
      "compressed should be smaller"
      true
      (String.length compressed < String.length original);

    Zstd.Decompress.with_dctx (fun dctx ->
      let rec decompress_all acc input =
        if String.length input = 0
        then acc
        else
          let status, consumed, output =
            Zstd.Decompress.decompress_stream dctx input
          in
          let remaining =
            String.sub input consumed (String.length input - consumed)
          in
          match status with
          | Zstd.Error -> Alcotest.fail "Decompression failed"
          | Zstd.Finished -> acc ^ output
          | Zstd.Needs_more_input -> decompress_all (acc ^ output) remaining
      in

      let decompressed = decompress_all "" compressed in
      Alcotest.(check string)
        "decompressed should match original"
        original
        decompressed))

let test_zstd_streaming () =
  let chunks =
    [ "First chunk of data. "
    ; "Second chunk of data. "
    ; "Third chunk of data. "
    ; "Final chunk of data."
    ]
  in
  let original = String.concat "" chunks in

  Zstd.Compress.with_cctx (fun cctx ->
    let compressed_parts = ref [] in

    List.iter
      (fun chunk ->
         let status, consumed, compressed, _is_finished =
           Zstd.Compress.compress_stream ~finish:false cctx chunk
         in

         Alcotest.(check bool)
           "compression should succeed"
           true
           (status = Zstd.Success);
         Alcotest.(check int)
           "should consume all input"
           (String.length chunk)
           consumed;

         if String.length compressed > 0
         then compressed_parts := compressed :: !compressed_parts)
      chunks;

    let status, _consumed, compressed, is_finished =
      Zstd.Compress.compress_stream ~finish:true cctx ""
    in

    Alcotest.(check bool) "finish should succeed" true (status = Zstd.Success);
    Alcotest.(check bool) "should be finished" true is_finished;

    if String.length compressed > 0
    then compressed_parts := compressed :: !compressed_parts;

    let full_compressed = String.concat "" (List.rev !compressed_parts) in

    Zstd.Decompress.with_dctx (fun dctx ->
      let rec decompress_all acc input =
        if String.length input = 0
        then acc
        else
          let status, consumed, output =
            Zstd.Decompress.decompress_stream dctx input
          in
          let remaining =
            String.sub input consumed (String.length input - consumed)
          in
          match status with
          | Zstd.Error -> Alcotest.fail "Decompression failed"
          | Zstd.Finished -> acc ^ output
          | Zstd.Needs_more_input -> decompress_all (acc ^ output) remaining
      in

      let decompressed = decompress_all "" full_compressed in
      Alcotest.(check string)
        "decompressed should match original"
        original
        decompressed))

let test_zstd_compression_levels () =
  let original = "Testing different compression levels with Zstd!" in

  List.iter
    (fun level ->
       Zstd.Compress.with_cctx ~level (fun cctx ->
         let status1, consumed1, compressed1, _is_finished1 =
           Zstd.Compress.compress_stream ~finish:false cctx original
         in

         Alcotest.(check bool)
           (Printf.sprintf "compression level %d should succeed" level)
           true
           (status1 = Zstd.Success);
         Alcotest.(check int)
           "should consume all input"
           (String.length original)
           consumed1;

         let status2, _consumed2, compressed2, is_finished2 =
           Zstd.Compress.compress_stream ~finish:true cctx ""
         in

         Alcotest.(check bool)
           "finish should succeed"
           true
           (status2 = Zstd.Success);
         Alcotest.(check bool) "should be finished" true is_finished2;

         let compressed = compressed1 ^ compressed2 in

         Zstd.Decompress.with_dctx (fun dctx ->
           let rec decompress_all acc input =
             if String.length input = 0
             then acc
             else
               let status, consumed, output =
                 Zstd.Decompress.decompress_stream dctx input
               in
               let remaining =
                 String.sub input consumed (String.length input - consumed)
               in
               match status with
               | Zstd.Error -> Alcotest.fail "Decompression failed"
               | Zstd.Finished -> acc ^ output
               | Zstd.Needs_more_input ->
                 decompress_all (acc ^ output) remaining
           in

           let decompressed = decompress_all "" compressed in
           Alcotest.(check string)
             (Printf.sprintf "decompressed at level %d should match" level)
             original
             decompressed)))
    [ 1; 3; 6; 9; 15 ]

let suite =
  [ "basic compression", `Quick, test_zstd_basic_compression
  ; "empty string", `Quick, test_zstd_empty_string
  ; "large data", `Quick, test_zstd_large_data
  ; "streaming", `Quick, test_zstd_streaming
  ; "compression levels", `Quick, test_zstd_compression_levels
  ]
