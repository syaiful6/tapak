open Tapak_compressions

let test_brotli_basic_compression () =
  let original =
    "Hello, World! This is a test string for Brotli compression."
  in

  Brotli.Encoder.with_encoder (fun encoder ->
    (* Compress the data with Finish operation *)
    let success, consumed, compressed, is_finished =
      Brotli.Encoder.compress_stream encoder original Brotli.Finish
    in

    Alcotest.(check bool) "compression should succeed" true success;
    Alcotest.(check int)
      "should consume all input"
      (String.length original)
      consumed;
    Alcotest.(check bool) "should be finished" true is_finished;

    (* Decompress and verify *)
    Brotli.Decoder.with_decoder (fun decoder ->
      let rec decompress_all acc input =
        if String.length input = 0
        then acc
        else
          let status, consumed, output =
            Brotli.Decoder.decompress_stream decoder input
          in
          let remaining =
            String.sub input consumed (String.length input - consumed)
          in
          match status with
          | Brotli.Error -> Alcotest.fail "Decompression failed"
          | Brotli.Success -> acc ^ output
          | Brotli.Needs_more_input -> decompress_all (acc ^ output) remaining
          | Brotli.Needs_more_output -> decompress_all (acc ^ output) input
      in

      let decompressed = decompress_all "" compressed in
      Alcotest.(check string)
        "decompressed should match original"
        original
        decompressed))

let test_brotli_empty_string () =
  let original = "" in

  Brotli.Encoder.with_encoder (fun encoder ->
    let success, consumed, compressed, is_finished =
      Brotli.Encoder.compress_stream encoder original Brotli.Finish
    in

    Alcotest.(check bool) "compression should succeed" true success;
    Alcotest.(check int) "should consume all input" 0 consumed;
    Alcotest.(check bool) "should be finished" true is_finished;

    (* Decompress empty compressed data *)
    Brotli.Decoder.with_decoder (fun decoder ->
      let rec decompress_all acc input =
        if String.length input = 0
        then acc
        else
          let status, consumed, output =
            Brotli.Decoder.decompress_stream decoder input
          in
          let remaining =
            String.sub input consumed (String.length input - consumed)
          in
          match status with
          | Brotli.Error -> Alcotest.fail "Decompression failed"
          | Brotli.Success -> acc ^ output
          | Brotli.Needs_more_input -> decompress_all (acc ^ output) remaining
          | Brotli.Needs_more_output -> decompress_all (acc ^ output) input
      in

      let decompressed = decompress_all "" compressed in
      Alcotest.(check string)
        "decompressed should be empty"
        original
        decompressed))

let test_brotli_large_data () =
  (* Create a large string with repetitive data (compresses well) *)
  let chunk = "The quick brown fox jumps over the lazy dog. " in
  let original = String.concat "" (List.init 1000 (fun _ -> chunk)) in

  Brotli.Encoder.with_encoder ~quality:6 (fun encoder ->
    (* Process the data first *)
    let success1, consumed1, compressed1, _is_finished1 =
      Brotli.Encoder.compress_stream encoder original Brotli.Process
    in

    Alcotest.(check bool) "compression should succeed" true success1;
    Alcotest.(check int)
      "should consume all input"
      (String.length original)
      consumed1;

    (* Finish the stream *)
    let success2, _consumed2, compressed2, is_finished2 =
      Brotli.Encoder.compress_stream encoder "" Brotli.Finish
    in

    Alcotest.(check bool) "finish should succeed" true success2;
    Alcotest.(check bool) "should be finished" true is_finished2;

    let compressed = compressed1 ^ compressed2 in

    (* Check that compression actually reduced the size *)
    Alcotest.(check bool)
      "compressed should be smaller"
      true
      (String.length compressed < String.length original);

    (* Decompress and verify *)
    Brotli.Decoder.with_decoder (fun decoder ->
      let rec decompress_all acc input prev_acc_len iterations =
        (* Safety: bail if too many iterations *)
        if iterations > 10000
        then Alcotest.fail "Too many decompression iterations";
        if String.length input = 0
        then acc
        else
          let status, consumed, output =
            Brotli.Decoder.decompress_stream decoder input
          in
          let remaining =
            String.sub input consumed (String.length input - consumed)
          in
          let new_acc = acc ^ output in
          match status with
          | Brotli.Error -> Alcotest.fail "Decompression failed"
          | Brotli.Success -> new_acc
          | Brotli.Needs_more_input ->
            decompress_all
              new_acc
              remaining
              (String.length new_acc)
              (iterations + 1)
          | Brotli.Needs_more_output ->
            (* Safety check: if we're not making progress, fail *)
            if
              consumed = 0
              && String.length output = 0
              && String.length new_acc = prev_acc_len
            then Alcotest.fail "Decompression stalled (no progress)";
            decompress_all new_acc input (String.length new_acc) (iterations + 1)
      in

      let decompressed = decompress_all "" compressed 0 0 in
      Alcotest.(check string)
        "decompressed should match original"
        original
        decompressed))

let test_brotli_streaming () =
  (* Test streaming compression with multiple chunks using Process and Finish *)
  let chunks =
    [ "First chunk of data. "
    ; "Second chunk of data. "
    ; "Third chunk of data. "
    ; "Final chunk of data."
    ]
  in
  let original = String.concat "" chunks in

  Brotli.Encoder.with_encoder (fun encoder ->
    let compressed_parts = ref [] in

    (* Compress each chunk *)
    List.iteri
      (fun i chunk ->
         let is_last = i = List.length chunks - 1 in
         let op = if is_last then Brotli.Finish else Brotli.Process in
         let success, consumed, compressed, is_finished =
           Brotli.Encoder.compress_stream encoder chunk op
         in

         Alcotest.(check bool) "compression should succeed" true success;
         Alcotest.(check int)
           "should consume all input"
           (String.length chunk)
           consumed;

         if String.length compressed > 0
         then compressed_parts := compressed :: !compressed_parts;

         if is_last
         then Alcotest.(check bool) "should be finished" true is_finished)
      chunks;

    let full_compressed = String.concat "" (List.rev !compressed_parts) in

    (* Decompress *)
    Brotli.Decoder.with_decoder (fun decoder ->
      let rec decompress_all acc input =
        if String.length input = 0
        then acc
        else
          let status, consumed, output =
            Brotli.Decoder.decompress_stream decoder input
          in
          let remaining =
            String.sub input consumed (String.length input - consumed)
          in
          match status with
          | Brotli.Error -> Alcotest.fail "Decompression failed"
          | Brotli.Success -> acc ^ output
          | Brotli.Needs_more_input -> decompress_all (acc ^ output) remaining
          | Brotli.Needs_more_output -> decompress_all (acc ^ output) input
      in

      let decompressed = decompress_all "" full_compressed in
      Alcotest.(check string)
        "decompressed should match original"
        original
        decompressed))

let test_brotli_flush_operation () =
  (* Test the Flush operation which forces output without finishing *)
  let original = "Data that needs flushing." in

  Brotli.Encoder.with_encoder (fun encoder ->
    (* Process data with Flush *)
    let success1, consumed1, compressed1, is_finished1 =
      Brotli.Encoder.compress_stream encoder original Brotli.Flush
    in

    Alcotest.(check bool) "flush should succeed" true success1;
    Alcotest.(check int)
      "should consume all input"
      (String.length original)
      consumed1;
    Alcotest.(check bool)
      "should not be finished after flush"
      false
      is_finished1;

    (* Finish the stream *)
    let success2, _consumed2, compressed2, is_finished2 =
      Brotli.Encoder.compress_stream encoder "" Brotli.Finish
    in

    Alcotest.(check bool) "finish should succeed" true success2;
    Alcotest.(check bool) "should be finished" true is_finished2;

    let full_compressed = compressed1 ^ compressed2 in

    (* Decompress and verify *)
    Brotli.Decoder.with_decoder (fun decoder ->
      let rec decompress_all acc input =
        if String.length input = 0
        then acc
        else
          let status, consumed, output =
            Brotli.Decoder.decompress_stream decoder input
          in
          let remaining =
            String.sub input consumed (String.length input - consumed)
          in
          match status with
          | Brotli.Error -> Alcotest.fail "Decompression failed"
          | Brotli.Success -> acc ^ output
          | Brotli.Needs_more_input -> decompress_all (acc ^ output) remaining
          | Brotli.Needs_more_output -> decompress_all (acc ^ output) input
      in

      let decompressed = decompress_all "" full_compressed in
      Alcotest.(check string)
        "decompressed should match original"
        original
        decompressed))

let test_brotli_quality_levels () =
  let original = "Testing different quality levels with Brotli!" in

  (* Test with different quality levels (0-11 for Brotli) *)
  List.iter
    (fun quality ->
       Brotli.Encoder.with_encoder ~quality (fun encoder ->
         let success, consumed, compressed, is_finished =
           Brotli.Encoder.compress_stream encoder original Brotli.Finish
         in

         Alcotest.(check bool)
           (Printf.sprintf "compression quality %d should succeed" quality)
           true
           success;
         Alcotest.(check int)
           "should consume all input"
           (String.length original)
           consumed;
         Alcotest.(check bool) "should be finished" true is_finished;

         (* Verify decompression *)
         Brotli.Decoder.with_decoder (fun decoder ->
           let rec decompress_all acc input =
             if String.length input = 0
             then acc
             else
               let status, consumed, output =
                 Brotli.Decoder.decompress_stream decoder input
               in
               let remaining =
                 String.sub input consumed (String.length input - consumed)
               in
               match status with
               | Brotli.Error -> Alcotest.fail "Decompression failed"
               | Brotli.Success -> acc ^ output
               | Brotli.Needs_more_input ->
                 decompress_all (acc ^ output) remaining
               | Brotli.Needs_more_output -> decompress_all (acc ^ output) input
           in

           let decompressed = decompress_all "" compressed in
           Alcotest.(check string)
             (Printf.sprintf "decompressed at quality %d should match" quality)
             original
             decompressed)))
    [ 0; 4; 6; 9; 11 ]

let suite =
  [ "basic compression", `Quick, test_brotli_basic_compression
  ; "empty string", `Quick, test_brotli_empty_string
  ; "large data", `Quick, test_brotli_large_data
  ; "streaming", `Quick, test_brotli_streaming
  ; "flush operation", `Quick, test_brotli_flush_operation
  ; "quality levels", `Quick, test_brotli_quality_levels
  ]
