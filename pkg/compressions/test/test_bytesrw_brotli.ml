open Bytesrw
open Tapak_compressions

let test_decompress_reads_basic () =
  let original =
    "Hello, World! This is a test string for Brotli compression."
  in

  let compressed =
    Brotli.Encoder.with_encoder ~quality:6 (fun encoder ->
      let success, _consumed, output, is_finished =
        Brotli.Encoder.compress_stream encoder original Brotli.Finish
      in
      assert success;
      assert is_finished;
      output)
  in

  let reader = Bytes.Reader.of_string compressed in
  let decompressed_reader = Bytesrw_brotli.decompress_reads () reader in
  let decompressed = Bytes.Reader.to_string decompressed_reader in

  Alcotest.(check string)
    "decompressed should match original"
    original
    decompressed

let test_compress_reads_basic () =
  let original =
    "Hello, World! This is a test string for Brotli compression."
  in

  let reader = Bytes.Reader.of_string original in
  let compressed_reader = Bytesrw_brotli.compress_reads ~quality:6 () reader in
  let compressed = Bytes.Reader.to_string compressed_reader in

  let decompressed =
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
          | Brotli.Error -> failwith "Decompression failed"
          | Brotli.Success -> acc ^ output
          | Brotli.Needs_more_input -> decompress_all (acc ^ output) remaining
          | Brotli.Needs_more_output -> decompress_all (acc ^ output) input
      in
      decompress_all "" compressed)
  in

  Alcotest.(check string)
    "roundtrip should match original"
    original
    decompressed

let test_roundtrip_reads () =
  let original =
    "Testing roundtrip compression and decompression with bytesrw!"
  in

  let reader = Bytes.Reader.of_string original in
  let compressed_reader = Bytesrw_brotli.compress_reads ~quality:4 () reader in
  let decompressed_reader =
    Bytesrw_brotli.decompress_reads () compressed_reader
  in
  let result = Bytes.Reader.to_string decompressed_reader in

  Alcotest.(check string) "roundtrip should match original" original result

let test_compress_writes_basic () =
  let original =
    "Hello, World! This is a test string for Brotli compression."
  in

  let buf = Buffer.create 256 in
  let underlying_writer =
    Bytes.Writer.make (fun slice ->
      if not (Bytes.Slice.is_eod slice)
      then Buffer.add_string buf (Bytes.Slice.to_string slice))
  in
  let compress_writer =
    Bytesrw_brotli.compress_writes ~quality:6 () ~eod:true underlying_writer
  in

  Bytes.Writer.write_string compress_writer original;
  Bytes.Writer.write_eod compress_writer;

  let compressed = Buffer.contents buf in

  let decompressed =
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
          | Brotli.Error -> failwith "Decompression failed"
          | Brotli.Success -> acc ^ output
          | Brotli.Needs_more_input -> decompress_all (acc ^ output) remaining
          | Brotli.Needs_more_output -> decompress_all (acc ^ output) input
      in
      decompress_all "" compressed)
  in

  Alcotest.(check string)
    "roundtrip should match original"
    original
    decompressed

let test_large_data () =
  let chunk = "The quick brown fox jumps over the lazy dog. " in
  let original = String.concat "" (List.init 1000 (fun _ -> chunk)) in

  let reader = Bytes.Reader.of_string original in
  let compressed_reader = Bytesrw_brotli.compress_reads ~quality:4 () reader in
  let compressed = Bytes.Reader.to_string compressed_reader in

  let decompressed_lowlevel =
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
          | Brotli.Error -> failwith "Decompression failed"
          | Brotli.Success -> acc ^ output
          | Brotli.Needs_more_input -> decompress_all (acc ^ output) remaining
          | Brotli.Needs_more_output -> decompress_all (acc ^ output) input
      in
      decompress_all "" compressed)
  in

  Alcotest.(check int)
    "low-level decompression length"
    (String.length original)
    (String.length decompressed_lowlevel);

  Alcotest.(check string)
    "low-level decompression should match"
    original
    decompressed_lowlevel;

  let compressed_reader2 = Bytes.Reader.of_string compressed in
  let decompressed_reader =
    Bytesrw_brotli.decompress_reads () compressed_reader2
  in
  let result = Bytes.Reader.to_string decompressed_reader in

  Alcotest.(check int)
    "bytesrw decompression length"
    (String.length original)
    (String.length result);

  Alcotest.(check string) "large data roundtrip should match" original result

let test_empty_string () =
  let original = "" in

  let reader = Bytes.Reader.of_string original in
  let compressed_reader = Bytesrw_brotli.compress_reads ~quality:4 () reader in
  let decompressed_reader =
    Bytesrw_brotli.decompress_reads () compressed_reader
  in
  let result = Bytes.Reader.to_string decompressed_reader in

  Alcotest.(check string) "empty string roundtrip should match" original result

let suite =
  [ "decompress_reads basic", `Quick, test_decompress_reads_basic
  ; "compress_reads basic", `Quick, test_compress_reads_basic
  ; "roundtrip reads", `Quick, test_roundtrip_reads
  ; "compress_writes basic", `Quick, test_compress_writes_basic
  ; "large data", `Quick, test_large_data
  ; "empty string", `Quick, test_empty_string
  ]
