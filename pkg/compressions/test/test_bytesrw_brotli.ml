open Bytesrw
open Tapak_compressions

let test_decompress_reads_basic () =
  let original =
    "Hello, World! This is a test string for Brotli compression."
  in
  let src = Brotli.Bbuf.make_empty () in
  let dst = Brotli.Bbuf.make 63553 in
  Brotli.Bbuf.src_set_slice src (Bytes.Slice.of_string original);

  Brotli.Encoder.with_encoder ~quality:6 (fun encoder ->
    let success = Brotli.Encoder.compress_stream encoder Finish ~src ~dst in
    assert (success = Brotli.Encoder.Finished));
  let compress = Brotli.Bbuf.dst_to_slice dst in
  Brotli.Bbuf.dst_clear dst;
  let reader = Bytes.Reader.of_slice compress in
  let decompressed_reader = Bytesrw_brotli.decompress_reads () reader in
  let decompressed = Bytes.Reader.to_string decompressed_reader in

  Alcotest.(check string)
    "decompressed should match original"
    original
    decompressed

let test_roundtrip_writer_reader () =
  let input = "Hello, World! This is a test string for Brotli compression." in
  let buffer = Buffer.create 256 in
  let writer =
    Bytesrw_brotli.compress_writes () ~eod:true (Bytes.Writer.of_buffer buffer)
  in
  Bytes.Writer.write_string writer input;
  Bytes.Writer.write_eod writer;
  let reader =
    Bytesrw_brotli.decompress_reads
      ()
      (Bytes.Reader.of_string (Buffer.contents buffer))
  in
  Alcotest.(check string)
    "roundtrip should match original"
    input
    (Bytes.Reader.to_string reader)

let test_quality_roundtrip buffer quality =
  let inputs =
    [ "a"
    ; "hello world"
    ; String.make 100 'x'
    ; String.make 1000 'y'
    ; String.init 256 Char.chr
    ; "The quick brown fox jumps over the lazy dog"
    ; "This is a test of Brotli compression with quality "
      ^ string_of_int quality
    ]
  in
  let writer =
    Bytesrw_brotli.compress_writes
      ~quality
      ()
      ~eod:true
      (Bytes.Writer.of_buffer buffer)
  in
  List.iter (Bytes.Writer.write_string writer) inputs;
  Bytes.Writer.write_eod writer;
  let reader =
    Bytesrw_brotli.decompress_reads
      ()
      (Bytes.Reader.of_string (Buffer.contents buffer))
  in
  Alcotest.(check string)
    ("roundtrip quality " ^ string_of_int quality)
    (String.concat "" inputs)
    (Bytes.Reader.to_string reader)

let test_quality_roundtrips () =
  let buffer = Buffer.create 65536 in
  List.iter
    (fun q ->
       test_quality_roundtrip buffer q;
       Buffer.reset buffer)
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 ]

let test_roundtrip_reader_reader () =
  let input = "Hello, World! This is a test string for Brotli compression." in
  let reader =
    Bytesrw_brotli.decompress_reads
      ()
      (Bytesrw_brotli.compress_reads () (Bytes.Reader.of_string input))
  in
  Alcotest.(check string)
    "reader+reader roundtrip should match original"
    input
    (Bytes.Reader.to_string reader)

let test_quality_roundtrip_readers quality =
  let inputs =
    [ "a"
    ; "hello world"
    ; String.make 100 'x'
    ; String.make 1000 'y'
    ; String.init 256 Char.chr
    ; "The quick brown fox jumps over the lazy dog"
    ; "This is a test of Brotli compression with quality "
      ^ string_of_int quality
    ]
  in
  let reader =
    Bytesrw_brotli.decompress_reads
      ()
      (Bytesrw_brotli.compress_reads
         ~quality
         ()
         (Bytes.Reader.of_string (String.concat "" inputs)))
  in
  Alcotest.(check string)
    ("reader+reader roundtrip quality " ^ string_of_int quality)
    (String.concat "" inputs)
    (Bytes.Reader.to_string reader)

let test_quality_roundtrips_reader () =
  List.iter test_quality_roundtrip_readers [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 ]

let test_quality_roundtrips_writer buffer quality =
  let inputs =
    [ "a"
    ; "hello world"
    ; String.make 100 'x'
    ; String.make 1000 'y'
    ; String.init 256 Char.chr
    ; "The quick brown fox jumps over the lazy dog"
    ; "This is a test of Brotli compression with quality "
      ^ string_of_int quality
    ]
  in
  let writer =
    Bytesrw_brotli.compress_writes
      ~quality
      ()
      ~eod:true
      (Bytes.Writer.of_buffer buffer)
  in
  List.iter (Bytes.Writer.write_string writer) inputs;
  Bytes.Writer.write_eod writer;
  let compressed = Buffer.contents buffer in
  (* reset buffer for decompressed writer *)
  Buffer.reset buffer;
  let w =
    Bytesrw_brotli.decompress_writes
      ()
      ~eod:true
      (Bytes.Writer.of_buffer buffer)
  in
  Bytes.Writer.write_string w compressed;
  Bytes.Writer.write_eod w;
  Alcotest.(check string)
    ("writer+writer roundtrip quality " ^ string_of_int quality)
    (String.concat "" inputs)
    (Buffer.contents buffer)

let test_quality_roundtrips_writer () =
  let buffer = Buffer.create 65536 in
  List.iter
    (fun quality ->
       test_quality_roundtrips_writer buffer quality;
       Buffer.reset buffer)
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 ]

let suite =
  [ "decompress_reads basic", `Quick, test_decompress_reads_basic
  ; "roundtrip writer+reader", `Quick, test_roundtrip_writer_reader
  ; "roundtrip reader+reader", `Quick, test_roundtrip_reader_reader
  ; "quality roundtrips writer+reader", `Quick, test_quality_roundtrips
  ; "quality roundtrips writer+writer", `Quick, test_quality_roundtrips_writer
  ; "quality roundtrips reader+reader", `Quick, test_quality_roundtrips_reader
  ]
