open Tapak_compressions
module Gzip = Decompressors.Gzip
module Deflate = Decompressors.Deflate

let stream_of_string s =
  let delivered = ref false in
  Piaf.Stream.from ~f:(fun () ->
    if !delivered
    then None
    else (
      delivered := true;
      let len = String.length s in
      let buf = Bigstringaf.of_string ~off:0 ~len s in
      Some { Piaf.IOVec.buffer = buf; off = 0; len }))

let stream_of_chunks chunks =
  let remaining = ref chunks in
  Piaf.Stream.from ~f:(fun () ->
    match !remaining with
    | [] -> None
    | chunk :: rest ->
      remaining := rest;
      let len = String.length chunk in
      let buf = Bigstringaf.of_string ~off:0 ~len chunk in
      Some { Piaf.IOVec.buffer = buf; off = 0; len })

let string_of_stream stream =
  let rec collect acc =
    match Piaf.Stream.take stream with
    | None -> String.concat "" (List.rev acc)
    | Some chunk -> collect (chunk :: acc)
  in
  collect []

let gzip_compress data =
  let temp_file = Filename.temp_file "test_gzip" ".txt" in
  let temp_gz = temp_file ^ ".gz" in

  try
    let oc = open_out_bin temp_file in
    output_string oc data;
    close_out oc;

    let cmd = Printf.sprintf "gzip -cn < %s > %s" temp_file temp_gz in
    let exit_code = Sys.command cmd in

    if exit_code <> 0
    then
      failwith
        (Printf.sprintf "gzip command failed with exit code %d" exit_code);

    let ic = open_in_bin temp_gz in
    let compressed_size = in_channel_length ic in
    let compressed = really_input_string ic compressed_size in
    close_in ic;

    Sys.remove temp_file;
    Sys.remove temp_gz;

    compressed
  with
  | e ->
    (try Sys.remove temp_file with _ -> ());
    (try Sys.remove temp_gz with _ -> ());
    raise e

let deflate_compress data =
  let zstream = Zlib.deflate_init 6 true in
  let buf = Bytes.create 4096 in
  let result_buf = Buffer.create (String.length data) in

  let rec compress_all ~off ~len =
    if len = 0
    then ()
    else
      let is_end, used_in, used_out =
        Zlib.deflate_string
          zstream
          data
          off
          len
          buf
          0
          (Bytes.length buf)
          Zlib.Z_NO_FLUSH
      in
      if used_out > 0 then Buffer.add_subbytes result_buf buf 0 used_out;
      if is_end
      then ()
      else if used_in < len
      then compress_all ~off:(off + used_in) ~len:(len - used_in)
  in

  compress_all ~off:0 ~len:(String.length data);

  let rec finish () =
    let is_end, _used_in, used_out =
      Zlib.deflate_string zstream "" 0 0 buf 0 (Bytes.length buf) Zlib.Z_FINISH
    in
    if used_out > 0 then Buffer.add_subbytes result_buf buf 0 used_out;
    if is_end then Zlib.deflate_end zstream else finish ()
  in
  finish ();
  Buffer.contents result_buf

let test_gzip_basic_decompression () =
  let original = "Hello, World! This is a test string for Gzip compression." in
  let compressed = gzip_compress original in

  let stream = stream_of_string compressed in

  match Gzip.decompress stream with
  | Error err ->
    Alcotest.failf "Decompression failed: %s" (Piaf.Error.to_string err)
  | Ok decompressed_stream ->
    let decompressed = string_of_stream decompressed_stream in
    Alcotest.(check string)
      "decompressed should match original"
      original
      decompressed

let test_deflate_basic_decompression () =
  let original =
    "Hello, World! This is a test string for Deflate compression."
  in
  let compressed = deflate_compress original in

  let stream = stream_of_string compressed in

  match Deflate.decompress stream with
  | Error err ->
    Alcotest.failf "Decompression failed: %s" (Piaf.Error.to_string err)
  | Ok decompressed_stream ->
    let decompressed = string_of_stream decompressed_stream in
    Alcotest.(check string)
      "decompressed should match original"
      original
      decompressed

let test_gzip_empty_string () =
  let original = "" in
  let compressed = gzip_compress original in

  let stream = stream_of_string compressed in

  match Gzip.decompress stream with
  | Error err ->
    Alcotest.failf "Decompression failed: %s" (Piaf.Error.to_string err)
  | Ok decompressed_stream ->
    let decompressed = string_of_stream decompressed_stream in
    Alcotest.(check string) "decompressed should be empty" original decompressed

let test_deflate_empty_string () =
  let original = "" in
  let compressed = deflate_compress original in

  let stream = stream_of_string compressed in

  match Deflate.decompress stream with
  | Error err ->
    Alcotest.failf "Decompression failed: %s" (Piaf.Error.to_string err)
  | Ok decompressed_stream ->
    let decompressed = string_of_stream decompressed_stream in
    Alcotest.(check string) "decompressed should be empty" original decompressed

let test_gzip_large_data () =
  let chunk = "The quick brown fox jumps over the lazy dog. " in
  let original = String.concat "" (List.init 1000 (fun _ -> chunk)) in

  let compressed = gzip_compress original in

  Alcotest.(check bool)
    "compressed should be smaller"
    true
    (String.length compressed < String.length original);

  let stream = stream_of_string compressed in

  match Gzip.decompress stream with
  | Error err ->
    Alcotest.failf "Decompression failed: %s" (Piaf.Error.to_string err)
  | Ok decompressed_stream ->
    let decompressed = string_of_stream decompressed_stream in
    Alcotest.(check string)
      "decompressed should match original"
      original
      decompressed

let test_deflate_large_data () =
  let chunk = "The quick brown fox jumps over the lazy dog. " in
  let original = String.concat "" (List.init 1000 (fun _ -> chunk)) in

  let compressed = deflate_compress original in

  Alcotest.(check bool)
    "compressed should be smaller"
    true
    (String.length compressed < String.length original);

  let stream = stream_of_string compressed in

  match Deflate.decompress stream with
  | Error err ->
    Alcotest.failf "Decompression failed: %s" (Piaf.Error.to_string err)
  | Ok decompressed_stream ->
    let decompressed = string_of_stream decompressed_stream in
    Alcotest.(check string)
      "decompressed should match original"
      original
      decompressed

let test_gzip_chunked_stream () =
  let original =
    "This is a test of chunked decompression. "
    ^ "We will compress the data as one block, "
    ^ "but decompress it in multiple chunks."
  in
  let compressed = gzip_compress original in

  let chunk_size = 100 in
  let rec split_into_chunks data acc =
    if String.length data = 0
    then List.rev acc
    else if String.length data <= chunk_size
    then List.rev (data :: acc)
    else
      let chunk = String.sub data 0 chunk_size in
      let rest = String.sub data chunk_size (String.length data - chunk_size) in
      split_into_chunks rest (chunk :: acc)
  in

  let chunks = split_into_chunks compressed [] in
  let stream = stream_of_chunks chunks in

  match Gzip.decompress stream with
  | Error err ->
    Alcotest.failf "Decompression failed: %s" (Piaf.Error.to_string err)
  | Ok decompressed_stream ->
    let decompressed = string_of_stream decompressed_stream in
    Alcotest.(check string)
      "decompressed should match original"
      original
      decompressed

let test_deflate_chunked_stream () =
  let original =
    "This is a test of chunked decompression. "
    ^ "We will compress the data as one block, "
    ^ "but decompress it in multiple chunks."
  in
  let compressed = deflate_compress original in

  let chunk_size = 100 in
  let rec split_into_chunks data acc =
    if String.length data = 0
    then List.rev acc
    else if String.length data <= chunk_size
    then List.rev (data :: acc)
    else
      let chunk = String.sub data 0 chunk_size in
      let rest = String.sub data chunk_size (String.length data - chunk_size) in
      split_into_chunks rest (chunk :: acc)
  in

  let chunks = split_into_chunks compressed [] in
  let stream = stream_of_chunks chunks in

  match Deflate.decompress stream with
  | Error err ->
    Alcotest.failf "Decompression failed: %s" (Piaf.Error.to_string err)
  | Ok decompressed_stream ->
    let decompressed = string_of_stream decompressed_stream in
    Alcotest.(check string)
      "decompressed should match original"
      original
      decompressed

let test_gzip_json_data () =
  let original =
    {|{
  "users": [
    {"id": 1, "name": "Alice", "email": "alice@example.com"},
    {"id": 2, "name": "Bob", "email": "bob@example.com"},
    {"id": 3, "name": "Charlie", "email": "charlie@example.com"}
  ],
  "total": 3,
  "page": 1
}|}
  in

  let compressed = gzip_compress original in
  let stream = stream_of_string compressed in

  match Gzip.decompress stream with
  | Error err ->
    Alcotest.failf "Decompression failed: %s" (Piaf.Error.to_string err)
  | Ok decompressed_stream ->
    let decompressed = string_of_stream decompressed_stream in
    Alcotest.(check string)
      "decompressed JSON should match original"
      original
      decompressed

let test_deflate_json_data () =
  let original =
    {|{
  "users": [
    {"id": 1, "name": "Alice", "email": "alice@example.com"},
    {"id": 2, "name": "Bob", "email": "bob@example.com"},
    {"id": 3, "name": "Charlie", "email": "charlie@example.com"}
  ],
  "total": 3,
  "page": 1
}|}
  in

  let compressed = deflate_compress original in
  let stream = stream_of_string compressed in

  match Deflate.decompress stream with
  | Error err ->
    Alcotest.failf "Decompression failed: %s" (Piaf.Error.to_string err)
  | Ok decompressed_stream ->
    let decompressed = string_of_stream decompressed_stream in
    Alcotest.(check string)
      "decompressed JSON should match original"
      original
      decompressed

let test_gzip_from_command () =
  let test_json =
    {|{"message": "Hello from gzip!", "items": [1, 2, 3, 4, 5]}|}
  in
  let temp_file = Filename.temp_file "test_gzip" ".json" in
  let temp_gz = temp_file ^ ".gz" in

  try
    let oc = open_out temp_file in
    output_string oc test_json;
    close_out oc;

    let cmd = Printf.sprintf "gzip -c %s > %s" temp_file temp_gz in
    let exit_code = Sys.command cmd in

    if exit_code <> 0
    then Alcotest.failf "gzip command failed with exit code %d" exit_code;

    let ic = open_in_bin temp_gz in
    let compressed_size = in_channel_length ic in
    let compressed = really_input_string ic compressed_size in
    close_in ic;

    Sys.remove temp_file;
    Sys.remove temp_gz;

    let stream = stream_of_string compressed in

    match Gzip.decompress stream with
    | Error err ->
      Alcotest.failf
        "Decompression of gzip command output failed: %s"
        (Piaf.Error.to_string err)
    | Ok decompressed_stream ->
      let decompressed = string_of_stream decompressed_stream in
      Alcotest.(check string)
        "decompressed should match original JSON"
        test_json
        decompressed
  with
  | e ->
    (try Sys.remove temp_file with _ -> ());
    (try Sys.remove temp_gz with _ -> ());
    raise e

let test_gzip_header_validation () =
  let original = "Test data for header validation" in
  let compressed = gzip_compress original in

  if String.length compressed < 2 then Alcotest.fail "Compressed data too short";

  let byte1 = Char.code compressed.[0] in
  let byte2 = Char.code compressed.[1] in

  Alcotest.(check int) "First byte should be 0x1f" 0x1f byte1;
  Alcotest.(check int) "Second byte should be 0x8b" 0x8b byte2;

  if String.length compressed < 3
  then Alcotest.fail "Compressed data missing method byte";

  let method_byte = Char.code compressed.[2] in
  Alcotest.(check int) "Method byte should be 8 (deflate)" 8 method_byte;

  let stream = stream_of_string compressed in

  match Gzip.decompress stream with
  | Error err ->
    Alcotest.failf "Decompression failed: %s" (Piaf.Error.to_string err)
  | Ok decompressed_stream ->
    let decompressed = string_of_stream decompressed_stream in
    Alcotest.(check string)
      "decompressed should match original"
      original
      decompressed

let suite =
  [ "gzip basic decompression", `Quick, test_gzip_basic_decompression
  ; "deflate basic decompression", `Quick, test_deflate_basic_decompression
  ; "gzip empty string", `Quick, test_gzip_empty_string
  ; "deflate empty string", `Quick, test_deflate_empty_string
  ; "gzip large data", `Quick, test_gzip_large_data
  ; "deflate large data", `Quick, test_deflate_large_data
  ; "gzip chunked stream", `Quick, test_gzip_chunked_stream
  ; "deflate chunked stream", `Quick, test_deflate_chunked_stream
  ; "gzip JSON data", `Quick, test_gzip_json_data
  ; "deflate JSON data", `Quick, test_deflate_json_data
  ; "gzip from command", `Quick, test_gzip_from_command
  ; "gzip header validation", `Quick, test_gzip_header_validation
  ]
