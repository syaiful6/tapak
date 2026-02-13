open Bytesrw

type Bytes.Stream.error += Error of string

let format_error =
  Bytes.Stream.make_format_error
    ~format:"brotli"
    ~case:(fun e -> Error e)
    ~message:(function Error msg -> msg | _ -> "Unknown brotli error")

let err_unexp_eod = "Unexpected end of compressed data"

let decompress_reads () =
 fun ?pos ?(slice_length = 65536) r ->
  let decoder = Brotli.Decoder.create () in
  let state = ref `Await in
  let src = ref "" in
  let destroyed = ref false in
  let cleanup () =
    if not !destroyed
    then (
      destroyed := true;
      Brotli.Decoder.destroy decoder)
  in
  let error e =
    state := `Eod;
    cleanup ();
    Bytes.Reader.error format_error r e
  in
  let rec decompress () =
    let status, consumed, output =
      Brotli.Decoder.decompress_stream ~output_size:slice_length decoder !src
    in
    let src_rem = String.length !src - consumed in
    src := if src_rem = 0 then "" else String.sub !src consumed src_rem;
    match status with
    | Brotli.Error -> error "brotli decompression error"
    | Brotli.Success ->
      state := `Eod;
      cleanup ();
      if String.length output = 0
      then Bytes.Slice.eod
      else Bytes.Slice.of_string output
    | Brotli.Needs_more_input ->
      state := `Await;
      if String.length output = 0 then read () else Bytes.Slice.of_string output
    | Brotli.Needs_more_output ->
      (* Stay in Flush to continue draining decoder output *)
      if String.length output = 0
      then decompress ()
      else Bytes.Slice.of_string output
  and read () =
    match !state with
    | `Eod -> Bytes.Slice.eod
    | `Flush -> decompress ()
    | `Await ->
      let slice = Bytes.Reader.read r in
      if Bytes.Slice.is_eod slice
      then
        if String.length !src > 0
        then error err_unexp_eod
        else (
          state := `Eod;
          cleanup ();
          Bytes.Slice.eod)
      else
        let data = Bytes.Slice.to_string slice in
        src := if String.length !src = 0 then data else !src ^ data;
        state := `Flush;
        decompress ()
  in
  Bytes.Reader.make ?pos ~slice_length read

let decompress_writes () =
 fun ?pos ?(slice_length = 65536) ~eod:write_eod w ->
  let decoder = Brotli.Decoder.create () in
  let src = ref "" in
  let destroyed = ref false in
  let cleanup () =
    if not !destroyed
    then (
      destroyed := true;
      Brotli.Decoder.destroy decoder)
  in
  let error e =
    cleanup ();
    Bytes.Writer.error format_error w e
  in
  let rec decompress () =
    if String.length !src = 0
    then ()
    else
      let status, consumed, output =
        Brotli.Decoder.decompress_stream ~output_size:slice_length decoder !src
      in
      let src_rem = String.length !src - consumed in
      src := if src_rem = 0 then "" else String.sub !src consumed src_rem;
      if String.length output > 0
      then Bytes.Writer.write w (Bytes.Slice.of_string output);
      match status with
      | Brotli.Error -> error "brotli decompression error"
      | Brotli.Success -> ()
      | Brotli.Needs_more_input -> ()
      | Brotli.Needs_more_output -> decompress ()
  in
  let write slice =
    if Bytes.Slice.is_eod slice
    then (
      decompress ();
      cleanup ();
      if write_eod then Bytes.Writer.write_eod w)
    else
      let data = Bytes.Slice.to_string slice in
      src := if String.length !src = 0 then data else !src ^ data;
      decompress ()
  in
  Bytes.Writer.make ?pos ~slice_length write

let compress_reads ?(quality = 6) () =
 fun ?pos ?(slice_length = 65536) r ->
  let encoder = Brotli.Encoder.create ~quality () in
  let state = ref `Await in
  let src = ref "" in
  let destroyed = ref false in
  let cleanup () =
    if not !destroyed
    then (
      destroyed := true;
      Brotli.Encoder.destroy encoder)
  in
  let error e =
    state := `Eod;
    cleanup ();
    Bytes.Reader.error format_error r e
  in
  let rec compress_eod () =
    let success, _consumed, output, is_finished =
      Brotli.Encoder.compress_stream
        ~output_size:slice_length
        encoder
        ""
        Brotli.Finish
    in
    if not success then error "brotli compression error";
    if is_finished
    then (
      state := `Eod;
      cleanup ();
      if String.length output = 0
      then Bytes.Slice.eod
      else Bytes.Slice.of_string output)
    else if String.length output = 0
    then compress_eod ()
    else Bytes.Slice.of_string output
  and compress () =
    let success, consumed, output, _is_finished =
      Brotli.Encoder.compress_stream
        ~output_size:slice_length
        encoder
        !src
        Brotli.Process
    in
    if not success then error "brotli compression error";
    let src_rem = String.length !src - consumed in
    src := if src_rem = 0 then "" else String.sub !src consumed src_rem;
    state := if src_rem > 0 then `Flush else `Await;
    if String.length output = 0 then read () else Bytes.Slice.of_string output
  and read () =
    match !state with
    | `Eod -> Bytes.Slice.eod
    | `Flush -> compress ()
    | `Flush_eod -> compress_eod ()
    | `Await ->
      let slice = Bytes.Reader.read r in
      if Bytes.Slice.is_eod slice
      then (
        state := `Flush_eod;
        compress_eod ())
      else (
        src := Bytes.Slice.to_string slice;
        state := `Flush;
        compress ())
  in
  Bytes.Reader.make ?pos ~slice_length read

let compress_writes ?(quality = 6) () =
 fun ?pos ?(slice_length = 65536) ~eod:write_eod w ->
  let encoder = Brotli.Encoder.create ~quality () in
  let destroyed = ref false in
  let cleanup () =
    if not !destroyed
    then (
      destroyed := true;
      Brotli.Encoder.destroy encoder)
  in
  let error e =
    cleanup ();
    Bytes.Writer.error format_error w e
  in
  let rec compress_eod () =
    let success, _consumed, output, is_finished =
      Brotli.Encoder.compress_stream
        ~output_size:slice_length
        encoder
        ""
        Brotli.Finish
    in
    if not success then error "brotli compression error";
    if String.length output > 0
    then Bytes.Writer.write w (Bytes.Slice.of_string output);
    if not is_finished then compress_eod ()
  in
  let rec compress input =
    let success, consumed, output, _is_finished =
      Brotli.Encoder.compress_stream
        ~output_size:slice_length
        encoder
        input
        Brotli.Process
    in
    if not success then error "brotli compression error";
    if String.length output > 0
    then Bytes.Writer.write w (Bytes.Slice.of_string output);
    let src_rem = String.length input - consumed in
    if src_rem > 0 then compress (String.sub input consumed src_rem)
  in
  let write slice =
    if Bytes.Slice.is_eod slice
    then (
      compress_eod ();
      cleanup ();
      if write_eod then Bytes.Writer.write_eod w)
    else compress (Bytes.Slice.to_string slice)
  in
  Bytes.Writer.make ?pos ~slice_length write
