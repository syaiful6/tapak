open Bytesrw
open Brotli

let err_unexp_eod = "Unexpected end of compressed data"

let decompress_reads () =
 fun ?pos ?(slice_length = 65536) r ->
  let decoder = Decoder.create () in
  let state = ref `Await in
  let src = Bbuf.make_empty ()
  and dst = Bbuf.make slice_length in
  let error = reader_error r in
  let destroyed = ref false in
  let cleanup () =
    if not !destroyed
    then (
      destroyed := true;
      Decoder.destroy decoder)
  in
  let rec decompress ~error ctx ~src ~dst =
    match Decoder.decompress_stream ctx ~src ~dst with
    | Decoder.Error ->
      state := `Eod;
      cleanup ();
      error "brotli decompression error"
    | Success ->
      state := `Leftover (Bbuf.src_to_slice_or_eod src);
      if Bbuf.dst_is_empty dst
      then read ()
      else
        let slice = Bbuf.dst_to_slice dst in
        Bbuf.dst_clear dst;
        slice
    | Needs_more_input ->
      state := `Await;
      if Bbuf.dst_is_empty dst
      then read ()
      else
        let slice = Bbuf.dst_to_slice dst in
        Bbuf.dst_clear dst;
        slice
    | Needs_more_output ->
      (* dst is full but brotli has more output to produce from the current src.
         Flush dst and come back to resume decompression with the same src. *)
      state := `Flush;
      let slice = Bbuf.dst_to_slice dst in
      Bbuf.dst_clear dst;
      slice
  and read () =
    match !state with
    | `Flush -> decompress ~error decoder ~src ~dst
    | `Await ->
      let slice = Bytes.Reader.read r in
      if Bytes.Slice.is_eod slice
      then (
        state := `Eod;
        cleanup ();
        error err_unexp_eod)
      else (
        Bbuf.src_set_slice src slice;
        decompress ~error decoder ~src ~dst)
    | `Eod -> Bytes.Slice.eod
    | `Leftover leftover ->
      let slice =
        if Bytes.Slice.is_eod leftover then Bytes.Reader.read r else leftover
      in
      if Bytes.Slice.is_eod slice
      then (
        state := `Eod;
        cleanup ();
        slice)
      else (
        Bbuf.src_set_slice src slice;
        decompress ~error decoder ~src ~dst)
  in
  Bytes.Reader.make ?pos ~slice_length read

let decompress_writes () =
 fun ?pos ?(slice_length = 65536) ~eod w ->
  let decoder = Decoder.create () in
  let src = Bbuf.make_empty () in
  let dst = Bbuf.make (Bytes.Writer.slice_length w) in
  let error = writer_error w in
  let destroyed = ref false in
  let cleanup () =
    if not !destroyed
    then (
      destroyed := true;
      Decoder.destroy decoder)
  in
  let rec decompress ?(final = false) ~error ctx ~src ~dst =
    match Decoder.decompress_stream ctx ~src ~dst with
    | Decoder.Error ->
      cleanup ();
      error "brotli decompression error"
    | Success ->
      if not (Bbuf.dst_is_empty dst)
      then begin
        let slice = Bbuf.dst_to_slice dst in
        Bbuf.dst_clear dst;
        Bytes.Writer.write w slice
      end;
      if not (Bbuf.src_is_consumed src)
      then decompress ~final ~error ctx ~src ~dst
    | Needs_more_input ->
      if final
      then (
        cleanup ();
        error err_unexp_eod)
      else () (* wait more input to be written before resuming decompression *)
    | Needs_more_output ->
      (* dst is always full here by definition; flush and keep going *)
      let slice = Bbuf.dst_to_slice dst in
      Bbuf.dst_clear dst;
      Bytes.Writer.write w slice;
      decompress ~final ~error ctx ~src ~dst
  in
  let write = function
    | slice when Bytes.Slice.is_eod slice ->
      decompress ~final:true ~error decoder ~src ~dst;
      cleanup ();
      if eod then Bytes.Writer.write_eod w
    | slice ->
      Bbuf.src_set_slice src slice;
      decompress ~error decoder ~src ~dst
  in
  Bytes.Writer.make ?pos ~slice_length write

let compress_reads ?(quality = 6) () =
 fun ?pos ?(slice_length = 65536) r ->
  let encoder = Encoder.create ~quality () in
  let src = Bbuf.make_empty ()
  and dst = Bbuf.make slice_length in
  let state = ref `Await in
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
    reader_error r e
  in
  let rec compress_eod ~error ctx ~src ~dst =
    match Encoder.compress_stream ctx Finish ~src ~dst with
    | Encoder.Error -> error "brotli compression error"
    | Finished ->
      state := `Eod;
      cleanup ();
      let slice =
        if Bbuf.dst_is_empty dst then Bytes.Slice.eod else Bbuf.dst_to_slice dst
      in
      Bbuf.dst_clear dst;
      slice
    | Ok ->
      if Bbuf.dst_is_empty dst
      then compress_eod ~error ctx ~src ~dst
      else
        let slice = Bbuf.dst_to_slice dst in
        Bbuf.dst_clear dst;
        slice
  and compress ~error ctx ~src ~dst =
    match Encoder.compress_stream ctx Process ~src ~dst with
    | Encoder.Error -> error "brotli compression error"
    | _ ->
      if Bbuf.dst_is_empty dst
      then (
        state := `Await;
        read ())
      else
        let slice = Bbuf.dst_to_slice dst in
        let flush = Bbuf.dst_is_full dst || not (Bbuf.src_is_consumed src) in
        state := if flush then `Flush else `Await;
        Bbuf.dst_clear dst;
        slice
  and read () =
    match !state with
    | `Flush -> compress ~error encoder ~src ~dst
    | `Await ->
      let slice = Bytes.Reader.read r in
      if Bytes.Slice.is_eod slice
      then (
        state := `Flush_eod;
        compress_eod ~error encoder ~src ~dst)
      else (
        Bbuf.src_set_slice src slice;
        compress ~error encoder ~src ~dst)
    | `Flush_eod -> compress_eod ~error encoder ~src ~dst
    | `Eod -> Bytes.Slice.eod
  in
  Bytes.Reader.make ?pos ~slice_length read

let compress_writes ?(quality = 6) () =
 fun ?pos ?(slice_length = 65536) ~eod:write_eod w ->
  let encoder = Encoder.create ~quality () in
  let src = Bbuf.make_empty () in
  let dst = Bbuf.make (Bytes.Writer.slice_length w) in
  let destroyed = ref false in
  let cleanup () =
    if not !destroyed
    then (
      destroyed := true;
      Brotli.Encoder.destroy encoder)
  in
  let error e =
    cleanup ();
    writer_error w e
  in
  let write_dst w dst =
    if Bbuf.dst_is_empty dst
    then ()
    else (
      Bytes.Writer.write w (Bbuf.dst_to_slice dst);
      Bbuf.dst_clear dst)
  in
  let rec compress_eod ~error w ctx ~src ~dst =
    match Encoder.compress_stream ctx Finish ~src ~dst with
    | Encoder.Error -> error "brotli compression error"
    | Finished ->
      write_dst w dst;
      cleanup ();
      if write_eod then Bytes.Writer.write_eod w
    | Ok ->
      write_dst w dst;
      compress_eod ~error w ctx ~src ~dst
  in
  let rec compress ~error w ctx ~src ~dst =
    match Encoder.compress_stream ctx Process ~src ~dst with
    | Encoder.Error -> error "brotli compression error"
    | _ ->
      let flush = Bbuf.dst_is_full dst || not (Bbuf.src_is_consumed src) in
      write_dst w dst;
      if flush then compress ~error w ctx ~src ~dst
  in
  let write = function
    | slice when Bytes.Slice.is_eod slice ->
      compress_eod ~error w encoder ~src ~dst
    | slice ->
      Bbuf.src_set_slice src slice;
      compress ~error w encoder ~src ~dst
  in
  Bytes.Writer.make ?pos ~slice_length write
