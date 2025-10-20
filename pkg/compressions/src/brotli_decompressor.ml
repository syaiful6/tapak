let decompress body =
  let decoder = Brotli.Decoder.create () in
  let destroyed = ref false in
  let cleanup () =
    if not !destroyed
    then (
      destroyed := true;
      Brotli.Decoder.destroy decoder)
  in
  let input_stream = body in
  let rec next_decompressed () =
    match Piaf.Stream.take input_stream with
    | None ->
      cleanup ();
      None
    | Some { Piaf.IOVec.buffer; off; len } ->
      let chunk = Bigstringaf.substring buffer ~off ~len in
      (try
         let status, _consumed, output =
           Brotli.Decoder.decompress_stream decoder chunk
         in
         match status with
         | Brotli.Error ->
           cleanup ();
           None
         | Brotli.Success ->
           cleanup ();
           if String.length output > 0 then Some output else None
         | Brotli.Needs_more_input | Brotli.Needs_more_output ->
           if String.length output > 0
           then Some output
           else next_decompressed ()
       with
      | e ->
        cleanup ();
        raise e)
  in
  let decompressed_stream = Piaf.Stream.from ~f:next_decompressed in
  Ok decompressed_stream
