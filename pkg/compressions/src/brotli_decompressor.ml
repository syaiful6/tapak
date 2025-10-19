let decompress body =
  let decoder = Brotli.Decoder.create () in
  let input_stream = body in
  let rec next_decompressed () =
    match Piaf.Stream.take input_stream with
    | None ->
      Brotli.Decoder.destroy decoder;
      None
    | Some chunk ->
      let status, _consumed, output =
        Brotli.Decoder.decompress_stream decoder chunk
      in
      (match status with
      | Brotli.Error ->
        Brotli.Decoder.destroy decoder;
        None
      | Brotli.Success ->
        Brotli.Decoder.destroy decoder;
        if String.length output > 0 then Some output else None
      | Brotli.Needs_more_input | Brotli.Needs_more_output ->
        if String.length output > 0 then Some output else next_decompressed ())
  in
  let decompressed_stream = Piaf.Stream.from ~f:next_decompressed in
  Ok decompressed_stream
