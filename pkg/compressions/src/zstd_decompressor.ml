let decompress body =
  let dctx = Zstd.Decompress.create () in
  let input_stream = body in
  let rec next_decompressed () =
    match Piaf.Stream.take input_stream with
    | None ->
      Zstd.Decompress.destroy dctx;
      None
    | Some chunk ->
      let status, _consumed, output =
        Zstd.Decompress.decompress_stream dctx chunk
      in
      (match status with
      | Zstd.Error ->
        Zstd.Decompress.destroy dctx;
        None
      | Zstd.Finished ->
        Zstd.Decompress.destroy dctx;
        if String.length output > 0 then Some output else None
      | Zstd.Needs_more_input ->
        if String.length output > 0 then Some output else next_decompressed ())
  in
  let decompressed_stream = Piaf.Stream.from ~f:next_decompressed in
  Ok decompressed_stream
