let decompress body =
  let dctx = Zstd.Decompress.create () in
  let destroyed = ref false in
  let cleanup () =
    if not !destroyed
    then (
      destroyed := true;
      Zstd.Decompress.destroy dctx)
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
           Zstd.Decompress.decompress_stream dctx chunk
         in
         match status with
         | Zstd.Error ->
           cleanup ();
           None
         | Zstd.Finished ->
           cleanup ();
           if String.length output > 0 then Some output else None
         | Zstd.Needs_more_input ->
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
