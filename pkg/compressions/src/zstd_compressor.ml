let compress body =
  let cctx = Zstd.Compress.create ~level:3 () in
  let destroyed = ref false in
  let cleanup () =
    if not !destroyed
    then (
      destroyed := true;
      Zstd.Compress.destroy cctx)
  in
  let input_stream = body in
  let finished = ref false in
  let rec next_compressed () =
    if !finished
    then (
      cleanup ();
      None)
    else
      match Piaf.Stream.take input_stream with
      | None ->
        (* Flush and finish *)
        (try
           let rec flush () =
             let status, _consumed, output, is_finished =
               Zstd.Compress.compress_stream ~finish:true cctx ""
             in
             match status with
             | Zstd.Error ->
               cleanup ();
               None
             | Zstd.Success ->
               if is_finished
               then (
                 finished := true;
                 cleanup ();
                 if String.length output > 0 then Some output else None)
               else if String.length output > 0
               then Some output
               else flush ()
           in
           flush ()
         with
        | e ->
          cleanup ();
          raise e)
      | Some chunk ->
        (try
           let status, _consumed, output, is_finished =
             Zstd.Compress.compress_stream cctx chunk
           in
           match status with
           | Zstd.Error ->
             cleanup ();
             None
           | Zstd.Success ->
             if is_finished
             then (
               finished := true;
               cleanup ();
               if String.length output > 0 then Some output else None)
             else if String.length output > 0
             then Some output
             else next_compressed ()
         with
        | e ->
          cleanup ();
          raise e)
  in
  let compressed_stream = Piaf.Stream.from ~f:next_compressed in
  Ok compressed_stream
