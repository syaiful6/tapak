let compress body =
  let encoder = Brotli.Encoder.create ~quality:4 () in
  let destroyed = ref false in
  let cleanup () =
    if not !destroyed
    then (
      destroyed := true;
      Brotli.Encoder.destroy encoder)
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
        (try
           let rec flush () =
             let success, _consumed, output, is_finished =
               Brotli.Encoder.compress_stream encoder "" Finish
             in
             if not success
             then (
               cleanup ();
               None)
             else if is_finished
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
           let success, _consumed, output, is_finished =
             Brotli.Encoder.compress_stream encoder chunk Process
           in
           if not success
           then (
             cleanup ();
             None)
           else if is_finished
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
