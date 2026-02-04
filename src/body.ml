include Piaf.Body

let limit ~sw ~max_bytes body =
  match length body with
  | `Fixed len when Int64.compare len max_bytes > 0 ->
    Error
      (`Msg
          (Printf.sprintf
             "Body size (%Ld bytes) exceeds limit of %Ld bytes"
             len
             max_bytes))
  | `Fixed _ | `Chunked | `Unknown | `Close_delimited ->
    let stream = to_stream body in
    let limited_stream, stream_push = Piaf.Stream.create 4 in
    let _limiter_fiber =
      Eio.Fiber.fork_promise ~sw (fun () ->
        let bytes_read = ref 0L in
        let rec copy_with_limit () =
          match Piaf.Stream.take stream with
          | None ->
            stream_push None;
            Ok ()
          | Some iovec ->
            let { Piaf.IOVec.len; _ } = iovec in
            bytes_read := Int64.add !bytes_read (Int64.of_int len);
            if Int64.compare !bytes_read max_bytes > 0
            then (
              stream_push None;
              Error
                (`Msg
                    (Printf.sprintf
                       "Body exceeds limit of %Ld bytes (read: %Ld bytes)"
                       max_bytes
                       !bytes_read)))
            else (
              stream_push (Some iovec);
              copy_with_limit ())
        in
        ignore (copy_with_limit ()))
    in
    Ok (of_stream limited_stream)
  | `Error _ as err ->
    Error
      (`Msg
          (Printf.sprintf
             "Body has error status: %s"
             (match err with
             | `Error `Bad_request -> "Bad request"
             | `Error `Bad_gateway -> "Bad gateway"
             | `Error `Internal_server_error -> "Internal server error")))
