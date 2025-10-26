include Tapak_kernel.Body

let limit ~max_bytes body =
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
    let bytes_read = ref 0L in
    let limited_stream, push = Piaf.Stream.create 128 in
    let rec copy_with_limit () =
      match Piaf.Stream.take stream with
      | None ->
        push None;
        Ok ()
      | Some iovec ->
        let { Piaf.IOVec.len; _ } = iovec in
        bytes_read := Int64.add !bytes_read (Int64.of_int len);
        if Int64.compare !bytes_read max_bytes > 0
        then (
          push None;
          Error
            (`Msg
                (Printf.sprintf
                   "Body exceeds limit of %Ld bytes (read: %Ld bytes)"
                   max_bytes
                   !bytes_read)))
        else (
          push (Some iovec);
          copy_with_limit ())
    in
    (match copy_with_limit () with
    | Ok () -> Ok (of_stream limited_stream)
    | Error _ as e -> e)
  | `Error _ as err ->
    Error
      (`Msg
          (Printf.sprintf
             "Body has error status: %s"
             (match err with
             | `Error `Bad_request -> "Bad request"
             | `Error `Bad_gateway -> "Bad gateway"
             | `Error `Internal_server_error -> "Internal server error")))
