type stream_state =
  { zstream : Zlib.stream
  ; mutable finished : bool
  ; buf_size : int
  }

let create_stream_state ~gzip ?(buf_size = 4096) () =
  { zstream = Zlib.inflate_init gzip; finished = false; buf_size }

let decompress_chunk state chunk =
  if state.finished
  then None
  else
    let buf = Bytes.create state.buf_size in
    let result_buf = Buffer.create state.buf_size in
    let rec inner ~off ~len =
      if len = 0
      then ()
      else
        let is_end, used_in, used_out =
          Zlib.inflate_string
            state.zstream
            chunk
            off
            len
            buf
            0
            state.buf_size
            Zlib.Z_SYNC_FLUSH
        in
        if used_out > 0 then Buffer.add_subbytes result_buf buf 0 used_out;
        if is_end
        then (
          state.finished <- true;
          Zlib.inflate_end state.zstream)
        else if used_in < len
        then inner ~off:(off + used_in) ~len:(len - used_in)
    in
    inner ~off:0 ~len:(String.length chunk);
    let result = Buffer.contents result_buf in
    if String.length result > 0 then Some result else None

let decompress_stream ~gzip body =
  let state = create_stream_state ~gzip () in
  let input_stream = body in
  let rec next_decompressed () =
    match Piaf.Stream.take input_stream with
    | None ->
      if not state.finished then Zlib.inflate_end state.zstream;
      None
    | Some chunk ->
      (match decompress_chunk state chunk with
      | Some decompressed -> Some decompressed
      | None -> next_decompressed ())
  in
  Piaf.Stream.from ~f:next_decompressed

module Deflate = struct
  let decompress body = Ok (decompress_stream ~gzip:false body)
end

module Gzip = struct
  let decompress body = Ok (decompress_stream ~gzip:true body)
end
