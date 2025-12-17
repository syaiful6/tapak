type stream_state =
  { zstream : Zlib.stream
  ; mutable finished : bool
  ; mutable cleaned_up : bool
  ; buf_size : int
  }

let create_stream_state ?(buf_size = 4096) ~use_zlib_wrapper () =
  { zstream = Zlib.inflate_init use_zlib_wrapper
  ; finished = false
  ; cleaned_up = false
  ; buf_size
  }

let cleanup_stream state =
  if not state.cleaned_up
  then (
    state.cleaned_up <- true;
    Zlib.inflate_end state.zstream)

let decompress_chunk state chunk ~off ~len =
  if state.finished
  then None, 0
  else
    try
      let buf = Bytes.create state.buf_size in
      let result_buf = Buffer.create state.buf_size in
      let rec inner ~off ~len =
        if len = 0
        then off
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
            cleanup_stream state;
            off + used_in)
          else if used_in < len
          then inner ~off:(off + used_in) ~len:(len - used_in)
          else off + used_in
      in
      let new_off = inner ~off ~len in
      let result = Buffer.contents result_buf in
      let consumed = new_off - off in
      if String.length result > 0 then Some result, consumed else None, consumed
    with
    | Zlib.Error (func, msg) ->
      (* Clean up on error *)
      cleanup_stream state;
      state.finished <- true;
      raise (Zlib.Error (func, msg))

let parse_gzip_header chunk =
  let len = String.length chunk in
  if len < 10
  then Error `Need_more_data
  else
    let get_byte i = Char.code chunk.[i] in
    let get_u16_le i = get_byte i + (get_byte (i + 1) lsl 8) in

    if get_byte 0 <> 0x1f || get_byte 1 <> 0x8b
    then Error `Invalid_gzip_header
    else if get_byte 2 <> 8
    then (* Compression method must be deflate *)
      Error `Unsupported_compression_method
    else
      let flags = get_byte 3 in
      let has_extra = flags land 0x04 <> 0 in
      let has_name = flags land 0x08 <> 0 in
      let has_comment = flags land 0x10 <> 0 in
      let has_crc = flags land 0x02 <> 0 in

      let pos = ref 10 in

      let result =
        if has_extra
        then (
          if !pos + 2 > len
          then Error `Need_more_data
          else
            let xlen = get_u16_le !pos in
            pos := !pos + 2 + xlen;
            if !pos > len then Error `Need_more_data else Ok ())
        else Ok ()
      in

      match result with
      | Error e -> Error e
      | Ok () ->
        let result =
          if has_name
          then
            let rec skip_null () =
              if !pos >= len
              then Error `Need_more_data
              else if get_byte !pos = 0
              then (
                pos := !pos + 1;
                Ok ())
              else (
                pos := !pos + 1;
                skip_null ())
            in
            skip_null ()
          else Ok ()
        in

        (match result with
        | Error e -> Error e
        | Ok () ->
          let result =
            if has_comment
            then
              let rec skip_null () =
                if !pos >= len
                then Error `Need_more_data
                else if get_byte !pos = 0
                then (
                  pos := !pos + 1;
                  Ok ())
                else (
                  pos := !pos + 1;
                  skip_null ())
              in
              skip_null ()
            else Ok ()
          in

          (match result with
          | Error e -> Error e
          | Ok () ->
            if has_crc then pos := !pos + 2;

            if !pos > len then Error `Need_more_data else Ok !pos))

let decompress_deflate_stream body =
  (* Note: HTTP "deflate" encoding officially means zlib-wrapped deflate (RFC
     1950). Some implementations use raw deflate, but most tools (Python zlib,
     Java, etc.) produce zlib-wrapped deflate. We use the wrapper by default. *)
  let state = create_stream_state ~use_zlib_wrapper:true () in
  let input_stream = body in
  let rec next_decompressed () =
    match Piaf.Stream.take input_stream with
    | None ->
      cleanup_stream state;
      None
    | Some { Piaf.IOVec.buffer; off; len } ->
      let chunk = Bigstringaf.substring buffer ~off ~len in
      (match decompress_chunk state chunk ~off:0 ~len:(String.length chunk) with
      | Some decompressed, _ -> Some decompressed
      | None, _ -> next_decompressed ())
  in
  Piaf.Stream.from ~f:next_decompressed

let decompress_gzip_stream body =
  let state = create_stream_state ~use_zlib_wrapper:false () in
  let input_stream = body in
  let header_parsed = ref false in
  let buffer = Buffer.create 1024 in

  let rec next_decompressed () =
    if not !header_parsed
    then (
      match Piaf.Stream.take input_stream with
      | None ->
        cleanup_stream state;
        None
      | Some { Piaf.IOVec.buffer = buf; off; len } ->
        let chunk = Bigstringaf.substring buf ~off ~len in
        Buffer.add_string buffer chunk;
        let accumulated = Buffer.contents buffer in
        (match parse_gzip_header accumulated with
        | Error `Need_more_data -> next_decompressed ()
        | Error `Invalid_gzip_header ->
          cleanup_stream state;
          failwith "Invalid gzip header"
        | Error `Unsupported_compression_method ->
          cleanup_stream state;
          failwith "Unsupported compression method"
        | Ok offset ->
          header_parsed := true;
          let data_len = String.length accumulated - offset in
          if data_len > 0
          then
            match
              decompress_chunk state accumulated ~off:offset ~len:data_len
            with
            | Some decompressed, _ -> Some decompressed
            | None, _ -> next_decompressed ()
          else next_decompressed ()))
    else
      match Piaf.Stream.take input_stream with
      | None ->
        cleanup_stream state;
        None
      | Some { Piaf.IOVec.buffer = buf; off; len } ->
        let chunk = Bigstringaf.substring buf ~off ~len in
        (match
           decompress_chunk state chunk ~off:0 ~len:(String.length chunk)
         with
        | Some decompressed, _ -> Some decompressed
        | None, _ -> next_decompressed ())
  in
  Piaf.Stream.from ~f:next_decompressed

let create_gzip_header () =
  let header = Bytes.create 10 in
  Bytes.set header 0 '\x1f';
  (* ID1 *)
  Bytes.set header 1 '\x8b';
  (* ID2 *)
  Bytes.set header 2 '\x08';
  (* CM = deflate *)
  Bytes.set header 3 '\x00';
  (* FLG = no flags *)
  (* MTIME = 0 (4 bytes) *)
  Bytes.set header 4 '\x00';
  Bytes.set header 5 '\x00';
  Bytes.set header 6 '\x00';
  Bytes.set header 7 '\x00';
  Bytes.set header 8 '\x00';
  (* XFL = 0 *)
  Bytes.set header 9 '\xff';
  (* OS = unknown *)
  Bytes.to_string header

let compress_stream ~use_gzip_wrapper body =
  (* Create compression state *)
  let zstream = Zlib.deflate_init 6 use_gzip_wrapper in
  let finished = ref false in
  let cleaned_up = ref false in
  let buf_size = 4096 in
  let input_stream = body in
  let header_sent = ref (not use_gzip_wrapper) in

  let cleanup () =
    if not !cleaned_up
    then (
      cleaned_up := true;
      Zlib.deflate_end zstream)
  in

  let rec next_compressed () =
    (* Send gzip header first if needed *)
    if not !header_sent
    then (
      header_sent := true;
      Some (create_gzip_header ()))
    else if !finished
    then (
      cleanup ();
      None)
    else
      match Piaf.Stream.take input_stream with
      | None ->
        (* Flush and finish *)
        (try
           let buf = Bytes.create buf_size in
           let result_buf = Buffer.create buf_size in
           let rec flush () =
             let is_end, _, used_out =
               Zlib.deflate_string zstream "" 0 0 buf 0 buf_size Zlib.Z_FINISH
             in
             if used_out > 0 then Buffer.add_subbytes result_buf buf 0 used_out;
             if is_end then () else flush ()
           in
           flush ();
           finished := true;
           let result = Buffer.contents result_buf in
           if String.length result > 0
           then Some result
           else (
             cleanup ();
             None)
         with
        | e ->
          cleanup ();
          raise e)
      | Some chunk ->
        (try
           let len = String.length chunk in
           let buf = Bytes.create buf_size in
           let result_buf = Buffer.create buf_size in
           let rec inner ~off ~len =
             if len = 0
             then off
             else
               let is_end, used_in, used_out =
                 Zlib.deflate_string
                   zstream
                   chunk
                   off
                   len
                   buf
                   0
                   buf_size
                   Zlib.Z_NO_FLUSH
               in
               if used_out > 0
               then Buffer.add_subbytes result_buf buf 0 used_out;
               if is_end
               then (
                 finished := true;
                 cleanup ();
                 off + used_in)
               else if used_in < len
               then inner ~off:(off + used_in) ~len:(len - used_in)
               else off + used_in
           in
           let _ = inner ~off:0 ~len in
           let result = Buffer.contents result_buf in
           if String.length result > 0 then Some result else next_compressed ()
         with
        | e ->
          cleanup ();
          raise e)
  in
  Piaf.Stream.from ~f:next_compressed

module Deflate = struct
  let decompress body = Ok (decompress_deflate_stream body)

  module Compress = struct
    let compress body = Ok (compress_stream ~use_gzip_wrapper:true body)
  end
end

module Gzip = struct
  let decompress body = Ok (decompress_gzip_stream body)

  module Compress = struct
    let compress body = Ok (compress_stream ~use_gzip_wrapper:false body)
  end
end
