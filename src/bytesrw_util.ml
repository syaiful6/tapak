open Bytesrw

module Reader_source = struct
  type t = Bytes.Reader.t

  let read_methods = []

  let single_read t buf =
    let slice = Bytes.Reader.read t in
    if Bytes.Slice.is_eod slice
    then raise End_of_file
    else begin
      Cstruct.blit_from_bytes
        (Bytes.Slice.bytes slice)
        (Bytes.Slice.first slice)
        buf
        0
        (Bytes.Slice.length slice);
      Bytes.Slice.length slice
    end
end

let source_of_reader =
  let handler = Eio.Flow.Pi.source (module Reader_source) in
  fun b -> Eio.Resource.T (b, handler)

let reader_of_flow
      ?(slice_length = Bytes.Slice.unix_io_buffer_size)
      (flow : _ Eio.Flow.source)
  =
  let buf_size = Bytes.Slice.check_length slice_length in
  let read () =
    let cstruct = Cstruct.create buf_size in
    match Eio.Flow.single_read flow cstruct with
    | 0 -> Bytes.Slice.eod
    | count ->
      let data = Cstruct.sub cstruct 0 count in
      let buf = Cstruct.to_bytes data in
      Bytes.Slice.make buf ~first:0 ~length:count
    | exception End_of_file -> Bytes.Slice.eod
  in
  Bytes.Reader.make read

let reader_of_stream stream =
  let read () =
    match Eio.Stream.take stream with
    | Some str ->
      (* we use unsafe here, safe, since that how Bytes.Reader expects the
         buffer to be used, and we won't modify it *)
      let buf = Bytes.unsafe_of_string str in
      Bytes.Slice.of_bytes buf
    | None -> Bytes.Slice.eod
  in
  Bytes.Reader.make read

let writer_of_flow
      ?(slice_length = Bytes.Slice.unix_io_buffer_size)
      (flow : _ Eio.Flow.sink)
  =
  let rec write slice =
    if Bytes.Slice.is_eod slice
    then ()
    else begin
      let bytes = Bytes.Slice.bytes slice in
      let first = Bytes.Slice.first slice in
      let length = Bytes.Slice.length slice in
      let cstruct = Cstruct.of_bytes ~off:first ~len:length bytes in
      match Eio.Flow.single_write flow [ cstruct ] with
      | count when count = length -> ()
      | count -> write (Option.get (Bytes.Slice.drop count slice))
    end
  in
  Bytes.Writer.make ~slice_length write

let chunked_writer : ?write_footer:bool -> unit -> Bytes.Writer.filter =
 fun ?(write_footer = true) () ?pos ?(slice_length = 65536) ~eod w ->
  let write slice =
    if Bytes.Slice.is_eod slice
    then (
      if write_footer then Bytes.Writer.write_string w "0\r\n\r\n";
      if eod then Bytes.Writer.write_eod w)
    else begin
      (* bytesrw guarantees that the slice written here is non empty, so we can
         just write it without checking chunk_size <> 0 *)
      let chunk_size = Bytes.Slice.length slice in
      let chunk_header = Printf.sprintf "%x\r\n" chunk_size in
      Bytes.Writer.write_string w chunk_header;
      Bytes.Writer.write w slice;
      Bytes.Writer.write_string w "\r\n"
    end
  in
  Bytes.Writer.make ?pos ~slice_length write

let writer_of_eio_buf ?pos ?(slice_length = Bytes.Slice.unix_io_buffer_size) buf
  =
  let write slice =
    if Bytes.Slice.is_eod slice
    then Eio.Buf_write.flush buf
    else begin
      Eio.Buf_write.bytes
        buf
        ~off:(Bytes.Slice.first slice)
        ~len:(Bytes.Slice.length slice)
        (Bytes.Slice.bytes slice)
    end
  in
  Bytes.Writer.make ?pos ~slice_length write
