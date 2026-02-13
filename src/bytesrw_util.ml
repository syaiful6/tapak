open Bytesrw

external bigstring_to_bigbytes :
   Bigstringaf.t
  -> (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  = "%identity"

external bigbytes_to_bigstring :
   (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> Bigstringaf.t
  = "%identity"

let reader_of_stream stream =
  Bytes.Reader.make @@ fun () ->
  match Piaf.Stream.take stream with
  | None -> Bytes.Slice.eod
  | Some { Piaf.IOVec.buffer; off; len } ->
    Bytes.Slice.of_bigbytes
      ~first:off
      ~last:(off + len)
      (bigstring_to_bigbytes buffer)

let stream_of_reader reader =
  Piaf.Stream.from ~f:(fun () ->
    let slice = Bytes.Reader.read reader in
    if Bytes.Slice.is_eod slice
    then None
    else
      let buffer = bigbytes_to_bigstring (Bytes.Slice.to_bigbytes slice) in
      let len = Bytes.Slice.length slice in
      Option.some { Piaf.IOVec.buffer; off = 0; len })
