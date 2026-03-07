open Bytesrw

type decoder_state
type encoder_state

module Bbuf = struct
  type t =
    { (* keep in sync with tapak_bbuf_fields enum in C *)
      mutable bytes : Bytes.t
    ; mutable size : int (* last read or write position + 1 *)
    ; mutable pos : int (* next read or write position *)
    }

  let make_empty () = { bytes = Bytes.empty; size = 0; pos = 0 }

  let make size =
    { bytes = Bytes.create (Bytes.Slice.check_length size); size; pos = 0 }

  let src_is_consumed buf = buf.pos >= buf.size
  let src_rem buf = buf.size - buf.pos

  let src_set_slice buf s =
    buf.bytes <- Bytes.Slice.bytes s;
    buf.size <- Bytes.Slice.first s + Bytes.Slice.length s;
    buf.pos <- Bytes.Slice.first s

  let src_to_slice_or_eod buf =
    let src_rem = src_rem buf in
    if src_rem = 0
    then Bytes.Slice.eod
    else Bytes.Slice.make buf.bytes ~first:buf.pos ~length:src_rem

  let dst_clear buf = buf.pos <- 0
  let dst_is_empty buf = buf.pos = 0
  let dst_is_full buf = buf.pos = buf.size
  let dst_to_slice buf = Bytes.Slice.make buf.bytes ~first:0 ~length:buf.pos
end

type Bytes.Stream.error += Error of string

let format_error =
  let case msg = Error msg in
  let message = function Error msg -> msg | _ -> assert false in
  Bytes.Stream.make_format_error ~format:"brotli" ~case ~message

let error e = Bytes.Stream.error format_error e
let reader_error r e = Bytes.Reader.error format_error r e
let writer_error w e = Bytes.Writer.error format_error w e

external decoder_create_instance :
   unit
  -> decoder_state
  = "tapak_brotli_decoder_create_instance"

external decoder_destroy_instance :
   decoder_state
  -> unit
  = "tapak_brotli_decoder_destroy_instance"

external decompress_stream_native :
   decoder_state
  -> Bbuf.t
  -> Bbuf.t
  -> int
  = "tapak_brotli_decompress_stream"

external encoder_create_instance :
   int
  -> encoder_state
  = "tapak_brotli_encoder_create_instance"

external encoder_destroy_instance :
   encoder_state
  -> unit
  = "tapak_brotli_encoder_destroy_instance"

external compress_stream_native :
   encoder_state
  -> Bbuf.t
  -> Bbuf.t
  -> int
  -> int
  = "tapak_brotli_compress_stream"

module Decoder = struct
  type t = decoder_state

  type result =
    | Error
    | Success
    | Needs_more_input
    | Needs_more_output

  let create () = decoder_create_instance ()
  let destroy = decoder_destroy_instance

  let decompress_stream state ~src ~dst =
    let status_int = decompress_stream_native state src dst in
    match status_int with
    | 0 -> Error
    | 1 -> Success
    | 2 -> Needs_more_input
    | 3 -> Needs_more_output
    | _ -> Error

  let with_decoder f =
    let decoder = create () in
    Fun.protect ~finally:(fun () -> destroy decoder) (fun () -> f decoder)
end

module Encoder = struct
  type t = encoder_state

  type op =
    | Process
    | Flush
    | Finish

  type result =
    | Error
    | Ok
    | Finished

  let create ?(quality = 6) () = encoder_create_instance quality
  let destroy = encoder_destroy_instance

  let compress_stream state op ~src ~dst =
    let op_int = match op with Process -> 0 | Flush -> 1 | Finish -> 2 in
    match compress_stream_native state src dst op_int with
    | 0 -> Error
    | 2 -> Finished
    | _ -> Ok

  let with_encoder ?quality f =
    let encoder = create ?quality () in
    Fun.protect ~finally:(fun () -> destroy encoder) (fun () -> f encoder)
end
