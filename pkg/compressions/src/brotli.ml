type decoder_state
type encoder_state

type decoder_result =
  | Error
  | Success
  | Needs_more_input
  | Needs_more_output

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
  -> string
  -> int
  -> int * int * string
  = "tapak_brotli_decompress_stream"

external encoder_create_instance :
   int
  -> encoder_state
  = "tapak_brotli_encoder_create_instance"

external encoder_destroy_instance :
   encoder_state
  -> unit
  = "tapak_brotli_encoder_destroy_instance"

type encoder_operation =
  | Process
  | Flush
  | Finish

external compress_stream_native :
   encoder_state
  -> string
  -> int
  -> int
  -> bool * int * string * bool
  = "tapak_brotli_compress_stream"

module Decoder = struct
  type t = decoder_state

  let create () = decoder_create_instance ()
  let destroy = decoder_destroy_instance

  let decompress_stream ?(output_size = 4096) state input =
    let status_int, consumed, output =
      decompress_stream_native state input output_size
    in
    let status =
      match status_int with
      | 0 -> Error
      | 1 -> Success
      | 2 -> Needs_more_input
      | 3 -> Needs_more_output
      | _ -> Error
    in
    status, consumed, output

  let with_decoder f =
    let decoder = create () in
    Fun.protect ~finally:(fun () -> destroy decoder) (fun () -> f decoder)
end

module Encoder = struct
  type t = encoder_state

  let create ?(quality = 6) () = encoder_create_instance quality
  let destroy = encoder_destroy_instance

  let compress_stream ?(output_size = 4096) state input op =
    let op_int = match op with Process -> 0 | Flush -> 1 | Finish -> 2 in
    compress_stream_native state input output_size op_int

  let with_encoder ?quality f =
    let encoder = create ?quality () in
    Fun.protect ~finally:(fun () -> destroy encoder) (fun () -> f encoder)
end
