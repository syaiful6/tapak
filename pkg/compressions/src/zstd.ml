type dctx
type cctx

type decompress_status =
  | Error
  | Finished
  | Needs_more_input

type compress_status =
  | Error
  | Success

external create_dctx : unit -> dctx = "tapak_zstd_create_dctx"
external free_dctx : dctx -> unit = "tapak_zstd_free_dctx"

external decompress_stream_native :
   dctx
  -> string
  -> int
  -> int * int * string
  = "tapak_zstd_decompress_stream"

external create_cctx : unit -> cctx = "tapak_zstd_create_cctx"
external free_cctx : cctx -> unit = "tapak_zstd_free_cctx"

external set_compression_level :
   cctx
  -> int
  -> unit
  = "tapak_zstd_set_compression_level"

external compress_stream_native :
   cctx
  -> string
  -> int
  -> bool
  -> int * int * string * int
  = "tapak_zstd_compress_stream"

module Decompress = struct
  type t = dctx

  let create () = create_dctx ()
  let destroy = free_dctx

  let decompress_stream ?(output_size = 4096) dctx input =
    let status_int, consumed, output =
      decompress_stream_native dctx input output_size
    in
    let status : decompress_status =
      match status_int with
      | 0 -> Error
      | 1 -> Finished
      | 2 -> Needs_more_input
      | _ -> Error
    in
    status, consumed, output

  let with_dctx f =
    let dctx = create () in
    Fun.protect ~finally:(fun () -> destroy dctx) (fun () -> f dctx)
end

(* High-level compression API *)
module Compress = struct
  type t = cctx

  let create ?(level = 3) () =
    let cctx = create_cctx () in
    set_compression_level cctx level;
    cctx

  let destroy = free_cctx

  let compress_stream ?(output_size = 4096) ?(finish = false) cctx input =
    let status_int, consumed, output, remaining =
      compress_stream_native cctx input output_size finish
    in
    let status = if status_int = 0 then Error else Success in
    let is_finished = finish && remaining = 0 in
    status, consumed, output, is_finished

  let with_cctx ?level f =
    let cctx = create ?level () in
    Fun.protect ~finally:(fun () -> destroy cctx) (fun () -> f cctx)
end
