type streaming_fn = Bytesrw.Bytes.Writer.t -> (unit -> unit) -> unit

type content =
  [ `Empty
  | `Raw of Eio.Buf_read.t -> Eio.Buf_write.t -> unit
  | `Stream of streaming_fn
  | `String of string
  ]

type t =
  { length : Int64.t option
  ; content : content
  }

let empty = { length = Some 0L; content = `Empty }

let of_string s =
  { length = Some (Int64.of_int (String.length s)); content = `String s }

(** [stream ?length f] create response streaming body. The function passed here take two parameters,
    the first parameter provides a means of sending another chunk of data, and the second parameter
    provides a means of flushing the data to the client. *)
let stream ?length f =
  let writer w flush =
    f
      (fun chunk ->
         if chunk <> "" then Bytesrw.Bytes.Writer.write_string w chunk)
      flush;
    Bytesrw.Bytes.Writer.write_eod w
  in
  { length; content = `Stream writer }

let writer ?length f = { length; content = `Stream f }
let raw f = { length = None; content = `Raw (fun src dst -> f src dst) }
let noop () = ()

let stream_to_string f =
  let buffer = Buffer.create 1024 in
  let buf_writer = Bytesrw.Bytes.Writer.of_buffer buffer in
  f buf_writer noop;
  Buffer.contents buffer

let raw_to_string f =
  let buffer = Buffer.create 1024 in
  let src = Eio.Buf_read.of_string "" in
  let buffer_flow = Eio.Flow.buffer_sink buffer in
  Eio.Buf_write.with_flow buffer_flow (fun dst -> f src dst);
  Buffer.contents buffer

let to_string = function
  | { content = `String s; _ } -> s
  | { content = `Stream f; _ } -> stream_to_string f
  | { content = `Raw f; _ } ->
    raw_to_string f (* or returns empty string here? *)
  | { content = `Empty; _ } -> ""

let to_streaming_fn = function
  | { content = `String s; _ } when s <> "" ->
    Some
      (fun w flush ->
        Bytesrw.Bytes.Writer.write_string w s;
        Bytesrw.Bytes.Writer.write_eod w;
        flush ())
  | { content = `Stream f; _ } -> Some f
  | _ -> None
