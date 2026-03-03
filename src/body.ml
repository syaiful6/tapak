type streaming_fn = Bytesrw.Bytes.Writer.t -> (unit -> unit) -> unit

type t =
  [ `Empty
  | `Raw of Eio.Buf_read.t -> Eio.Buf_write.t -> unit
  | `Stream of Int64.t option * streaming_fn
  | `String of string
  ]

let empty = `Empty
let of_string s = `String s

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
  `Stream (length, writer)

let raw f = `Raw f
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
  | `String s -> s
  | `Stream (_, f) -> stream_to_string f
  | `Raw f -> raw_to_string f (* or returns empty string here? *)
  | `Empty -> ""

let length = function
  | `String s -> Some (Int64.of_int (String.length s))
  | `Stream (len, _) -> len
  | `Raw _ -> None
  | `Empty -> Some 0L
