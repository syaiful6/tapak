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

val empty : t
(** [empty] the empty response body *)

val of_string : string -> t
(* [of_string s] create response body from string [s]. The length of the body is
   set to the length of the string *)

val stream :
   ?length:Int64.t
  -> ((string -> unit) -> (unit -> unit) -> unit)
  -> t
(** [stream ?length f] create response body with streaming function. It's a function
  of two parameters; the first parameter provides a means of sending another chunk of
  data, and the second parameter provides a means of flushing the data to the client.

  If you passed length, then you must exactly send that length of data, if you don't know
  then left it as None *)

val writer :
   ?length:Int64.t
  -> (Bytesrw.Bytes.Writer.t -> (unit -> unit) -> unit)
  -> t
(** [writer ?length f] like {!stream} but take Bytesrw.Bytes.Writer.t instead of function
    to send chunk of chunk data. *)

val raw : (Eio.Buf_read.t -> Eio.Buf_write.t -> unit) -> t
(** [raw f] create response body with IO function that is expected
    to write the response body. The IO function has access to the
    underlying input and output channel, which allows writing a response body
    more efficiently, or switch protocols entirely *)

val to_string : t -> string
(** [to_string body] convert response body to string. If the body is a stream or raw, it will be consumed and converted to string. *)

val to_streaming_fn : t -> streaming_fn option
(** [to_streaming_fn body] convert response body to streaming function. If the body is a string, it will be converted 
to a stream that sends the string as a single chunk. 
If the body is raw or empty, it will return None .*)
