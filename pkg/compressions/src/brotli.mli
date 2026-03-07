module Bbuf : sig
  type t =
    { mutable bytes : Bytesrw.Bytes.t
    ; mutable size : int
    ; mutable pos : int
    }

  val make_empty : unit -> t
  val make : int -> t
  val src_is_consumed : t -> bool
  val src_rem : t -> int
  val src_set_slice : t -> Bytesrw.Bytes.Slice.t -> unit
  val src_to_slice_or_eod : t -> Bytesrw.Bytes.Slice.t
  val dst_clear : t -> unit
  val dst_is_empty : t -> bool
  val dst_is_full : t -> bool
  val dst_to_slice : t -> Bytesrw.Bytes.Slice.t
end

type Bytesrw.Bytes.Stream.error += Error of string

val format_error : string Bytesrw.Bytes.Stream.format_error
val error : string -> 'a
val reader_error : Bytesrw.Bytes.Reader.t -> string -> 'a
val writer_error : Bytesrw.Bytes.Writer.t -> string -> 'a

module Decoder : sig
  type t

  type result =
    | Error
    | Success
    | Needs_more_input
    | Needs_more_output

  val create : unit -> t
  val destroy : t -> unit
  val decompress_stream : t -> src:Bbuf.t -> dst:Bbuf.t -> result
  val with_decoder : (t -> 'a) -> 'a
end

module Encoder : sig
  type t

  type op =
    | Process
    | Flush
    | Finish

  type result =
    | Error
    | Ok
    | Finished

  val create : ?quality:int -> unit -> t
  val destroy : t -> unit
  val compress_stream : t -> op -> src:Bbuf.t -> dst:Bbuf.t -> result
  val with_encoder : ?quality:int -> (t -> 'a) -> 'a
end
