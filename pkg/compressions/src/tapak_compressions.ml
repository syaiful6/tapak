module Brotli = Brotli
module Zstd = Zstd
module Predicate = Predicate

module Decompressors = struct
  include Gzip
  module Brotli = Brotli_decompressor
  module Zstd = Zstd_decompressor
end

module Compressors = struct
  include Gzip
  module Brotli = Brotli_compressor
  module Zstd = Zstd_compressor
end

module type Decompressor = sig
  val decompress :
     Bigstringaf.t Piaf.IOVec.t Piaf.Stream.t
    -> (string Piaf.Stream.t, [> Piaf.Error.t ]) result
end

module type Compressor = sig
  val compress :
     string Piaf.Stream.t
    -> (string Piaf.Stream.t, [> Piaf.Error.t ]) result
end

(** Decoder function for use with decompression middleware.
    Returns None for unsupported encodings. *)
let decoder :
   [< `Gzip | `Deflate | `Br | `Zstd | `Identity | `Star | `Other of string ]
  -> (module Decompressor) option
  =
 fun encoding ->
  match encoding with
  | `Gzip -> Some (module Gzip.Gzip : Decompressor)
  | `Deflate -> Some (module Gzip.Deflate : Decompressor)
  | `Br -> Some (module Brotli_decompressor : Decompressor)
  | `Zstd -> Some (module Zstd_decompressor : Decompressor)
  | `Identity ->
    Some
      (module struct
        let decompress body =
          (* Convert IOVec stream to string stream *)
          let string_stream =
            Piaf.Stream.from ~f:(fun () ->
              match Piaf.Stream.take body with
              | None -> None
              | Some { Piaf.IOVec.buffer; off; len } ->
                Some (Bigstringaf.substring buffer ~off ~len))
          in
          Ok string_stream
      end : Decompressor)
  | `Star | `Other _ -> None

(** Encoder function for use with compression middleware.
    Returns None for unsupported encodings. *)
let encoder :
   [< `Gzip | `Deflate | `Br | `Zstd | `Identity | `Star | `Other of string ]
  -> (module Compressor) option
  =
 fun encoding ->
  match encoding with
  | `Gzip -> Some (module Gzip.Gzip.Compress : Compressor)
  | `Deflate -> Some (module Gzip.Deflate.Compress : Compressor)
  | `Br -> Some (module Brotli_compressor : Compressor)
  | `Zstd -> Some (module Zstd_compressor : Compressor)
  | `Identity ->
    Some
      (module struct
        let compress body = Ok body
      end : Compressor)
  | `Star | `Other _ -> None
