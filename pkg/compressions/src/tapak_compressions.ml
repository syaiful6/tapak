module Brotli = Brotli
module Zstd = Zstd

module Decompressors = struct
  include Gzip
  module Brotli = Brotli_decompressor
  module Zstd = Zstd_decompressor
end

module type Decompressor = sig
  val decompress :
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
        let decompress body = Ok body
      end : Decompressor)
  | `Star | `Other _ -> None
