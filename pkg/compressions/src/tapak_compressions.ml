module Brotli = Brotli
module Bytesrw_brotli = Bytesrw_brotli
module Predicate = Predicate

type compressor = Bytesrw.Bytes.Reader.filter
type decompressor = Bytesrw.Bytes.Reader.filter

let decoder :
   [< `Gzip | `Deflate | `Br | `Zstd | `Identity | `Star | `Other of string ]
  -> decompressor option
  =
 fun encoding ->
  match encoding with
  | `Gzip -> Some (Bytesrw_zlib.Gzip.decompress_reads ())
  | `Deflate -> Some (Bytesrw_zlib.Deflate.decompress_reads ())
  | `Br -> Some (Bytesrw_brotli.decompress_reads ())
  | `Zstd -> Some (Bytesrw_zstd.decompress_reads ())
  | `Identity -> None
  | `Star | `Other _ -> None

let encoder :
   [< `Gzip | `Deflate | `Br | `Zstd | `Identity | `Star | `Other of string ]
  -> compressor option
  =
 fun encoding ->
  match encoding with
  | `Gzip -> Some (Bytesrw_zlib.Gzip.compress_reads ~level:6 ())
  | `Deflate -> Some (Bytesrw_zlib.Deflate.compress_reads ~level:6 ())
  | `Br -> Some (Bytesrw_brotli.compress_reads ~quality:6 ())
  | `Zstd -> Some (Bytesrw_zstd.compress_reads ())
  | `Identity -> None
  | `Star | `Other _ -> None
