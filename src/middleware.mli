include module type of Tapak_kernel.Middleware

(** {1 Request Decompression Middleware} *)

module Decompression : sig
  (** Module type for decompression implementations *)
  module type Decoder = sig
    val decompress :
       Bigstringaf.t Piaf.IOVec.t Piaf.Stream.t
      -> (string Piaf.Stream.t, [> Piaf.Error.t ]) result
  end

  type decoder = Middleware_decompression.decoder
  (** Decoder function that maps content encodings to decompressor modules.

      Use {!Tapak_compressions.decoder} to get a pre-configured decoder that
      supports gzip, deflate, brotli, and zstd. *)

  val middleware : Middleware_decompression.args -> t
  (** Create a decompression middleware.

      This middleware automatically decompresses request bodies based on the
      [Content-Encoding] header. It supports multiple compression algorithms
      and can chain multiple decompression steps for multiply-encoded content.

      @param decoder Function that provides decompressor modules for supported encodings

      Example:
      {[
        open Tapak

        let app =
          App.create routes
          |> App.(<+>) (Middleware.Decompression.middleware
                          Tapak_compressions.decoder)
      ]} *)
end

(** {1 Response Compression Middleware} *)

module Compression : sig
  (** Module type for compression implementations *)
  module type Encoder = sig
    val compress :
       string Piaf.Stream.t
      -> (string Piaf.Stream.t, [> Piaf.Error.t ]) result
  end

  type encoder = Middleware_compression.encoder
  (** Encoder function that maps content encodings to compressor modules.

      Use {!Tapak_compressions.encoder} to get a pre-configured encoder that
      supports gzip, deflate, brotli, and zstd. *)

  type predicate = Middleware_compression.predicate
  (** Predicate function to determine if a response should be compressed.

      Takes the request and response, returns [true] if compression should be applied.
      Use {!Tapak_compressions.Predicate} module for common predicates. *)

  type args =
    { encoder : encoder
      (** Encoder function for supported compression algorithms *)
    ; predicate : predicate
      (** Predicate to decide if response should be compressed *)
    ; preferred_encodings : Header_parser.Accept.encoding list
      (** List of encodings to try, in order of preference *)
    }

  val middleware :
     ?predicate:predicate
    -> ?preferred_encodings:Header_parser.Accept.encoding list
    -> encoder
    -> t
  (** Create a compression middleware.

      This middleware automatically compresses response bodies based on the
      [Accept-Encoding] header and a configurable predicate. It uses content
      negotiation to select the best encoding supported by both the client and server.

      @param predicate Function to determine if a response should be compressed
                       (default: compress all responses)
      @param preferred_encodings List of encodings to offer, in preference order
                                 (default: empty list, meaning use all available)
      @param encoder Function that provides compressor modules for supported encodings

      The middleware:
      - Checks the predicate to see if compression should be applied
      - Negotiates encoding using [Accept-Encoding] header
      - Compresses the response body if an acceptable encoding is found
      - Updates [Content-Encoding] header and removes [Content-Length]
      - Adds [Vary: Accept-Encoding] header for cache compatibility

      Example with all options:
      {[
        open Tapak

        let app =
          App.create routes
          |> App.(<+>) (Middleware.Compression.middleware
                          ~predicate:Tapak_compressions.Predicate.default_predicate
                          ~preferred_encodings:[`Br; `Zstd; `Gzip; `Deflate]
                          Tapak_compressions.encoder)
      ]}

      Example with defaults:
      {[
        open Tapak

        let app =
          App.create routes
          |> App.(<+>) (Middleware.Compression.middleware
                          Tapak_compressions.encoder)
      ]} *)
end
