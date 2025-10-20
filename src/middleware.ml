include Tapak_kernel.Middleware

module Decompression = struct
  module type Decoder = sig
    val decompress :
       Bigstringaf.t Piaf.IOVec.t Piaf.Stream.t
      -> (string Piaf.Stream.t, [> Piaf.Error.t ]) result
  end

  type decoder = Middleware_decompression.decoder

  let middleware decoder =
    use ~name:"Decompression" (module Middleware_decompression) decoder
end

module Compression = struct
  open Header_parser

  module type Encoder = sig
    val compress :
       string Piaf.Stream.t
      -> (string Piaf.Stream.t, [> Piaf.Error.t ]) result
  end

  type encoder = Middleware_compression.encoder
  type predicate = Middleware_compression.predicate

  type args =
    { encoder : encoder
    ; predicate : predicate
    ; preferred_encodings : Accept.encoding list
    }

  let middleware
        ?(predicate = fun _ _ -> true)
        ?(preferred_encodings = [])
        encoder
    =
    use
      ~name:"Compression"
      (module Middleware_compression)
      { encoder; predicate; preferred_encodings }
end
