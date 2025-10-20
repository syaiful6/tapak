include Tapak_kernel.Middleware

module Decompression = struct
  type decoder = Middleware_decompression.decoder

  let middleware decoder =
    use ~name:"Decompression" (module Middleware_decompression) decoder
end

module Compression = struct
  open Header_parser

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
