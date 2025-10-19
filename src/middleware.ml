include Tapak_kernel.Middleware

module Decompression = struct
  type decoder = Middleware_decompression.decoder

  let create decoder =
    use ~name:"Decompression" (module Middleware_decompression) decoder
end
