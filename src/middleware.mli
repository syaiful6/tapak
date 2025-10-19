include module type of Tapak_kernel.Middleware

module Decompression : sig
  type decoder = Middleware_decompression.decoder

  val create : Middleware_decompression.args -> t
end
