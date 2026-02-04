type t = (Request.t, Response.t) Filter.simple

module type Intf = sig
  type t

  val call : t -> (Request.t, Response.t) Filter.simple
end

let use (type a) (module M : Intf with type t = a) (args : a) = M.call args

module Decompression = struct
  module type Decoder = sig
    val decompress :
       Bigstringaf.t Piaf.IOVec.t Piaf.Stream.t
      -> (string Piaf.Stream.t, [> Piaf.Error.t ]) result
  end

  include Middleware_decompression
end

module Compression = struct
  module type Encoder = sig
    val compress :
       string Piaf.Stream.t
      -> (string Piaf.Stream.t, [> Piaf.Error.t ]) result
  end

  include Middleware_compression
end

module Request_logger = Middleware_logger
module Limit_request_size = Middleware_limit_request_size
module CORS = Middleware_cors

let head = Middleware_head.m
