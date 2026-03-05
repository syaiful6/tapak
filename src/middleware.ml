type t = (Request.t, Response.t) Service.t -> (Request.t, Response.t) Service.t

let apply_all xs service = List.fold_right (fun f acc -> f acc) xs service

module type Intf = sig
  type t

  val call :
     t
    -> (Request.t, Response.t) Service.t
    -> (Request.t, Response.t) Service.t
end

let use (type a) (module M : Intf with type t = a) (args : a) = M.call args

module Decompression = struct
  include Middleware_decompression
end

module Compression = struct
  include Middleware_compression
end

module Request_logger = Middleware_logger
module CORS = Middleware_cors

let head = Middleware_head.m
