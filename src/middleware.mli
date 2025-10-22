include module type of Tapak_kernel.Middleware

module Decompression : sig
  module type Decoder = sig
    val decompress :
       Bigstringaf.t Piaf.IOVec.t Piaf.Stream.t
      -> (string Piaf.Stream.t, [> Piaf.Error.t ]) result
  end

  type decoder = Header_parser.Accept.encoding -> (module Decoder) option

  include
    Tapak_kernel.Middleware.Intf
    with type args = decoder
     and type state = decoder
end

module Compression : sig
  module type Encoder = sig
    val compress :
       string Piaf.Stream.t
      -> (string Piaf.Stream.t, [> Piaf.Error.t ]) result
  end

  type encoder = Header_parser.Accept.encoding -> (module Encoder) option
  type predicate = Request.t -> Response.t -> bool

  type args =
    { encoder : encoder
    ; predicate : predicate
    ; preferred_encodings : Header_parser.Accept.encoding list
    }

  include
    Tapak_kernel.Middleware.Intf with type args := args and type state = args
end

module Request_logger : sig
  type request_info =
    { client_ip : string
    ; request_method : string
    ; request_uri : string
    ; request_protocol : string
    ; response_bytes : int option
    ; referer : string option
    ; user_agent : string option
    ; request_id : string option
    ; duration_ms : float
    }

  type formatter = request_info -> string

  val apache_common_log_format : formatter

  type args =
    { now : unit -> float
    ; formatter : formatter
    ; trusted_proxies : Ipaddr.Prefix.t list
    }

  val args :
     now:(unit -> float)
    -> trusted_proxies:Ipaddr.Prefix.t list
    -> ?formatter:formatter
    -> unit
    -> args

  include
    Tapak_kernel.Middleware.Intf with type args := args and type state = args
end
