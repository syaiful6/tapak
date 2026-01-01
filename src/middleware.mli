include module type of Tapak_kernel.Middleware

module Decompression : sig
  module type Decoder = sig
    val decompress :
       Bigstringaf.t Piaf.IOVec.t Piaf.Stream.t
      -> (string Piaf.Stream.t, [> Piaf.Error.t ]) result
  end

  type decoder = Header_parser.Accept.encoding -> (module Decoder) option

  include Tapak_kernel.Middleware.Intf with type t = decoder
end

module Compression : sig
  module type Encoder = sig
    val compress :
       string Piaf.Stream.t
      -> (string Piaf.Stream.t, [> Piaf.Error.t ]) result
  end

  type encoder = Header_parser.Accept.encoding -> (module Encoder) option
  type predicate = Request.t -> Response.t -> bool

  type t =
    { encoder : encoder
    ; predicate : predicate
    ; preferred_encodings : Header_parser.Accept.encoding list
    }

  val args :
     encoder:encoder
    -> predicate:predicate
    -> preferred_encodings:Header_parser.Accept.encoding list
    -> t

  include Tapak_kernel.Middleware.Intf with type t := t
end

module Request_logger : sig
  type log_info =
    { client_ip : string
    ; request_method : string
    ; request_uri : string
    ; request_protocol : string
    ; status : int
    ; response_bytes : int option
    ; referer : string option
    ; user_agent : string option
    ; request_id : string option
    ; duration_seconds : float
    }

  type formatter = log_info -> string

  val apache_common_log_format : formatter

  type t =
    { now : unit -> float
    ; formatter : formatter
    ; trusted_proxies : Ipaddr.Prefix.t list
    }

  val args :
     now:(unit -> float)
    -> trusted_proxies:Ipaddr.Prefix.t list
    -> ?formatter:formatter
    -> unit
    -> t

  include Tapak_kernel.Middleware.Intf with type t := t
end

module Limit_request_size : sig
  type t = { max_bytes : int64 }

  val args : max_bytes:int64 -> t

  include Tapak_kernel.Middleware.Intf with type t := t
end

module CORS : sig
  (** CORS (Cross-Origin Resource Sharing) middleware.

      This middleware handles CORS preflight requests and adds appropriate
      CORS headers to responses according to the W3C CORS specification. *)

  type origin_policy =
    [ `Allow_all
    | `Allow_list of string list
    | `Allow_predicate of string -> bool
    ]

  type t =
    { origins : origin_policy
      (** Origin validation policy. Default: [Allow_all] *)
    ; methods : Piaf.Method.t list
      (** Allowed HTTP methods. Default: GET, POST, PUT, PATCH, DELETE, HEAD, OPTIONS *)
    ; headers : string list
      (** Allowed request headers. Default: Accept, Content-Type, Authorization, etc. *)
    ; exposed_headers : string list
      (** Headers exposed to JavaScript. Default: empty *)
    ; credentials : bool
      (** Allow cookies and authentication. Default: false *)
    ; max_age : int option
      (** Preflight cache duration in seconds. Default: None *)
    ; send_preflight : bool
      (** Automatically handle OPTIONS preflight requests. Default: true *)
    }

  val args :
     ?origins:origin_policy
    -> ?methods:Piaf.Method.t list
    -> ?headers:string list
    -> ?exposed_headers:string list
    -> ?credentials:bool
    -> ?max_age:int
    -> ?send_preflight:bool
    -> unit
    -> t
  (** [args ()] creates a CORS configuration with sensible defaults.

      Example:
      {[
        (* Development - allow all origins *)
        use (module CORS) (CORS.args ())

        (* Production - specific origins with credentials *)
        use (module CORS)
          (CORS.args
            ~origins:(Allow_list ["https://example.com"; "https://app.example.com"])
            ~credentials:true
            ~max_age:86400
            ())
      ]} *)

  val permissive : unit -> t
  (** [permissive ()] creates a permissive CORS policy suitable for development.
      Allows all origins, all common methods, and any headers. *)

  val strict : origins:string list -> t
  (** [strict ~origins] creates a strict CORS policy suitable for production.
      Only allows specified origins with GET/POST methods and standard headers. *)

  include Tapak_kernel.Middleware.Intf with type t := t
end

val head : t
(** [head] is a middleware that automatically handles HEAD requests by converting
    them to GET requests, processing them through the service, then removing the
    response body. This follows the HTTP specification and Finagle's approach.

    According to RFC 7231, a HEAD request is identical to GET except the
    server MUST NOT send a message body. The server SHOULD send the same
    headers as it would for a GET request. *)
