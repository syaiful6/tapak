module Context = Context
module Request = Request
module Response = Response
module Headers = Headers
module Body = Body
module Router = Router
module Middleware = Middleware
module Handler = Handler
module Filter = Filter
module Service = Service
module Form = Form
module Cookies = Cookies
module Csrf = Csrf
module Static = Static
module Sse = Sse
module Pubsub = Pubsub
module Channel = Channel

module Socket : sig
  type t = Socket.t =
    { id : string option
    ; assigns : Context.t
    ; transport : string
    ; joined_topics : string list
    }

  type connect_info = Socket.connect_info =
    { request : Request.t
    ; params : Form.Urlencoded.t
    }

  module type S = Socket.S

  val assigns : t -> Context.t
  val assign : 'a Context.key -> 'a -> t -> t
  val find_assign : 'a Context.key -> t -> 'a option
  val find_assign_exn : 'a Context.key -> t -> 'a
  val id : t -> string option
  val transport : t -> string
  val joined_topics : t -> string list

  module Endpoint = Socket_endpoint
  module Protocol = Socket_protocol

  val mount :
     ?middlewares:Middleware.t list
    -> ?transports:(module Endpoint.Transport) list
    -> ('a, 'a) Router.path
    -> Socket_endpoint.t
    -> Router.route
end

module Presence = Presence

module type SOCKET = Socket.S
module type CHANNEL = Channel.S
module type PUBSUB = Pubsub.S

type request = Request.t
and response = Response.t
and handler = Handler.t
and middleware = Middleware.t

module type MIDDLEWARE = sig
  type t

  val call : t -> middleware
end

val use : (module MIDDLEWARE with type t = 'a) -> 'a -> middleware
val pipe : through:middleware list -> middleware

val response :
   ?version:Piaf.Versions.HTTP.t
  -> ?status:Piaf.Status.t
  -> ?headers:Piaf.Headers.t
  -> ?context:Context.t
  -> string
  -> response

val html :
   ?version:Piaf.Versions.HTTP.t
  -> ?status:Piaf.Status.t
  -> ?headers:Piaf.Headers.t
  -> ?context:Context.t
  -> string
  -> response

val json :
   ?version:Piaf.Versions.HTTP.t
  -> ?status:Piaf.Status.t
  -> ?headers:Piaf.Headers.t
  -> ?context:Context.t
  -> string
  -> response

val redirect :
   ?version:Piaf.Versions.HTTP.t
  -> ?status:Piaf.Status.redirection
  -> ?headers:Piaf.Headers.t
  -> ?context:Context.t
  -> string
  -> response

val stream :
   sw:Eio.Switch.t
  -> ?status:Piaf.Status.t
  -> ?headers:Headers.t
  -> ?context:Context.t
  -> ((string option -> unit) -> unit)
  -> response

val sse :
   sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> ?interval:float
  -> ?headers:Headers.t
  -> ?context:Context.t
  -> ((Sse.Event.t option -> unit) -> unit)
  -> response

val static :
   env:Static.fs_env
  -> ?config:Static.config
  -> ?follow:bool
  -> ?ttl_seconds:float
  -> _ Eio.Path.t
  -> unit
  -> request
  -> string list
  -> response

val openapi :
   ?title:string
  -> ?version:string
  -> ?description:string
  -> ?base_path:string
  -> Router.route list
  -> string

val run_with :
   ?error_handler:Piaf.Server.error_handler
  -> config:Piaf.Server.Config.t
  -> env:Eio_unix.Stdenv.base
  -> Handler.t
  -> Piaf.Server.Command.t

type systemd

type server =
  [ `Piaf of Piaf.Server.Command.t
  | `Systemd of systemd
  ]

val run_with_systemd_socket :
   ?error_handler:Piaf.Server.error_handler
  -> config:Piaf.Server.Config.t
  -> env:Eio_unix.Stdenv.base
  -> Handler.t
  -> server

val shutdown : server -> unit
