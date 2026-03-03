module Context = Context
module Request = Request
module Response = Response
module Headers = Headers
module Body = Body
module Router = Router
module Middleware = Middleware
module Handler = Handler
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
   ?version:Http.Version.t
  -> ?status:Http.Status.t
  -> ?headers:Headers.t
  -> Body.t
  -> Response.t

val html :
   ?version:Http.Version.t
  -> ?headers:Headers.t
  -> ?status:Http.Status.t
  -> string
  -> Response.t

val json :
   ?version:Http.Version.t
  -> ?headers:Headers.t
  -> ?status:Http.Status.t
  -> string
  -> Response.t

val redirect :
   ?version:Http.Version.t
  -> ?headers:Headers.t
  -> ?status:Http.Status.redirection
  -> string
  -> Response.t

val stream :
   ?version:Http.Version.t
  -> ?headers:Headers.t
  -> ?status:Http.Status.t
  -> ?length:int64
  -> ((string -> unit) -> (unit -> unit) -> unit)
  -> Response.t

val sse :
   ?version:Http.Version.t
  -> ?headers:Headers.t
  -> ((Sse.Event.t -> unit) -> (unit -> unit) -> unit)
  -> Response.t

val static :
   env:Static.fs_env
  -> ?config:Static.config
  -> ?follow:bool
  -> ?ttl_seconds:float
  -> [> Eio.Fs.dir_ty ] Eio.Path.t
  -> unit
  -> Request.t
  -> string list
  -> Response.t

val openapi :
   ?title:string
  -> ?version:string
  -> ?description:string
  -> ?base_path:string
  -> Router.route list
  -> string

val run :
   ?max_connections:int
  -> ?additional_domains:_ Eio.Domain_manager.t * int
  -> ?stop:'a Eio.Promise.t
  -> on_error:(exn -> unit)
  -> _ Eio.Net.listening_socket
  -> Handler.t
  -> 'a
