module Request = Request
module Response = Response
module Headers = Headers
module Body = Body
module Router = Router
module Context = Context
module Middleware = Middleware
module Handler = Handler
module Service = Service
module Form = Form
module Server = Server
module Cookies = Cookies
module Csrf = Csrf
module Static = Static
module Sse = Sse
module Openapi = Openapi
module Pubsub = Pubsub
module Channel = Channel
module Presence = Presence

module Socket = struct
  include Socket
  module Endpoint = Socket_endpoint
  module Protocol = Socket_protocol

  let mount
        ?(middlewares = [])
        ?(transports = [ (module Websocket : Endpoint.Transport) ])
        prefix
        endpoint
    =
    let routes =
      transports
      |> List.concat_map (fun (module T : Endpoint.Transport) ->
        T.routes endpoint)
    in
    Router.scope ~middlewares prefix routes
end

module type SOCKET = Socket.S
module type PUBSUB = Pubsub.S
module type CHANNEL = Channel.S

type request = Request.t
and response = Response.t
and handler = Handler.t
and middleware = Middleware.t

module type MIDDLEWARE = sig
  type t

  val call : t -> middleware
end

let use = Middleware.use
let pipe ~through = Middleware.apply_all through

let response ?(version = `HTTP_1_1) ?(status = `OK) ?headers body =
  Response.make ~version ?headers ~status body

let html = Response.html
let json = Response.json
let redirect = Response.redirect
let stream = Response.stream
let sse = Sse.stream

let static ~env ?config ?(follow = false) ?ttl_seconds path () =
  let backend = Static.filesystem ~env ~follow ?ttl_seconds path in
  Static.serve backend ?config ()

let openapi = Openapi.generate_string
let run = Server.run
