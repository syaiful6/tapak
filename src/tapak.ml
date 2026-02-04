module Request = Request
module Response = Response
module Headers = Headers
module Body = Body
module Router = Router
module Schema = Schema
module Context = Context
module Middleware = Middleware
module Handler = Handler
module Filter = Filter
module Service = Service
module Form = Form
module Server = Server
module Cookies = Cookies
module Csrf = Csrf
module Static = Static
module Sse = Sse
module Openapi = Openapi
module Channel = Channel

module type SOCKET = Channel.SOCKET
module type PUBSUB = Channel.PUBSUB
module type CHANNEL = Channel.CHANNEL

type request = Request.t
and response = Response.t
and handler = Handler.t
and middleware = Middleware.t

module type MIDDLEWARE = sig
  type t

  val call : t -> middleware
end

let use = Middleware.use
let pipe ~through = Filter.apply_all through

let response
      ?(version = Piaf.Versions.HTTP.HTTP_1_1)
      ?(status = `OK)
      ?(headers = Piaf.Headers.empty)
      ?(context = Context.empty)
      body
  =
  Response.create ~version ~context ~headers ~body:(Body.of_string body) status

let html = Response.of_html
let json = Response.of_json
let redirect = Response.redirect

let stream ~sw ?(status = `OK) ?headers ?context callback =
  let string_stream, writer = Piaf.Stream.create 4 in
  Eio.Fiber.fork ~sw (fun () -> callback writer);
  Response.of_string_stream ?headers ?context ~body:string_stream status

let sse ~sw ~clock ?(interval = 15.0) ?headers ?context callback =
  let event_stream, writer = Piaf.Stream.create 4 in
  let kept_alive = Sse.keep_alive ~sw ~clock ~interval event_stream in
  Eio.Fiber.fork ~sw (fun () -> callback writer);
  Sse.stream ?headers ?context kept_alive

let static ~env ?config ?(follow = false) ?ttl_seconds path () =
  let backend = Static.filesystem ~env ~follow ?ttl_seconds path in
  Static.serve backend ?config ()

let openapi = Openapi.generate

type systemd = Server.Systemd.t

type server =
  [ `Piaf of Piaf.Server.Command.t
  | `Systemd of systemd
  ]

let run_with = Server.run_with
let run_with_systemd_socket = Server.run_with_systemd_socket

let shutdown = function
  | `Piaf piaf_server -> Piaf.Server.Command.shutdown piaf_server
  | `Systemd systemd_server -> Server.Systemd.shutdown systemd_server
