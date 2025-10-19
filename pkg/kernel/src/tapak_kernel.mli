module App : sig
  type t = App.t =
    { middlewares : Middleware.t list
    ; handler : (Request.t, Response.t) Service.t
    }

  val ( <+> ) : t -> Middleware.t -> t

  val create :
     ?middlewares:Middleware.t list
    -> handler:(Request.t, Response.t) Service.t
    -> unit
    -> t

  val call : t -> Request.t -> Response.t
  val to_piaf : App.t -> Piaf.Request_info.t Piaf.Server.Handler.t
end

module Context = Context
module Filter = Filter
module Form = Form
module Handler = Handler
module Middleware = Middleware
module Request = Request
module Request_info = Request_info
module Response = Response
module Router = Router
module Service = Service
module Body = Body
module Server_connection = Server_connection
