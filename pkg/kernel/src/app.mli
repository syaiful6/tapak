type t =
  { middlewares : Middleware.t list
  ; handler : (Request.t, Response.t) Service.t
  }

val ( <+> ) : t -> Middleware.t -> t
val ( <++> ) : t -> Middleware.t list -> t

val create :
   ?middlewares:Middleware.t list
  -> handler:(Request.t, Response.t) Service.t
  -> unit
  -> t

val call : t -> Request.t -> Response.t

val use :
   name:string
  -> (module Middleware.Intf with type t = 'a)
  -> 'a
  -> Middleware.t

val to_piaf : t -> Piaf.Request_info.t Piaf.Server.Handler.t
