type t =
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
