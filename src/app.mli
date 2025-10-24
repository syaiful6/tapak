include module type of Tapak_kernel.App

val routes :
   ?not_found:(Request.t -> Response.t)
  -> Router.route list
  -> ?middlewares:Tapak_kernel.Middleware.t list
  -> unit
  -> t
