type t = (Request.t, Response.t) Service.t

val not_found : 'a -> Response.t
val to_piaf : t -> Piaf.Request_info.t Piaf.Server.Handler.t
