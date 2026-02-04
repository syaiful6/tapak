type t = (Request.t, Response.t) Service.t

val not_found : t

val to_piaf :
   (Request.t, Response.t) Service.t
  -> Piaf.Request_info.t Piaf.Server.Handler.t
