type t =
  { request : Piaf.Request.t
  ; ctx : Request_info.t
  }

val meth : t -> Piaf.Method.t
val target : t -> string
val version : t -> Piaf.Versions.HTTP.t
val headers : t -> Piaf.Headers.t
val scheme : t -> Piaf.Scheme.t
val body : t -> Piaf.Body.t
val uri : t -> Uri.t
val info : t -> Request_info.t
val context : t -> Context.t

val create :
   scheme:Piaf.Scheme.t
  -> version:Piaf.Versions.HTTP.t
  -> ?headers:Piaf.Headers.t
  -> meth:Piaf.Method.t
  -> body:Piaf.Body.t
  -> ?ctx:Request_info.t
  -> string
  -> t

val with_ :
   ?meth:Piaf.Method.t
  -> ?target:string
  -> ?version:Piaf.Versions.HTTP.t
  -> ?headers:Piaf.Headers.t
  -> ?body:Piaf.Body.t
  -> ?ctx:Request_info.t
  -> t
  -> t

val with_context : Context.t -> t -> t
val pp_hum : Format.formatter -> t -> unit
