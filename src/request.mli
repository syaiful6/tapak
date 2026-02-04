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
val to_piaf : t -> Piaf.Request.t
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

val header : string -> t -> string option
(** Extended functions *)

val add_header : string -> string -> t -> t
val remove_header : string -> t -> t
val replace_header : string -> string -> t -> t
val remote_ip : t -> string option
val client_ip : trusted_proxies:Ipaddr.Prefix.t list -> t -> string
val is_secure : trusted_proxies:Ipaddr.Prefix.t list -> t -> bool
