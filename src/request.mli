type t =
  { headers : Http.Header.t
  ; meth : Http.Method.t
  ; target : string
  ; version : Http.Version.t
  ; body : Eio.Flow.source_ty Eio.Resource.t
  ; client_addr : string option
  ; is_secure : bool
  }

include Headers.S with type t := t

val meth : t -> Http.Method.t
val target : t -> string
val version : t -> Http.Version.t
val body : t -> Eio.Flow.source_ty Eio.Resource.t
val is_secure : ?trusted_proxies:Ipaddr.Prefix.t list -> t -> bool
val client_ip : ?trusted_proxies:Ipaddr.Prefix.t list -> t -> string
val is_keep_alive : t -> bool

val pp_field :
   string
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a
  -> unit

val make :
   ?version:Http.Version.t
  -> ?body:Eio.Flow.source_ty Eio.Resource.t
  -> ?client_addr:string
  -> ?headers:Headers.t
  -> ?meth:Http.Method.t
  -> ?is_secure:bool
  -> string
  -> t

val with_ :
   ?meth:Http.Method.t
  -> ?target:string
  -> ?version:Http.Version.t
  -> ?headers:Http.Header.t
  -> ?body:Eio.Flow.source_ty Eio.Resource.t
  -> ?client_addr:string option
  -> ?is_secure:bool
  -> t
  -> t

val pp : Format.formatter -> t -> unit
