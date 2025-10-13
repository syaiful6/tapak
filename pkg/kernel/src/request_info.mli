type t =
  { scheme : Piaf.Scheme.t
  ; version : Piaf.Versions.HTTP.t
  ; client_address : Eio.Net.Sockaddr.stream option
  ; sw : Eio.Switch.t option
  ; env : Context.t
  }

val of_piaf : Piaf.Request_info.t -> t
val default : t

val with_ :
   ?scheme:Piaf.Scheme.t
  -> ?version:Piaf.Versions.HTTP.t
  -> ?client_address:Eio.Net.Sockaddr.stream option
  -> ?sw:Eio.Switch.t option
  -> ?env:Context.t
  -> t
  -> t
