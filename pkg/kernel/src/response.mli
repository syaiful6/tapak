type t =
  { response : Piaf.Response.t
  ; context : Context.t
  }

val status : t -> Piaf.Status.t
val headers : t -> Piaf.Headers.t
val version : t -> Piaf.Versions.HTTP.t
val body : t -> Body.t
val context : t -> Context.t
val to_piaf : t -> Piaf.Response.t

val create :
   ?version:Piaf.Versions.HTTP.t
  -> ?headers:Piaf.Headers.t
  -> ?body:Body.t
  -> ?context:Context.t
  -> Piaf.Status.t
  -> t

val with_ :
   ?status:Piaf.Status.t
  -> ?headers:Piaf.Headers.t
  -> ?version:Piaf.Versions.HTTP.t
  -> ?context:Context.t
  -> ?body:Body.t
  -> t
  -> t

val of_string :
   ?version:Piaf.Versions.HTTP.t
  -> ?headers:Piaf.Headers.t
  -> ?context:Context.t
  -> body:string
  -> Piaf.Status.t
  -> t

val of_bigstring :
   ?version:Piaf.Versions.HTTP.t
  -> ?headers:Piaf.Headers.t
  -> ?context:Context.t
  -> body:Bigstringaf.t
  -> Piaf.Status.t
  -> t

val of_string_stream :
   ?version:Piaf.Versions.HTTP.t
  -> ?headers:Piaf.Headers.t
  -> ?context:Context.t
  -> body:string Piaf.Stream.t
  -> Piaf.Status.t
  -> t

val of_stream :
   ?version:Piaf.Versions.HTTP.t
  -> ?headers:Piaf.Headers.t
  -> ?context:Context.t
  -> body:Bigstringaf.t Piaf.IOVec.t Piaf.Stream.t
  -> Piaf.Status.t
  -> t

val sendfile :
   ?version:Piaf.Versions.HTTP.t
  -> ?headers:Piaf.Headers.t
  -> ?context:Context.t
  -> string
  -> (t, [> Piaf.Error.common ]) result

val copy_file :
   ?version:Piaf.Versions.HTTP.t
  -> ?headers:Piaf.Headers.t
  -> ?context:Context.t
  -> string
  -> (t, [> Piaf.Error.common ]) result

module Upgrade : sig
  val generic :
     ?version:Piaf.Versions.HTTP.t
    -> ?headers:Piaf.Headers.t
    -> ?context:Context.t
    -> (sw:Eio.Switch.t -> (Gluten.impl -> unit) -> unit)
    -> t

  val websocket :
     f:(Piaf.Ws.Descriptor.t -> unit)
    -> ?headers:Piaf.Headers.t
    -> ?context:Context.t
    -> Request.t
    -> (t, [> Piaf.Error.common ]) result
end

val or_internal_error : (t, Piaf.Error.t) result -> Piaf.Response.t
val persistent_connection : t -> bool
val pp_hum : Format.formatter -> t -> unit
