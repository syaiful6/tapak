type t =
  { version : Http.Version.t
  ; status : Http.Status.t
  ; headers : Http.Header.t
  ; body : Body.t
  }

include Headers.S with type t := t

val make :
   ?version:Http.Version.t
  -> ?headers:Http.Header.t
  -> ?status:Http.Status.t
  -> Body.t
  -> t

val pp_field :
   string
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a
  -> unit

val pp : Format.formatter -> t -> unit
val status : t -> Http.Status.t
val version : t -> Http.Version.t
val body : t -> Body.t

val with_ :
   ?status:Http.Status.t
  -> ?headers:Http.Header.t
  -> ?version:Http.Version.t
  -> ?body:Body.t
  -> t
  -> t

val of_string :
   ?version:Http.Version.t
  -> ?headers:Http.Header.t
  -> ?status:Http.Status.t
  -> string
  -> t

val stream :
   ?version:Http.Version.t
  -> ?headers:Http.Header.t
  -> ?status:Http.Status.t
  -> ?length:int64
  -> ((string -> unit) -> (unit -> unit) -> unit)
  -> t

val plain :
   ?version:Http.Version.t
  -> ?headers:Http.Header.t
  -> ?status:Http.Status.t
  -> string
  -> t

val html :
   ?version:Http.Version.t
  -> ?headers:Http.Header.t
  -> ?status:Http.Status.t
  -> string
  -> t

val json :
   ?version:Http.Version.t
  -> ?headers:Http.Header.t
  -> ?status:Http.Status.t
  -> string
  -> t

val redirect :
   ?version:Http.Version.t
  -> ?headers:Headers.t
  -> ?status:Http.Status.redirection
  -> string
  -> t

val negotiate :
   ?version:Http.Version.t
  -> ?status:Http.Status.t
  -> ?headers:Http.Header.t
  -> ?formats:[ `Json | `Html | `Xml | `Text | `Other of string ] list
  -> Request.t
  -> ([ `Json | `Html | `Xml | `Text | `Other of string ]
      -> (string * string) option)
  -> t

val websocket :
   ?size_limit:Cows.Frame.Size_limit.t
  -> (Cows.conn -> unit)
  -> Request.t
  -> t

val file :
   ?version:Http.Version.t
  -> ?headers:Headers.t
  -> ?status:Http.Status.t
  -> ?follow:bool
  -> _ Eio.Path.t
  -> t
