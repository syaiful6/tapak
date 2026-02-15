module Event : sig
  type t

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string

  val text :
     ?id:string
    -> ?event:string
    -> ?comment:string
    -> ?retry:int
    -> string
    -> t

  val json :
     ?id:string
    -> ?event:string
    -> ?comment:string
    -> ?retry:int
    -> 'a Jsont.t
    -> 'a
    -> t

  val comment : string -> t
end

val keep_alive :
   sw:Eio.Switch.t
  -> clock:[> float Eio.Time.clock_ty ] Eio.Time.clock
  -> ?interval:float
  -> ?comment:string
  -> Event.t Piaf.Stream.t
  -> Event.t Piaf.Stream.t
(** Keeps event source connection alive when no events sent over a some time.

    Some proxy servers may drop HTTP connection after a some timeout of inactivity.
    This function wrap original stream, so it send comment events every interval
    of inactivity. *)

val stream :
   ?version:Piaf.Versions.HTTP.t
  -> ?headers:Headers.t
  -> ?context:Context.t
  -> Event.t Piaf.Stream.t
  -> Response.t
(** Convert an event stream into an SSE response.

    This is the main function for Server-Sent Events. It takes a stream of {!Event.t}
    and returns a {!Response.t} with appropriate SSE headers. *)
