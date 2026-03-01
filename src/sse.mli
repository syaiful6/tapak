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

val stream :
   ?version:Http.Version.t
  -> ?headers:Headers.t
  -> ((Event.t -> unit) -> (unit -> unit) -> unit)
  -> Response.t
(** [stream ?version ?headers f]Stream a sequence of events. The first argument is a function that takes two
    arguments: a function to send an event, and a function to flush it to client *)
