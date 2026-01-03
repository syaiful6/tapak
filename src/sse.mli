(** Server-Sent Events (SSE) support.

    The primary API is {!stream}, which takes an [Event.t Piaf.Stream.t] and returns
    a [Response.t] configured for Server-Sent Events with proper headers.

    {b: Basic usage: }

    {[
      open Tapak

      let sse_events () =
        Piaf.Stream.of_list [
          Event.{ id = Some "1"; data = Some (`Text "Hello"); event = None; comment = None; retry = None };
          Event.{ id = Some "2"; data = Some (`Json (`Assoc [("message", `String "World")])); event = Some "greeting"; comment = None; retry = None };
        ]

      let app =
        let open Router in
        App.(
          routes [ get (s "sse") @-> (fun _req -> Sse.stream (sse_events ())) ]
        )
    ]}

    For long-lived connections, use {!keep_alive} to wrap your event stream and
    automatically send keep-alive comments during periods of inactivity.

    See examples/sse-chat for a complete chat application using SSE. See also the {{:https://developer.mozilla.org/en-US/docs/Web/API/EventSource} {b EventSource}}
*)

module Event : sig
  type data =
    [ `Json of Yojson.Safe.t
    | `Text of string
    ]

  type t =
    { id : string option
    ; data : data option
    ; event : string option
    ; comment : string option
    ; retry : int option
    }

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
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
