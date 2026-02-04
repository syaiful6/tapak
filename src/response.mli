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

(** {1 Extended functions} *)

val redirect :
   ?version:Piaf.Versions.HTTP.t
  -> ?status:Piaf.Status.redirection
  -> ?headers:Piaf.Headers.t
  -> ?context:Context.t
  -> string
  -> t
(** [redirect ?version ?status ?headers ?context location] creates a redirect
    response to the given [location]. Default status is [`Found] (302). *)

val header : string -> t -> string option
(** [header key t] returns the value of header [key] in response [t]. *)

val multi_header : string -> t -> string list
(** [multi_header key t] returns all values for header [key] in response [t]. *)

val add_header : string * string -> t -> t
(** [add_header (key, value) t] adds a header to the response. *)

val add_header_or_replace : string * string -> t -> t
(** [add_header_or_replace (key, value) t] adds a header or replaces it if
    it already exists. *)

val add_header_unless_exists : string * string -> t -> t
(** [add_header_unless_exists (key, value) t] adds a header only if it
    doesn't already exist. *)

val add_headers : (string * string) list -> t -> t
(** [add_headers headers t] adds multiple headers to the response. *)

val remove_header : string -> t -> t
(** [remove_header key t] removes the header [key] from the response. *)

val add_to_list_header : string * string -> t -> t
(** [add_to_list_header (key, value) t] appends [value] to an existing
    comma-separated header value, or creates the header if it doesn't exist. *)

val of_string' :
   ?content_type:string
  -> ?version:Piaf.Versions.HTTP.t
  -> ?status:Piaf.Status.t
  -> ?headers:Headers.t
  -> ?context:Context.t
  -> string
  -> t
(** [of_string' ?content_type ?version ?status ?headers ?context body] creates
    a response with the given string body and optional content type (defaults
    to "text/plain"). *)

val of_html :
   ?version:Piaf.Versions.HTTP.t
  -> ?status:Piaf.Status.t
  -> ?headers:Headers.t
  -> ?context:Context.t
  -> string
  -> t
(** [of_html ?version ?status ?headers ?context body] creates an HTML response
    with Content-Type "text/html; charset=utf-8". *)

val of_json :
   ?version:Piaf.Versions.HTTP.t
  -> ?status:Piaf.Status.t
  -> ?headers:Headers.t
  -> ?context:Context.t
  -> Yojson.Safe.t
  -> t
(** [of_json ?version ?status ?headers ?context json] creates a JSON response
    with Content-Type "application/json; charset=utf-8". *)

val negotiate :
   ?version:Piaf.Versions.HTTP.t
  -> ?status:Piaf.Status.t
  -> ?headers:Headers.t
  -> ?context:Context.t
  -> ?available_formats:[ `Json | `Html | `Xml | `Text | `Other of string ] list
  -> Request.t
  -> ([ `Json | `Html | `Xml | `Text | `Other of string ]
      -> (string * string) option)
  -> t
(** [negotiate ?version ?status ?headers ?context ?available_formats request render]
    performs content negotiation based on the request's Accept header.
    The [render] function should return [Some (content_type, body)] for
    supported formats, or [None] if the format cannot be rendered.
    Returns a 406 Not Acceptable response if no suitable format is found. *)
