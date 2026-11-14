type t

include Headers.S with type t := t

val meth : t -> Http.Method.t
val target : t -> string
val version : t -> Http.Version.t
val body : t -> Bytesrw.Bytes.Reader.t
val is_secure : ?trusted_proxies:Ipaddr.Prefix.t list -> t -> bool
val client_ip : ?trusted_proxies:Ipaddr.Prefix.t list -> t -> string
val is_keep_alive : t -> bool

val make :
   ?version:Http.Version.t
  -> ?body:Bytesrw.Bytes.Reader.t
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
  -> ?body:Bytesrw.Bytes.Reader.t
  -> ?client_addr:string option
  -> ?is_secure:bool
  -> t
  -> t

val pp : Format.formatter -> t -> unit

(** {1 Exchange lifetime}

    A handler can return before the server writes its response. In particular,
    [`Stream] and [`Raw] bodies run later. These functions report when the
    handler and response body have both finished. *)

type outcome =
  [ `Completed  (** the response, including its body, was written *)
  | `Failed of exn
    (** the handler raised, writing the body raised, or the connection was
        lost while writing it *)
  ]

val on_finished : t -> (outcome -> unit) -> unit
(** [on_finished request hook] runs [hook] once, when the server has finished
    this request's response. For a [`Raw] body, such as a WebSocket upgrade,
    that is when the [`Raw] function returns.

    The server runs hooks newest first in the connection fiber, before reading
    the next request on a keep-alive connection. If a hook raises, the server
    logs the exception and continues with the remaining hooks. Registering a
    hook after the request has finished runs it immediately. Requests created
    with {!with_} share their hooks with the original request.

    A hook must not block for long: the connection waits for it.

    [on_finished], {!finish}, and {!finished} are safe to call concurrently
    from different domains. Each hook runs exactly once: in the fiber calling
    {!finish}, or in the registering fiber if registration happens later. *)

val finished : t -> outcome option
(** [finished request] returns the exchange outcome, or [None] while it is
    running. If no server calls {!finish}, as when a request built with {!make}
    is passed directly to a handler, it remains pending and its hooks do not
    run. *)

val finish : t -> outcome -> unit
(** [finish request outcome] ends the exchange: it records [outcome] and runs
    the hooks registered with {!on_finished}, in the caller's fiber. Only the
    first call has an effect.

    {b Do not call this from a handler or middleware.} [Tapak.run] calls it for
    you. Other server integrations must call it after the complete response,
    including any [`Stream] or [`Raw] body, has been written or has failed.
    Tests that invoke handlers directly may also call it. Calling it too early
    can release resources that the body still uses. *)
