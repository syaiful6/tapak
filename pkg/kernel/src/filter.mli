type ('req, 'res, 'req2, 'res2) t =
  ('req, 'res) Service.t -> ('req2, 'res2) Service.t
(** The filter type represents a service transformation.

    - ['req] is the request type the inner service accepts
    - ['res] is the response type the inner service produces
    - ['req2] is the request type the outer service accepts
    - ['res2] is the response type the outer service produces *)

type ('req, 'res) simple = ('req, 'res, 'req, 'res) t
(** A simple filter that doesn't change request/response types.
    Most filters are simple filters. *)

val ( >>> ) :
   ('q1, 'p1, 'q2, 'p2) t
  -> ('q2, 'p2, 'q3, 'p3) t
  -> ('q1, 'p1, 'q3, 'p3) t
(** [f1 >>> f2] composes two filters sequentially.

    The request flows through [f2] first, then [f1]. The response flows
    back through [f1], then [f2]. This matches the intuitive left-to-right
    reading order.

    Example:
    {[
      let combined =
        strip_prefix ~prefix:"/api"
        >>> logging_filter
        >>> auth_filter

      (* Request flow: strip_prefix -> logging -> auth -> service *)
      (* Response flow: service -> auth -> logging -> strip_prefix *)
    ]}
*)

val apply_all :
   ('req, 'res) simple list
  -> ('req, 'res) Service.t
  -> ('req, 'res) Service.t
(** [apply_all filters service] applies a list of filters to a service.

    Filters are applied in order from first to last, meaning the first filter
    in the list is the outermost filter (processes requests first).

    Example:
    {[
      let protected =
        Filter.apply_all
          [ strip_prefix ~prefix:"/api"
          ; logging_filter
          ; auth_filter
          ]
          my_service
    ]}
*)

val strip_prefix : prefix:string -> (Request.t, Response.t) simple
(** [strip_prefix ~prefix] creates a filter that removes a path prefix from
    incoming requests.

    This is useful for mounting services at specific paths, particularly when
    using {!Static.app} or delegating to sub-applications that expect paths
    relative to their mount point. *)
