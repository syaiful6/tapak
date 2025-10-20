include module type of Tapak_kernel.Router

val routes :
   ?not_found:(Request.t -> Response.t)
  -> t list
  -> Request.t
  -> Response.t
(** [routes ~not_found routes] creates a handler that dispatches to the given routes.
    If no route matches, the [not_found] handler is called instead of raising an exception. *)
