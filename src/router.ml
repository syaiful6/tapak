include Tapak_kernel.Router

(** [route ~not_found routes] creates a handler that dispatches to the given routes.
    If no route matches, the [not_found] handler is called instead of raising an exception. *)
let route
      ?(not_found =
        fun _req -> Response.of_string' ~status:`Not_found "Not Found")
      routes
  =
 fun req -> try router routes req with Not_found -> not_found req
