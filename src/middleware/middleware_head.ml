(** HEAD request auto-handler middleware.

    This middleware automatically handles HEAD requests by converting them
    to GET requests, processing them through the service, then removing the
    response body. This follows the HTTP specification and Finagle's approach.

    According to RFC 7231, a HEAD request is identical to GET except the
    server MUST NOT send a message body. The server SHOULD send the same
    headers as it would for a GET request. *)

let m next request =
  if Request.meth request = `HEAD
  then
    let get_request = Request.with_ ~meth:`GET request in
    let response = next get_request in
    Response.with_ ~body:Body.empty response
  else next request
