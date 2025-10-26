type t = (Request.t, Response.t) Service.t

let not_found =
  let body = "<html><body><h1>404 - Not found</h1></body></html>" in
  fun _ -> Response.of_string ~body `Not_found
