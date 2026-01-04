type t = (Request.t, Response.t) Service.t

let not_found =
  let body = "<html><body><h1>404 - Not found</h1></body></html>" in
  fun _ -> Response.of_string ~body `Not_found

let to_piaf handler ctx =
  let module H = Piaf.Server.Handler in
  let context = Request_info.of_piaf ctx.H.ctx in
  let request = { Request.request = ctx.H.request; ctx = context } in
  handler request |> Response.to_piaf
