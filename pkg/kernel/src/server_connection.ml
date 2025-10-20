module H = Piaf.Server.Handler

let to_piaf_request_handler app ctx =
  let context = Request_info.of_piaf ctx.H.ctx in
  let request = { Request.request = ctx.H.request; ctx = context } in
  App.call app request |> Response.to_piaf
