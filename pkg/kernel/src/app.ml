type t =
  { middlewares : Middleware.t list
  ; handler : (Request.t, Response.t) Service.t
  }

let ( <+> ) t m = { t with middlewares = t.middlewares @ [ m ] }
let ( <++> ) t ms = { t with middlewares = t.middlewares @ ms }
let create ?(middlewares = []) ~handler () = { middlewares; handler }

let call t request =
  let service = Filter.apply_all t.middlewares t.handler in
  service request

let use = Middleware.use

let to_piaf app ctx =
  let module H = Piaf.Server.Handler in
  let context = Request_info.of_piaf ctx.H.ctx in
  let request = { Request.request = ctx.H.request; ctx = context } in
  call app request |> Response.to_piaf
