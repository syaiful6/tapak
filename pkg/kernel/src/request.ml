type t =
  { request : Piaf.Request.t
  ; ctx : Request_info.t
  }

let meth { request; _ } = Piaf.Request.meth request
let target { request; _ } = Piaf.Request.target request
let version { request; _ } = Piaf.Request.version request
let headers { request; _ } = Piaf.Request.headers request
let scheme { request; _ } = Piaf.Request.scheme request
let body { request; _ } = Piaf.Request.body request
let uri { request; _ } = Piaf.Request.uri request
let info { ctx; _ } = ctx
let context t = t |> info |> fun ctx -> ctx.env

let create
      ~scheme
      ~version
      ?(headers = Piaf.Headers.empty)
      ~meth
      ~body
      ?(ctx = Request_info.default)
      target
  =
  let request =
    Piaf.Request.create ~scheme ~version ~headers ~meth ~body target
  in
  { request; ctx }

let with_ ?meth ?target ?version ?headers ?body ?ctx request =
  { request =
      Piaf.Request.with_ ?meth ?target ?version ?headers ?body request.request
  ; ctx = Option.value ctx ~default:request.ctx
  }

let with_context env request =
  let ctx = Request_info.with_ ~env request.ctx in
  { request with ctx }

let pp_hum fmt t = Piaf.Request.pp_hum fmt t.request
