type t =
  { middlewares : Middleware.t list
  ; handler : (Request.t, Response.t) Service.t
  }

let ( <+> ) t m = { t with middlewares = t.middlewares @ [ m ] }
let ( <++> ) t ms = { t with middlewares = t.middlewares @ ms }
let create ?(middlewares = []) ~handler () = { middlewares; handler }

let call t request =
  let service = Middleware.apply_all t.middlewares t.handler in
  service request
