Basic route ppx functionality
  $ ../ppx.sh input.ml
  let user_handler_path =
    Tapak.Router.( / ) (Tapak.Router.s "users")
      (Tapak.Router.p "id" Tapak.Router.int)
  
  let user_handler_request_path =
    Tapak.Router.( / ) (Tapak.Router.s "users")
      (Tapak.Router.p "id" Tapak.Router.int)
  
  let user_handler ~id =
    Tapak.Response.of_string' (Format.sprintf "User ID: %d" id)
  [@@route GET, "/users/:id"]
  
  let user_handler_request ~id request =
    Tapak.Response.of_string'
      (Format.sprintf "User ID: %d, Method: %s" id
         (Tapak.Request.meth request |> Piaf.Method.to_string))
  [@@route POST, "/users/:id"]
  
  let user_handler_route =
    Tapak.Router.get
      (Tapak.Router.( / ) (Tapak.Router.s "users")
         (Tapak.Router.p "id" Tapak.Router.int))
    |> Tapak.Router.into (fun id -> user_handler ~id)
  
  let user_handler_request_route =
    Tapak.Router.post
      (Tapak.Router.( / ) (Tapak.Router.s "users")
         (Tapak.Router.p "id" Tapak.Router.int))
    |> Tapak.Router.request
    |> Tapak.Router.into (fun request id -> user_handler_request ~id request)
