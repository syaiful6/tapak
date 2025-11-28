Basic route ppx functionality
  $ ../ppx.sh input.ml
  let user_handler_path =
    Tapak.Router.( / ) (Tapak.Router.s "users")
      (Tapak.Router.p "id" Tapak.Router.int)
  
  let user_handler ~id request =
    Tapak.Response.of_string' (Format.sprintf "User ID: %d" id)
  [@@route GET, "/users/:id"]
  
  let user_handler_route =
    Tapak.Router.get
      (Tapak.Router.( / ) (Tapak.Router.s "users")
         (Tapak.Router.p "id" Tapak.Router.int))
    |> Tapak.Router.into (fun id request -> user_handler ~id request)
