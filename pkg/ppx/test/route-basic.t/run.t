Basic route ppx functionality
  $ ../ppx.sh input.ml
  let user_handler_path =
    Tapak.Router.(/) (Tapak.Router.s "users") Tapak.Router.int
  let user_handler ~id request =
    Tapak.Response.of_string' (Format.sprintf "User ID: %d" id)[@@route
                                                                 (GET,
                                                                   "/users/:id")]
  let user_handler_route =
    Tapak.Router.(@->)
      (Tapak.Router.get
         (Tapak.Router.(/) (Tapak.Router.s "users") Tapak.Router.int))
      (fun id request -> user_handler ~id request)
