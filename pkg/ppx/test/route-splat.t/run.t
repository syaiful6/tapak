Splat syntax is "**" and passed as ~splat argument
  $ ../ppx.sh input.ml
  let static_handler_path =
    Tapak.Router.( / ) (Tapak.Router.s "static") Tapak.Router.splat
  
  let static_handler ~splat request =
    Tapak.Response.of_text (String.concat "/" splat)
  [@@route GET, "/static/**"]
  
  let static_handler_route =
    Tapak.Router.get
      (Tapak.Router.( / ) (Tapak.Router.s "static") Tapak.Router.splat)
    |> Tapak.Router.into (fun splat request -> static_handler ~splat request)
