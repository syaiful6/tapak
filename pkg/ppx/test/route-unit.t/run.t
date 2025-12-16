Unit handler ppx functionality
  $ ../ppx.sh input.ml
  let home_path = Tapak.Router.s ""
  let about_path = Tapak.Router.s "about"
  let home () = Tapak.Response.of_string' "Home" [@@route GET, "/"]
  let about () = Tapak.Response.of_string' "About" [@@route GET, "/about"]
  
  let home_route =
    Tapak.Router.get (Tapak.Router.s "")
    |> Tapak.Router.unit |> Tapak.Router.into home
  
  let about_route =
    Tapak.Router.get (Tapak.Router.s "about")
    |> Tapak.Router.unit |> Tapak.Router.into about
