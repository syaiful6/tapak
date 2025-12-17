Circular dependencies between routes
  $ ../ppx.sh input.ml
  let get_user_path =
    Tapak.Router.( / ) (Tapak.Router.s "users")
      (Tapak.Router.p "id" Tapak.Router.int64)
  
  let edit_user_path =
    Tapak.Router.( / ) (Tapak.Router.s "users")
      (Tapak.Router.( / )
         (Tapak.Router.p "id" Tapak.Router.int64)
         (Tapak.Router.s "edit"))
  
  let get_user ~id =
    let _edit_url = Tapak.Router.sprintf edit_user_path id in
    Tapak.Response.of_string' "User page"
  [@@route GET, "/users/<int64:id>"]
  
  let edit_user ~id =
    let _back_url = Tapak.Router.sprintf get_user_path id in
    Tapak.Response.of_string' "Edit user page"
  [@@route GET, "/users/<int64:id>/edit"]
  
  let get_user_route =
    Tapak.Router.get
      (Tapak.Router.( / ) (Tapak.Router.s "users")
         (Tapak.Router.p "id" Tapak.Router.int64))
    |> Tapak.Router.into (fun id -> get_user ~id)
  
  let edit_user_route =
    Tapak.Router.get
      (Tapak.Router.( / ) (Tapak.Router.s "users")
         (Tapak.Router.( / )
            (Tapak.Router.p "id" Tapak.Router.int64)
            (Tapak.Router.s "edit")))
    |> Tapak.Router.into (fun id -> edit_user ~id)
