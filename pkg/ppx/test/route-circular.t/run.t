Circular dependencies between routes
  $ ../ppx.sh input.ml
  let get_user_path =
    Tapak.Router.( / ) (Tapak.Router.s "users") Tapak.Router.int64
  
  let edit_user_path =
    Tapak.Router.( / ) (Tapak.Router.s "users")
      (Tapak.Router.( / ) Tapak.Router.int64 (Tapak.Router.s "edit"))
  
  let get_user ~id request =
    let _edit_url = Tapak.Router.sprintf edit_user_path id in
    Tapak.Response.of_string' "User page"
  [@@route GET, "/users/<int64:id>"]
  
  let edit_user ~id request =
    let _back_url = Tapak.Router.sprintf get_user_path id in
    Tapak.Response.of_string' "Edit user page"
  [@@route GET, "/users/<int64:id>/edit"]
  
  let get_user_route =
    Tapak.Router.( @-> )
      (Tapak.Router.get
         (Tapak.Router.( / ) (Tapak.Router.s "users") Tapak.Router.int64))
      (fun id request -> get_user ~id request)
  
  let edit_user_route =
    Tapak.Router.( @-> )
      (Tapak.Router.get
         (Tapak.Router.( / ) (Tapak.Router.s "users")
            (Tapak.Router.( / ) Tapak.Router.int64 (Tapak.Router.s "edit"))))
      (fun id request -> edit_user ~id request)
