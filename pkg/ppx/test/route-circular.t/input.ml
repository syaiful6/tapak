let get_user ~id request =
  let _edit_url = Tapak.Router.sprintf edit_user_path id in
  Tapak.Response.of_string' "User page"
[@@route GET, "/users/<int64:id>"]

let edit_user ~id request =
  let _back_url = Tapak.Router.sprintf get_user_path id in
  Tapak.Response.of_string' "Edit user page"
[@@route GET, "/users/<int64:id>/edit"]
