let user_handler ~id =
  Tapak.Response.of_string' (Format.sprintf "User ID: %d" id)
[@@route GET, "/users/:id"]

let user_handler_request ~id request =
  Tapak.Response.of_string'
    (Format.sprintf
       "User ID: %d, Method: %s"
       id
       (Tapak.Request.meth request |> Piaf.Method.to_string))
[@@route POST, "/users/:id"]
