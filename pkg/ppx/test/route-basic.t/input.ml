let user_handler ~id request =
  Tapak.Response.of_string' (Format.sprintf "User ID: %d" id)
[@@route GET, "/users/:id"]
