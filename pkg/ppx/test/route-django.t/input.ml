let get_user ~id =
  Tapak.Response.of_string' (Format.sprintf "User ID: %Ld" id)
[@@route GET, "/users/<int64:id>"]

let get_post ~slug =
  Tapak.Response.of_string' (Format.sprintf "Post: %s" slug)
[@@route GET, "/posts/<slug:slug>"]

let toggle ~enabled =
  Tapak.Response.of_string' (Format.sprintf "Enabled: %b" enabled)
[@@route GET, "/toggle/<bool:enabled>"]

let get_article ~id =
  Tapak.Response.of_string' (Format.sprintf "Article: %s" id)
[@@route GET, "/articles/<uuid:id>"]
