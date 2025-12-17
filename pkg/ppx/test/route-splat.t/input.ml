let static_handler ~splat = Tapak.Response.of_text (String.concat "/" splat)
[@@route GET, "/static/**"]
