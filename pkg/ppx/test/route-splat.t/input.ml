let static_handler ~splat request =
  Tapak.Response.of_text (String.concat "/" splat)
[@@route GET, "/static/**"]
