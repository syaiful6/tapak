type location =
  [ `Header
  | `Query
  | `Cookie
  ]

let find ~(in_ : location) name req =
  let value =
    match in_ with
    | `Header -> Request.header name req
    | `Query -> Form.Urlencoded.(get name (of_query req))
    | `Cookie ->
      List.assoc_opt name (Cookies.Cookie.parse (Request.headers req))
  in
  match value with Some "" -> None | value -> value
