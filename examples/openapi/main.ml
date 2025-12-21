open Tapak

type user =
  { id : int
  ; name : string
  ; email : string
  }

let user_to_json { id; name; email } =
  `Assoc [ "id", `Int id; "name", `String name; "email", `String email ]

let user_schema =
  let open Schema.Syntax in
  let+ name =
    Schema.(
      str
        ~constraint_:
          (Constraint.all_of
             [ Constraint.min_length 1; Constraint.max_length 125 ])
        "name")
  and+ email = Schema.(str ~constraint_:(Constraint.format `Email) "email") in
  name, email

type search =
  { q : string option
  ; limit : int
  ; offset : int
  }

let search_schema =
  let open Schema in
  let open Syntax in
  let+ q = option "q" (Field.str ())
  and+ limit =
    int
      ~default:10
      ~constraint_:
        (Constraint.all_of
           [ Constraint.int_range 5 100; Constraint.int_multiple_of 5 ])
      "limit"
  and+ offset =
    int ~default:0 ~constraint_:(Constraint.int_range 0 1000) "offset"
  in
  { q; limit; offset }

let auth_headers_schema =
  let open Schema.Syntax in
  let+ api_key =
    Schema.(str ~constraint_:(Constraint.min_length 32) "X-API-Key")
  and+ request_id = Schema.(option "X-Request-ID" (Field.str ())) in
  api_key, request_id

let session_cookies_schema =
  let open Schema.Syntax in
  let+ session_id =
    Schema.(str ~constraint_:(Constraint.min_length 16) "session_id")
  and+ user_pref = Schema.(option "theme" (Field.str ())) in
  session_id, user_pref

let list_users search =
  let users =
    [ { id = 1; name = "Alice"; email = "alice@example.com" }
    ; { id = 2; name = "Bob"; email = "bob@example.com" }
    ]
  in
  if search.offset >= List.length users
  then Response.of_json ~status:`OK (`List [])
  else
    let paginated_users =
      users
      |> List.drop search.offset
      |> List.take search.limit
      |>
      match search.q with
      | None -> Fun.id
      | Some query ->
        (* the search use starting with for simplicity *)
        List.filter (fun user ->
          String.starts_with ~prefix:query user.name
          || String.starts_with ~prefix:query user.email)
    in
    Response.of_json ~status:`OK (`List (List.map user_to_json paginated_users))

let get_user id =
  Response.of_json
    ~status:`OK
    (user_to_json { id; name = "Alice"; email = "alice@example.com" })

let create_user (name, email) (_api_key, _request_id) =
  Response.of_json ~status:`Created (user_to_json { id = 2; name; email })

let update_user (name, email) (_api_key, _request_id) id =
  Response.of_json ~status:`OK (user_to_json { id; name; email })

let delete_user (_api_key, _request_id) id =
  Response.of_string ~body:(Format.sprintf "User %d deleted" id) `No_content

let get_user_profile (session_id, theme) =
  let theme_value = Option.value theme ~default:"light" in
  Response.of_json
    ~status:`OK
    (`Assoc
        [ "session_id", `String session_id
        ; "theme", `String theme_value
        ; "message", `String "User profile retrieved successfully"
        ])

let v1_api_routes =
  let open Router in
  [ get (s "users")
    |> query search_schema
    |> summary "List all users"
    |> operation_id "listUsers"
    |> tags [ "Users" ]
    |> into list_users
  ; get (s "users" / p "userId" int)
    |> summary "Get a user by ID"
    |> operation_id "getUser"
    |> tags [ "Users" ]
    |> into get_user
  ; post (s "users")
    |> header auth_headers_schema
    |> body Schema.Json user_schema
    |> summary "Create a new user"
    |> description "Requires authentication via X-API-Key header"
    |> operation_id "createUser"
    |> tags [ "Users" ]
    |> into create_user
  ; put (s "users" / p "userId" int)
    |> header auth_headers_schema
    |> body Schema.Json user_schema
    |> summary "Update a user"
    |> description "Requires authentication via X-API-Key header"
    |> operation_id "updateUser"
    |> tags [ "Users" ]
    |> into update_user
  ; delete (s "users" / p "userId" int)
    |> header auth_headers_schema
    |> summary "Delete a user"
    |> description "Requires authentication via X-API-Key header"
    |> operation_id "deleteUser"
    |> tags [ "Users" ]
    |> into delete_user
  ; get (s "profile")
    |> cookie session_cookies_schema
    |> summary "Get user profile"
    |> description "Returns user profile information from session cookies"
    |> operation_id "getUserProfile"
    |> tags [ "Users" ]
    |> into get_user_profile
  ]

let api_v1_routes =
  let open Router in
  [ scope (s "v1") v1_api_routes ]

let openapi_schema ?base_path routes =
  Response.of_json
    ~status:`OK
    (Openapi.generate
       ~title:"User API"
       ~version:"1.0.0"
       ~description:"A simple user management API"
       ?base_path
       routes)

let swagger_ui_handler () =
  let html =
    {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>User API - Swagger UI</title>
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui.css" />
  <style>
    .swagger-ui .topbar {
      display: none;
    }
  </style>
</head>
<body>
  <div id="swagger-ui"></div>
  <script src="https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui-bundle.js"></script>
  <script src="https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui-standalone-preset.js"></script>
  <script>
    window.onload = function() {
      window.ui = SwaggerUIBundle({
        url: '/openapi.json',
        dom_id: '#swagger-ui',
        deepLinking: true,
        presets: [
          SwaggerUIBundle.presets.apis,
          SwaggerUIStandalonePreset
        ],
        plugins: [
          SwaggerUIBundle.plugins.DownloadUrl
        ],
        layout: "StandaloneLayout"
      });
    };
  </script>
</body>
</html>|}
  in
  Response.of_html ~status:`OK html

let error_handler next request =
  try next request with
  | Router.Validation_failed errors ->
    let body =
      `Assoc
        [ ( "errors"
          , `List
              (List.map
                 (fun (field, msg) ->
                    `Assoc [ "field", `String field; "message", `String msg ])
                 errors) )
        ]
    in
    Response.of_json ~status:`Bad_request body
  | Router.Bad_request msg ->
    let body = `Assoc [ "error", `String msg ] in
    Response.of_json ~status:`Bad_request body

let app env =
  let open Router in
  let open Middleware in
  let clock = Eio.Stdenv.clock env in
  let now () = Eio.Time.now clock in

  App.(
    routes
      [ scope (s "api") api_v1_routes
      ; get (s "docs") |> unit |> into swagger_ui_handler
      ; get (s "openapi.json")
        |> into (openapi_schema ~base_path:"/api" api_v1_routes)
      ]
      ()
    <++> [ head
         ; use
             (module Request_logger)
             (Request_logger.args ~now ~trusted_proxies:[] ())
         ; error_handler
         ])

let () =
  Logs_threaded.enable ();
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  Eio_main.run @@ fun env ->
  let address = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let config = Piaf.Server.Config.create address in
  Logs.info (fun m -> m "Open Api Example");
  Logs.info (fun m -> m "Listening on http://localhost:8080");
  ignore (Server.run_with ~config ~env (app env))
