open Tapak

type user =
  { id : int
  ; name : string
  ; email : string
  }

let user_to_json { id; name; email } =
  `Assoc [ "id", `Int id; "name", `String name; "email", `String email ]

let list_users _req =
  Response.of_json
    ~status:`OK
    (`List
        (List.map
           user_to_json
           [ { id = 1; name = "Alice"; email = "alice@example.com" } ]))

let get_user id _req =
  Response.of_json
    ~status:`OK
    (user_to_json { id; name = "Alice"; email = "alice@example.com" })

let create_user (name, email) _req =
  Response.of_json ~status:`Created (user_to_json { id = 2; name; email })

let update_user (name, email) id _req =
  Response.of_json ~status:`OK (user_to_json { id; name; email })

let delete_user id _req =
  Response.of_string ~body:(Format.sprintf "User %d deleted" id) `No_content

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

let v1_api_routes =
  let open Router in
  [ get (s "users")
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
    |> body Schema.Json user_schema
    |> summary "Create a new user"
    |> operation_id "createUser"
    |> tags [ "Users" ]
    |> into create_user
  ; put (s "users" / p "userId" int)
    |> body Schema.Json user_schema
    |> summary "Update a user"
    |> operation_id "updateUser"
    |> tags [ "Users" ]
    |> into update_user
  ; delete (s "users" / p "userId" int)
    |> summary "Delete a user"
    |> operation_id "deleteUser"
    |> tags [ "Users" ]
    |> into delete_user
  ]

let api_v1_routes =
  let open Router in
  [ scope (s "v1") v1_api_routes ]

let openapi_schema ?base_path routes _ =
  Response.of_json
    ~status:`OK
    (Openapi.generate
       ~title:"User API"
       ~version:"1.0.0"
       ~description:"A simple user management API"
       ~base_path:(Option.value ~default:"" base_path)
       routes)

let swagger_ui_handler _ =
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

let app env =
  let open Router in
  let open Middleware in
  let clock = Eio.Stdenv.clock env in
  let now () = Eio.Time.now clock in

  App.(
    routes
      [ scope (s "api") api_v1_routes
      ; get (s "docs") |> into swagger_ui_handler
      ; get (s "openapi.json")
        |> into (openapi_schema ~base_path:"/api" api_v1_routes)
      ]
      ()
    <++> [ head
         ; use
             (module Request_logger)
             (Request_logger.args ~now ~trusted_proxies:[] ())
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
