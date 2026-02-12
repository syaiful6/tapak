type user =
  { id : int
  ; name : string
  ; email : string
  }

let user_to_json { id; name; email } =
  `Assoc [ "id", `Int id; "name", `String name; "email", `String email ]

let user_schema =
  let open Tapak.Schema.Syntax in
  let+ name =
    Tapak.Schema.(
      str
        ~constraint_:
          (Constraint.all_of
             [ Constraint.min_length 1; Constraint.max_length 125 ])
        "name")
  and+ email =
    Tapak.Schema.(str ~constraint_:(Constraint.format `Email) "email")
  in
  name, email

let user_response_schema =
  let open Tapak.Schema.Syntax in
  let+ id = Tapak.Schema.int "id"
  and+ name =
    Tapak.Schema.(
      str
        ~constraint_:
          (Constraint.all_of
             [ Constraint.min_length 1; Constraint.max_length 125 ])
        "name")
  and+ email =
    Tapak.Schema.(str ~constraint_:(Constraint.format `Email) "email")
  in
  { id; name; email }

let user_list_response_schema = Tapak.Schema.(list "users" user_response_schema)

(* list of user as direct array, not wrapped in an object *)
let user_list_direct_schema = Tapak.Schema.Field.list user_response_schema
let user_create_list_direct_schema = Tapak.Schema.Field.list user_schema

type delete_response = { message : string }

let delete_response_schema =
  let open Tapak.Schema.Syntax in
  let+ message = Tapak.Schema.str "message" in
  { message }

type profile_response =
  { session_id : string
  ; theme : string
  ; message : string
  }

let profile_response_schema =
  let open Tapak.Schema.Syntax in
  let+ session_id = Tapak.Schema.str "session_id"
  and+ theme = Tapak.Schema.str "theme"
  and+ message = Tapak.Schema.str "message" in
  { session_id; theme; message }

type search =
  { q : string option
  ; limit : int
  ; offset : int
  }

let search_schema =
  let open Tapak.Schema in
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

let session_cookies_schema =
  let open Tapak.Schema.Syntax in
  let+ session_id =
    Tapak.Schema.(str ~constraint_:(Constraint.min_length 16) "session_id")
  and+ user_pref = Tapak.Schema.(option "theme" (Field.str ())) in
  session_id, user_pref

let list_take n l =
  let[@tail_mod_cons] rec aux n l =
    match n, l with 0, _ | _, [] -> [] | n, x :: l -> x :: aux (n - 1) l
  in
  if n <= 0 then [] else aux n l

let list_drop n l =
  let rec aux i = function
    | _x :: l when i < n -> aux (i + 1) l
    | rest -> rest
  in
  if n <= 0 then l else aux 0 l

let list_users search =
  let users =
    [ { id = 1; name = "Alice"; email = "alice@example.com" }
    ; { id = 2; name = "Bob"; email = "bob@example.com" }
    ]
  in
  if search.offset >= List.length users
  then []
  else
    users
    |> list_drop search.offset
    |> list_take search.limit
    |>
    match search.q with
    | None -> Fun.id
    | Some query ->
      (* the search use starting with for simplicity *)
      List.filter (fun user ->
        String.starts_with ~prefix:query user.name
        || String.starts_with ~prefix:query user.email)

let get_user id = { id; name = "Alice"; email = "alice@example.com" }
let create_user (name, email) = { id = 2; name; email }

let bulk_create_users users =
  List.mapi (fun i (name, email) -> { id = i + 100; name; email }) users

let update_user (name, email) id = { id; name; email }
let delete_user id = { message = Format.sprintf "User %d deleted" id }

let get_user_profile (session_id, theme) =
  let theme_value = Option.value theme ~default:"light" in
  { session_id
  ; theme = theme_value
  ; message = "User profile retrieved successfully"
  }

let v1_api_routes =
  Tapak.Router.
    [ get (s "users")
      |> query search_schema
      |> summary "List all users"
      |> operation_id "listUsers"
      |> tags [ "Users" ]
      |> response_model
           ~status:`OK
           ~schema:user_list_response_schema
           ~encoder:(fun users ->
             `Assoc [ "users", `List (List.map user_to_json users) ])
      |> into list_users
    ; get (s "users" / p "userId" int)
      |> summary "Get a user by ID"
      |> operation_id "getUser"
      |> tags [ "Users" ]
      |> response_model
           ~status:`OK
           ~schema:user_response_schema
           ~encoder:user_to_json
      |> into get_user
    ; post (s "users")
      |> body Tapak.Schema.Json user_schema
      |> summary "Create a new user"
      |> description "Requires authentication via X-API-Key header"
      |> operation_id "createUser"
      |> tags [ "Users" ]
      |> response_model
           ~status:`Created
           ~schema:user_response_schema
           ~encoder:user_to_json
      |> into create_user
    ; post (s "users" / s "bulk")
      |> body Tapak.Schema.Json user_create_list_direct_schema
      |> summary "Bulk create users"
      |> operation_id "bulkCreateUsers"
      |> tags [ "Users" ]
      |> response_model
           ~status:`Created
           ~schema:user_list_direct_schema
           ~encoder:(fun users -> `List (List.map user_to_json users))
      |> into bulk_create_users
    ; put (s "users" / p "userId" int)
      |> body Tapak.Schema.Json user_schema
      |> summary "Update a user"
      |> description "Requires authentication via X-API-Key header"
      |> operation_id "updateUser"
      |> tags [ "Users" ]
      |> response_model
           ~status:`OK
           ~schema:user_response_schema
           ~encoder:user_to_json
      |> into update_user
    ; delete (s "users" / p "userId" int)
      |> summary "Delete a user"
      |> description "Requires authentication via X-API-Key header"
      |> operation_id "deleteUser"
      |> tags [ "Users" ]
      |> response_model
           ~status:`No_content
           ~schema:delete_response_schema
           ~encoder:(fun { message } -> `Assoc [ "message", `String message ])
      |> into delete_user
    ; get (s "profile")
      |> cookie session_cookies_schema
      |> summary "Get user profile"
      |> description "Returns user profile information from session cookies"
      |> operation_id "getUserProfile"
      |> tags [ "Users" ]
      |> response_model
           ~status:`OK
           ~schema:profile_response_schema
           ~encoder:(fun { session_id; theme; message } ->
             `Assoc
               [ "session_id", `String session_id
               ; "theme", `String theme
               ; "message", `String message
               ])
      |> into get_user_profile
    ]

let api_v1_routes = Tapak.Router.[ scope (s "v1") v1_api_routes ]

let openapi_schema ?base_path routes =
  Tapak.json
    ~status:`OK
    (Tapak.openapi
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
  Tapak.html ~status:`OK html

let error_handler next request =
  try next request with
  | Tapak.Router.Validation_failed errors ->
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
    Tapak.json ~status:`Bad_request body
  | Tapak.Router.Bad_request msg ->
    let body = `Assoc [ "error", `String msg ] in
    Tapak.json ~status:`Bad_request body

let app env =
  let clock = Eio.Stdenv.clock env in
  let now () = Eio.Time.now clock in

  Tapak.(
    Router.(
      routes
        [ scope (s "api") api_v1_routes
        ; get (s "docs") |> unit |> into swagger_ui_handler
        ; get (s "openapi.json")
          |> into (openapi_schema ~base_path:"/api" api_v1_routes)
        ])
    |> pipe
         ~through:
           [ use
               (module Middleware.Request_logger)
               (Middleware.Request_logger.args ~now ~trusted_proxies:[] ())
           ; error_handler
           ; Middleware.head
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
  ignore (Tapak.run_with ~config ~env (app env))
