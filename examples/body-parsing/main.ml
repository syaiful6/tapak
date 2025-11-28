open Tapak

let create_user json_body _req =
  let open Yojson.Safe.Util in
  try
    let name = json_body |> member "name" |> to_string in
    let email = json_body |> member "email" |> to_string in
    let _age = json_body |> member "age" |> to_int_option in

    let response_json =
      `Assoc
        [ "status", `String "created"
        ; "user", `Assoc [ "name", `String name; "email", `String email ]
        ]
    in

    Response.of_json ~status:`Created response_json
  with
  | Type_error (msg, _) ->
    let error = `Assoc [ "error", `String ("Invalid JSON: " ^ msg) ] in
    Response.of_json ~status:`Bad_request error
  | _ ->
    let error = `Assoc [ "error", `String "Failed to parse JSON" ] in
    Response.of_json ~status:`Bad_request error

let update_user json_body id _req =
  let open Yojson.Safe.Util in
  try
    let name = json_body |> member "name" |> to_string_option in
    let email = json_body |> member "email" |> to_string_option in

    let updates = [] in
    let updates =
      match name with
      | Some n -> ("name", `String n) :: updates
      | None -> updates
    in
    let updates =
      match email with
      | Some e -> ("email", `String e) :: updates
      | None -> updates
    in

    let response_json =
      `Assoc
        [ "status", `String "updated"
        ; "id", `Int (Int64.to_int id)
        ; "updates", `Assoc updates
        ]
    in

    Response.of_json ~status:`OK response_json
  with
  | _ ->
    let error = `Assoc [ "error", `String "Failed to update user" ] in
    Response.of_json ~status:`Bad_request error

let login_form form_data _req =
  match
    ( Form.Urlencoded.get "username" form_data
    , Form.Urlencoded.get "password" form_data )
  with
  | Some username, Some password ->
    (* In a real app, you'd verify credentials here *)
    if username <> "" && password <> ""
    then
      let response_json =
        `Assoc
          [ "status", `String "authenticated"
          ; "username", `String username
          ; "message", `String "Login successful"
          ]
      in
      Response.of_json ~status:`OK response_json
    else
      let error =
        `Assoc [ "error", `String "Username and password cannot be empty" ]
      in
      Response.of_json ~status:`Bad_request error
  | None, _ ->
    let error = `Assoc [ "error", `String "Missing username" ] in
    Response.of_json ~status:`Bad_request error
  | _, None ->
    let error = `Assoc [ "error", `String "Missing password" ] in
    Response.of_json ~status:`Bad_request error

let contact_form form_data _req =
  let name = Form.Urlencoded.get "name" form_data in
  let email = Form.Urlencoded.get "email" form_data in
  let message = Form.Urlencoded.get "message" form_data in
  let subscribe = Form.Urlencoded.get "subscribe" form_data in

  match name, email, message with
  | Some n, Some e, Some m when n <> "" && e <> "" && m <> "" ->
    let is_subscribed =
      match subscribe with
      | Some "on" | Some "true" | Some "1" -> true
      | _ -> false
    in

    let response_json =
      `Assoc
        [ "status", `String "received"
        ; ( "data"
          , `Assoc
              [ "name", `String n
              ; "email", `String e
              ; "message", `String m
              ; "subscribed", `Bool is_subscribed
              ] )
        ]
    in
    Response.of_json ~status:`OK response_json
  | _ ->
    let error =
      `Assoc
        [ "error", `String "Missing required fields: name, email, message" ]
    in
    Response.of_json ~status:`Bad_request error

let upload_file _multipart_data _req =
  let response_json =
    `Assoc
      [ "status", `String "received"
      ; "message", `String "Multipart upload endpoint"
      ]
  in
  Response.of_json ~status:`OK response_json

let list_users _req =
  let users =
    `List
      [ `Assoc [ "id", `Int 1; "name", `String "Alice" ]
      ; `Assoc [ "id", `Int 2; "name", `String "Bob" ]
      ]
  in
  Response.of_json ~status:`OK users

let html_page title body =
  Printf.sprintf
    {|<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>%s</title>
  <style>
    body { font-family: sans-serif; max-width: 800px; margin: 50px auto; padding: 20px; }
    h1 { color: #333; }
    .example { background: #f5f5f5; padding: 20px; margin: 20px 0; border-radius: 5px; }
    code { background: #e0e0e0; padding: 2px 5px; border-radius: 3px; }
    pre { background: #2d2d2d; color: #f8f8f2; padding: 15px; border-radius: 5px; overflow-x: auto; }
    .endpoint { margin: 10px 0; }
    .method { font-weight: bold; display: inline-block; width: 80px; }
    .method.post { color: #49cc90; }
    .method.put { color: #fca130; }
    .method.get { color: #61affe; }
  </style>
</head>
<body>
  <h1>%s</h1>
  %s
</body>
</html>|}
    title
    title
    body

let index_page _req =
  let body =
    {|
  <p>This example demonstrates <strong>type-safe request body parsing</strong> with Tapak.</p>

  <h2>Available Endpoints</h2>

  <div class="example">
    <h3>1. JSON Body (application/json)</h3>
    <div class="endpoint">
      <span class="method post">POST</span>
      <code>/api/users</code> - Create a user
    </div>
    <pre>curl -X POST http://localhost:8080/api/users \
  -H "Content-Type: application/json" \
  -d '{"name": "Alice", "email": "alice@example.com", "age": 30}'</pre>

    <div class="endpoint">
      <span class="method put">PUT</span>
      <code>/api/users/:id</code> - Update a user
    </div>
    <pre>curl -X PUT http://localhost:8080/api/users/1 \
  -H "Content-Type: application/json" \
  -d '{"name": "Alice Smith", "email": "alice.smith@example.com"}'</pre>

    <div class="endpoint">
      <span class="method get">GET</span>
      <code>/api/users</code> - List users
    </div>
    <pre>curl http://localhost:8080/api/users</pre>
  </div>

  <div class="example">
    <h3>2. URL-Encoded Form (application/x-www-form-urlencoded)</h3>
    <div class="endpoint">
      <span class="method post">POST</span>
      <code>/login</code> - Login form
    </div>
    <pre>curl -X POST http://localhost:8080/login \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "username=alice&password=secret123"</pre>

    <div class="endpoint">
      <span class="method post">POST</span>
      <code>/contact</code> - Contact form
    </div>
    <pre>curl -X POST http://localhost:8080/contact \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "name=Alice&email=alice@example.com&message=Hello!&subscribe=on"</pre>
  </div>

  <div class="example">
    <h3>3. Multipart Form (multipart/form-data)</h3>
    <div class="endpoint">
      <span class="method post">POST</span>
      <code>/upload</code> - File upload (pending implementation)
    </div>
    <pre>curl -X POST http://localhost:8080/upload \
  -F "file=@document.pdf" \
  -F "description=My document"</pre>
  </div>

  <h2>Type Safety Features</h2>
  <ul>
    <li>POST/PUT/PATCH can have request bodies</li>
    <li>GET/HEAD <strong>cannot</strong> have request bodies (compile-time error)</li>
    <li>Request body is automatically parsed based on Content-Type</li>
    <li>Type-safe body access in handlers</li>
  </ul>
  |}
  in
  Response.of_html ~status:`OK (html_page "Body Parsing Example" body)

let app env =
  let open Router in
  let open Middleware in
  let clock = Eio.Stdenv.clock env in
  let now () = Eio.Time.now clock in
  App.(
    routes
      [ get (s "") |> into index_page
      ; post (s "api" / s "users") |> req_body Json |> into create_user
      ; put (s "api" / s "users" / p "userId" int64)
        |> req_body Json
        |> into update_user
      ; get (s "api" / s "users") |> into list_users
      ; post (s "login") |> req_body Urlencoded |> into login_form
      ; post (s "contact") |> req_body Urlencoded |> into contact_form
      ; post (s "upload") |> req_body Multipart |> into upload_file
      ]
      ()
    <++> [ Middleware.use
             ~name:"Request_logger"
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
  Logs.info (fun m -> m "Body Parsing Example");
  Logs.info (fun m -> m "Listening on http://localhost:8080");
  ignore (Server.run_with ~config ~env (app env))
