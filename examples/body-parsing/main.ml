open Tapak

type user_input =
  { name : string
  ; email : string
  ; age : int option
  }

let user_schema =
  let open Schema.Syntax in
  let+ name = Schema.str "name" |> Schema.validate Schema.Validator.nes
  and+ email = Schema.str "email" |> Schema.validate Schema.Validator.nes
  and+ age = Schema.option "age" (Schema.Field.int ()) in
  { name; email; age }

let create_user user _req =
  let user_data =
    [ "name", `String user.name; "email", `String user.email ]
    @ match user.age with Some age -> [ "age", `Int age ] | None -> []
  in
  let response_json =
    `Assoc [ "status", `String "created"; "user", `Assoc user_data ]
  in
  Response.of_json ~status:`Created response_json

type user_update =
  { name : string option
  ; email : string option
  }

let update_schema =
  let open Schema.Syntax in
  let+ name = Schema.option "name" (Schema.Field.str ())
  and+ email = Schema.option "email" (Schema.Field.str ()) in
  { name; email }

let update_user update id _req =
  let updates = [] in
  let updates =
    match update.name with
    | Some n -> ("name", `String n) :: updates
    | None -> updates
  in
  let updates =
    match update.email with
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

type login_input =
  { username : string
  ; password : string
  }

let login_schema =
  let open Schema.Syntax in
  let+ username = Schema.str "username" |> Schema.validate Schema.Validator.nes
  and+ password =
    Schema.str "password" |> Schema.validate Schema.Validator.nes
  in
  { username; password }

let login_form login _req =
  (* verify login.username and login.password against your user database *)
  let _password_to_verify = login.password in
  let response_json =
    `Assoc
      [ "status", `String "authenticated"
      ; "username", `String login.username
      ; "message", `String "Login successful"
      ]
  in
  Response.of_json ~status:`OK response_json

type contact_input =
  { name : string
  ; email : string
  ; message : string
  ; subscribe : bool
  }

let contact_schema =
  let open Schema.Syntax in
  let+ name = Schema.str "name" |> Schema.validate Schema.Validator.nes
  and+ email = Schema.str "email" |> Schema.validate Schema.Validator.nes
  and+ message = Schema.str "message" |> Schema.validate Schema.Validator.nes
  and+ subscribe = Schema.bool ~default:false "subscribe" in
  { name; email; message; subscribe }

let contact_form contact _req =
  let response_json =
    `Assoc
      [ "status", `String "received"
      ; ( "data"
        , `Assoc
            [ "name", `String contact.name
            ; "email", `String contact.email
            ; "message", `String contact.message
            ; "subscribed", `Bool contact.subscribe
            ] )
      ]
  in
  Response.of_json ~status:`OK response_json

type upload_input =
  { file : Form.Multipart.part
  ; description : string
  }

let upload_schema =
  let open Schema.Syntax in
  let+ file = Schema.file "file"
  and+ description = Schema.str ~default:"No description" "description" in
  { file; description }

let upload_file upload _req =
  (* save the upload.file somewhere *)
  let _file_part = upload.file in
  let response_json =
    `Assoc
      [ "status", `String "received"
      ; "message", `String "File uploaded successfully"
      ; "description", `String upload.description
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

let error_handler =
  let filter next request =
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
  in
  Middleware.create ~name:"Error_handler" ~filter

let app env =
  let open Router in
  let open Middleware in
  let clock = Eio.Stdenv.clock env in
  let now () = Eio.Time.now clock in
  App.(
    routes
      [ get (s "") |> into index_page
      ; scope
          (s "api")
          [ get (s "users") |> into list_users
          ; post (s "users") |> body Schema.Json user_schema |> into create_user
          ; put (s "users" / p "userId" int64)
            |> body Schema.Json update_schema
            |> into update_user
          ]
      ; post (s "login")
        |> body Schema.Urlencoded login_schema
        |> into login_form
      ; post (s "contact")
        |> body Schema.Urlencoded contact_schema
        |> into contact_form
      ; post (s "upload")
        |> body Schema.Multipart upload_schema
        |> into upload_file
      ]
      ()
    <++> [ Middleware.use
             ~name:"Request_logger"
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
  Logs.info (fun m -> m "Body Parsing Example");
  Logs.info (fun m -> m "Listening on http://localhost:8080");
  ignore (Server.run_with ~config ~env (app env))
