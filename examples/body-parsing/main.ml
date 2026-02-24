module User_request = struct
  type t =
    { name : string
    ; email : string
    ; age : int option
    }

  let schema =
    Sch.Object.(
      define ~kind:"UserRequest"
      @@
      let+ name =
        mem
          ~enc:(fun u -> u.name)
          "email"
          Sch.(with_ ~constraint_:(Constraint.min_length 3) string)
      and+ email =
        mem
          ~enc:(fun u -> u.email)
          "email"
          Sch.(with_ ~constraint_:(Constraint.format `Email) string)
      and+ age =
        mem_opt
          ~enc:(fun u -> u.age)
          "age"
          Sch.(with_ ~constraint_:(Constraint.int_min 18) int)
      in
      { name; email; age })
end

module User = struct
  type t =
    { id : int
    ; name : string
    ; email : string
    ; age : int option
    }

  let of_request id (req : User_request.t) =
    { id; name = req.name; email = req.email; age = req.age }

  let schema =
    Sch.Object.(
      define ~kind:"User"
      @@
      let+ id = mem ~enc:(fun u -> u.id) "id" Sch.int
      and+ name =
        mem
          ~enc:(fun u -> u.name)
          "email"
          Sch.(with_ ~constraint_:(Constraint.min_length 3) string)
      and+ email =
        mem
          ~enc:(fun u -> u.email)
          "email"
          Sch.(with_ ~constraint_:(Constraint.format `Email) string)
      and+ age =
        mem_opt
          ~enc:(fun u -> u.age)
          "age"
          Sch.(with_ ~constraint_:(Constraint.int_min 18) int)
      in
      { id; name; email; age })
end

let create_user user =
  Tapak.json
    ~status:`Created
    (Sch.Json.encode_string User.schema (User.of_request 12 user))

type user_update =
  { name : string option
  ; email : string option
  }

let update_schema =
  Sch.Object.(
    define ~kind:"UserUpdate"
    @@
    let+ name =
      mem_opt
        ~enc:(fun u -> u.name)
        "name"
        Sch.(with_ ~constraint_:(Constraint.min_length 3) string)
    and+ email =
      mem_opt
        ~enc:(fun u -> u.email)
        "email"
        Sch.(with_ ~constraint_:(Constraint.format `Email) string)
    in
    { name; email })

let update_user update id =
  let updates = [] in
  let updates =
    match update.name with
    | Some n -> (Jsont.Json.name "name", Jsont.Json.string n) :: updates
    | None -> updates
  in
  let updates =
    match update.email with
    | Some e -> (Jsont.Json.name "email", Jsont.Json.string e) :: updates
    | None -> updates
  in
  let response_json =
    Jsont.Json.(
      object'
        [ name "status", string "updated"
        ; name "id", string (Int64.to_string id)
        ; name "updates", object' updates
        ])
  in
  Tapak.json
    ~status:`OK
    (Jsont_bytesrw.encode_string Jsont.json response_json |> Result.get_ok)

type login_input =
  { username : string
  ; password : string
  }

let login_schema =
  Sch.Object.(
    define ~kind:"LoginInput"
    @@
    let+ username =
      mem
        ~enc:(fun l -> l.username)
        "username"
        Sch.(with_ ~constraint_:(Constraint.min_length 3) string)
    and+ password =
      mem
        ~enc:(fun l -> l.password)
        "password"
        Sch.(
          with_
            ~constraint_:Constraint.(all_of [ min_length 8; max_length 32 ])
            string)
    in
    { username; password })

let login_form login =
  (* verify login.username and login.password against your user database *)
  let _password_to_verify = login.password in
  let response_json =
    Jsont.Json.(
      object'
        [ name "status", string "authenticated"
        ; name "username", string login.username
        ; name "message", string "Login successful"
        ])
  in
  Tapak.json
    ~status:`OK
    (Jsont_bytesrw.encode_string Jsont.json response_json |> Result.get_ok)

type contact_input =
  { name : string
  ; email : string
  ; message : string
  ; subscribe : bool
  }

let contact_schema =
  Sch.Object.(
    define ~kind:"ContactInput"
    @@
    let+ name =
      mem
        ~enc:(fun c -> c.name)
        "name"
        Sch.(with_ ~constraint_:(Constraint.min_length 1) string)
    and+ email =
      mem
        ~enc:(fun c -> c.email)
        "email"
        Sch.(with_ ~constraint_:(Constraint.format `Email) string)
    and+ message =
      mem
        ~enc:(fun c -> c.message)
        "message"
        Sch.(with_ ~constraint_:(Constraint.min_length 1) string)
    and+ subscribe =
      mem ~default:false ~enc:(fun c -> c.subscribe) "subscribe" Sch.bool
    in
    { name; email; message; subscribe })

let contact_form contact =
  let response_json =
    Jsont.Json.(
      object'
        [ name "status", string "received"
        ; ( name "data"
          , object'
              [ name "name", string contact.name
              ; name "email", string contact.email
              ; name "message", string contact.message
              ; name "subscribed", bool contact.subscribe
              ] )
        ])
  in
  Tapak.json
    (Jsont_bytesrw.encode_string Jsont.json response_json |> Result.get_ok)

type upload_input =
  { file : Sch.File.t
  ; description : string
  }

let upload_input_schema =
  Sch.Object.(
    define ~kind:"UploadInput"
    @@
    let+ file = mem ~enc:(fun u -> u.file) "file" Sch.file
    and+ description =
      mem
        ~default:"No description"
        ~enc:(fun u -> u.description)
        "description"
        Sch.string
    in
    { file; description })

let upload_file upload =
  (* save the upload.file somewhere *)
  let _file_part = upload.file in
  let response_json =
    Jsont.Json.(
      object'
        [ name "status", string "received"
        ; name "message", string "File uploaded successfully"
        ; name "description", string upload.description
        ])
  in
  Tapak.json
    (Jsont_bytesrw.encode_string Jsont.json response_json |> Result.get_ok)

let list_users _req =
  let users =
    [ { User.id = 1
      ; name = "Alice"
      ; email = "alice@example.com"
      ; age = Some 18
      }
    ; { id = 2; name = "Bob"; email = "bob@example.com"; age = None }
    ]
  in
  Tapak.json (Sch.Json.encode_string (Sch.list User.schema) users)

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
  Tapak.html (html_page "Body Parsing Example" body)

let error_handler next request =
  try next request with
  | Tapak.Router.Validation_failed errors ->
    Tapak.json
      ~status:`Bad_request
      (Jsont_bytesrw.encode_string
         Jsont.json
         (Tapak.Router.validation_error_to_json errors)
      |> Result.get_ok)
  | Tapak.Router.Bad_request msg ->
    let body = Jsont.Json.(object' [ name "error", string msg ]) in
    Tapak.json
      ~status:`Bad_request
      (Jsont_bytesrw.encode_string Jsont.json body |> Result.get_ok)

let app env =
  let clock = Eio.Stdenv.clock env in
  let now () = Eio.Time.now clock in
  Tapak.(
    Router.(
      routes
        [ get (s "") |> unit |> into index_page
        ; scope
            (s "api")
            [ get (s "users") |> unit |> into list_users
            ; post (s "users")
              |> body Json User_request.schema
              |> into create_user
            ; put (s "users" / p "userId" int64)
              |> body Json update_schema
              |> into update_user
            ]
        ; post (s "login") |> body Urlencoded login_schema |> into login_form
        ; post (s "contact")
          |> body Urlencoded contact_schema
          |> into contact_form
        ; post (s "upload")
          |> body Multipart upload_input_schema
          |> into upload_file
        ])
    |> pipe
         ~through:
           [ use
               (module Middleware.Request_logger)
               (Middleware.Request_logger.args ~now ~trusted_proxies:[] ())
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
  ignore (Tapak.run_with ~config ~env (app env))
