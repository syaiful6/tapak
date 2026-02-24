module User_request = struct
  type t =
    { name : string
    ; email : string
    ; age : int
    }

  let schema =
    Sch.Object.(
      define ~kind:"User"
      @@
      let+ name =
        mem
          ~enc:(fun u -> u.name)
          "name"
          Sch.(with_ ~constraint_:(Constraint.min_length 3) string)
      and+ email =
        mem
          ~enc:(fun u -> u.email)
          "email"
          Sch.(with_ ~constraint_:(Constraint.format `Email) string)
      and+ age =
        mem
          ~enc:(fun u -> u.age)
          "age"
          Sch.(with_ ~constraint_:(Constraint.int_range 18 120) int)
      in
      { name; email; age })
end

module User = struct
  type t =
    { id : int
    ; name : string
    ; email : string
    ; age : int
    }

  let of_request ~id (req : User_request.t) =
    { id; name = req.name; email = req.email; age = req.age }

  let schema =
    Sch.Object.(
      define ~kind:"User"
      @@
      let+ id = mem ~enc:(fun u -> u.id) "id" Sch.int
      and+ name =
        mem
          ~enc:(fun u -> u.name)
          "name"
          Sch.(with_ ~constraint_:(Constraint.min_length 3) string)
      and+ email =
        mem
          ~enc:(fun u -> u.email)
          "email"
          Sch.(with_ ~constraint_:(Constraint.format `Email) string)
      and+ age =
        mem
          ~enc:(fun u -> u.age)
          "age"
          Sch.(with_ ~constraint_:(Constraint.int_range 18 120) int)
      in
      { id; name; email; age })
end

let create_user_json user =
  Tapak.json
    ~status:`Created
    (Sch.Json.encode_string User.schema (User.of_request ~id:123 user))

let create_user_html user =
  let html =
    Printf.sprintf
      {|<div class="success">
  <h2>User Created!</h2>
  <p><strong>Name:</strong> %s</p>
  <p><strong>Email:</strong> %s</p>
  <p><strong>Age:</strong> %d</p>
</div>|}
      user.User_request.name
      user.email
      user.age
  in
  Tapak.html ~status:`Created html

let json_error_handler _request exn =
  match exn with
  | Tapak.Router.Validation_failed errors ->
    Option.some
    @@ Tapak.json
         ~status:`Bad_request
         (Jsont_bytesrw.encode_string
            Jsont.json
            (Tapak.Router.validation_error_to_json errors)
         |> Result.get_ok)
  | Tapak.Router.Bad_request msg ->
    let body = Jsont.Json.(object' [ name "error", string msg ]) in
    Option.some
    @@ Tapak.json
         ~status:`Bad_request
         (Jsont_bytesrw.encode_string Jsont.json body |> Result.get_ok)
  | _ -> None

let html_error_handler _request exn =
  match exn with
  | Tapak.Router.Validation_failed errors ->
    let error_items =
      errors
      |> List.map (fun (field, msg) ->
        Printf.sprintf "<li><strong>%s:</strong> %s</li>" field msg)
      |> String.concat "\n"
    in
    let html =
      Printf.sprintf
        {|<div class="error-box">
  <h3>Validation Failed</h3>
  <ul class="errors">%s</ul>
  <p><a href="javascript:history.back()">Go Back</a></p>
</div>|}
        error_items
    in
    Some (Tapak.html ~status:`Bad_request html)
  | Tapak.Router.Bad_request msg ->
    let html =
      Printf.sprintf
        {|<div class="error-box">
  <h3>Bad Request</h3>
  <p>%s</p>
  <p><a href="javascript:history.back()">Go Back</a></p>
</div>|}
        msg
    in
    Some (Tapak.html ~status:`Bad_request html)
  | _ -> None

let negotiated_error_handler request exn =
  match exn with
  | Tapak.Router.Validation_failed errors ->
    Some
      (Tapak.Response.negotiate ~status:`Bad_request request (fun format ->
         match format with
         | `Json ->
           Some
             ( "application/json"
             , Jsont_bytesrw.encode_string
                 Jsont.json
                 (Tapak.Router.validation_error_to_json errors)
               |> Result.get_ok )
         | `Html ->
           let error_items =
             errors
             |> List.map (fun (field, msg) ->
               Printf.sprintf "<li><strong>%s:</strong> %s</li>" field msg)
             |> String.concat "\n"
           in
           let html =
             Printf.sprintf
               {|<div class="error-box">
  <h3>Validation Failed</h3>
  <ul class="errors">%s</ul>
</div>|}
               error_items
           in
           Some ("text/html; charset=utf-8", html)
         | _ -> None))
  | Tapak.Router.Bad_request msg ->
    Some
      (Tapak.Response.negotiate ~status:`Bad_request request (fun format ->
         match format with
         | `Json ->
           Some
             ( "application/json"
             , Jsont_bytesrw.encode_string
                 Jsont.json
                 (Jsont.Json.object' Jsont.Json.[ name "error", string msg ])
               |> Result.get_ok )
         | `Html ->
           Some
             ( "text/html; charset=utf-8"
             , Printf.sprintf
                 "<div class='error-box'><h3>Error</h3><p>%s</p></div>"
                 msg )
         | _ -> None))
  | _ -> None

let home_page _req =
  let html =
    {|<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Error Handling Example</title>
  <style>
    body { font-family: sans-serif; max-width: 900px; margin: 50px auto; padding: 20px; }
    h1 { color: #333; }
    .pattern { background: #f5f5f5; padding: 20px; margin: 20px 0; border-radius: 5px; }
    code { background: #e0e0e0; padding: 2px 5px; border-radius: 3px; font-family: monospace; }
    pre { background: #2d2d2d; color: #f8f8f2; padding: 15px; border-radius: 5px; overflow-x: auto; font-size: 13px; }
    .error-box { background: #fee; border: 1px solid #fcc; padding: 15px; border-radius: 5px; color: #c33; }
    .success { background: #efe; border: 1px solid #cfc; padding: 15px; border-radius: 5px; color: #3c3; }
    .method { font-weight: bold; color: #49cc90; }
  </style>
</head>
<body>
  <h1>Error Handling with <code>recover</code></h1>
  <p>This example demonstrates different error handling patterns using the <code>recover</code> function.</p>

  <div class="pattern">
    <h2>Pattern 1: JSON Error Handler (APIs)</h2>
    <p>Returns validation errors in JSON format - perfect for REST APIs.</p>
    <p><span class="method">POST</span> <code>/api/users</code></p>
    <pre>curl -X POST http://localhost:8080/api/users \
  -H "Content-Type: application/json" \
  -d '{"name":"ab","email":"invalid","age":15}'</pre>
    <p>Try a valid request:</p>
    <pre>curl -X POST http://localhost:8080/api/users \
  -H "Content-Type: application/json" \
  -d '{"name":"Alice","email":"alice@example.com","age":25}'</pre>
  </div>

  <div class="pattern">
    <h2>Pattern 2: HTML Error Handler (Forms)</h2>
    <p>Returns validation errors as HTML - perfect for server-rendered forms.</p>
    <p><span class="method">POST</span> <code>/signup</code></p>
    <pre>curl -X POST http://localhost:8080/signup \
  -H "Content-Type: application/json" \
  -d '{"name":"ab","email":"invalid","age":15}'</pre>
  </div>

  <div class="pattern">
    <h2>Pattern 3: Content-Negotiated Handler</h2>
    <p>Returns JSON or HTML based on the <code>Accept</code> header.</p>
    <p><span class="method">POST</span> <code>/register</code></p>
    <pre># Returns JSON
curl -X POST http://localhost:8080/register \
  -H "Accept: application/json" \
  -H "Content-Type: application/json" \
  -d '{"name":"ab","email":"invalid","age":15}'

# Returns HTML
curl -X POST http://localhost:8080/register \
  -H "Accept: text/html" \
  -H "Content-Type: application/json" \
  -d '{"name":"ab","email":"invalid","age":15}'</pre>
  </div>

  <div class="pattern">
    <h2>Pattern 4: Scope-Level Error Handling</h2>
    <p>Apply error handler to all routes in a scope.</p>
    <p><span class="method">POST</span> <code>/admin/users</code></p>
    <pre>curl -X POST http://localhost:8080/admin/users \
  -H "Content-Type: application/json" \
  -d '{"name":"ab","email":"invalid","age":15}'</pre>
  </div>

  <h2>Validation Rules</h2>
  <ul>
    <li><strong>name:</strong> minimum 3 characters</li>
    <li><strong>email:</strong> valid email format</li>
    <li><strong>age:</strong> between 18 and 120</li>
  </ul>
</body>
</html>|}
  in
  Tapak.html ~status:`OK html

let handle_admin_errors _req exn =
  match exn with
  | Tapak.Router.Validation_failed _ ->
    Some
      (Tapak.json
         ~status:`Bad_request
         (Jsont_bytesrw.encode_string
            Jsont.json
            Jsont.Json.(
              object'
                [ name "error", string "Admin validation failed"
                ; name "hint", string "Check your input data"
                ])
         |> Result.get_ok))
  | _ -> None

let app env =
  let clock = Eio.Stdenv.clock env in
  let now () = Eio.Time.now clock in
  Tapak.(
    Router.(
      routes
        [ get (s "") |> unit |> into home_page
        ; post (s "api" / s "users")
          |> body Json User_request.schema
          |> into create_user_json
          |> recover json_error_handler
        ; post (s "signup")
          |> body Json User_request.schema
          |> into create_user_html
          |> recover html_error_handler
        ; post (s "register")
          |> body Json User_request.schema
          |> into create_user_json
          |> recover negotiated_error_handler
        ; scope
            (s "admin")
            [ post (s "users")
              |> body Json User_request.schema
              |> into create_user_json
            ]
          |> recover handle_admin_errors
        ])
    |> use
         (module Middleware.Request_logger)
         (Middleware.Request_logger.args ~now ~trusted_proxies:[] ()))

let () =
  Logs_threaded.enable ();
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  Eio_main.run @@ fun env ->
  let address = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let config = Piaf.Server.Config.create address in
  Logs.info (fun m -> m "Error Handling Example");
  Logs.info (fun m -> m "Listening on http://localhost:8080");
  Logs.info (fun m -> m "Try the examples shown on the home page!");
  ignore (Tapak.run_with ~config ~env (app env))
