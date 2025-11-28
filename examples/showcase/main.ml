open Tapak
module Log = (val Logs.src_log Logs.default : Logs.LOG)

let trusted_proxies =
  [ Ipaddr.Prefix.of_string_exn "127.0.0.1/32"
  ; (* Localhost IPv4 *)
    Ipaddr.Prefix.of_string_exn "::1/128"
    (* Localhost IPv6 *)
    (* Add actual load balancer/CDN IPs/ranges here, e.g.: *)
    (* Ipaddr.Prefix.of_string_exn "10.0.0.0/8"; *)
    (* Ipaddr.Prefix.of_string_exn "172.16.0.0/12"; *)
    (* Ipaddr.Prefix.of_string_exn "192.168.0.0/16"; *)
  ]

let home_handler _req =
  Response.of_html
    ~status:`OK
    {|<h1>Tapak Showcases</h1>
<ul>
  <li><a href="/users/123">User Profile</a></li>
  <li><a href="/api/version">JSON API</a></li>
  <li><a href="/files/docs/readme.md">File Browser</a></li>
  <li><a href="/echo">Echo (POST with body)</a></li>
  <li><a href="/form">CSRF-Protected Form</a></li>
</ul>|}

let user_handler id _req =
  let html = Printf.sprintf "<h1>User Profile</h1><p>User ID: %Ld</p>" id in
  Response.of_html ~status:`OK html

let api_version_handler req =
  Response.negotiate req (function
    | `Json ->
      Some
        ( "application/json"
        , {|{"message": "Hello from Tapak!", "version": "1.0"}|} )
    | `Html ->
      Some ("text/html", "<h1>Hello from Tapak!</h1><p>Version: 1.0</p>")
    | `Text -> Some ("text/plain", "Hello from Tapak!\nVersion: 1.0")
    | _ -> None)

let files_handler path _req =
  let html =
    Printf.sprintf "<h1>File Browser</h1><p>Requested path: %s</p>" path
  in
  Response.of_string' ~content_type:"text/html" ~status:`OK html

let echo_handler req =
  let method_ = Request.meth req in

  match method_ with
  | `POST | `PUT ->
    let body_content =
      Result.fold
        ~ok:Fun.id
        ~error:(fun _ -> "")
        (Body.to_string (Request.body req))
    in
    let encoding_header = Request.header "Content-Encoding" req in
    let encoding_status =
      match encoding_header with
      | None -> "none"
      | Some _ -> "removed after decompression"
    in
    let response_text =
      Printf.sprintf
        "Received %d bytes\nContent-Encoding: %s\n\nBody:\n%s"
        (String.length body_content)
        encoding_status
        body_content
    in
    Response.of_string' ~status:`OK response_text
  | _ ->
    Response.of_string'
      ~status:`Method_not_allowed
      "Please POST or PUT data to this endpoint"

let form_get_handler req =
  let token, secret = Csrf.input req in
  let html =
    Printf.sprintf
      {|<h1>CSRF-Protected Form</h1>
<p>This form demonstrates CSRF protection using Tapak's CSRF module.</p>

<form method="POST" action="/form">
  <label for="message">Message:</label><br>
  <input type="text" id="message" name="message" required><br><br>

  <input type="hidden" name="csrf_token" value="%s">

  <button type="submit">Submit</button>
</form>

<hr>
<h2>How it works</h2>
<ol>
  <li>Server generates a secret and stores it in a cookie</li>
  <li>Server generates a masked token from the secret</li>
  <li>Token is embedded in the form as a hidden field</li>
  <li>On submit, server verifies token matches the cookie secret</li>
  <li>Uses constant-time comparison to prevent timing attacks</li>
</ol>

<p><strong>Current Token:</strong> <code>%s</code></p>
<p><strong>Cookie Secret:</strong> Set in XSRF-TOKEN cookie (check browser dev tools)</p>

<p><a href="/">Back to home</a></p>
|}
      token
      token
  in
  Response.of_html ~status:`OK html |> Csrf.with_cookie secret

let form_post_handler req =
  let form_data =
    Form.Urlencoded.of_body (Request.body req)
    |> Result.map Form.Urlencoded.normalize
  in
  match Result.map (Form.Urlencoded.get "csrf_token") form_data with
  | Ok (Some token) when Csrf.verify_token ~token req ->
    let form_data = Result.get_ok form_data in
    let message =
      Form.Urlencoded.get "message" form_data
      |> Option.value ~default:"(no message)"
    in
    let html =
      Printf.sprintf
        {|<h1>Form Submitted Successfully!</h1>
<p><strong>Your message:</strong> %s</p>

<p>The CSRF token was validated successfully. This proves:</p>
<ul>
  <li>The request came from your form (not a malicious site)</li>
  <li>The token matches the secret in your cookie</li>
  <li>The request is protected against CSRF attacks</li>
</ul>

<p><a href="/form">Submit another message</a> | <a href="/">Back to home</a></p>
|}
        (String.escaped message)
    in
    Response.of_html ~status:`OK html
  | Ok (Some _) | Ok None ->
    Response.of_html
      ~status:`Forbidden
      "<h1>403 Forbidden</h1><p>Invalid CSRF token</p><p><a href=\"/form\">Try \
       again</a></p>"
  | Error _ ->
    Response.of_html
      ~status:`Bad_request
      "<h1>400 Bad Request</h1><p>Invalid form data</p><p><a \
       href=\"/form\">Try again</a></p>"

let api_users_handler _req =
  Response.of_string
    ~body:{|{"users": [{"id": 1, "name": "Alice"}, {"id": 2, "name": "Bob"}]}|}
    `OK

let api_detail_user_handler id _req =
  match id with
  | 1L -> Response.of_string ~body:{|{"id": 1, "name": "Alice"}|} `OK
  | 2L -> Response.of_string ~body:{|{"id": 2, "name": "Bob"}|} `OK
  | _ -> Response.of_string ~body:{|{"error": "User not found"}|} `Not_found

let api_update_user_handler id req =
  let body_content =
    Result.fold
      ~ok:Fun.id
      ~error:(fun _ -> "")
      (Body.to_string (Request.body req))
  in
  let response_text =
    Printf.sprintf
      {|{"message": "User %Ld updated", "data": %s}|}
      id
      (String.escaped body_content)
  in
  Response.of_string' ~status:`OK response_text

let not_found _req =
  Response.of_string'
    ~status:`Not_found
    "<h1>404 Not Found</h1><p>The page you requested could not be found.</p>"

let setup_app env =
  let open Middleware in
  let open Router in
  let now () = Eio.Time.now (Eio.Stdenv.clock env) in
  let decoder = Tapak_compressions.decoder in
  let max_bytes = Int64.mul 10L (Int64.mul 1024L 1024L) in

  App.(
    routes
      ~not_found
      [ get (s "") |> into home_handler
      ; get (s "users" / int64) |> into user_handler
      ; scope
          (s "api")
          [ get (s "version") |> into api_version_handler
          ; scope
              ~middlewares:
                [ use
                    ~name:"Limit_request_size"
                    (module Limit_request_size)
                    (Limit_request_size.args ~max_bytes)
                ]
              (s "users")
              [ get (s "") |> into api_users_handler
              ; get int64 |> into api_detail_user_handler
              ; post int64 |> into api_update_user_handler
              ]
          ]
      ; get (s "files" / str) |> into files_handler
      ; post (s "echo") |> into echo_handler
      ; put (s "echo") |> into echo_handler
      ; get (s "form") |> into form_get_handler
      ; post (s "form") |> into form_post_handler
      ]
      ()
    <++> [ use
             ~name:"Request_logger"
             (module Request_logger)
             (Request_logger.args ~now ~trusted_proxies ())
         ; use ~name:"Decompression" (module Decompression) decoder
         ])

let setup_log ?(threaded = false) ?style_renderer level =
  let () = if threaded then Logs_threaded.enable () else () in
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let () =
  let domains =
    match Sys.getenv_opt "DOMAINS" with Some d -> int_of_string d | None -> 1
  in
  setup_log ~threaded:(domains > 1) (Some Logs.Debug);
  Eio_main.run @@ fun env ->
  let use_systemd =
    match Sys.getenv_opt "TAPAK_SYSTEMD" with
    | Some "false" | Some "0" -> false
    | _ -> true
  in
  let port =
    match Sys.getenv_opt "PORT" with Some p -> int_of_string p | None -> 3000
  in
  let address = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let config = Piaf.Server.Config.create ~domains address in

  if use_systemd
  then (
    Log.info (fun log ->
      log
        "Starting with systemd socket activation support (domains: %d)"
        domains);
    ignore (Server.run_with_systemd_socket ~config ~env (setup_app env)))
  else (
    Log.warn (fun log ->
      log "Starting Tapak Showcase WITHOUT systemd support on port %d" port);
    ignore (Server.run_with ~config ~env (setup_app env)))
