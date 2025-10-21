open Tapak
module Log = (val Logs.src_log Logs.default : Logs.LOG)

let trusted_proxies =
  [ "127.0.0.1"
  ; (* Localhost IPv4 *)
    "::1"
    (* Localhost IPv6 *)
    (* Add actual load balancer/CDN IPs/ranges here, e.g.: *)
    (* "10.0.0.0/8"; *)
    (* "172.16.0.0/12"; *)
    (* "192.168.0.0/16"; *)
  ]

let home_handler _req =
  Response.of_html
    ~status:`OK
    {|<h1>Tapak Showcases</h1>
<ul>
  <li><a href="/users/123">User Profile</a></li>
  <li><a href="/api/hello">JSON API</a></li>
  <li><a href="/files/docs/readme.md">File Browser</a></li>
  <li><a href="/echo">Echo (POST with body)</a></li>
</ul>|}

let user_handler req =
  match Router.route_params req with
  | Some params ->
    (match List.assoc_opt "id" params with
    | Some id ->
      let html = Printf.sprintf "<h1>User Profile</h1><p>User ID: %s</p>" id in
      Response.of_string' ~content_type:"text/html" ~status:`OK html
    | None -> Response.of_string' ~status:`Not_found "User ID not found")
  | None -> Response.of_string' ~status:`Not_found "No route params"

let api_hello_handler req =
  (* Demonstrate content negotiation *)
  let accept_header = Request.header "Accept" req in
  let format =
    Header_parser.Content_negotiation.preferred_format
      accept_header
      [ `Json; `Html ]
  in

  match format with
  | `Json ->
    Response.of_string'
      ~content_type:"application/json"
      ~status:`OK
      {|{"message": "Hello from Tapak!", "version": "1.0"}|}
  | `Html ->
    Response.of_string'
      ~content_type:"text/html"
      ~status:`OK
      "<h1>Hello from Tapak!</h1><p>Version: 1.0</p>"
  | _ -> Response.of_string' ~status:`OK "Hello from Tapak!\nVersion: 1.0"

let files_handler req =
  match Router.route_splat req with
  | Some paths ->
    let path = String.concat "/" paths in
    let html =
      Printf.sprintf "<h1>File Browser</h1><p>Requested path: %s</p>" path
    in
    Response.of_string' ~content_type:"text/html" ~status:`OK html
  | None -> Response.of_string' ~status:`Not_found "No file path"

let echo_handler req =
  let method_ = Request.meth req in

  match method_ with
  | `POST | `PUT ->
    let body = Request.body req in
    let body_content =
      let stream = Body.to_stream body in
      let rec read acc =
        match Piaf.Stream.take stream with
        | Some { Piaf.IOVec.buffer; off; len } ->
          let str = Bigstringaf.substring buffer ~off ~len in
          read (str :: acc)
        | None -> String.concat "" (List.rev acc)
      in
      read []
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

let not_found _req =
  Response.of_string'
    ~status:`Not_found
    "<h1>404 Not Found</h1><p>The page you requested could not be found.</p>"

let setup_app env =
  let open Middleware in
  let now () = Eio.Time.now (Eio.Stdenv.clock env) in

  App.(
    routes
      ~not_found
      [ Router.get "/" home_handler
      ; Router.get "/users/:id" user_handler
      ; Router.get "/api/hello" api_hello_handler
      ; Router.get "/files/**" files_handler
      ; Router.post "/echo" echo_handler
      ; Router.put "/echo" echo_handler
      ]
      ()
    <++> [ use
             ~name:"Request Logger"
             (module Request_logger)
             (Request_logger.args ~now ~trusted_proxies ())
         ; use
             ~name:"Decompression"
             (module Decompression)
             Tapak_compressions.decoder
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
