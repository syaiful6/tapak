open Tapak

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

let about _request =
  Response.of_string' "About Tapak - A composable web framework"
[@@route GET, "/about"]

let get_user ~id _request =
  Response.of_string' (Printf.sprintf "User ID: %d" id)
[@@route GET, "/users/:id"]

let create_user _request = Response.of_string' "User created"
[@@route POST, "/users"]

let blog_post ~slug _request =
  Response.of_string' (Printf.sprintf "Blog Post Slug: %s" slug)
[@@route GET, "/blog/<slug:slug>"]

let home _request =
  Response.of_html
    ~status:`OK
    (Format.asprintf
       {|<h1>Tapak Showcases</h1>
<ul>
  <li><a href="%s">About</a></li>
  <li><a href="%s">User detail</a></li>
  <li><a href="%s">Blog post</a></li>
</ul>|}
       (Router.sprintf about_path)
       (Router.sprintf get_user_path 42)
       (Router.sprintf blog_post_path "hello-world"))
[@@route GET, "/"]

let not_found _req =
  Response.of_string'
    ~status:`Not_found
    "<h1>404 Not Found</h1><p>The page you requested could not be found.</p>"

let setup_log ?(threaded = false) ?style_renderer level =
  let () = if threaded then Logs_threaded.enable () else () in
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let () =
  let open Middleware in
  setup_log ~threaded:false (Some Logs.Debug);
  Eio_main.run @@ fun env ->
  let now () = Eio.Time.now (Eio.Stdenv.clock env) in
  let app =
    App.(
      routes
        ~not_found
        [ home_route
        ; about_route
        ; get_user_route
        ; create_user_route
        ; blog_post_route
        ]
        ()
      <++> [ use
               ~name:"Request_logger"
               (module Request_logger)
               (Request_logger.args ~now ~trusted_proxies ())
           ])
  in
  let address = `Tcp (Eio.Net.Ipaddr.V4.any, 8080) in
  let config = Piaf.Server.Config.create address in
  ignore (Server.run_with ~config ~env app)
