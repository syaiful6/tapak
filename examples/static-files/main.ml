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

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in

  let public_dir = Eio.Path.(cwd / "examples" / "static-files" / "public") in
  let fs_backend = Static.filesystem ~follow:false public_dir in
  let now () = Eio.Time.now (Eio.Stdenv.clock env) in

  let static_config =
    { Static.default_config with
      max_age = `Seconds 3600 (* Cache for 1 hour *)
    ; use_weak_etags = true
    ; serve_hidden_files = false
    ; follow_symlinks = false
    ; index_files = [ "index.html"; "index.htm" ]
    }
  in

  let api_handler _request =
    let json_body =
      {|{"message":"Hello from API","timestamp":"|}
      ^ Float.to_string (Ptime_clock.now () |> Ptime.to_float_s)
      ^ {|"}|}
    in
    let headers =
      Piaf.Headers.of_list [ "Content-Type", "application/json; charset=utf-8" ]
    in
    Response.of_string ~headers ~body:json_body `OK
  in

  let open Router in
  let app =
    App.(
      routes
        [ get (s "api" / s "hello") |> into api_handler
        ; get splat |> into (Static.serve fs_backend ~config:static_config ())
        ]
        ()
      <++> [ Middleware.(
               use
                 (module Request_logger)
                 (Request_logger.args ~now ~trusted_proxies ()))
           ; Middleware.head
           ])
  in

  let port = 8080 in
  let address = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  let config = Piaf.Server.Config.create address in

  Log.info (fun m -> m "Starting static file server on http://127.0.0.1:8080");
  Log.info (fun m ->
    m "Serving files from: %s" (Eio.Path.native_exn public_dir));

  ignore (Server.run_with ~config ~env app)
