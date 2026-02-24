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

let message_schema =
  Sch.Object.(
    define ~kind:"Message"
    @@
    let+ message = mem ~enc:Stdlib.fst "message" Sch.string
    and+ timestamp = mem ~enc:Stdlib.snd "timestamp" Sch.float in
    message, timestamp)

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in

  let public_dir = Eio.Path.(cwd / "examples" / "static-files" / "public") in
  let now () = Eio.Time.now (Eio.Stdenv.clock env) in

  let static_config =
    { Tapak.Static.default_config with
      max_age = `Seconds 3600 (* Cache for 1 hour *)
    ; use_weak_etags = true
    ; serve_hidden_files = false
    ; follow_symlinks = false
    ; index_files = [ "index.html"; "index.htm" ]
    }
  in

  let api_handler () =
    let json_body = "Hello from API", Ptime_clock.now () |> Ptime.to_float_s in
    let headers =
      Piaf.Headers.of_list [ "Content-Type", "application/json; charset=utf-8" ]
    in
    Tapak.json ~headers (Sch.Json.encode_string message_schema json_body)
  in

  let handler =
    Tapak.(
      Router.(
        routes
          [ get (s "api" / s "hello") |> unit |> into api_handler
          ; get splat
            |> request
            |> into
                 (Tapak.static
                    ~env:(env :> Tapak.Static.fs_env)
                    ~config:static_config
                    public_dir
                    ())
          ])
      |> use
           (module Middleware.Request_logger)
           (Middleware.Request_logger.args ~now ~trusted_proxies ()))
  in

  let port = 8080 in
  let address = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  let config = Piaf.Server.Config.create address in

  Log.info (fun m -> m "Starting static file server on http://127.0.0.1:8080");
  Log.info (fun m ->
    m "Serving files from: %s" (Eio.Path.native_exn public_dir));

  ignore (Tapak.run_with ~config ~env handler)
