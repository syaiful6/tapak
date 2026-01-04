let user_id_key : string Tapak.Context.key =
  Tapak.Context.Key.create { name = Some "user_id"; show = Some Fun.id }

let user_color_key : string Tapak.Context.key =
  Tapak.Context.Key.create { name = Some "user_color"; show = Some Fun.id }

module Cursor_socket : Tapak_channel.Socket.HANDLER = struct
  let connect (info : Tapak_channel.Socket.connect_info) =
    let open Yojson.Safe.Util in
    let user_id =
      match
        info.Tapak_channel.Socket.params |> member "user_id" |> to_string_option
      with
      | Some id -> id
      | None -> Printf.sprintf "user_%d" (Random.int 10000)
    in
    let colors =
      [| "#FF6B6B"
       ; "#4ECDC4"
       ; "#45B7D1"
       ; "#96CEB4"
       ; "#FFEAA7"
       ; "#DDA0DD"
       ; "#98D8C8"
       ; "#F7DC6F"
       ; "#BB8FCE"
       ; "#85C1E9"
      |]
    in
    let color = colors.(Random.int (Array.length colors)) in
    let assigns =
      Tapak.Context.empty
      |> Tapak.Context.add user_id_key user_id
      |> Tapak.Context.add user_color_key color
    in
    Ok assigns

  let id assigns =
    match Tapak.Context.find user_id_key assigns with
    | Some uid -> Some (Printf.sprintf "cursor_socket:%s" uid)
    | None -> None
end

module Cursor_channel : Tapak_channel.Channel.HANDLER = struct
  type state =
    { user_id : string
    ; color : string
    ; phx_ref : string
    }

  let init () = { user_id = ""; color = ""; phx_ref = "" }

  let join ~topic ~payload:_ ~socket _state =
    if String.equal topic "cursors:lobby"
    then (
      let user_id = Tapak_channel.Socket.get_assign_exn user_id_key socket in
      let color = Tapak_channel.Socket.get_assign_exn user_color_key socket in

      Logs.info (fun m -> m "User %s joining cursors:lobby" user_id);

      let meta =
        `Assoc [ "user_id", `String user_id; "color", `String color ]
      in
      let phx_ref = Tapak_channel.Channel.track_presence ~key:user_id ~meta in

      Logs.info (fun m ->
        m "Tracked presence for user %s with phx_ref %s" user_id phx_ref);

      Ok ({ user_id; color; phx_ref }, socket, `Assoc []))
    else Error (`Assoc [ "reason", `String "invalid topic" ])

  let handle_in ~event ~payload ~socket state =
    match event with
    | "cursor_move" ->
      (try
         let open Yojson.Safe.Util in
         let x =
           match payload |> member "x" with
           | `Int i -> float_of_int i
           | `Float f -> f
           | _ -> 0.0
         in
         let y =
           match payload |> member "y" with
           | `Int i -> float_of_int i
           | `Float f -> f
           | _ -> 0.0
         in
         let broadcast_payload =
           `Assoc
             [ "user_id", `String state.user_id
             ; "color", `String state.color
             ; "x", `Float x
             ; "y", `Float y
             ]
         in
         Tapak_channel.Channel.broadcast_from
           ~topic:"cursors:lobby"
           ~event:"cursor_update"
           ~payload:broadcast_payload;
         Tapak_channel.Channel.Reply
           (Ok, `Assoc [ "status", `String "ok" ], state, socket)
       with
      | exn ->
        Logs.err (fun m ->
          m "Error in cursor_move handler: %s" (Printexc.to_string exn));
        Tapak_channel.Channel.Reply
          ( Error
          , `Assoc [ "error", `String (Printexc.to_string exn) ]
          , state
          , socket ))
    | _ ->
      Logs.debug (fun m -> m "Unknown event: %s" event);
      Tapak_channel.Channel.No_reply (state, socket)

  let handle_info (msg : Tapak_channel.Channel.broadcast_msg) ~socket state =
    Tapak_channel.Channel.Push msg.Tapak_channel.Channel.payload, state, socket

  let handle_out ~event:_ ~payload ~socket state =
    Tapak_channel.Channel.Push payload, state, socket

  let terminate ~reason:_ ~socket:_ state =
    Logs.info (fun m ->
      m "User %s terminating (phx_ref: %s)" state.user_id state.phx_ref)

  let intercept = []
end

let serve_static_file path =
  let content_type =
    if String.ends_with ~suffix:".html" path
    then "text/html"
    else if String.ends_with ~suffix:".js" path
    then "application/javascript"
    else if String.ends_with ~suffix:".css" path
    then "text/css"
    else "text/plain"
  in
  let file_path = "examples/live-cursors/static/" ^ path in
  match Tapak.Response.copy_file file_path with
  | Ok response ->
    Tapak.Response.with_
      ~headers:
        (Piaf.Headers.of_list
           [ "content-type", content_type; "cache-control", "no-cache" ])
      response
  | Error _ -> Tapak.Response.of_string ~body:"File not found" `Not_found

let () =
  Random.self_init ();
  let domains =
    match Sys.getenv_opt "DOMAINS" with Some d -> int_of_string d | None -> 1
  in

  let () = if domains > 1 then Logs_threaded.enable () else () in
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pubsub = Tapak_channel.local_pubsub ~sw in
  let clock = Eio.Stdenv.clock env in
  let node_name = Printf.sprintf "node-%d" (Random.int 10000) in
  let presence =
    Tapak_channel.Presence.create ~sw ~pubsub ~node_name ~clock ()
  in
  let now () = Eio.Time.now clock in

  let cursor_channel_pattern = Tapak_channel.Topic.(s "cursors" / str) in
  let ws_config =
    Tapak_channel.(
      Endpoint.create_config
        ~socket:(module Cursor_socket)
        ~channels:
          [ Endpoint.channel cursor_channel_pattern (module Cursor_channel) ]
        ~pubsub
        ~presence
        ~clock
        ())
  in

  let all_routes =
    Tapak.Router.
      [ get (s "") |> unit |> into (fun () -> serve_static_file "index.html")
      ; get (s "static" / str) |> into (fun file -> serve_static_file file)
      ; (* Mount WebSocket endpoint at /socket/websocket *)
        scope (s "socket") (Tapak_channel.Endpoint.routes ws_config)
      ]
  in
  let app =
    Tapak.(
      Router.routes all_routes
      |> use
           (module Middleware.Request_logger)
           (Middleware.Request_logger.args ~now ~trusted_proxies:[] ()))
  in

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
    Logs.info (fun log ->
      log
        "Starting Live Cursors with systemd socket activation support \
         (domains: %d)"
        domains);
    ignore (Tapak.run_with_systemd_socket ~config ~env app))
  else (
    Logs.warn (fun log ->
      log "Starting Live Cursors WITHOUT systemd support on port %d" port);
    ignore (Tapak.run_with ~config ~env app))
