module Log = (val Logs.src_log Logs.default : Logs.LOG)

let user_id_key : string Tapak.Context.key =
  Tapak.Context.Key.create { name = Some "user_id"; show = Some Fun.id }

let user_color_key : string Tapak.Context.key =
  Tapak.Context.Key.create { name = Some "user_color"; show = Some Fun.id }

module Cursor_socket : Tapak.SOCKET = struct
  let connect (info : Tapak.Socket.connect_info) =
    let user_id =
      match info.Tapak.Socket.params |> Tapak.Form.Urlencoded.get "user_id" with
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

module Point = struct
  type t =
    { x : float
    ; y : float
    }

  let jsont =
    Jsont.Object.map ~kind:"point" (fun x y -> { x; y })
    |> Jsont.Object.mem ~enc:(fun p -> p.x) "x" Jsont.number
    |> Jsont.Object.mem ~enc:(fun p -> p.y) "y" Jsont.number
    |> Jsont.Object.finish
end

module User_cursor = struct
  type t =
    { user_id : string
    ; color : string
    ; x : float
    ; y : float
    }

  let jsont =
    Jsont.Object.map ~kind:"user_cursor" (fun user_id color x y ->
      { user_id; color; x; y })
    |> Jsont.Object.mem ~enc:(fun c -> c.user_id) "user_id" Jsont.string
    |> Jsont.Object.mem ~enc:(fun c -> c.color) "color" Jsont.string
    |> Jsont.Object.mem ~enc:(fun c -> c.x) "x" Jsont.number
    |> Jsont.Object.mem ~enc:(fun c -> c.y) "y" Jsont.number
    |> Jsont.Object.finish
end

module User_meta = struct
  type t =
    { user_id : string
    ; color : string
    }

  let jsont : t Jsont.t =
    Jsont.Object.map ~kind:"user_meta" (fun user_id color -> { user_id; color })
    |> Jsont.Object.mem ~enc:(fun u -> u.user_id) "user_id" Jsont.string
    |> Jsont.Object.mem ~enc:(fun u -> u.color) "color" Jsont.string
    |> Jsont.Object.finish
end

module Cursor_channel : Tapak.CHANNEL = struct
  type t =
    { user_id : string
    ; color : string
    ; phx_ref : string
    }

  let init () = { user_id = ""; color = ""; phx_ref = "" }

  let join ~topic ~payload:_ ~socket _state =
    if String.equal topic "cursors:lobby"
    then (
      let user_id = Tapak.Socket.find_assign_exn user_id_key socket in
      let color = Tapak.Socket.find_assign_exn user_color_key socket in

      Logs.info (fun m -> m "User %s joining cursors:lobby" user_id);

      let meta =
        Jsont.Json.encode User_meta.jsont User_meta.{ user_id; color }
        |> Result.get_ok
      in
      let phx_ref = Tapak.Channel.track_presence ~key:user_id ~meta in

      Logs.info (fun m ->
        m "Tracked presence for user %s with phx_ref %s" user_id phx_ref);

      let state = { user_id; color; phx_ref } in
      let ctx : t Tapak.Channel.ctx = { state; socket } in
      Tapak.Channel.Join.ok ctx (Jsont.Json.object' []))
    else
      Tapak.Channel.Join.error
        Jsont.Json.(object' [ mem (name "reason") (string "invalid topic") ])

  let handle_in ~event ~payload ~socket (state : t) =
    match event with
    | "cursor_move" ->
      let point = Jsont.Json.decode Point.jsont payload in
      (match point with
      | Ok point ->
        let broadcast =
          User_cursor.
            { user_id = state.user_id
            ; color = state.color
            ; x = point.x
            ; y = point.y
            }
          |> Jsont.Json.encode User_cursor.jsont
          |> Result.get_ok
        in
        Tapak.Channel.broadcast_from
          ~topic:"cursors:lobby"
          ~event:"cursor_update"
          ~payload:broadcast;

        Tapak.Channel.Reply.ok
          { state; socket }
          Jsont.Json.(object' [ mem (name "status") (string "ok") ])
      | Error _ -> Tapak.Channel.Reply.noop { state; socket })
    | _ -> Tapak.Channel.Reply.noop { state; socket }

  let handle_info (msg : Tapak.Channel.broadcast) ~socket state =
    let ctx : t Tapak.Channel.ctx = { state; socket } in
    Tapak.Channel.Push.push ctx msg.payload

  let handle_out ~event:_ ~payload ~socket state =
    let ctx : t Tapak.Channel.ctx = { state; socket } in
    Tapak.Channel.Push.push ctx payload

  let terminate ~reason:_ ~socket:_ (state : t) =
    Logs.info (fun m ->
      m "User %s terminating (phx_ref: %s)" state.user_id state.phx_ref)

  let intercept = []
end

let serve_static_file root path =
  let file_path = "examples/live-cursors/static/" ^ path in
  Tapak.Response.file Eio.Path.(root / file_path)

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
  let pubsub = Tapak.Pubsub.local ~sw in
  let clock = Eio.Stdenv.clock env in
  let node_name = Printf.sprintf "node-%d" (Random.int 10000) in
  let presence = Tapak.Presence.create ~sw ~pubsub ~node_name ~clock () in
  let now () = Eio.Time.now clock in
  let root = Eio.Stdenv.fs env in

  let endpoint =
    Tapak.Socket.Endpoint.(
      create
        ~socket:(module Cursor_socket)
        ~channels:[ channel "^cursors:.*$" (module Cursor_channel) ]
        ~pubsub
        ~presence
        ~clock
        ())
  in

  let all_routes =
    Tapak.Router.
      [ get (s "")
        |> unit
        |> into (fun () -> serve_static_file root "index.html")
      ; get (s "static" / str) |> into (serve_static_file root)
      ; Tapak.Socket.mount (s "socket") endpoint
      ]
  in
  let app =
    Tapak.(
      Router.of_list all_routes
      |> use
           (module Middleware.Request_logger)
           (Middleware.Request_logger.args ~now ~trusted_proxies:[] ()))
  in

  let port = 8080 in
  let address = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let socket =
    Eio.Net.listen ~reuse_addr:true ~backlog:1024 ~sw env#net address
  in

  match domains with
  | domains when domains > 1 ->
    let domain_mgr = Eio.Stdenv.domain_mgr env in
    Log.info (fun f ->
      f "Server listening on port %d with domains %d" port domains);
    Tapak.run
      ~additional_domains:(domain_mgr, domains)
      ~on_error:(fun exn ->
        Log.warn (fun f -> f "Uncaught exception %s" (Printexc.to_string exn)))
      socket
      app
  | _ ->
    Log.info (fun f -> f "Server listening on port %d" port);
    Tapak.run
      ~on_error:(fun exn ->
        Logs.warn (fun f -> f "Uncaught exception %s" (Printexc.to_string exn)))
      socket
      app
