open Imports
open Bytesrw
module Log = (val Logs.src_log (Logs.Src.create "tapak.websocket"))

module Outgoing = struct
  module Json = Jsont.Json

  type t = Socket_protocol.t

  let message ?join_ref ?ref_ ~topic ~event payload =
    Socket_protocol.{ join_ref; ref_; topic; event; payload }

  let reply_payload ~status ~response =
    Json.(
      object'
        [ mem (name "status") (string status); mem (name "response") response ])

  let reply ?join_ref ?ref_ ~topic ~status response =
    message
      ?join_ref
      ?ref_
      ~topic
      ~event:Socket_protocol.phx_reply
      (reply_payload ~status ~response)

  let heartbeat (msg : Socket_protocol.t) =
    let open Option.Syntax in
    reply
      ?join_ref:msg.join_ref
      ?ref_:(msg.ref_ <|> Some "")
      ~topic:msg.topic
      ~status:"ok"
      (Json.null ())

  let join_ok ~topic ~join_ref ~ref_ ~response =
    reply ~join_ref ~ref_ ~topic ~status:"ok" response

  let join_error ?join_ref ?ref_ ~topic reason =
    reply ?join_ref ?ref_ ~topic ~status:"error" reason

  let push ?join_ref ?ref_ ~topic ~event payload =
    message ?join_ref ?ref_ ~topic ~event payload

  let presence_state ~topic payload =
    push ~topic ~event:"presence_state" payload

  let phx_error ~topic reason =
    message ~topic ~event:Socket_protocol.phx_error (Json.string reason)

  let not_joined topic = phx_error ~topic "not joined to channel"

  let reply_error ?join_ref ?ref_ ~topic reason =
    let response = Json.(object' [ mem (name "reason") (string reason) ]) in
    reply ?join_ref ?ref_ ~topic ~status:"error" response
end

module Transport = struct
  let send ?context ws msg =
    match Jsont_bytesrw.encode_string Socket_protocol.jsont msg with
    | Ok encoded -> Piaf.Ws.Descriptor.send_string ws encoded
    | Error e ->
      let what = Option.value context ~default:"websocket message" in
      Log.err (fun m -> m "Failed to encode %s: %s" what e)
end

module Session = struct
  type t =
    { config : Socket_endpoint.t
    ; socket : Socket.t ref
    ; channels : (string, Socket_endpoint.channel_state) Hashtbl.t
    ; subscriptions : (string, int) Hashtbl.t
    ; broadcast_queue : Pubsub.message Eio.Stream.t
    ; last_heartbeat : float ref
    ; closed : bool ref
    ; ws : Piaf.Ws.Descriptor.t
    ; active_sockets : (string, Piaf.Ws.Descriptor.t list) Hashtbl.t
    ; active_sockets_mutex : Eio.Mutex.t
    }

  let create ~config ~socket ~ws ~sockets ~mutex =
    { config
    ; socket
    ; channels = Hashtbl.create 8
    ; subscriptions = Hashtbl.create 8
    ; broadcast_queue = Eio.Stream.create 100
    ; last_heartbeat = ref (Eio.Time.now config.clock)
    ; closed = ref false
    ; ws
    ; active_sockets = sockets
    ; active_sockets_mutex = mutex
    }

  let handle_heartbeat session (msg : Socket_protocol.t) =
    session.last_heartbeat := Eio.Time.now session.config.clock;
    Transport.send
      ~context:"heartbeat reply"
      session.ws
      (Outgoing.heartbeat msg)

  let push_message ws (broadcast : Channel.broadcast) =
    let message =
      Outgoing.push
        ~topic:broadcast.topic
        ~event:broadcast.event
        broadcast.payload
    in
    Transport.send ~context:"channel broadcast" ws message

  let run_with_effect ?subscription ?(topic = "") ~presence_refs session cb =
    let effect_ctx =
      Socket_endpoint.Effect_handler.
        { pubsub = session.config.pubsub
        ; presence = session.config.presence
        ; push = push_message session.ws
        ; topic
        ; presence_refs
        ; subscription
        }
    in
    Socket_endpoint.Effect_handler.run effect_ctx cb

  let handle_join_channel
        (module H : Channel.S)
        session
        (msg : Socket_protocol.t)
    =
    let open Option.Syntax in
    let topic = msg.topic in
    let state = H.init () in
    let sub =
      Pubsub.subscribe session.config.pubsub topic (fun pmsg ->
        Eio.Stream.add session.broadcast_queue pmsg)
    in
    Hashtbl.replace session.subscriptions topic sub;
    let presence_refs_acc = ref [] in
    let join_result =
      try
        run_with_effect
          ~subscription:sub
          ~topic
          ~presence_refs:presence_refs_acc
          session
          (fun () ->
             H.join ~topic ~payload:msg.payload ~socket:!(session.socket) state)
      with
      | exn ->
        Log.err (fun m ->
          m "Channel join topic %s crashed: %s" topic (Printexc.to_string exn));
        Channel.Join.error (Jsont.Json.string "join crashed")
    in
    match join_result with
    | Channel.Join.Ok { transition; response } ->
      let updated_socket =
        { transition.socket with
          joined_topics = topic :: transition.socket.joined_topics
        }
      in
      session.socket := updated_socket;
      let join_ref = Option.value msg.join_ref ~default:"" in
      let chan_state =
        Socket_endpoint.Channel_state
          { channel = (module H)
          ; state = transition.state
          ; topic
          ; join_ref
          ; presence_refs = !presence_refs_acc
          }
      in
      Hashtbl.replace session.channels topic chan_state;
      Outgoing.join_ok
        ~topic
        ~join_ref
        ~ref_:(Option.value ~default:"" msg.ref_)
        ~response
      |> Transport.send ~context:"channel join ok" session.ws;
      (* push presence state after join *)
      session.config.presence
      |> Option.iter (fun presence ->
        Presence.list presence ~topic
        |> Presence.state_to_json
        |> Outgoing.presence_state ~topic
        |> Transport.send ~context:"presence state" session.ws)
    | Error { reason } ->
      Pubsub.unsubscribe session.config.pubsub sub;
      Hashtbl.remove session.subscriptions topic;
      Outgoing.join_error
        ?join_ref:msg.join_ref
        ?ref_:(msg.ref_ <|> Some "")
        ~topic
        reason
      |> Transport.send ~context:"channel join error" session.ws

  let handle_join session (msg : Socket_protocol.t) =
    let topic = msg.topic in
    match Socket_endpoint.find_channel session.config.channels topic with
    | Some (module H) -> handle_join_channel (module H) session msg
    | None ->
      Outgoing.phx_error ~topic "no channel"
      |> Transport.send ~context:"channel lookup error" session.ws

  let handle_leave session (msg : Socket_protocol.t) =
    let open Option.Syntax in
    let topic = msg.topic in
    Hashtbl.find_opt session.channels topic
    |> Option.iter
         (fun
             (Socket_endpoint.Channel_state
                { channel = (module H); state; presence_refs; _ })
            ->
            session.config.presence
            |> Option.iter (fun presence ->
              presence_refs
              |> List.iter (fun (_, phx_ref) ->
                Presence.untrack_ref presence ~topic ~phx_ref));
            H.terminate ~reason:Left ~socket:!(session.socket) state;
            Hashtbl.remove session.channels topic;
            let current_socket = !(session.socket) in
            let updated_socket =
              { current_socket with
                joined_topics =
                  List.filter
                    (fun t -> not (String.equal t topic))
                    current_socket.joined_topics
              }
            in
            session.socket := updated_socket;
            Hashtbl.find_opt session.subscriptions topic
            |> Option.iter (fun sub ->
              Pubsub.unsubscribe session.config.pubsub sub;
              Hashtbl.remove session.subscriptions topic);
            Outgoing.reply
              ?join_ref:msg.join_ref
              ?ref_:(msg.ref_ <|> Some "")
              ~topic
              ~status:"ok"
              Jsont.Json.(object' [])
            |> Transport.send ~context:"channel leave" session.ws)

  let handle_custom_event session (msg : Socket_protocol.t) =
    let open Option.Syntax in
    let topic = msg.topic in
    match Hashtbl.find_opt session.channels topic with
    | Some
        (Socket_endpoint.Channel_state
           { channel = (module H); state; join_ref; presence_refs; _ }) ->
      let current_presence_refs = ref presence_refs in
      let subscription = Hashtbl.find_opt session.subscriptions topic in
      let handle_result =
        try
          run_with_effect
            ?subscription
            ~topic
            ~presence_refs:current_presence_refs
            session
            (fun () ->
               Result.ok
               @@ H.handle_in
                    ~event:msg.event
                    ~payload:msg.payload
                    ~socket:!(session.socket)
                    state)
        with
        | exn ->
          Log.err (fun m ->
            m
              "Channel handle_in topic %s event %s crashed: %s"
              topic
              msg.event
              (Printexc.to_string exn));
          Result.error "handle in crashed"
      in
      (match handle_result with
      | Ok (Channel.Reply.Respond { transition; status; payload }) ->
        session.socket := transition.socket;
        Hashtbl.replace
          session.channels
          topic
          (Socket_endpoint.Channel_state
             { channel = (module H)
             ; state = transition.state
             ; topic
             ; join_ref
             ; presence_refs = !current_presence_refs
             });
        Outgoing.reply
          ?join_ref:(Some join_ref)
          ?ref_:(msg.ref_ <|> Some "")
          ~topic
          ~status:(Channel.Reply.string_of_status status)
          payload
        |> Transport.send ~context:"channel respond" session.ws
      | Ok (Channel.Reply.Noop transition) ->
        session.socket := transition.socket;
        Hashtbl.replace
          session.channels
          topic
          (Socket_endpoint.Channel_state
             { channel = (module H)
             ; state = transition.state
             ; topic
             ; join_ref
             ; presence_refs = !current_presence_refs
             })
      | Ok (Channel.Reply.Stop { transition; reason }) ->
        session.socket := transition.socket;
        H.terminate
          ~reason:(Error (Failure reason))
          ~socket:!(session.socket)
          transition.state;
        Hashtbl.remove session.channels topic
      | Error reason ->
        Log.err (fun m -> m "Channel handler for topic %s crashed" topic);
        Outgoing.reply_error
          ?join_ref:(Some join_ref)
          ?ref_:(msg.ref_ <|> Some "")
          ~topic
          reason
        |> Transport.send ~context:"channel handler error" session.ws)
    | None ->
      Log.warn (fun m -> m "Channel not joined for topic %s" topic);
      Outgoing.not_joined topic
      |> Transport.send ~context:"channel not joined" session.ws

  let handle_ws_message session (msg : Socket_protocol.t) =
    match msg.event with
    | event when String.equal event Socket_protocol.heartbeat ->
      handle_heartbeat session msg
    | event when String.equal event Socket_protocol.phx_join ->
      handle_join session msg
    | event when String.equal event Socket_protocol.phx_leave ->
      handle_leave session msg
    | _ -> handle_custom_event session msg

  let cleanup session =
    session.closed := true;
    session.channels
    |> Hashtbl.iter
         (fun
             topic
              (Socket_endpoint.Channel_state
                 { channel = (module H); state; presence_refs; _ })
            ->
            session.config.presence
            |> Option.iter (fun presence ->
              presence_refs
              |> List.iter (fun (_, phx_ref) ->
                Presence.untrack_ref presence ~topic ~phx_ref));
            H.terminate ~reason:Closed ~socket:!(session.socket) state);
    session.subscriptions
    |> Hashtbl.iter (fun _ sub -> Pubsub.unsubscribe session.config.pubsub sub);
    Socket.id !(session.socket)
    |> Option.iter (fun id ->
      Eio.Mutex.use_rw ~protect:true session.active_sockets_mutex (fun () ->
        Hashtbl.find_opt session.active_sockets id
        |> Option.iter (fun wss ->
          let remaining = List.filter (fun w -> w != session.ws) wss in
          if List.length remaining = 0
          then Hashtbl.remove session.active_sockets id
          else Hashtbl.replace session.active_sockets id remaining));
      session.config.presence
      |> Option.iter (fun presence ->
        Presence.broadcast_disconnect presence ~socket_id:id))

  let process_broadcast session pubsubmsg =
    let topic = pubsubmsg.Pubsub.topic in
    let send_push payload =
      Outgoing.push ~topic ~event:pubsubmsg.event payload
      |> Transport.send ~context:"broadcast push" session.ws
    in
    Hashtbl.find_opt session.channels topic
    |> Option.iter
         (fun
             (Socket_endpoint.Channel_state
                { channel = (module H); state; join_ref; presence_refs; _ })
            ->
            let broadcast_msg =
              Channel.
                { topic; event = pubsubmsg.event; payload = pubsubmsg.payload }
            in
            let push =
              H.handle_info broadcast_msg ~socket:!(session.socket) state
            in
            session.socket := Channel.Push.socket push;
            Hashtbl.replace
              session.channels
              topic
              (Socket_endpoint.Channel_state
                 { channel = (module H)
                 ; state = Channel.Push.state push
                 ; topic
                 ; join_ref
                 ; presence_refs
                 });
            let should_intercept = List.mem pubsubmsg.event H.intercept in
            match push with
            | Channel.Push.Push { transition; payload } ->
              if should_intercept
              then (
                let push_out =
                  H.handle_out
                    ~event:pubsubmsg.event
                    ~payload
                    ~socket:!(session.socket)
                    transition.state
                in
                session.socket := Channel.Push.socket push_out;
                Hashtbl.replace
                  session.channels
                  topic
                  (Socket_endpoint.Channel_state
                     { channel = (module H)
                     ; state = Channel.Push.state push_out
                     ; topic
                     ; join_ref
                     ; presence_refs
                     });
                match push_out with
                | Channel.Push.Push { payload; _ } | Intercept { payload; _ } ->
                  send_push payload
                | Suppress _ -> ())
              else send_push payload
            | Intercept { payload; transition = { state; socket } } ->
              let push_out =
                H.handle_out
                  ~event:pubsubmsg.event
                  ~payload
                  ~socket:!(session.socket)
                  state
              in
              session.socket := socket;
              Hashtbl.replace
                session.channels
                topic
                (Socket_endpoint.Channel_state
                   { channel = (module H)
                   ; state = Channel.Push.state push_out
                   ; topic
                   ; join_ref
                   ; presence_refs
                   });
              (match push_out with
              | Channel.Push.Push { payload; _ } | Intercept { payload; _ } ->
                send_push payload
              | Suppress _ -> ())
            | Suppress _ -> ())

  let run_message_loop session =
    let messages = Piaf.Ws.Descriptor.messages session.ws in
    let process_frame (opcode, iovec) =
      match opcode with
      | `Text | `Binary ->
        let buf = iovec.Piaf.IOVec.buffer in
        let off = iovec.Piaf.IOVec.off in
        let len = iovec.Piaf.IOVec.len in
        let reader =
          Bytes.Reader.of_slice
            (Bytes.Slice.of_bigbytes
               ~first:off
               ~last:(off + len)
               (Bytesrw_util.bigstring_to_bigbytes buf))
        in
        Result.iter
          (handle_ws_message session)
          (Jsont_bytesrw.decode Socket_protocol.jsont reader)
      | `Connection_close ->
        cleanup session;
        Piaf.Ws.Descriptor.close session.ws
      | _ -> ()
    in
    Piaf.Stream.iter ~f:process_frame messages;
    cleanup session

  let run_broadcast_loop session =
    try
      while not !(session.closed) do
        let msg = Eio.Stream.take session.broadcast_queue in
        if not !(session.closed) then process_broadcast session msg
      done
    with
    | exn ->
      Log.err (fun m -> m "Broadcast loop crashed: %s" (Printexc.to_string exn))

  let run_heartbeat_monitor session =
    try
      while not !(session.closed) do
        Eio.Time.sleep session.config.clock session.config.heartbeat_interval;
        let now = Eio.Time.now session.config.clock in
        let elapsed = now -. !(session.last_heartbeat) in
        if elapsed > session.config.timeout
        then (
          Log.info (fun m ->
            m
              "Socket timeout: no heartbeat for %.2f seconds (threshold: %.2f \
               seconds)"
              elapsed
              session.config.timeout);
          cleanup session;
          Piaf.Ws.Descriptor.close session.ws)
      done
    with
    | exn ->
      Log.err (fun m ->
        m "Heartbeat monitor crashed: %s" (Printexc.to_string exn))
end

type socket_state =
  { active_sockets : (string, Piaf.Ws.Descriptor.t list) Hashtbl.t
  ; active_sockets_mutex : Eio.Mutex.t
  }

let socket_state_key : socket_state Context.key =
  Context.Key.create
    { name = Some "socket_state"
    ; show =
        Some
          (fun state ->
            Format.sprintf
              "socket state: %d"
              (Hashtbl.length state.active_sockets))
    }

let name = "websocket"

let ws_handler state config request ws =
  let (module S : Socket.S) = config.Socket_endpoint.socket in
  let params = Form.Urlencoded.of_query request in
  let connect_info = Socket.{ params; request } in
  match S.connect connect_info with
  | Error _ -> Piaf.Ws.Descriptor.close ws
  | Ok initial_assigns ->
    let socket_id = S.id initial_assigns in
    let socket =
      ref
        Socket.
          { id = socket_id
          ; assigns = initial_assigns
          ; transport = name
          ; joined_topics = []
          }
    in
    socket_id
    |> Option.iter (fun id ->
      Eio.Mutex.use_rw ~protect:true state.active_sockets_mutex (fun () ->
        let existing =
          Hashtbl.find_opt state.active_sockets id |> Option.value ~default:[]
        in
        Hashtbl.replace state.active_sockets id (ws :: existing)));
    let session =
      Session.create
        ~config
        ~socket
        ~ws
        ~sockets:state.active_sockets
        ~mutex:state.active_sockets_mutex
    in
    Eio.Fiber.all
      [ (fun () -> Session.run_message_loop session)
      ; (fun () -> Session.run_broadcast_loop session)
      ; (fun () -> Session.run_heartbeat_monitor session)
      ]

let handler : socket_state -> Socket_endpoint.t -> Request.t -> Response.t =
 fun state config request ->
  match
    Response.Upgrade.websocket ~f:(ws_handler state config request) request
  with
  | Ok response -> response
  | Error _ ->
    Response.of_string ~body:"Websocket upgrade failed" `Internal_server_error

let routes : Socket_endpoint.t -> Router.route list =
 fun config ->
  let state =
    { active_sockets = Hashtbl.create 16
    ; active_sockets_mutex = Eio.Mutex.create ()
    }
  in
  config.context <- Context.add socket_state_key state config.context;
  Router.[ get (s "websocket") |> request |> into (handler state config) ]

let shutdown : Socket_endpoint.t -> Socket.t option -> unit =
 fun config msocket ->
  ignore
  @@
  let open Option.Syntax in
  let* state = Context.find socket_state_key config.context in
  let* socket_id = Option.bind msocket Socket.id in
  let* wss_to_close =
    Eio.Mutex.use_rw ~protect:true state.active_sockets_mutex (fun () ->
      Hashtbl.find_opt state.active_sockets socket_id)
  in
  Option.some (List.iter Piaf.Ws.Descriptor.close wss_to_close)
