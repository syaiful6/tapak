module Topic : sig
  type t = string
  (** A topic string, e.g., "room:lobby" or "user:123" *)

  type (_, _) pattern

  val s : string -> ('a, 'a) pattern
  (** Literal string segment. Matches exactly the given string.
  *)

  val str : (string -> 'a, 'a) pattern
  (** String parameter segment. Captures any string value. *)

  val int : (int -> 'a, 'a) pattern
  (** Integer parameter segment. Parses and captures an
      integer. *)

  val int64 : (int64 -> 'a, 'a) pattern
  (** Int64 parameter segment. Parses and captures a 64-bit
      integer. *)

  val ( / ) : ('a, 'b) pattern -> ('b, 'c) pattern -> ('a, 'c) pattern
  (** Concatenate patterns with ":" separator.

      {[
        let user_room = Topic.(s "user" / int / s "room" / str)
        (* Matches "user:123:room:lobby" *)
      ]} *)

  (** {2 Pattern Operations} *)

  val sprintf : ('a, t) pattern -> 'a
  (** Build a topic string from a pattern and arguments.

      {[
        let room_topic = Topic.(s "room" / str)
        let topic = Topic.sprintf room_topic "lobby"
        (* topic = "room:lobby" *)
      ]} *)

  val sscanf : ('a, 'b) pattern -> t -> ('a -> 'b) option
  (** Match a topic string against a pattern, extracting
      parameters. Returns [None] if the topic doesn't match the
      pattern.

      {[
        let room_topic = Topic.(s "room" / str)
        match Topic.sscanf room_topic "room:lobby" with
        | Some f -> f (fun room_id -> room_id)  (* "lobby" *)
        | None -> failwith "no match"
      ]} *)

  val matches : (_, _) pattern -> t -> bool
  (** Check if a topic matches a pattern *)
end

(** {1 PubSub}

    Publish-subscribe messaging system for broadcasting
    messages across the application. *)
module Pubsub : sig
  type message =
    { topic : Topic.t
    ; event : string
    ; payload : Yojson.Safe.t
    }
  (** A broadcast message *)

  (** {2 Backend Interface}

      Implement this signature to create custom PubSub
      backends. *)
  module type BACKEND = sig
    type t

    type subscription
    (** Backend-specific subscription identifier *)

    val start : sw:Eio.Switch.t -> t
    (** Start the backend within the given switch *)

    val subscribe : t -> Topic.t -> (message -> unit) -> subscription
    (** Subscribe to a topic. The callback is invoked for each
        message. *)

    val unsubscribe : t -> subscription -> unit
    (** Unsubscribe from a topic *)

    val broadcast : t -> message -> unit
    (** Broadcast a message to all subscribers of the topic *)

    val node_name : t -> string
    (** Get the node identifier (for distributed setups) *)
  end

  (** {2 PubSub Instance} *)

  type t
  (** PubSub instance type *)

  val create :
     (module BACKEND with type t = 'a and type subscription = 'b)
    -> 'a
    -> t
  (** Create a PubSub instance with the given backend *)

  val subscribe : t -> Topic.t -> (message -> unit) -> int
  (** Subscribe to a topic *)

  val unsubscribe : t -> int -> unit
  (** Unsubscribe *)

  val broadcast : t -> message -> unit
  (** Broadcast to all subscribers *)

  val broadcast_from : t -> self:int -> message -> unit
  (** Broadcast to all subscribers except those matching the
      predicate *)

  val direct_broadcast : t -> int -> message -> unit
  (** Direct message to a specific subscription *)

  val node_name : t -> string
  (** Get node name *)

  module Local : BACKEND with type subscription = int
  (** {2 Local Backend}

      In-memory PubSub backend using EIO streams. Suitable for
      single-node deployments or development. *)
end

(** {1 Socket}

    Represents a persistent connection (WebSocket or LongPoll)
    from a client. Sockets can have multiple channels
    multiplexed over them. *)
module Socket : sig
  type id = string option
  (** Socket identifier for targeted disconnects *)

  (** Transport type *)
  type transport =
    | Websocket
    | Longpoll

  type t
  (** Socket state *)

  type connect_info =
    { params : Yojson.Safe.t
    ; request : Tapak_kernel.Request.t
    }
  (** Connection parameters passed during socket connection *)

  (** {2 Socket Handler}

      Implement this signature to handle socket connections. *)
  module type HANDLER = sig
    val connect : connect_info -> (Tapak_kernel.Context.t, string) result
    (** Called when a client attempts to connect. Return
        [Ok assigns] to accept the connection with initial
        assigns, or [Error reason] to reject. *)

    val id : Tapak_kernel.Context.t -> id
    (** Generate a unique identifier for this socket. Used for
        targeted disconnects (e.g., "user_socket:123"). Return
        [None] for anonymous sockets. *)
  end

  (** {2 Socket Operations} *)

  val assigns : t -> Tapak_kernel.Context.t
  (** Get socket assigns (type-safe context) *)

  val assign : 'a Tapak_kernel.Context.key -> 'a -> t -> t
  (** Update socket assigns *)

  val get_assign : 'a Tapak_kernel.Context.key -> t -> 'a option
  (** Get a value from assigns *)

  val get_assign_exn : 'a Tapak_kernel.Context.key -> t -> 'a
  (** Get a value from assigns, raising if not found *)

  val id : t -> id
  (** Get socket identifier *)

  val transport : t -> transport
  (** Get transport type *)

  val joined_topics : t -> Topic.t list
  (** Get list of joined topics *)
end

(** {1 Channel}

    Channels are the primary abstraction for real-time
    communication. Each channel handles a specific topic
    pattern and manages its own state. *)
module Channel : sig
  (** {2 Response Types} *)

  (** Reply status for client responses *)
  type reply_status =
    | Ok
    | Error

  (** Response from [handle_in] callback *)
  type 'state reply =
    | Reply of reply_status * Yojson.Safe.t * 'state * Socket.t
    (** Send a reply to the client *)
    | No_reply of 'state * Socket.t  (** No reply needed *)
    | Stop of string * 'state * Socket.t  (** Stop the channel with reason *)

  type broadcast_msg =
    { topic : Topic.t
    ; event : string
    ; payload : Yojson.Safe.t
    }
  (** Broadcast message received via [handle_info] *)

  (** Control for outgoing messages *)
  type push_control =
    | Push of Yojson.Safe.t  (** Send the message *)
    | Intercept of Yojson.Safe.t  (** Send modified message *)
    | Suppress  (** Don't send the message *)

  (** Termination reason *)
  type terminate_reason =
    | Normal  (** Clean shutdown *)
    | Left  (** Client left the channel *)
    | Closed  (** Connection closed *)
    | Error of exn  (** Error occurred *)

  (** {2 Channel Handler}

      Implement this signature to create a channel. *)
  module type HANDLER = sig
    type state
    (** Channel-specific state type *)

    val init : unit -> state
    (** Initial state factory *)

    val join :
       topic:Topic.t
      -> payload:Yojson.Safe.t
      -> socket:Socket.t
      -> state
      -> (state * Socket.t * Yojson.Safe.t, Yojson.Safe.t) result
    (** Called when a client joins the channel.

        @param topic
          The topic being joined (e.g., "room:lobby")
        @param payload Join parameters from client
        @param socket The socket state
        @param state Current channel state
        @return
          [Ok (state, socket, response)] to accept with
          response payload, [Error payload] to reject *)

    val handle_in :
       event:string
      -> payload:Yojson.Safe.t
      -> socket:Socket.t
      -> state
      -> state reply
    (** Handle incoming client events.

        @param event Event name (e.g., "new_msg")
        @param payload Event data
        @param socket Socket state
        @param state Channel state
        @return Response with updated state *)

    val handle_info :
       broadcast_msg
      -> socket:Socket.t
      -> state
      -> push_control * state * Socket.t
    (** Handle PubSub broadcasts. Called when a message is
        broadcast to this channel's topic. *)

    val handle_out :
       event:string
      -> payload:Yojson.Safe.t
      -> socket:Socket.t
      -> state
      -> push_control * state * Socket.t
    (** Intercept outgoing broadcasts before delivery. Only
        called for events listed in [intercept]. *)

    val terminate : reason:terminate_reason -> socket:Socket.t -> state -> unit
    (** Cleanup when channel terminates *)

    val intercept : string list
    (** List of events to intercept via [handle_out] *)
  end

  (** {2 Default Implementations}

      Sensible defaults for optional callbacks *)
  module Default : sig
    val handle_info :
       broadcast_msg
      -> socket:Socket.t
      -> 'a
      -> push_control * 'a * Socket.t

    val handle_out :
       string
      -> Yojson.Safe.t
      -> socket:Socket.t
      -> 'a
      -> push_control * 'a * Socket.t

    val terminate : reason:terminate_reason -> socket:Socket.t -> 'a -> unit
    val intercept : string list
  end

  type context
  (** {2 Channel Context}

      Context for performing channel operations within
      handlers. Passed implicitly via effect handlers or
      explicitly. *)

  val push_ctx : context -> event:string -> payload:Yojson.Safe.t -> unit
  (** Push an unsolicited message to the client *)

  val broadcast_ctx :
     context
    -> topic:Topic.t
    -> event:string
    -> payload:Yojson.Safe.t
    -> unit
  (** Broadcast to all subscribers of the topic *)

  (** {2 Effect-based API}

      These functions use OCaml effects to perform operations
      within handlers. The runtime automatically handles the
      effects when called from within [handle_in],
      [handle_info], or [handle_out] callbacks. *)

  val broadcast : topic:Topic.t -> event:string -> payload:Yojson.Safe.t -> unit
  (** Broadcast a message to all subscribers of a topic.

      {[
        let handle_in ~event ~payload ~socket state =
          Channel.broadcast ~topic:"room:lobby"
            ~event:"new_msg" ~payload;
          Reply (Ok, `Null, state, socket)
      ]} *)

  val broadcast_from :
     topic:Topic.t
    -> event:string
    -> payload:Yojson.Safe.t
    -> unit
  (** Broadcast a message to all subscribers except the current
      channel. This is useful for "echo to others" patterns
      where you don't want the sender to receive their own message.

      {[
        let handle_in ~event ~payload ~socket state =
          (* Broadcast to everyone else *)
          Channel.broadcast_from ~topic:"room:lobby"
            ~event:"new_msg" ~payload;
          Reply (Ok, `Null, state, socket)
      ]} *)

  val push : event:string -> payload:Yojson.Safe.t -> unit
  (** Push an unsolicited message to the current client. *)

  val track_presence : key:string -> meta:Yojson.Safe.t -> string
  (** Track presence for the current channel's topic. Returns
      the phx_ref for this presence instance. The key is
      typically a user ID, and meta contains presence data.
      This automatically broadcasts a presence_diff event.

      {[
        let join ~topic ~payload ~socket state =
          let user_id = get_user_id socket in
          let meta =
            `Assoc
              [
                ( "online_at",
                  `Int (Unix.time () |> int_of_float) );
              ]
          in
          let phx_ref =
            Channel.track_presence ~key:user_id ~meta
          in
          Ok ({ state with phx_ref }, socket)
      ]} *)

  val untrack_presence : phx_ref:string -> unit
  (** Untrack a specific presence instance by its phx_ref. This
      automatically broadcasts a presence_diff event. *)

  val get_presence_list : unit -> Yojson.Safe.t
  (** Get the current presence list for the channel's topic as
      JSON. *)
end

(** {1 Presence}

    Distributed presence tracking with automatic synchronization
    across multiple nodes via the PubSub backend. Uses Last-Write-Wins
    (LWW) conflict resolution and heartbeat-based node health monitoring.

    Useful for showing online users, typing indicators, etc. *)
module Presence : sig
  type meta = Yojson.Safe.t
  (** Presence metadata (arbitrary JSON) *)

  type entry = { metas : meta list  (** Multiple connections per key *) }
  (** Presence entry for a single key (e.g., user) *)

  type state
  (** Presence state - map of key to entry *)

  type diff =
    { joins : (string * entry) list
    ; leaves : (string * entry) list
    }
  (** Presence diff for synchronization *)

  type t
  (** Distributed presence tracker instance, parameterized by PubSub
      subscription type *)

  val create :
     sw:Eio.Switch.t
    -> pubsub:Pubsub.t
    -> node_name:string
    -> clock:float Eio.Time.clock_ty Eio.Resource.t
    -> ?broadcast_period:float
    -> ?down_period:float
    -> unit
    -> t
  (** Create a distributed presence tracker.

      @param sw Eio switch for managing fibers
      @param pubsub PubSub backend for cross-node communication
      @param node_name Unique identifier for this node
      @param clock Eio time source
      @param broadcast_period Heartbeat interval in seconds (default 1.5)
      @param down_period Time before marking node as down (default 30.0) *)

  val track : t -> topic:Topic.t -> key:string -> meta:meta -> string
  (** Track a presence entry. Returns the phx_ref for this
      specific presence instance. The phx_ref is automatically
      added to the meta and can be used to untrack this
      specific instance later. Broadcasts to all nodes. *)

  val untrack_ref : t -> topic:Topic.t -> phx_ref:string -> unit
  (** Untrack a specific presence instance by its phx_ref.
      Broadcasts to all nodes. *)

  val list : t -> topic:Topic.t -> state
  (** Get merged presence state from all nodes for a topic *)

  val broadcast_disconnect : t -> socket_id:string -> unit
  (** Broadcast a disconnect command to all nodes. Any node with
      this socket_id will close its WebSocket connection. *)

  val on_disconnect : t -> (string -> unit) -> unit
  (** Register a handler for disconnect commands from other nodes *)

  val close : t -> unit
  (** Stop the tracker's heartbeat loop *)

  val node_name : t -> string
  (** Get this tracker's node name *)

  val diff : old_state:state -> new_state:state -> diff
  (** Compute diff between two states *)

  val to_json : state -> Yojson.Safe.t
  (** Encode presence state as JSON (for sending to clients) *)

  val diff_to_json : diff -> Yojson.Safe.t
  (** Encode presence diff as JSON *)
end

(** {1 Protocol}

    Phoenix-compatible wire protocol for WebSocket
    communication. *)
module Protocol : sig
  type message =
    { join_ref : string option  (** Reference for join lifecycle *)
    ; ref_ : string option  (** Message reference for replies *)
    ; topic : string  (** Topic name *)
    ; event : string  (** Event name *)
    ; payload : Yojson.Safe.t  (** Message payload *)
    }
  (** Wire protocol message format *)

  (** {2 Reserved Events} *)

  val phx_join : string
  (** "phx_join" *)

  val phx_leave : string
  (** "phx_leave" *)

  val phx_reply : string
  (** "phx_reply" *)

  val phx_error : string
  (** "phx_error" *)

  val phx_close : string
  (** "phx_close" *)

  val heartbeat : string
  (** "heartbeat" *)

  (** {2 Serialization} *)

  val encode_json : message -> string
  (** Encode message to JSON string (Phoenix format) *)

  val decode_json : string -> (message, string) result
  (** Decode JSON string to message *)

  val make_reply :
     join_ref:string option
    -> ref_:string
    -> topic:string
    -> status:Channel.reply_status
    -> payload:Yojson.Safe.t
    -> message
  (** Create a reply message *)

  val make_push :
     topic:string
    -> event:string
    -> payload:Yojson.Safe.t
    -> message
  (** Create a push message (server-initiated) *)

  val make_error : topic:string -> payload:Yojson.Safe.t -> message
  (** Create an error message *)
end

(** {1 Endpoint}

    Integration with Tapak's router for handling WebSocket
    connections. *)
module Endpoint : sig
  type channel_route
  (** Channel registration *)

  val channel :
     ('a, Topic.t) Topic.pattern
    -> (module Channel.HANDLER)
    -> channel_route
  (** Create a channel route with type-safe topic pattern *)

  type config
  (** Endpoint configuration, parameterized by PubSub
      subscription type. Create with {!create_config}. *)

  val create_config :
     socket:(module Socket.HANDLER)
    -> channels:channel_route list
    -> pubsub:Pubsub.t
    -> ?presence:Presence.t
    -> ?heartbeat_interval:float
    -> ?timeout:float
    -> clock:float Eio.Time.clock_ty Eio.Resource.t
    -> unit
    -> config
  (** Create endpoint configuration.
      @param socket Socket handler module
      @param channels List of channel routes
      @param pubsub PubSub instance
      @param presence Optional distributed presence tracker
      @param heartbeat_interval
        Heartbeat interval in seconds (default: 30.0)
      @param timeout
        Connection timeout in seconds (default: 60.0)
      @param clock EIO clock for timeout tracking *)

  val default_heartbeat_interval : float
  (** Default heartbeat interval (30.0 seconds) *)

  val default_timeout : float
  (** Default timeout (60.0 seconds) *)

  val routes : config -> Tapak_kernel.Router.route list
  (** Create Tapak routes for the endpoint. Returns routes for
      WebSocket upgrade at "websocket" path. Use with
      Router.scope to mount at desired path, e.g.:
      {[
        scope (s "socket") (Endpoint.routes config)
      ]}
      This will handle requests to /socket/websocket *)

  val handler : config -> Tapak_kernel.Request.t -> Tapak_kernel.Response.t
  (** Create a single handler for custom integration *)

  val disconnect : config -> Socket.id -> unit
  (** Disconnect all sockets with the given ID *)
end

(** {1 Convenience Re-exports} *)

val local_pubsub : sw:Eio.Switch.t -> Pubsub.t
(** Create an in-memory PubSub instance *)
