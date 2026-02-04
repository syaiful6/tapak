(** Tapak Channel - Phoenix-inspired real-time communication *)

(** {1 Topics} *)

module Topic : sig
  type t = string
  type (_, _) pattern

  val s : string -> ('a, 'a) pattern
  val str : (string -> 'a, 'a) pattern
  val int : (int -> 'a, 'a) pattern
  val int64 : (int64 -> 'a, 'a) pattern
  val ( / ) : ('a, 'b) pattern -> ('b, 'c) pattern -> ('a, 'c) pattern
  val sprintf : ('a, t) pattern -> 'a
  val sscanf : ('a, 'b) pattern -> t -> ('a -> 'b) option
  val matches : (_, _) pattern -> t -> bool
end

(** {1 Socket} *)

module Socket : sig
  type t
  type id = string option

  type transport =
    | Websocket
    | Longpoll

  type connect_info =
    { params : Yojson.Safe.t
    ; request : Request.t
    }

  module type HANDLER = sig
    val connect : connect_info -> (Context.t, string) result
    val id : Context.t -> id
  end

  val assign : 'a Context.key -> 'a -> t -> t
  val get_assign : 'a Context.key -> t -> 'a option
  val get_assign_exn : 'a Context.key -> t -> 'a
  val assigns : t -> Context.t
  val id : t -> id
  val transport : t -> transport
  val joined_topics : t -> Topic.t list
end

(** {1 Channel} *)

module Channel : sig
  type reply_status =
    | Ok
    | Error

  type 'state reply =
    | Reply of reply_status * Yojson.Safe.t * 'state * Socket.t
    | No_reply of 'state * Socket.t
    | Stop of string * 'state * Socket.t

  type broadcast_msg =
    { topic : Topic.t
    ; event : string
    ; payload : Yojson.Safe.t
    }

  type push_control =
    | Push of Yojson.Safe.t
    | Intercept of Yojson.Safe.t
    | Suppress

  type terminate_reason =
    | Normal
    | Left
    | Closed
    | Error of exn

  module type HANDLER = sig
    type state

    val init : unit -> state

    val join :
       topic:Topic.t
      -> payload:Yojson.Safe.t
      -> socket:Socket.t
      -> state
      -> (state * Socket.t * Yojson.Safe.t, Yojson.Safe.t) result

    val handle_in :
       event:string
      -> payload:Yojson.Safe.t
      -> socket:Socket.t
      -> state
      -> state reply

    val handle_info :
       broadcast_msg
      -> socket:Socket.t
      -> state
      -> push_control * state * Socket.t

    val handle_out :
       event:string
      -> payload:Yojson.Safe.t
      -> socket:Socket.t
      -> state
      -> push_control * state * Socket.t

    val terminate : reason:terminate_reason -> socket:Socket.t -> state -> unit
    val intercept : string list
  end

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

  val broadcast : topic:Topic.t -> event:string -> payload:Yojson.Safe.t -> unit
  val push : event:string -> payload:Yojson.Safe.t -> unit

  val broadcast_from :
     topic:Topic.t
    -> event:string
    -> payload:Yojson.Safe.t
    -> unit

  val track_presence : key:string -> meta:Yojson.Safe.t -> string
  val untrack_presence : phx_ref:string -> unit
  val get_presence_list : unit -> Yojson.Safe.t
end

(** {1 PubSub} *)

module Pubsub : sig
  type t

  type message =
    { topic : Topic.t
    ; event : string
    ; payload : Yojson.Safe.t
    }

  module type BACKEND = sig
    type t
    type subscription

    val start : sw:Eio.Switch.t -> t
    val subscribe : t -> Topic.t -> (message -> unit) -> subscription
    val unsubscribe : t -> subscription -> unit
    val broadcast : t -> message -> unit
    val node_name : t -> string
  end

  val create :
     (module BACKEND with type subscription = 's and type t = 'a)
    -> 'a
    -> t

  val subscribe : t -> Topic.t -> (message -> unit) -> int
  val unsubscribe : t -> int -> unit
  val broadcast : t -> message -> unit
  val broadcast_from : t -> self:int -> message -> unit
  val direct_broadcast : t -> int -> message -> unit
  val node_name : t -> string
end

(** {1 Presence} *)

module Presence : sig
  type t
  type meta = Yojson.Safe.t
  type entry = { metas : meta list }
  type state = (string, entry) Hashtbl.t

  val create :
     sw:Eio.Switch.t
    -> pubsub:Pubsub.t
    -> node_name:string
    -> clock:float Eio.Time.clock_ty Eio.Resource.t
    -> ?broadcast_period:float
    -> ?down_period:float
    -> unit
    -> t

  val track : t -> topic:Topic.t -> key:string -> meta:meta -> string
  val untrack_ref : t -> topic:Topic.t -> phx_ref:string -> unit
  val list : t -> topic:Topic.t -> state
  val to_json : state -> Yojson.Safe.t
  val close : t -> unit
  val on_disconnect : t -> (string -> unit) -> unit
  val broadcast_disconnect : t -> socket_id:string -> unit
  val node_name : t -> string

  type diff =
    { joins : (string * entry) list
    ; leaves : (string * entry) list
    }

  val diff : old_state:state -> new_state:state -> diff
  val diff_to_json : diff -> Yojson.Safe.t
end

(** {1 Protocol} *)

module Protocol : sig
  type message =
    { join_ref : string option
    ; ref_ : string option
    ; topic : string
    ; event : string
    ; payload : Yojson.Safe.t
    }

  val phx_join : string
  val phx_leave : string
  val phx_reply : string
  val phx_error : string
  val phx_close : string
  val heartbeat : string
  val encode_json : message -> string
  val decode_json : string -> (message, string) result

  val make_reply :
     join_ref:string option
    -> ref_:string
    -> topic:string
    -> status:Channel.reply_status
    -> payload:Yojson.Safe.t
    -> message

  val make_push :
     topic:string
    -> event:string
    -> payload:Yojson.Safe.t
    -> message

  val make_error : topic:string -> payload:Yojson.Safe.t -> message
end

(** {1 Endpoint} *)

module Endpoint : sig
  type config
  type channel_route

  val channel :
     ('a, Topic.t) Topic.pattern
    -> (module Channel.HANDLER)
    -> channel_route

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

  val handler : config -> Request.t -> Response.t
  val routes : config -> Router.route list
  val disconnect : config -> Socket.id -> unit
end

(** {1 Top-Level Types} 

    These are exposed at the Tapak module level *)

type socket_connect_info = Socket.connect_info

module type SOCKET = Socket.HANDLER
module type CHANNEL = Channel.HANDLER
module type PUBSUB = Pubsub.BACKEND

val local_pubsub : sw:Eio.Switch.t -> Pubsub.t
