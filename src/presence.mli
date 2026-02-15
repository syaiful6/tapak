type meta = Jsont.json
(** Metadata payload stored for each presence entry. *)

type entry = { metas : meta list }
(** Presence entry grouping all tracked metas for a specific key. *)

type state = (string, entry) Hashtbl.t
(** Presence state map keyed by user-provided keys. *)

type t
(** Presence tracker handle. *)

val create :
   sw:Eio.Switch.t
  -> pubsub:Pubsub.t
  -> node_name:string
  -> clock:float Eio.Time.clock_ty Eio.Resource.t
  -> ?broadcast_period:float
  -> ?down_period:float
  -> unit
  -> t
(** Start the distributed presence tracker. *)

val close : t -> unit
(** Stop the heartbeat/broadcast loops and clean up resources. *)

val track : topic:string -> key:string -> meta:meta -> t -> string
(** Track a presence for [key] on [topic]. Returns the generated [phx_ref]. *)

val untrack_ref : t -> topic:string -> phx_ref:string -> unit
(** Stop tracking a presence identified by [phx_ref]. *)

val list : t -> topic:string -> state
(** Get the current local presence state for a topic. *)

val state_to_json : state -> Jsont.json
(** Encode a presence state as JSON for pushing to clients. *)

val broadcast_disconnect : t -> socket_id:string -> unit
(** Notify other nodes that the socket with [socket_id] disconnected. *)

val on_disconnect : t -> (string -> unit) -> unit
(** Register a callback invoked when a remote disconnect message is received. *)

val node_name : t -> string
(** Return this node's identifier used in presence sync. *)
