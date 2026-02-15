type 'a transition =
  { state : 'a
  ; socket : Socket.t
  }

module Reply : sig
  type status =
    [ `Ok
    | `Error
    ]

  type 'a t =
    | Respond of
        { transition : 'a transition
        ; status : status
        ; payload : Jsont.json
        }
    | Stop of
        { transition : 'a transition
        ; reason : string
        }
    | Noop of 'a transition

  val string_of_status : status -> string

  val respond :
     transition:'a transition
    -> status:status
    -> payload:Jsont.json
    -> 'a t

  val stop : transition:'a transition -> reason:string -> 'a t
  val noop : 'a transition -> 'a t
end

module Join : sig
  type 'a t =
    | Ok of
        { transition : 'a transition
        ; response : Jsont.json
        }
    | Error of { reason : Jsont.json }

  val ok : transition:'a transition -> response:Jsont.json -> 'a t
  val error : Jsont.json -> 'a t
end

module Push : sig
  type 'a t =
    | Push of
        { transition : 'a transition
        ; payload : Jsont.json
        }
    | Intercept of
        { transition : 'a transition
        ; payload : Jsont.json
        }
    | Suppress of 'a transition

  val push : transition:'a transition -> payload:Jsont.json -> 'a t
  val intercept : transition:'a transition -> payload:Jsont.json -> 'a t
  val suppress : 'a transition -> 'a t
  val socket : 'a t -> Socket.t
  val state : 'a t -> 'a
end

type broadcast =
  { topic : string
  ; event : string
  ; payload : Jsont.json
  }

type terminate_reason =
  | Normal
  | Left
  | Closed
  | Error of exn

module type S = sig
  type t

  val init : unit -> t

  val join :
     topic:string
    -> payload:Jsont.json
    -> socket:Socket.t
    -> t
    -> t Join.t

  val handle_in :
     event:string
    -> payload:Jsont.json
    -> socket:Socket.t
    -> t
    -> t Reply.t

  val handle_info : broadcast -> socket:Socket.t -> t -> t Push.t

  val handle_out :
     event:string
    -> payload:Jsont.json
    -> socket:Socket.t
    -> t
    -> t Push.t

  val terminate : reason:terminate_reason -> socket:Socket.t -> t -> unit
  val intercept : string list
end

module Default : sig
  val handle_info : broadcast -> socket:Socket.t -> 'a -> 'a Push.t

  val handle_out :
     event:string
    -> payload:Jsont.json
    -> socket:Socket.t
    -> 'a
    -> 'a Push.t

  val terminate : reason:terminate_reason -> socket:Socket.t -> 'a -> unit
  val intercept : string list
end

val broadcast : topic:string -> event:string -> payload:Jsont.json -> unit
val push : event:string -> payload:Jsont.json -> unit
val broadcast_from : topic:string -> event:string -> payload:Jsont.json -> unit
val track_presence : key:string -> meta:Jsont.json -> string
val untrack_presence : phx_ref:string -> unit
val get_presence_list : unit -> Jsont.json
