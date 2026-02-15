type 'a transition =
  { state : 'a
  ; socket : Socket.t
  }

module Reply = struct
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

  let string_of_status = function `Ok -> "ok" | `Error -> "error"

  let respond ~transition ~status ~payload =
    Respond { transition; status; payload }

  let stop ~transition ~reason = Stop { transition; reason }
  let noop transition = Noop transition
end

module Join = struct
  type 'a t =
    | Ok of
        { transition : 'a transition
        ; response : Jsont.json
        }
    | Error of { reason : Jsont.json }

  let ok ~transition ~response = Ok { transition; response }
  let error reason = Error { reason }
end

module Push = struct
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

  let push ~transition ~payload = Push { transition; payload }
  let intercept ~transition ~payload = Intercept { transition; payload }
  let suppress transition = Suppress transition

  let socket = function
    | Push { transition = { socket; _ }; _ }
    | Intercept { transition = { socket; _ }; _ }
    | Suppress { socket; _ } ->
      socket

  let state = function
    | Push { transition = { state; _ }; _ }
    | Intercept { transition = { state; _ }; _ }
    | Suppress { state; _ } ->
      state
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

module Default = struct
  let handle_info msg ~socket state =
    Push.push ~transition:{ state; socket } ~payload:msg.payload

  let handle_out ~event:_ ~payload ~socket state =
    Push.push ~transition:{ state; socket } ~payload

  let terminate ~reason:_ ~socket:_ _state = ()
  let intercept = []
end

let broadcast = Channel_effect.broadcast
let broadcast_from = Channel_effect.broadcast_from
let push = Channel_effect.push
let track_presence = Channel_effect.track_presence
let untrack_presence = Channel_effect.untrack_presence
let get_presence_list = Channel_effect.get_presence_list
