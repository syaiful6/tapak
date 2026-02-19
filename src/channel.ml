type 'a ctx =
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
        { ctx : 'a ctx
        ; status : status
        ; payload : Jsont.json
        }
    | Stop of
        { ctx : 'a ctx
        ; reason : string
        }
    | Noop of 'a ctx

  let string_of_status = function `Ok -> "ok" | `Error -> "error"
  let respond ctx status payload = Respond { ctx; status; payload }
  let ok ctx payload = respond ctx `Ok payload
  let error ctx payload = respond ctx `Error payload
  let stop ctx reason = Stop { ctx; reason }
  let noop ctx = Noop ctx
end

module Join = struct
  type 'a t =
    | Ok of
        { ctx : 'a ctx
        ; response : Jsont.json
        }
    | Error of { reason : Jsont.json }

  let ok ctx response = Ok { ctx; response }
  let error reason = Error { reason }
end

module Push = struct
  type 'a t =
    | Push of
        { ctx : 'a ctx
        ; payload : Jsont.json
        }
    | Intercept of
        { ctx : 'a ctx
        ; payload : Jsont.json
        }
    | Suppress of 'a ctx

  let push ctx payload = Push { ctx; payload }
  let intercept ctx payload = Intercept { ctx; payload }
  let suppress ctx = Suppress ctx

  let socket = function
    | Push { ctx = { socket; _ }; _ }
    | Intercept { ctx = { socket; _ }; _ }
    | Suppress { socket; _ } ->
      socket

  let state = function
    | Push { ctx = { state; _ }; _ }
    | Intercept { ctx = { state; _ }; _ }
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
  let handle_info msg ~socket state = Push.push { state; socket } msg.payload

  let handle_out ~event:_ ~payload ~socket state =
    Push.push { state; socket } payload

  let terminate ~reason:_ ~socket:_ _state = ()
  let intercept = []
end

let broadcast = Channel_effect.broadcast
let broadcast_from = Channel_effect.broadcast_from
let push = Channel_effect.push
let track_presence = Channel_effect.track_presence
let untrack_presence = Channel_effect.untrack_presence
let get_presence_list = Channel_effect.get_presence_list
