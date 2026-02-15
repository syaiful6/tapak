type _ Effect.t +=
  | Broadcast :
      { topic : string
      ; event : string
      ; payload : Jsont.json
      }
      -> unit Effect.t
  | Broadcast_from :
      { topic : string
      ; event : string
      ; payload : Jsont.json
      }
      -> unit Effect.t
  | Push :
      { event : string
      ; payload : Jsont.json
      }
      -> unit Effect.t
  | Track_presence :
      { key : string
      ; meta : Jsont.json
      }
      -> string Effect.t
  | Untrack_presence : { phx_ref : string } -> unit Effect.t
  | Get_presence_list : unit -> Jsont.json Effect.t

let broadcast ~topic ~event ~payload =
  Effect.perform (Broadcast { topic; event; payload })

let broadcast_from ~topic ~event ~payload =
  Effect.perform (Broadcast_from { topic; event; payload })

let push ~event ~payload = Effect.perform (Push { event; payload })
let track_presence ~key ~meta = Effect.perform (Track_presence { key; meta })
let untrack_presence ~phx_ref = Effect.perform (Untrack_presence { phx_ref })
let get_presence_list () = Effect.perform (Get_presence_list ())
