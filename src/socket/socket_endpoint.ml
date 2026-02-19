module Log = (val Logs.src_log (Logs.Src.create "tapak.socket_endpoint"))

type route = Route : Re.re * (module Channel.S) -> route

let channel pattern handler =
  let re = Re.compile (Re.Perl.re pattern) in
  Route (re, handler)

let find_channel channels topic =
  channels
  |> List.find_map (fun (Route (pattern, handler)) ->
    if Re.execp pattern topic then Some handler else None)

type t =
  { socket : (module Socket.S)
  ; channels : route list
  ; pubsub : Pubsub.t
  ; presence : Presence.t option
  ; heartbeat_interval : float
  ; timeout : float
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; mutable context : Context.t
  }

let create
      ~socket
      ~channels
      ~pubsub
      ?presence
      ?(heartbeat_interval = 30.)
      ?(timeout = 60.)
      ~clock
      ()
  =
  { socket
  ; channels
  ; pubsub
  ; presence
  ; heartbeat_interval
  ; timeout
  ; clock
  ; context = Context.empty
  }

type channel_state =
  | Channel_state :
      { channel : (module Channel.S with type t = 's)
      ; state : 's
      ; topic : string
      ; join_ref : string
      ; mutable presence_refs : (string * string) list
      }
      -> channel_state

module type Transport = sig
  val name : string
  val routes : t -> Router.route list
  val shutdown : t -> Socket.t option -> unit
end

module Effect_handler = struct
  (* effect handler for channel effects *)
  type t =
    { pubsub : Pubsub.t
    ; presence : Presence.t option
    ; topic : string
    ; push : Channel.broadcast -> unit
    ; presence_refs : (string * string) list ref
    ; subscription : int option
    ; origin : string option
    }

  let make_handler : type a. t -> (a, a) Effect.Deep.handler =
   fun ctx ->
    { Effect.Deep.retc = Fun.id
    ; exnc = raise
    ; effc =
        (fun (type b) (eff : b Effect.t) ->
          match eff with
          | Channel_effect.Broadcast { topic; event; payload } ->
            Some
              (fun (k : (b, a) Effect.Deep.continuation) ->
                Pubsub.broadcast
                  ctx.pubsub
                  { topic; event; payload; origin = None };
                Effect.Deep.continue k ())
          | Channel_effect.Broadcast_from { topic; event; payload } ->
            Some
              (fun k ->
                Pubsub.broadcast
                  ctx.pubsub
                  { topic; event; payload; origin = ctx.origin };
                Effect.Deep.continue k ())
          | Channel_effect.Push { event; payload } ->
            Some
              (fun k ->
                ctx.push { topic = ctx.topic; event; payload };
                Effect.Deep.continue k ())
          | Channel_effect.Track_presence { key; meta } ->
            Some
              (fun k ->
                match ctx.presence with
                | Some presence ->
                  let phx_ref =
                    Presence.track ~topic:ctx.topic ~key ~meta presence
                  in
                  ctx.presence_refs := (key, phx_ref) :: !(ctx.presence_refs);
                  Effect.Deep.continue k phx_ref
                | None ->
                  Log.warn (fun m ->
                    m "Presence tracking attempted but no presence configured");
                  Effect.Deep.continue k "")
          | Channel_effect.Untrack_presence { phx_ref } ->
            Some
              (fun k ->
                match ctx.presence with
                | Some presence ->
                  Presence.untrack_ref presence ~topic:ctx.topic ~phx_ref;
                  ctx.presence_refs :=
                    List.filter
                      (fun (_, ref_) -> not (String.equal ref_ phx_ref))
                      !(ctx.presence_refs);
                  Effect.Deep.continue k ()
                | None -> Effect.Deep.continue k ())
          | Channel_effect.Get_presence_list () ->
            Some
              (fun k ->
                match ctx.presence with
                | Some presence ->
                  let state = Presence.list presence ~topic:ctx.topic in
                  Effect.Deep.continue k (Presence.state_to_json state)
                | None -> Effect.Deep.continue k (Jsont.Json.object' []))
          | _ -> None)
    }

  let run ctx f = Effect.Deep.match_with f () (make_handler ctx)
end
