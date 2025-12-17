module Log = (val Logs.src_log (Logs.Src.create "tapak.realtime"))

module Topic = struct
  type t = string

  type (_, _) pattern =
    | Nil : ('a, 'a) pattern
    | Lit : string * ('a, 'b) pattern -> ('a, 'b) pattern
    | Str : ('a, 'b) pattern -> (string -> 'a, 'b) pattern
    | Int : ('a, 'b) pattern -> (int -> 'a, 'b) pattern
    | Int64 : ('a, 'b) pattern -> (int64 -> 'a, 'b) pattern

  let s literal = Lit (literal, Nil)
  let str = Str Nil
  let int = Int Nil
  let int64 = Int64 Nil

  let rec ( / ) : type a b c. (a, c) pattern -> (c, b) pattern -> (a, b) pattern
    =
   fun left right ->
    match left with
    | Nil -> right
    | Lit (s, rest) -> Lit (s, rest / right)
    | Str rest -> Str (rest / right)
    | Int rest -> Int (rest / right)
    | Int64 rest -> Int64 (rest / right)

  let sprintf pattern =
    let rec go : type a b. (a, b) pattern -> (string list -> b) -> a =
     fun p k ->
      match p with
      | Nil -> k []
      | Lit (s, rest) -> go rest (fun parts -> k (s :: parts))
      | Str rest -> fun s -> go rest (fun parts -> k (s :: parts))
      | Int rest -> fun i -> go rest (fun parts -> k (string_of_int i :: parts))
      | Int64 rest ->
        fun i -> go rest (fun parts -> k (Int64.to_string i :: parts))
    in
    go pattern (fun parts -> String.concat ":" parts)

  let split_topic topic = String.split_on_char ':' topic

  let sscanf : type a b. (a, b) pattern -> t -> (a -> b) option =
   fun pattern topic ->
    let parts = split_topic topic in
    let rec go : type x y.
      (x, y) pattern -> string list -> (string list * (x -> y)) option
      =
     fun p remaining ->
      match p with
      | Nil -> Some (remaining, Fun.id)
      | Lit (expected, rest) ->
        (match remaining with
        | hd :: tl when String.equal expected hd -> go rest tl
        | _ -> None)
      | Str rest ->
        (match remaining with
        | hd :: tl ->
          (match go rest tl with
          | Some (final, apply_rest) -> Some (final, fun k -> k hd |> apply_rest)
          | None -> None)
        | [] -> None)
      | Int rest ->
        (match remaining with
        | hd :: tl ->
          (match int_of_string_opt hd with
          | Some i ->
            (match go rest tl with
            | Some (final, apply_rest) ->
              Some (final, fun k -> k i |> apply_rest)
            | None -> None)
          | None -> None)
        | [] -> None)
      | Int64 rest ->
        (match remaining with
        | hd :: tl ->
          (match Int64.of_string_opt hd with
          | Some i ->
            (match go rest tl with
            | Some (final, apply_rest) ->
              Some (final, fun k -> k i |> apply_rest)
            | None -> None)
          | None -> None)
        | [] -> None)
    in
    match go pattern parts with
    | Some ([], f) -> Some f (* All parts consumed *)
    | Some (_ :: _, _) -> None (* Extra parts remaining *)
    | None -> None

  let matches pattern topic =
    match sscanf pattern topic with Some _ -> true | None -> false
end

module Pubsub = struct
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

  type t =
    { subscribe : Topic.t -> (message -> unit) -> int
    ; unsubscribe : int -> unit
    ; broadcast : message -> unit
    ; broadcast_from_impl : self:int -> message -> unit
    ; direct_broadcast_impl : int -> message -> unit
    ; node_name : unit -> string
    }

  let create
        (type a s)
        (module B : BACKEND with type t = a and type subscription = s)
        (backend : a) : t
    =
    let id = Atomic.make 0 in
    let subscriptions : (int, s * (message -> unit)) Saturn.Htbl.t =
      Saturn.Htbl.create ()
    in
    let subscribe topic callback =
      let backend_sub = B.subscribe backend topic callback in
      let sub_id = Atomic.fetch_and_add id 1 in
      (* not sure how to do if this fail *)
      let _ =
        Saturn.Htbl.try_add subscriptions sub_id (backend_sub, callback)
      in
      sub_id
    in
    let unsubscribe sub =
      match Saturn.Htbl.find_opt subscriptions sub with
      | Some (backend_sub, _) ->
        B.unsubscribe backend backend_sub;
        ignore (Saturn.Htbl.try_remove subscriptions sub)
      | None -> ()
    in
    let broadcast msg = B.broadcast backend msg in
    let broadcast_from_impl ~self msg =
      Saturn.Htbl.to_seq subscriptions
      |> Seq.iter (fun (id, (_, callback)) ->
        if not (Int.equal self id) then callback msg)
    in
    let direct_broadcast_impl sub msg =
      match Saturn.Htbl.find_opt subscriptions sub with
      | Some (_, callback) -> callback msg
      | None -> ()
    in
    { subscribe
    ; unsubscribe
    ; broadcast
    ; broadcast_from_impl
    ; direct_broadcast_impl
    ; node_name = (fun () -> B.node_name backend)
    }

  let subscribe t = t.subscribe
  let unsubscribe t = t.unsubscribe
  let broadcast t = t.broadcast
  let broadcast_from t ~self msg = t.broadcast_from_impl ~self msg
  let direct_broadcast t sub msg = t.direct_broadcast_impl sub msg
  let node_name t = t.node_name ()

  module Local : BACKEND with type subscription = int = struct
    type t =
      { next_id : int Atomic.t
      ; subscriptions : (int, Topic.t * (message -> unit)) Saturn.Htbl.t
      }

    type subscription = int

    let start ~sw:_ =
      { next_id = Atomic.make 0; subscriptions = Saturn.Htbl.create () }

    let subscribe t topic callback =
      let id = Atomic.fetch_and_add t.next_id 1 in
      ignore (Saturn.Htbl.try_add t.subscriptions id (topic, callback));
      id

    let unsubscribe t id = ignore @@ Saturn.Htbl.try_remove t.subscriptions id

    let broadcast t msg =
      Saturn.Htbl.to_seq t.subscriptions
      |> Seq.iter (fun (_, (topic, callback)) ->
        if String.equal topic msg.topic then callback msg)

    let node_name _ = "local"
  end
end

module Socket = struct
  type id = string option

  type transport =
    | Websocket
    | Longpoll

  type t =
    { id : id
    ; assigns : Tapak_kernel.Context.t
    ; transport : transport
    ; joined_topics : Topic.t list
    }

  type connect_info =
    { params : Yojson.Safe.t
    ; request : Tapak_kernel.Request.t
    }

  module type HANDLER = sig
    val connect : connect_info -> (Tapak_kernel.Context.t, string) result
    val id : Tapak_kernel.Context.t -> id
  end

  let assigns t = t.assigns

  let assign key value t =
    { t with assigns = Tapak_kernel.Context.add key value t.assigns }

  let get_assign key t = Tapak_kernel.Context.find key t.assigns

  let get_assign_exn key t =
    match get_assign key t with
    | Some v -> v
    | None -> failwith "Socket.get_assign_exn: key not found"

  let id t = t.id
  let transport t = t.transport
  let joined_topics t = t.joined_topics
end

module Channel = struct
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

  type _ Effect.t +=
    | Broadcast_effect :
        { topic : Topic.t
        ; event : string
        ; payload : Yojson.Safe.t
        }
        -> unit Effect.t
    | Broadcast_from_effect :
        { topic : Topic.t
        ; event : string
        ; payload : Yojson.Safe.t
        }
        -> unit Effect.t
    | Push_effect :
        { event : string
        ; payload : Yojson.Safe.t
        }
        -> unit Effect.t
    | Track_presence_effect :
        { key : string
        ; meta : Yojson.Safe.t
        }
        -> string Effect.t
    | Untrack_presence_effect : { phx_ref : string } -> unit Effect.t
    | Get_presence_list_effect : unit -> Yojson.Safe.t Effect.t

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

  module Default = struct
    let handle_info msg ~socket state = Push msg.payload, state, socket
    let handle_out _event payload ~socket state = Push payload, state, socket
    let terminate ~reason:_ ~socket:_ _state = ()
    let intercept = []
  end

  type context =
    { pubsub : Pubsub.t
    ; socket : Socket.t
    ; push_fn : event:string -> payload:Yojson.Safe.t -> unit
    }

  let push_ctx ctx ~event ~payload = ctx.push_fn ~event ~payload

  let broadcast_ctx ctx ~topic ~event ~payload =
    Pubsub.broadcast ctx.pubsub { topic; event; payload }

  let broadcast ~topic ~event ~payload =
    Effect.perform (Broadcast_effect { topic; event; payload })

  let broadcast_from ~topic ~event ~payload =
    Effect.perform (Broadcast_from_effect { topic; event; payload })

  let push ~event ~payload = Effect.perform (Push_effect { event; payload })

  let track_presence ~key ~meta =
    Effect.perform (Track_presence_effect { key; meta })

  let untrack_presence ~phx_ref =
    Effect.perform (Untrack_presence_effect { phx_ref })

  let get_presence_list () = Effect.perform (Get_presence_list_effect ())
end

module Presence = struct
  type meta = Yojson.Safe.t
  type entry = { metas : meta list }
  type state = (string, entry) Hashtbl.t

  type diff =
    { joins : (string * entry) list
    ; leaves : (string * entry) list
    }

  let sync_topic = "__presence_sync__"

  type lww_meta =
    { topic : Topic.t
    ; key : string
    ; meta : meta
    ; phx_ref : string
    ; node : string
    ; timestamp : float
    }

  type sync_message =
    | Heartbeat of
        { node : string
        ; timestamp : float
        }
    | Delta of
        { node : string
        ; joins : lww_meta list
        ; leaves : (Topic.t * string) list
        }
    | Disconnect of { socket_id : string }
    | StateRequest of { node : string }
    | StateResponse of
        { node : string
        ; state : (Topic.t * (string * lww_meta list) list) list
        }

  type t =
    { pubsub : Pubsub.t
    ; node_name : string
    ; clock : float Eio.Time.clock_ty Eio.Resource.t
    ; global_state : (Topic.t, (string, lww_meta list) Hashtbl.t) Hashtbl.t
    ; local_state : (Topic.t, state) Hashtbl.t
    ; node_heartbeats : (string, float) Hashtbl.t
    ; state_mutex : Eio.Mutex.t
    ; broadcast_period : float
    ; down_period : float
    ; mutable closed : bool
    ; disconnect_handlers : (string -> unit) list ref
    ; next_ref : int Atomic.t
    ; stop_signal : unit Eio.Promise.t * unit Eio.Promise.u
    }

  let generate_ref t =
    let ref_id = Atomic.fetch_and_add t.next_ref 1 in
    Printf.sprintf "phx-%d" ref_id

  let encode_lww_meta m =
    `Assoc
      [ "topic", `String m.topic
      ; "key", `String m.key
      ; "meta", m.meta
      ; "phx_ref", `String m.phx_ref
      ; "node", `String m.node
      ; "timestamp", `Float m.timestamp
      ]

  let decode_lww_meta json =
    let open Yojson.Safe.Util in
    { topic = json |> member "topic" |> to_string
    ; key = json |> member "key" |> to_string
    ; meta = json |> member "meta"
    ; phx_ref = json |> member "phx_ref" |> to_string
    ; node = json |> member "node" |> to_string
    ; timestamp = json |> member "timestamp" |> to_float
    }

  let encode_sync_message msg =
    match msg with
    | Heartbeat { node; timestamp } ->
      `Assoc
        [ "type", `String "heartbeat"
        ; "node", `String node
        ; "timestamp", `Float timestamp
        ]
    | Delta { node; joins; leaves } ->
      `Assoc
        [ "type", `String "delta"
        ; "node", `String node
        ; "joins", `List (List.map encode_lww_meta joins)
        ; ( "leaves"
          , `List
              (List.map
                 (fun (topic, phx_ref) ->
                    `Assoc
                      [ "topic", `String topic; "phx_ref", `String phx_ref ])
                 leaves) )
        ]
    | Disconnect { socket_id } ->
      `Assoc [ "type", `String "disconnect"; "socket_id", `String socket_id ]
    | StateRequest { node } ->
      `Assoc [ "type", `String "state_request"; "node", `String node ]
    | StateResponse { node; state } ->
      let state_json =
        List.map
          (fun (topic, entries) ->
             ( topic
             , `List
                 (List.map
                    (fun (key, metas) ->
                       `Assoc
                         [ "key", `String key
                         ; "metas", `List (List.map encode_lww_meta metas)
                         ])
                    entries) ))
          state
      in
      `Assoc
        [ "type", `String "state_response"
        ; "node", `String node
        ; "state", `Assoc state_json
        ]

  let decode_sync_message json =
    let open Yojson.Safe.Util in
    match json |> member "type" |> to_string with
    | "heartbeat" ->
      Heartbeat
        { node = json |> member "node" |> to_string
        ; timestamp = json |> member "timestamp" |> to_float
        }
    | "delta" ->
      Delta
        { node = json |> member "node" |> to_string
        ; joins = json |> member "joins" |> to_list |> List.map decode_lww_meta
        ; leaves =
            json
            |> member "leaves"
            |> to_list
            |> List.map (fun l ->
              ( l |> member "topic" |> to_string
              , l |> member "phx_ref" |> to_string ))
        }
    | "disconnect" ->
      Disconnect { socket_id = json |> member "socket_id" |> to_string }
    | "state_request" ->
      StateRequest { node = json |> member "node" |> to_string }
    | "state_response" ->
      let state =
        json
        |> member "state"
        |> to_assoc
        |> List.map (fun (topic, entries_json) ->
          let entries =
            entries_json
            |> to_list
            |> List.map (fun entry ->
              let key = entry |> member "key" |> to_string in
              let metas =
                entry |> member "metas" |> to_list |> List.map decode_lww_meta
              in
              key, metas)
          in
          topic, entries)
      in
      StateResponse { node = json |> member "node" |> to_string; state }
    | _ -> failwith "Unknown sync message type"

  let merge_lww_metas existing new_metas =
    let dominated existing_meta new_meta =
      String.equal existing_meta.phx_ref new_meta.phx_ref
      && new_meta.timestamp > existing_meta.timestamp
    in
    let dominated_by_new existing_meta =
      List.exists (fun nm -> dominated existing_meta nm) new_metas
    in
    let existing_filtered =
      List.filter (fun m -> not (dominated_by_new m)) existing
    in
    let new_unique =
      List.filter
        (fun nm ->
           not
             (List.exists
                (fun em -> String.equal em.phx_ref nm.phx_ref)
                existing_filtered))
        new_metas
    in
    existing_filtered @ new_unique

  let rebuild_local_state t =
    Hashtbl.clear t.local_state;
    Hashtbl.iter
      (fun topic global_topic_state ->
         let presence_state = Hashtbl.create 16 in
         Hashtbl.iter
           (fun key lww_metas ->
              let metas =
                List.map
                  (fun lww ->
                     match lww.meta with
                     | `Assoc fields ->
                       `Assoc (("phx_ref", `String lww.phx_ref) :: fields)
                     | other ->
                       `Assoc [ "phx_ref", `String lww.phx_ref; "data", other ])
                  lww_metas
              in
              if List.length metas > 0
              then Hashtbl.add presence_state key { metas })
           global_topic_state;
         if Hashtbl.length presence_state > 0
         then Hashtbl.add t.local_state topic presence_state)
      t.global_state

  let handle_sync_message t msg =
    match msg with
    | Heartbeat { node; timestamp } ->
      if not (String.equal node t.node_name)
      then
        Eio.Mutex.use_rw ~protect:true t.state_mutex (fun () ->
          Hashtbl.replace t.node_heartbeats node timestamp)
    | Delta { node; joins; leaves } ->
      if not (String.equal node t.node_name)
      then
        Eio.Mutex.use_rw ~protect:true t.state_mutex (fun () ->
          List.iter
            (fun lww ->
               let topic_state =
                 match Hashtbl.find_opt t.global_state lww.topic with
                 | Some s -> s
                 | None ->
                   let s = Hashtbl.create 16 in
                   Hashtbl.add t.global_state lww.topic s;
                   s
               in
               let existing =
                 Option.value (Hashtbl.find_opt topic_state lww.key) ~default:[]
               in
               let merged = merge_lww_metas existing [ lww ] in
               Hashtbl.replace topic_state lww.key merged)
            joins;
          List.iter
            (fun (topic, phx_ref) ->
               match Hashtbl.find_opt t.global_state topic with
               | Some topic_state ->
                 Hashtbl.filter_map_inplace
                   (fun _key metas ->
                      let filtered =
                        List.filter
                          (fun m -> not (String.equal m.phx_ref phx_ref))
                          metas
                      in
                      if List.length filtered > 0 then Some filtered else None)
                   topic_state
               | None -> ())
            leaves;
          rebuild_local_state t)
    | Disconnect { socket_id } ->
      let handlers =
        Eio.Mutex.use_rw ~protect:false t.state_mutex (fun () ->
          !(t.disconnect_handlers))
      in
      List.iter (fun handler -> handler socket_id) handlers
    | StateRequest { node = requesting_node } ->
      if not (String.equal requesting_node t.node_name)
      then
        let state =
          Eio.Mutex.use_rw ~protect:false t.state_mutex (fun () ->
            Hashtbl.fold
              (fun topic topic_state acc ->
                 let entries =
                   Hashtbl.fold
                     (fun key metas acc -> (key, metas) :: acc)
                     topic_state
                     []
                 in
                 (topic, entries) :: acc)
              t.global_state
              [])
        in
        let response = StateResponse { node = t.node_name; state } in
        Pubsub.broadcast
          t.pubsub
          { topic = sync_topic
          ; event = "sync"
          ; payload = encode_sync_message response
          }
    | StateResponse { node; state } ->
      if not (String.equal node t.node_name)
      then
        Eio.Mutex.use_rw ~protect:true t.state_mutex (fun () ->
          List.iter
            (fun (topic, entries) ->
               let topic_state =
                 match Hashtbl.find_opt t.global_state topic with
                 | Some s -> s
                 | None ->
                   let s = Hashtbl.create 16 in
                   Hashtbl.add t.global_state topic s;
                   s
               in
               List.iter
                 (fun (key, metas) ->
                    let existing =
                      Option.value
                        (Hashtbl.find_opt topic_state key)
                        ~default:[]
                    in
                    let merged = merge_lww_metas existing metas in
                    Hashtbl.replace topic_state key merged)
                 entries)
            state;
          rebuild_local_state t)

  let remove_node_presences t node =
    let leaves = ref [] in
    Hashtbl.iter
      (fun topic topic_state ->
         Hashtbl.filter_map_inplace
           (fun _key metas ->
              let removed, remaining =
                List.partition (fun m -> String.equal m.node node) metas
              in
              List.iter
                (fun m -> leaves := (topic, m.phx_ref) :: !leaves)
                removed;
              if List.length remaining > 0 then Some remaining else None)
           topic_state)
      t.global_state;
    if List.length !leaves > 0 then rebuild_local_state t

  let check_node_health t =
    let now = Eio.Time.now t.clock in
    Eio.Mutex.use_rw ~protect:true t.state_mutex (fun () ->
      let down_nodes = ref [] in
      Hashtbl.iter
        (fun node last_seen ->
           if now -. last_seen > t.down_period
           then down_nodes := node :: !down_nodes)
        t.node_heartbeats;
      List.iter
        (fun node ->
           Log.info (fun m -> m "Node %s is down, removing presences" node);
           Hashtbl.remove t.node_heartbeats node;
           remove_node_presences t node)
        !down_nodes)

  let run_heartbeat_loop t =
    let stop_promise, _ = t.stop_signal in
    let rec loop () =
      if t.closed
      then ()
      else
        Eio.Fiber.first
          (fun () ->
             Eio.Time.sleep t.clock t.broadcast_period;
             if not t.closed
             then (
               let now = Eio.Time.now t.clock in
               let msg = Heartbeat { node = t.node_name; timestamp = now } in
               Pubsub.broadcast
                 t.pubsub
                 { topic = sync_topic
                 ; event = "sync"
                 ; payload = encode_sync_message msg
                 };
               check_node_health t);
             loop ())
          (fun () -> Eio.Promise.await stop_promise)
    in
    loop ()

  let create
        ~sw
        ~pubsub
        ~node_name
        ~clock
        ?(broadcast_period = 1.5)
        ?(down_period = 30.0)
        ()
    =
    let stop_signal = Eio.Promise.create () in
    let t =
      { pubsub
      ; node_name
      ; clock
      ; global_state = Hashtbl.create 16
      ; local_state = Hashtbl.create 16
      ; node_heartbeats = Hashtbl.create 8
      ; state_mutex = Eio.Mutex.create ()
      ; broadcast_period
      ; down_period
      ; closed = false
      ; disconnect_handlers = ref []
      ; next_ref = Atomic.make 0
      ; stop_signal
      }
    in
    let _sub =
      Pubsub.subscribe pubsub sync_topic (fun msg ->
        if String.equal msg.event "sync"
        then
          try
            let sync_msg = decode_sync_message msg.payload in
            handle_sync_message t sync_msg
          with
          | exn ->
            Log.err (fun m ->
              m "Failed to decode sync message: %s" (Printexc.to_string exn)))
    in
    Eio.Fiber.fork ~sw (fun () -> run_heartbeat_loop t);
    let request = StateRequest { node = node_name } in
    Pubsub.broadcast
      pubsub
      { topic = sync_topic
      ; event = "sync"
      ; payload = encode_sync_message request
      };
    t

  let track t ~topic ~key ~meta =
    let timestamp = Eio.Time.now t.clock in
    let phx_ref = generate_ref t in
    let lww_meta =
      { topic; key; meta; phx_ref; node = t.node_name; timestamp }
    in
    Eio.Mutex.use_rw ~protect:true t.state_mutex (fun () ->
      let topic_state =
        match Hashtbl.find_opt t.global_state topic with
        | Some s -> s
        | None ->
          let s = Hashtbl.create 16 in
          Hashtbl.add t.global_state topic s;
          s
      in
      let existing =
        Option.value (Hashtbl.find_opt topic_state key) ~default:[]
      in
      Hashtbl.replace topic_state key (lww_meta :: existing);
      rebuild_local_state t);
    let delta =
      Delta { node = t.node_name; joins = [ lww_meta ]; leaves = [] }
    in
    Pubsub.broadcast
      t.pubsub
      { topic = sync_topic
      ; event = "sync"
      ; payload = encode_sync_message delta
      };
    Pubsub.broadcast
      t.pubsub
      { topic
      ; event = "presence_diff"
      ; payload =
          `Assoc
            [ ( "joins"
              , `Assoc
                  [ ( key
                    , `Assoc
                        [ ( "metas"
                          , `List
                              [ (match meta with
                                | `Assoc fields ->
                                  `Assoc (("phx_ref", `String phx_ref) :: fields)
                                | other ->
                                  `Assoc
                                    [ "phx_ref", `String phx_ref
                                    ; "data", other
                                    ])
                              ] )
                        ] )
                  ] )
            ; "leaves", `Assoc []
            ]
      };
    phx_ref

  let untrack_ref t ~topic ~phx_ref =
    let removed_key =
      Eio.Mutex.use_rw ~protect:true t.state_mutex (fun () ->
        let removed_key = ref None in
        (match Hashtbl.find_opt t.global_state topic with
        | Some topic_state ->
          Hashtbl.filter_map_inplace
            (fun key metas ->
               let removed, remaining =
                 List.partition
                   (fun m ->
                      String.equal m.phx_ref phx_ref
                      && String.equal m.node t.node_name)
                   metas
               in
               if List.length removed > 0 then removed_key := Some key;
               if List.length remaining > 0 then Some remaining else None)
            topic_state
        | None -> ());
        (match !removed_key with Some _ -> rebuild_local_state t | None -> ());
        !removed_key)
    in
    match removed_key with
    | Some key ->
      let delta =
        Delta { node = t.node_name; joins = []; leaves = [ topic, phx_ref ] }
      in
      Pubsub.broadcast
        t.pubsub
        { topic = sync_topic
        ; event = "sync"
        ; payload = encode_sync_message delta
        };
      Pubsub.broadcast
        t.pubsub
        { topic
        ; event = "presence_diff"
        ; payload =
            `Assoc
              [ "joins", `Assoc []
              ; ( "leaves"
                , `Assoc
                    [ ( key
                      , `Assoc
                          [ ( "metas"
                            , `List [ `Assoc [ "phx_ref", `String phx_ref ] ] )
                          ] )
                    ] )
              ]
        }
    | None -> ()

  let list t ~topic =
    Eio.Mutex.use_rw ~protect:false t.state_mutex (fun () ->
      match Hashtbl.find_opt t.local_state topic with
      | Some s -> s
      | None -> Hashtbl.create 0)

  let broadcast_disconnect t ~socket_id =
    let msg = Disconnect { socket_id } in
    Pubsub.broadcast
      t.pubsub
      { topic = sync_topic; event = "sync"; payload = encode_sync_message msg }

  let on_disconnect t handler =
    Eio.Mutex.use_rw ~protect:true t.state_mutex (fun () ->
      t.disconnect_handlers := handler :: !(t.disconnect_handlers))

  let close t =
    t.closed <- true;
    let _, resolver = t.stop_signal in
    Eio.Promise.resolve resolver ()

  let node_name t = t.node_name

  let diff ~old_state ~new_state =
    let joins = ref [] in
    let leaves = ref [] in
    (* Find joins: keys in new_state not in old_state or with new metas *)
    Hashtbl.iter
      (fun key entry ->
         match Hashtbl.find_opt old_state key with
         | None -> joins := (key, entry) :: !joins
         | Some old_entry ->
           if List.length entry.metas > List.length old_entry.metas
           then joins := (key, entry) :: !joins)
      new_state;
    (* Find leaves: keys in old_state not in new_state or with fewer metas *)
    Hashtbl.iter
      (fun key entry ->
         match Hashtbl.find_opt new_state key with
         | None -> leaves := (key, entry) :: !leaves
         | Some new_entry ->
           if List.length entry.metas > List.length new_entry.metas
           then leaves := (key, entry) :: !leaves)
      old_state;
    { joins = !joins; leaves = !leaves }

  let to_json state =
    let entries =
      Hashtbl.fold
        (fun key entry acc ->
           let metas_json = `List entry.metas in
           (key, `Assoc [ "metas", metas_json ]) :: acc)
        state
        []
    in
    `Assoc entries

  let diff_to_json diff =
    let joins_json =
      List.map
        (fun (key, entry) -> key, `Assoc [ "metas", `List entry.metas ])
        diff.joins
    in
    let leaves_json =
      List.map
        (fun (key, entry) -> key, `Assoc [ "metas", `List entry.metas ])
        diff.leaves
    in
    `Assoc [ "joins", `Assoc joins_json; "leaves", `Assoc leaves_json ]
end

module Protocol = struct
  type message =
    { join_ref : string option
    ; ref_ : string option
    ; topic : string
    ; event : string
    ; payload : Yojson.Safe.t
    }

  let phx_join = "phx_join"
  let phx_leave = "phx_leave"
  let phx_reply = "phx_reply"
  let phx_error = "phx_error"
  let phx_close = "phx_close"
  let heartbeat = "heartbeat"

  let encode_json msg =
    let open Yojson.Safe in
    let join_ref =
      match msg.join_ref with Some s -> `String s | None -> `Null
    in
    let ref_ = match msg.ref_ with Some s -> `String s | None -> `Null in
    to_string
      (`List
          [ join_ref; ref_; `String msg.topic; `String msg.event; msg.payload ])

  let decode_json str =
    try
      let open Yojson.Safe.Util in
      match Yojson.Safe.from_string str with
      | `List [ join_ref; ref_; topic; event; payload ] ->
        let join_ref =
          match join_ref with
          | `Null -> None
          | `String s -> Some s
          | _ -> failwith "invalid join_ref"
        in
        let ref_ =
          match ref_ with
          | `Null -> None
          | `String s -> Some s
          | _ -> failwith "invalid ref"
        in
        let topic = to_string topic in
        let event = to_string event in
        Result.Ok { join_ref; ref_; topic; event; payload }
      | _ -> Result.Error "invalid message format"
    with
    | exn -> Result.Error (Printexc.to_string exn)

  let make_reply ~join_ref ~ref_ ~topic ~status ~payload =
    let status_str =
      match status with Channel.Ok -> "ok" | Channel.Error -> "error"
    in
    { join_ref
    ; ref_ = Some ref_
    ; topic
    ; event = phx_reply
    ; payload = `Assoc [ "status", `String status_str; "response", payload ]
    }

  let make_push ~topic ~event ~payload =
    { join_ref = None; ref_ = None; topic; event; payload }

  let make_error ~topic ~payload =
    { join_ref = None; ref_ = None; topic; event = phx_error; payload }
end

module Endpoint = struct
  type channel_route =
    | Route :
        ('a, Topic.t) Topic.pattern * (module Channel.HANDLER)
        -> channel_route

  let channel pattern handler = Route (pattern, handler)

  type config =
    { socket : (module Socket.HANDLER)
    ; channels : channel_route list
    ; pubsub : Pubsub.t
    ; presence : Presence.t option
    ; heartbeat_interval : float
    ; timeout : float
    ; clock : float Eio.Time.clock_ty Eio.Resource.t
    ; active_sockets : (string, Piaf.Ws.Descriptor.t list) Hashtbl.t
    ; active_sockets_mutex : Eio.Mutex.t
    }

  let default_heartbeat_interval = 30.0
  let default_timeout = 60.0

  let create_config
        ~socket
        ~channels
        ~pubsub
        ?presence
        ?(heartbeat_interval = default_heartbeat_interval)
        ?(timeout = default_timeout)
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
    ; active_sockets = Hashtbl.create 16
    ; active_sockets_mutex = Eio.Mutex.create ()
    }

  let find_channel channels topic =
    List.find_map
      (fun (Route (pattern, handler)) ->
         if Topic.matches pattern topic
         then Some (Route (pattern, handler))
         else None)
      channels

  type channel_state =
    | Channel_state :
        { handler : (module Channel.HANDLER with type state = 's)
        ; state : 's
        ; topic : Topic.t
        ; join_ref : string
        ; mutable presence_refs : (string * string) list
        }
        -> channel_state

  module Effect_context = struct
    type t =
      { pubsub : Pubsub.t
      ; presence : Presence.t option
      ; topic : Topic.t
      ; ws : Piaf.Ws.Descriptor.t
      ; presence_refs : (string * string) list ref
      ; subscription : int option
      }

    let create ~pubsub ~presence ~topic ~ws ~presence_refs ~subscription =
      { pubsub; presence; topic; ws; presence_refs; subscription }

    let make_handler ctx =
      { Effect.Deep.retc = Fun.id
      ; exnc = raise
      ; effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Channel.Broadcast_effect
                { topic = btopic; event = bevent; payload = bpayload } ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Pubsub.broadcast
                    ctx.pubsub
                    { topic = btopic; event = bevent; payload = bpayload };
                  Effect.Deep.continue k ())
            | Channel.Broadcast_from_effect
                { topic = btopic; event = bevent; payload = bpayload } ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  (match ctx.subscription with
                  | Some sub ->
                    Pubsub.broadcast_from
                      ctx.pubsub
                      ~self:sub
                      { topic = btopic; event = bevent; payload = bpayload }
                  | None ->
                    (* Fallback to regular broadcast if no subscription *)
                    Pubsub.broadcast
                      ctx.pubsub
                      { topic = btopic; event = bevent; payload = bpayload });
                  Effect.Deep.continue k ())
            | Channel.Push_effect { event = pevent; payload = ppayload } ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  let push_msg =
                    Protocol.make_push
                      ~topic:ctx.topic
                      ~event:pevent
                      ~payload:ppayload
                  in
                  let push_str = Protocol.encode_json push_msg in
                  Piaf.Ws.Descriptor.send_string ctx.ws push_str;
                  Effect.Deep.continue k ())
            | Channel.Track_presence_effect { key; meta } ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  match ctx.presence with
                  | Some presence ->
                    let phx_ref =
                      Presence.track presence ~topic:ctx.topic ~key ~meta
                    in
                    ctx.presence_refs := (key, phx_ref) :: !(ctx.presence_refs);
                    Effect.Deep.continue k phx_ref
                  | None ->
                    Log.warn (fun m ->
                      m "Presence tracking attempted but no presence configured");
                    Effect.Deep.continue k "")
            | Channel.Untrack_presence_effect { phx_ref } ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  match ctx.presence with
                  | Some presence ->
                    Presence.untrack_ref presence ~topic:ctx.topic ~phx_ref;
                    ctx.presence_refs :=
                      List.filter
                        (fun (_, ref_) -> not (String.equal ref_ phx_ref))
                        !(ctx.presence_refs);
                    Effect.Deep.continue k ()
                  | None -> Effect.Deep.continue k ())
            | Channel.Get_presence_list_effect () ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  match ctx.presence with
                  | Some presence ->
                    let state = Presence.list presence ~topic:ctx.topic in
                    Effect.Deep.continue k (Presence.to_json state)
                  | None -> Effect.Deep.continue k (`Assoc []))
            | _ -> None)
      }

    let run ctx f = Effect.Deep.match_with f () (make_handler ctx)
  end

  module Session = struct
    type t =
      { config : config
      ; socket : Socket.t ref
      ; channels : (Topic.t, channel_state) Hashtbl.t
      ; subscriptions : (Topic.t, int) Hashtbl.t
      ; broadcast_queue : Pubsub.message Eio.Stream.t
      ; last_heartbeat : float ref
      ; closed : bool ref
      ; ws : Piaf.Ws.Descriptor.t
      }

    let create ~config ~socket ~ws =
      { config
      ; socket
      ; channels = Hashtbl.create 8
      ; subscriptions = Hashtbl.create 8
      ; broadcast_queue = Eio.Stream.create 100
      ; last_heartbeat = ref (Eio.Time.now config.clock)
      ; closed = ref false
      ; ws
      }
  end

  module Message_handler = struct
    let handle_heartbeat session (msg : Protocol.message) =
      session.Session.last_heartbeat :=
        Eio.Time.now session.Session.config.clock;
      let reply =
        Protocol.make_reply
          ~join_ref:msg.join_ref
          ~ref_:(Option.value msg.ref_ ~default:"")
          ~topic:msg.topic
          ~status:Channel.Ok
          ~payload:`Null
      in
      let reply_str = Protocol.encode_json reply in
      Piaf.Ws.Descriptor.send_string session.Session.ws reply_str

    let handle_join session (msg : Protocol.message) =
      let topic = msg.topic in
      match find_channel session.Session.config.channels topic with
      | Some (Route (_pattern, (module H))) ->
        let state = H.init () in
        let sub =
          Pubsub.subscribe
            session.Session.config.pubsub
            topic
            (fun pubsub_msg ->
               Eio.Stream.add session.Session.broadcast_queue pubsub_msg)
        in
        Hashtbl.replace session.Session.subscriptions topic sub;
        let presence_refs_acc = ref [] in
        let effect_ctx =
          Effect_context.create
            ~pubsub:session.Session.config.pubsub
            ~presence:session.Session.config.presence
            ~topic
            ~ws:session.Session.ws
            ~presence_refs:presence_refs_acc
            ~subscription:(Some sub)
        in
        let join_result =
          try
            Effect_context.run effect_ctx (fun () ->
              H.join
                ~topic
                ~payload:msg.payload
                ~socket:!(session.Session.socket)
                state)
          with
          | exn ->
            Log.err (fun m ->
              m
                "Channel join for topic %s crashed: %s"
                topic
                (Printexc.to_string exn));
            Error (`String "join crashed")
        in
        (match join_result with
        | Ok (new_state, new_socket, response_payload) ->
          let updated_socket =
            { new_socket with
              Socket.joined_topics = topic :: new_socket.Socket.joined_topics
            }
          in
          session.Session.socket := updated_socket;
          let join_ref = Option.value msg.join_ref ~default:"" in
          let chan_state =
            Channel_state
              { handler = (module H)
              ; state = new_state
              ; topic
              ; join_ref
              ; presence_refs = !presence_refs_acc
              }
          in
          Hashtbl.replace session.Session.channels topic chan_state;
          let reply =
            Protocol.make_reply
              ~join_ref:msg.join_ref
              ~ref_:(Option.value msg.ref_ ~default:"")
              ~topic
              ~status:Channel.Ok
              ~payload:response_payload
          in
          let reply_str = Protocol.encode_json reply in
          Piaf.Ws.Descriptor.send_string session.Session.ws reply_str;

          (match session.Session.config.presence with
          | Some presence ->
            let state = Presence.list presence ~topic in
            let presences = Presence.to_json state in
            let push_msg =
              Protocol.make_push
                ~topic
                ~event:"presence_state"
                ~payload:presences
            in
            let push_str = Protocol.encode_json push_msg in
            Piaf.Ws.Descriptor.send_string session.Session.ws push_str
          | None -> ())
        | Error reason ->
          Pubsub.unsubscribe session.Session.config.pubsub sub;
          Hashtbl.remove session.Session.subscriptions topic;
          let reply =
            Protocol.make_reply
              ~join_ref:msg.join_ref
              ~ref_:(Option.value msg.ref_ ~default:"")
              ~topic
              ~status:Channel.Error
              ~payload:reason
          in
          let reply_str = Protocol.encode_json reply in
          Piaf.Ws.Descriptor.send_string session.Session.ws reply_str)
      | None ->
        let error =
          Protocol.make_error ~topic ~payload:(`String "no channel")
        in
        let error_str = Protocol.encode_json error in
        Piaf.Ws.Descriptor.send_string session.Session.ws error_str

    let handle_leave session (msg : Protocol.message) =
      let topic = msg.topic in
      match Hashtbl.find_opt session.Session.channels topic with
      | Some (Channel_state { handler = (module H); state; presence_refs; _ })
        ->
        (match session.Session.config.presence with
        | Some presence ->
          List.iter
            (fun (_key, phx_ref) ->
               Presence.untrack_ref presence ~topic ~phx_ref)
            presence_refs
        | None -> ());
        H.terminate ~reason:Left ~socket:!(session.Session.socket) state;
        Hashtbl.remove session.Session.channels topic;
        let current_socket = !(session.Session.socket) in
        let updated_socket =
          { current_socket with
            Socket.joined_topics =
              List.filter
                (fun t -> not (String.equal t topic))
                current_socket.Socket.joined_topics
          }
        in
        session.Session.socket := updated_socket;
        (match Hashtbl.find_opt session.Session.subscriptions topic with
        | Some sub ->
          Pubsub.unsubscribe session.Session.config.pubsub sub;
          Hashtbl.remove session.Session.subscriptions topic
        | None -> ());
        let reply =
          Protocol.make_reply
            ~join_ref:msg.join_ref
            ~ref_:(Option.value msg.ref_ ~default:"")
            ~topic
            ~status:Channel.Ok
            ~payload:(`Assoc [])
        in
        let reply_str = Protocol.encode_json reply in
        Piaf.Ws.Descriptor.send_string session.Session.ws reply_str
      | None -> ()

    let handle_custom_event session (msg : Protocol.message) =
      let topic = msg.topic in
      match Hashtbl.find_opt session.Session.channels topic with
      | Some
          (Channel_state
             { handler = (module H); state; join_ref; presence_refs; _ } as
           chan_state) ->
        let current_presence_refs = ref presence_refs in
        let subscription =
          Hashtbl.find_opt session.Session.subscriptions topic
        in
        let effect_ctx =
          Effect_context.create
            ~pubsub:session.Session.config.pubsub
            ~presence:session.Session.config.presence
            ~topic
            ~ws:session.Session.ws
            ~presence_refs:current_presence_refs
            ~subscription
        in
        (try
           let result =
             Effect_context.run effect_ctx (fun () ->
               H.handle_in
                 ~event:msg.event
                 ~payload:msg.payload
                 ~socket:!(session.Session.socket)
                 state)
           in
           (match chan_state with
           | Channel_state cs -> cs.presence_refs <- !current_presence_refs);
           match result with
           | Channel.Reply (status, payload, new_state, new_socket) ->
             session.Session.socket := new_socket;
             Hashtbl.replace
               session.Session.channels
               topic
               (Channel_state
                  { handler = (module H)
                  ; state = new_state
                  ; topic
                  ; join_ref
                  ; presence_refs = !current_presence_refs
                  });
             let reply =
               Protocol.make_reply
                 ~join_ref:(Some join_ref)
                 ~ref_:(Option.value msg.ref_ ~default:"")
                 ~topic
                 ~status
                 ~payload
             in
             let reply_str = Protocol.encode_json reply in
             Piaf.Ws.Descriptor.send_string session.Session.ws reply_str
           | Channel.No_reply (new_state, new_socket) ->
             session.Session.socket := new_socket;
             Hashtbl.replace
               session.Session.channels
               topic
               (Channel_state
                  { handler = (module H)
                  ; state = new_state
                  ; topic
                  ; join_ref
                  ; presence_refs = !current_presence_refs
                  })
           | Channel.Stop (reason, new_state, new_socket) ->
             session.Session.socket := new_socket;
             H.terminate
               ~reason:(Error (Failure reason))
               ~socket:!(session.Session.socket)
               new_state;
             Hashtbl.remove session.Session.channels topic
         with
        | exn ->
          Log.err (fun m ->
            m
              "Channel handler for topic %s crashed: %s"
              topic
              (Printexc.to_string exn));
          let reply =
            Protocol.make_reply
              ~join_ref:(Some join_ref)
              ~ref_:(Option.value msg.ref_ ~default:"")
              ~topic
              ~status:Channel.Error
              ~payload:(`Assoc [ "reason", `String "crash" ])
          in
          let reply_str = Protocol.encode_json reply in
          Piaf.Ws.Descriptor.send_string session.Session.ws reply_str;
          H.terminate
            ~reason:(Error exn)
            ~socket:!(session.Session.socket)
            state;
          Hashtbl.remove session.Session.channels topic;
          (match Hashtbl.find_opt session.Session.subscriptions topic with
          | Some sub ->
            Pubsub.unsubscribe session.Session.config.pubsub sub;
            Hashtbl.remove session.Session.subscriptions topic
          | None -> ()))
      | None ->
        Log.warn (fun m ->
          m
            "Channel not joined for topic %s. Available topics: [%s]"
            topic
            (Hashtbl.to_seq_keys session.Session.channels
            |> Seq.map (fun t -> "\"" ^ t ^ "\"")
            |> List.of_seq
            |> String.concat ", "));
        let error =
          Protocol.make_error ~topic ~payload:(`String "channel not joined")
        in
        let error_str = Protocol.encode_json error in
        Piaf.Ws.Descriptor.send_string session.Session.ws error_str
  end

  let handle_ws_message session (msg : Protocol.message) =
    match msg.event with
    | event when String.equal event Protocol.heartbeat ->
      Message_handler.handle_heartbeat session msg
    | event when String.equal event Protocol.phx_join ->
      Message_handler.handle_join session msg
    | event when String.equal event Protocol.phx_leave ->
      Message_handler.handle_leave session msg
    | _ -> Message_handler.handle_custom_event session msg

  let cleanup_session session =
    session.Session.closed := true;
    Hashtbl.iter
      (fun topic
        (Channel_state { handler = (module H); state; presence_refs; _ }) ->
         (match session.Session.config.presence with
         | Some presence ->
           List.iter
             (fun (_key, phx_ref) ->
                Presence.untrack_ref presence ~topic ~phx_ref)
             presence_refs
         | None -> ());
         H.terminate ~reason:Closed ~socket:!(session.Session.socket) state)
      session.Session.channels;
    Hashtbl.iter
      (fun _topic sub -> Pubsub.unsubscribe session.Session.config.pubsub sub)
      session.Session.subscriptions;
    match Socket.id !(session.Session.socket) with
    | Some id ->
      Eio.Mutex.use_rw
        ~protect:true
        session.Session.config.active_sockets_mutex
        (fun () ->
           match Hashtbl.find_opt session.Session.config.active_sockets id with
           | Some wss ->
             let remaining =
               List.filter (fun w -> w != session.Session.ws) wss
             in
             if List.length remaining = 0
             then Hashtbl.remove session.Session.config.active_sockets id
             else
               Hashtbl.replace
                 session.Session.config.active_sockets
                 id
                 remaining
           | None -> ());
      (* Broadcast disconnect to other nodes *)
      (match session.Session.config.presence with
      | Some presence -> Presence.broadcast_disconnect presence ~socket_id:id
      | None -> ())
    | None -> ()

  let process_broadcast session pubsub_msg =
    let topic = pubsub_msg.Pubsub.topic in
    match Hashtbl.find_opt session.Session.channels topic with
    | Some
        (Channel_state
           { handler = (module H); state; join_ref; presence_refs; _ }) ->
      let broadcast_msg =
        Channel.
          { topic; event = pubsub_msg.event; payload = pubsub_msg.payload }
      in
      let control, new_state, new_socket =
        H.handle_info broadcast_msg ~socket:!(session.Session.socket) state
      in
      session.Session.socket := new_socket;
      Hashtbl.replace
        session.Session.channels
        topic
        (Channel_state
           { handler = (module H)
           ; state = new_state
           ; topic
           ; join_ref
           ; presence_refs
           });
      let should_intercept = List.mem pubsub_msg.event H.intercept in
      (match control with
      | Channel.Push payload ->
        if should_intercept
        then (
          let control2, new_state2, new_socket2 =
            H.handle_out
              ~event:pubsub_msg.event
              ~payload
              ~socket:!(session.Session.socket)
              new_state
          in
          session.Session.socket := new_socket2;
          Hashtbl.replace
            session.Session.channels
            topic
            (Channel_state
               { handler = (module H)
               ; state = new_state2
               ; topic
               ; join_ref
               ; presence_refs
               });
          match control2 with
          | Channel.Push final_payload | Channel.Intercept final_payload ->
            let push_msg =
              Protocol.make_push
                ~topic
                ~event:pubsub_msg.event
                ~payload:final_payload
            in
            let push_str = Protocol.encode_json push_msg in
            Piaf.Ws.Descriptor.send_string session.Session.ws push_str
          | Channel.Suppress -> ())
        else
          let push_msg =
            Protocol.make_push ~topic ~event:pubsub_msg.event ~payload
          in
          let push_str = Protocol.encode_json push_msg in
          Piaf.Ws.Descriptor.send_string session.Session.ws push_str
      | Channel.Intercept payload ->
        let control2, new_state2, new_socket2 =
          H.handle_out
            ~event:pubsub_msg.event
            ~payload
            ~socket:!(session.Session.socket)
            new_state
        in
        session.Session.socket := new_socket2;
        Hashtbl.replace
          session.Session.channels
          topic
          (Channel_state
             { handler = (module H)
             ; state = new_state2
             ; topic
             ; join_ref
             ; presence_refs
             });
        (match control2 with
        | Channel.Push final_payload | Channel.Intercept final_payload ->
          let push_msg =
            Protocol.make_push
              ~topic
              ~event:pubsub_msg.event
              ~payload:final_payload
          in
          let push_str = Protocol.encode_json push_msg in
          Piaf.Ws.Descriptor.send_string session.Session.ws push_str
        | Channel.Suppress -> ())
      | Channel.Suppress -> ())
    | None -> ()

  let run_message_loop session =
    let messages = Piaf.Ws.Descriptor.messages session.Session.ws in
    let process_frame (opcode, iovec) =
      match opcode with
      | `Text | `Binary ->
        let buf = iovec.Piaf.IOVec.buffer in
        let off = iovec.Piaf.IOVec.off in
        let len = iovec.Piaf.IOVec.len in
        let str = Bigstringaf.substring buf ~off ~len in
        (match Protocol.decode_json str with
        | Ok msg -> handle_ws_message session msg
        | Error err ->
          Log.err (fun m -> m "Failed to decode WebSocket message: %s" err))
      | `Connection_close ->
        cleanup_session session;
        Piaf.Ws.Descriptor.close session.Session.ws
      | _ -> ()
    in
    (try Piaf.Stream.iter ~f:process_frame messages with _ -> ());
    cleanup_session session

  let run_broadcast_loop session =
    try
      while not !(session.Session.closed) do
        let msg = Eio.Stream.take session.Session.broadcast_queue in
        if not !(session.Session.closed) then process_broadcast session msg
      done
    with
    | _ -> ()

  let run_heartbeat_monitor session =
    try
      while not !(session.Session.closed) do
        Eio.Time.sleep
          session.Session.config.clock
          session.Session.config.heartbeat_interval;
        let now = Eio.Time.now session.Session.config.clock in
        let elapsed = now -. !(session.Session.last_heartbeat) in
        if elapsed > session.Session.config.timeout
        then (
          Log.info (fun m ->
            m
              "Socket timeout: no heartbeat for %.1fs (timeout: %.1fs)"
              elapsed
              session.Session.config.timeout);
          cleanup_session session;
          Piaf.Ws.Descriptor.close session.Session.ws)
      done
    with
    | _ -> ()

  let ws_handler config request ws =
    let (module S : Socket.HANDLER) = config.socket in
    let uri = Tapak_kernel.Request.to_piaf request |> Piaf.Request.uri in
    let params =
      match Uri.query uri with
      | [] -> `Assoc []
      | query ->
        `Assoc
          (List.map (fun (k, vs) -> k, `String (String.concat "," vs)) query)
    in
    let connect_info = Socket.{ params; request } in
    match S.connect connect_info with
    | Error _reason -> Piaf.Ws.Descriptor.close ws
    | Ok initial_assigns ->
      let socket_id = S.id initial_assigns in
      let socket =
        ref
          Socket.
            { id = socket_id
            ; assigns = initial_assigns
            ; transport = Websocket
            ; joined_topics = []
            }
      in
      (match socket_id with
      | Some id ->
        Eio.Mutex.use_rw ~protect:true config.active_sockets_mutex (fun () ->
          let existing =
            Option.value (Hashtbl.find_opt config.active_sockets id) ~default:[]
          in
          Hashtbl.replace config.active_sockets id (ws :: existing))
      | None -> ());
      let session = Session.create ~config ~socket ~ws in
      Eio.Fiber.all
        [ (fun () -> run_message_loop session)
        ; (fun () -> run_broadcast_loop session)
        ; (fun () -> run_heartbeat_monitor session)
        ]

  let handler : config -> Tapak_kernel.Request.t -> Tapak_kernel.Response.t =
   fun config request ->
    match
      Tapak_kernel.Response.Upgrade.websocket
        ~f:(ws_handler config request)
        request
    with
    | Ok response -> response
    | Error _ ->
      Tapak_kernel.Response.of_string
        ~body:"WebSocket upgrade failed"
        `Internal_server_error

  let routes : config -> Tapak_kernel.Router.route list =
   fun config ->
    let open Tapak_kernel.Router in
    [ get (s "websocket") |> request |> into (handler config) ]

  let disconnect : config -> Socket.id -> unit =
   fun config socket_id ->
    match socket_id with
    | Some id ->
      let wss_to_close =
        Eio.Mutex.use_rw ~protect:false config.active_sockets_mutex (fun () ->
          Hashtbl.find_opt config.active_sockets id)
      in
      (match wss_to_close with
      | Some wss -> List.iter Piaf.Ws.Descriptor.close wss
      | None -> ())
    | None -> ()
end

let local_pubsub ~sw =
  let backend = Pubsub.Local.start ~sw in
  Pubsub.create (module Pubsub.Local) backend
