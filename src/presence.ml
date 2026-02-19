module Log = (val Logs.src_log (Logs.Src.create "tapak.presence"))

type meta = Jsont.json
type entry = { metas : meta list }
type state = (string, entry) Hashtbl.t

let sync_topic = "__presence_sync__"
(* the name of the topic is not important, but it should be unique enough to
   avoid conflict with other topics in pubsub *)

type lww_meta =
  { topic : string
  ; key : string
  ; meta : meta
  ; phx_ref : string
  ; node : string
  ; timestamp : float
  }

let lww_meta_jsont =
  let make topic key meta phx_ref node timestamp =
    { topic; key; meta; phx_ref; node; timestamp }
  in
  Jsont.Object.map ~kind:"lww_meta" make
  |> Jsont.Object.mem ~enc:(fun m -> m.topic) "topic" Jsont.string
  |> Jsont.Object.mem ~enc:(fun m -> m.key) "key" Jsont.string
  |> Jsont.Object.mem ~enc:(fun m -> m.meta) "meta" Jsont.json
  |> Jsont.Object.mem ~enc:(fun m -> m.phx_ref) "phx_ref" Jsont.string
  |> Jsont.Object.mem ~enc:(fun m -> m.node) "node" Jsont.string
  |> Jsont.Object.mem ~enc:(fun m -> m.timestamp) "timestamp" Jsont.number
  |> Jsont.Object.finish

let tup2 codec_a codec_b =
  Jsont.map
    ~kind:"tuple2"
    ~enc:(fun (a, b) ->
      match Jsont.Json.encode codec_a a, Jsont.Json.encode codec_b b with
      | Ok a, Ok b -> Jsont.Array ([ a; b ], Jsont.Meta.none)
      | _ -> Jsont.Error.msgf Jsont.Meta.none "Failed to encode tuple2")
    ~dec:(fun json ->
      match json with
      | Jsont.Array ([ a; b ], _) ->
        (match Jsont.Json.decode codec_a a, Jsont.Json.decode codec_b b with
        | Ok a, Ok b -> a, b
        | _ -> Jsont.Error.msgf Jsont.Meta.none "Failed to decode tuple2")
      | _ -> Jsont.Error.msgf Jsont.Meta.none "Expected an array of length 2")
    Jsont.json_array

let entry_jsont =
  let make metas = { metas } in
  Jsont.Object.map ~kind:"entry" make
  |> Jsont.Object.mem ~enc:(fun m -> m.metas) "metas" (Jsont.list Jsont.json)
  |> Jsont.Object.finish

let entry_assoc_jsont =
  let module M = Map.Make (String) in
  Jsont.map
    ~kind:"entry_assoc"
    ~dec:(fun m -> M.bindings m)
    ~enc:(fun l -> M.of_seq (List.to_seq l))
    (Jsont.Object.as_string_map entry_jsont)

module Diff = struct
  type t =
    { joins : (string * entry) list
    ; leaves : (string * entry) list
    }

  let jsont =
    let make joins leaves = { joins; leaves } in
    Jsont.Object.map ~kind:"diff" make
    |> Jsont.Object.mem ~enc:(fun m -> m.joins) "joins" entry_assoc_jsont
    |> Jsont.Object.mem ~enc:(fun m -> m.leaves) "leaves" entry_assoc_jsont
    |> Jsont.Object.finish
end

module Message = struct
  type heartbeat =
    { node : string
    ; timestamp : float
    }

  type delta =
    { node : string
    ; joins : lww_meta list
    ; leaves : (string * string) list (* list of topic and key *)
    }

  type disconnect = { socket_id : string }
  type state_request = { node : string }

  type state_response =
    { node : string
    ; state : (string * (string * lww_meta list) list) list
    }

  type t =
    | Heartbeat of heartbeat
    | Delta of delta
    | Disconnect of disconnect
    | State_request of state_request
    | State_response of state_response

  let leaves_jsont = tup2 Jsont.string Jsont.string

  let state_jsont =
    Jsont.list
      (tup2
         Jsont.string
         (Jsont.list (tup2 Jsont.string (Jsont.list lww_meta_jsont))))

  let heartbeat_jsont : heartbeat Jsont.t =
    let make : string -> float -> heartbeat =
     fun node timestamp -> { node; timestamp }
    in
    Jsont.Object.map ~kind:"heartbeat" make
    |> Jsont.Object.mem ~enc:(fun (m : heartbeat) -> m.node) "node" Jsont.string
    |> Jsont.Object.mem ~enc:(fun m -> m.timestamp) "timestamp" Jsont.number
    |> Jsont.Object.finish

  let delta_jsont : delta Jsont.t =
    let make node joins leaves = { node; joins; leaves } in
    Jsont.Object.map ~kind:"delta" make
    |> Jsont.Object.mem ~enc:(fun (m : delta) -> m.node) "node" Jsont.string
    |> Jsont.Object.mem
         ~enc:(fun m -> m.joins)
         "joins"
         (Jsont.list lww_meta_jsont)
    |> Jsont.Object.mem
         ~enc:(fun m -> m.leaves)
         "leaves"
         (Jsont.list leaves_jsont)
    |> Jsont.Object.finish

  let disconnect_jsont : disconnect Jsont.t =
    let make socket_id = { socket_id } in
    Jsont.Object.map ~kind:"disconnect" make
    |> Jsont.Object.mem ~enc:(fun m -> m.socket_id) "socket_id" Jsont.string
    |> Jsont.Object.finish

  let state_request_jsont : state_request Jsont.t =
    let make node = { node } in
    Jsont.Object.map ~kind:"state_request" make
    |> Jsont.Object.mem
         ~enc:(fun (m : state_request) -> m.node)
         "node"
         Jsont.string
    |> Jsont.Object.finish

  let state_response_jsont : state_response Jsont.t =
    let make node state = { node; state } in
    Jsont.Object.map ~kind:"state_response" make
    |> Jsont.Object.mem
         ~enc:(fun (m : state_response) -> m.node)
         "node"
         Jsont.string
    |> Jsont.Object.mem ~enc:(fun m -> m.state) "state" state_jsont
    |> Jsont.Object.finish

  let case_map obj dec = Jsont.Object.Case.map (Jsont.kind obj) obj ~dec

  let jsont =
    let case_heartbeat = case_map heartbeat_jsont (fun h -> Heartbeat h) in
    let case_delta = case_map delta_jsont (fun d -> Delta d) in
    let case_disconnect = case_map disconnect_jsont (fun d -> Disconnect d) in
    let case_state_request =
      case_map state_request_jsont (fun r -> State_request r)
    in
    let state_response =
      case_map state_response_jsont (fun r -> State_response r)
    in

    let enc_case = function
      | Heartbeat h -> Jsont.Object.Case.value case_heartbeat h
      | Delta d -> Jsont.Object.Case.value case_delta d
      | Disconnect d -> Jsont.Object.Case.value case_disconnect d
      | State_request r -> Jsont.Object.Case.value case_state_request r
      | State_response r -> Jsont.Object.Case.value state_response r
    in

    let cases =
      Jsont.Object.Case.
        [ make case_heartbeat
        ; make case_delta
        ; make case_disconnect
        ; make case_state_request
        ; make state_response
        ]
    in
    Jsont.Object.map ~kind:"presence_message" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.finish
end

type t =
  { pubsub : Pubsub.t
  ; node_name : string
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; global_state : (string, (string, lww_meta list) Hashtbl.t) Hashtbl.t
  ; local_state : (string, state) Hashtbl.t
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

let dominated_lww_meta existing new_meta =
  String.equal existing.phx_ref new_meta.phx_ref
  && new_meta.timestamp > existing.timestamp

let merge_lww_metas existing new_metas =
  let dominated_by_new existing_meta =
    List.exists (fun nm -> dominated_lww_meta existing_meta nm) new_metas
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

let add_ref ~ref json =
  match json with
  | Jsont.Object (mems, mt) ->
    Jsont.Json.(object' ~meta:mt ([ mem (name "phx_ref") (string ref) ] @ mems))
  | other ->
    Jsont.Json.(
      object' [ mem (name "phx_ref") (string ref); mem (name "other") other ])

let rebuild_local_state t =
  Hashtbl.clear t.local_state;
  t.global_state
  |> Hashtbl.iter (fun topic global_topic_state ->
    let presence_state = Hashtbl.create 16 in
    Hashtbl.iter
      (fun key lww_metas ->
         let metas =
           lww_metas |> List.map (fun lww -> add_ref ~ref:lww.phx_ref lww.meta)
         in
         if List.length metas > 0 then Hashtbl.add presence_state key { metas })
      global_topic_state;
    if Hashtbl.length presence_state > 0
    then Hashtbl.add t.local_state topic presence_state)

let handle_sync_message t msg =
  match msg with
  | Message.Heartbeat { node; timestamp } ->
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
  | State_request { node = requesting_node } ->
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
      let response = Message.State_response { node = t.node_name; state } in
      Pubsub.broadcast
        t.pubsub
        { topic = sync_topic
        ; event = "sync"
        ; payload = Jsont.Json.encode Message.jsont response |> Result.get_ok
        ; origin = None
        }
  | State_response { node; state } ->
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
                    Option.value (Hashtbl.find_opt topic_state key) ~default:[]
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
            List.iter (fun m -> leaves := (topic, m.phx_ref) :: !leaves) removed;
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
             let msg =
               Message.Heartbeat { node = t.node_name; timestamp = now }
             in
             Pubsub.broadcast
               t.pubsub
               { topic = sync_topic
               ; event = "sync"
               ; origin = None
               ; payload = Jsont.Json.encode Message.jsont msg |> Result.get_ok
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
          match Jsont.Json.decode Message.jsont msg.payload with
          | Ok p -> handle_sync_message t p
          | Error err ->
            Log.err (fun m -> m "Failed to decode sync message: %s" err)
        with
        | exn ->
          Log.err (fun m ->
            m "Failed to decode sync message: %s" (Printexc.to_string exn)))
  in
  Eio.Fiber.fork ~sw (fun () -> run_heartbeat_loop t);
  let request = Message.State_request { node = node_name } in
  Pubsub.broadcast
    pubsub
    { topic = sync_topic
    ; event = "sync"
    ; origin = None
    ; payload = Jsont.Json.encode Message.jsont request |> Result.get_ok
    };
  t

let track ~topic ~key ~meta t =
  let timestamp = Eio.Time.now t.clock in
  let phx_ref = generate_ref t in
  let lww_meta = { topic; key; meta; phx_ref; node = t.node_name; timestamp } in
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
    Message.Delta { node = t.node_name; joins = [ lww_meta ]; leaves = [] }
  in
  Pubsub.broadcast
    t.pubsub
    { topic = sync_topic
    ; event = "sync"
    ; origin = None
    ; payload = Jsont.Json.encode Message.jsont delta |> Result.get_ok
    };
  let diff =
    { Diff.joins = [ key, { metas = [ add_ref ~ref:phx_ref meta ] } ]
    ; leaves = []
    }
  in
  Pubsub.broadcast
    t.pubsub
    { topic
    ; event = "presence_diff"
    ; origin = None
    ; payload = Jsont.Json.encode Diff.jsont diff |> Result.get_ok
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
      Message.Delta
        { node = t.node_name; joins = []; leaves = [ topic, phx_ref ] }
    in
    Pubsub.broadcast
      t.pubsub
      { topic = sync_topic
      ; event = "sync"
      ; origin = None
      ; payload = Jsont.Json.encode Message.jsont delta |> Result.get_ok
      };
    let entry =
      { metas = Jsont.Json.[ object' [ mem (name "phx_ref") (string phx_ref) ] ]
      }
    in
    let diff = { Diff.joins = []; leaves = [ key, entry ] } in
    Pubsub.broadcast
      t.pubsub
      { topic
      ; event = "presence_diff"
      ; origin = None
      ; payload = Jsont.Json.encode Diff.jsont diff |> Result.get_ok
      }
  | None -> ()

let list t ~topic =
  Eio.Mutex.use_rw ~protect:false t.state_mutex (fun () ->
    match Hashtbl.find_opt t.local_state topic with
    | Some s -> s
    | None -> Hashtbl.create 0)

let broadcast_disconnect t ~socket_id =
  let msg = Message.Disconnect { socket_id } in
  Pubsub.broadcast
    t.pubsub
    { topic = sync_topic
    ; event = "sync"
    ; origin = None
    ; payload = Jsont.Json.encode Message.jsont msg |> Result.get_ok
    }

let on_disconnect t handler =
  Eio.Mutex.use_rw ~protect:true t.state_mutex (fun () ->
    t.disconnect_handlers := handler :: !(t.disconnect_handlers))

let close t =
  t.closed <- true;
  let _, resolver = t.stop_signal in
  Eio.Promise.resolve resolver ()

let node_name t = t.node_name

let state_to_json : state -> Jsont.json =
 fun state ->
  let entries =
    Hashtbl.fold
      (fun key entry acc ->
         let metas_json = Jsont.Json.list entry.metas in
         let entry_json =
           Jsont.Json.(object' [ mem (name "metas") metas_json ])
         in
         Jsont.Json.(mem (name key) entry_json) :: acc)
      state
      []
  in
  Jsont.Json.object' entries
