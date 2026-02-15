module Channel = Tapak.Channel
module Socket = Tapak.Socket
module Presence = Tapak.Presence

let json_eq = Alcotest.testable Jsont.Json.pp Jsont.Json.equal
let encode_json jsont v = Jsont_bytesrw.encode_string jsont v |> Result.get_ok

let dummy_socket =
  Socket.
    { id = Some "test-socket"
    ; assigns = Tapak.Context.empty
    ; transport = "test"
    ; joined_topics = []
    }

let dummy_payload = Jsont.Json.(object' [ mem (name "body") (string "hi") ])

let test_protocol_encode_json () =
  let msg =
    Socket.Protocol.
      { join_ref = Some "1"
      ; ref_ = Some "2"
      ; topic = "room:lobby"
      ; event = "new_msg"
      ; payload = Jsont.Json.(object' [ mem (name "body") (string "hello") ])
      }
  in
  let encoded = encode_json Socket.Protocol.jsont msg in
  let expected = {|["1","2","room:lobby","new_msg",{"body":"hello"}]|} in
  Alcotest.(check string) "encode protocol" expected encoded

let test_protocol_encode_json_nulls () =
  let msg =
    Socket.Protocol.
      { join_ref = None
      ; ref_ = None
      ; topic = "room:lobby"
      ; event = "new_msg"
      ; payload = Jsont.Json.(object' [])
      }
  in
  let encoded = encode_json Socket.Protocol.jsont msg in
  let expected = {|[null,null,"room:lobby","new_msg",{}]|} in
  Alcotest.(check string) "encode protocol with nulls" expected encoded

let test_protocol_decode_json () =
  let json = {|["1","2","room:lobby","new_msg",{"body":"hello"}]|} in
  let decoded = Jsont_bytesrw.decode_string Socket.Protocol.jsont json in
  match decoded with
  | Ok msg ->
    Alcotest.(check (option string)) "join_ref" (Some "1") msg.join_ref;
    Alcotest.(check (option string)) "ref" (Some "2") msg.ref_;
    Alcotest.(check string) "topic" "room:lobby" msg.topic;
    Alcotest.(check string) "event" "new_msg" msg.event
  | Error e -> Alcotest.failf "Failed to decode protocol: %s" e

let test_protocol_decode_json_nulls () =
  let json = {|[null,null,"room:lobby","new_msg",{}]|} in
  let decoded = Jsont_bytesrw.decode_string Socket.Protocol.jsont json in
  match decoded with
  | Ok msg ->
    Alcotest.(check (option string)) "join_ref" None msg.join_ref;
    Alcotest.(check (option string)) "ref" None msg.ref_;
    Alcotest.(check string) "topic" "room:lobby" msg.topic;
    Alcotest.(check string) "event" "new_msg" msg.event
  | Error e -> Alcotest.failf "Failed to decode protocol with nulls: %s" e

let test_protocol_roundtrip () =
  let msg =
    Socket.Protocol.
      { join_ref = Some "ref-1"
      ; ref_ = Some "42"
      ; topic = "chat:general"
      ; event = "new_msg"
      ; payload =
          Jsont.Json.(
            object'
              [ mem (name "text") (string "hello")
              ; mem (name "count") (number 3.)
              ])
      }
  in
  let encoded = encode_json Socket.Protocol.jsont msg in
  match Jsont_bytesrw.decode_string Socket.Protocol.jsont encoded with
  | Ok decoded ->
    Alcotest.(check (option string)) "join_ref" msg.join_ref decoded.join_ref;
    Alcotest.(check (option string)) "ref_" msg.ref_ decoded.ref_;
    Alcotest.(check string) "topic" msg.topic decoded.topic;
    Alcotest.(check string) "event" msg.event decoded.event;
    Alcotest.(check json_eq) "payload" msg.payload decoded.payload
  | Error e -> Alcotest.failf "Roundtrip failed: %s" e

let protocol_tests =
  [ "encode_json", `Quick, test_protocol_encode_json
  ; "encode_json_nulls", `Quick, test_protocol_encode_json_nulls
  ; "decode_json", `Quick, test_protocol_decode_json
  ; "decode_json_nulls", `Quick, test_protocol_decode_json_nulls
  ; "roundtrip", `Quick, test_protocol_roundtrip
  ]

let test_reply_string_of_status () =
  Alcotest.(check string) "ok" "ok" (Channel.Reply.string_of_status `Ok);
  Alcotest.(check string)
    "error"
    "error"
    (Channel.Reply.string_of_status `Error)

let test_reply_respond () =
  let transition = Channel.{ state = 42; socket = dummy_socket } in
  match
    Channel.Reply.respond ~transition ~status:`Ok ~payload:dummy_payload
  with
  | Channel.Reply.Respond { transition = t; status; payload } ->
    Alcotest.(check int) "state" 42 t.state;
    Alcotest.(check string)
      "status"
      "ok"
      (Channel.Reply.string_of_status status);
    Alcotest.(check json_eq) "payload" dummy_payload payload
  | _ -> Alcotest.fail "Expected Respond"

let test_reply_stop () =
  let transition = Channel.{ state = "stopped"; socket = dummy_socket } in
  match Channel.Reply.stop ~transition ~reason:"timeout" with
  | Channel.Reply.Stop { transition = t; reason } ->
    Alcotest.(check string) "state" "stopped" t.state;
    Alcotest.(check string) "reason" "timeout" reason
  | _ -> Alcotest.fail "Expected Stop"

let test_reply_noop () =
  let transition = Channel.{ state = 99; socket = dummy_socket } in
  match Channel.Reply.noop transition with
  | Channel.Reply.Noop t -> Alcotest.(check int) "state" 99 t.state
  | _ -> Alcotest.fail "Expected Noop"

let reply_tests =
  [ "string_of_status", `Quick, test_reply_string_of_status
  ; "respond", `Quick, test_reply_respond
  ; "stop", `Quick, test_reply_stop
  ; "noop", `Quick, test_reply_noop
  ]

let test_join_ok () =
  let transition = Channel.{ state = "ready"; socket = dummy_socket } in
  let response =
    Jsont.Json.(object' [ mem (name "status") (string "joined") ])
  in
  match Channel.Join.ok ~transition ~response with
  | Channel.Join.Ok { transition = t; response = r } ->
    Alcotest.(check string) "state" "ready" t.state;
    Alcotest.(check json_eq) "response" response r
  | _ -> Alcotest.fail "Expected Ok"

let test_join_error () =
  let reason = Jsont.Json.(string "unauthorized") in
  match Channel.Join.error reason with
  | Channel.Join.Error { reason = r } ->
    Alcotest.(check json_eq) "reason" reason r
  | _ -> Alcotest.fail "Expected Error"

let join_tests =
  [ "ok", `Quick, test_join_ok; "error", `Quick, test_join_error ]

let test_push_push () =
  let transition = Channel.{ state = 10; socket = dummy_socket } in
  let push = Channel.Push.push ~transition ~payload:dummy_payload in
  Alcotest.(check int) "state" 10 (Channel.Push.state push);
  Alcotest.(check (option string))
    "socket.id"
    (Some "test-socket")
    (Socket.id (Channel.Push.socket push));
  match push with
  | Channel.Push.Push { payload; _ } ->
    Alcotest.(check json_eq) "payload" dummy_payload payload
  | _ -> Alcotest.fail "Expected Push"

let test_push_intercept () =
  let transition = Channel.{ state = 20; socket = dummy_socket } in
  let push = Channel.Push.intercept ~transition ~payload:dummy_payload in
  Alcotest.(check int) "state" 20 (Channel.Push.state push);
  match push with
  | Channel.Push.Intercept { payload; _ } ->
    Alcotest.(check json_eq) "payload" dummy_payload payload
  | _ -> Alcotest.fail "Expected Intercept"

let test_push_suppress () =
  let transition = Channel.{ state = 30; socket = dummy_socket } in
  let push = Channel.Push.suppress transition in
  Alcotest.(check int) "state" 30 (Channel.Push.state push);
  match push with
  | Channel.Push.Suppress _ -> ()
  | _ -> Alcotest.fail "Expected Suppress"

let push_tests =
  [ "push", `Quick, test_push_push
  ; "intercept", `Quick, test_push_intercept
  ; "suppress", `Quick, test_push_suppress
  ]

let test_default_handle_info () =
  let msg =
    Channel.{ topic = "room:1"; event = "msg"; payload = dummy_payload }
  in
  let push = Channel.Default.handle_info msg ~socket:dummy_socket "mystate" in
  Alcotest.(check string) "state" "mystate" (Channel.Push.state push);
  match push with
  | Channel.Push.Push { payload; _ } ->
    Alcotest.(check json_eq) "forwards payload" dummy_payload payload
  | _ -> Alcotest.fail "Expected Push from handle_info"

let test_default_handle_out () =
  let push =
    Channel.Default.handle_out
      ~event:"new_msg"
      ~payload:dummy_payload
      ~socket:dummy_socket
      "mystate"
  in
  Alcotest.(check string) "state" "mystate" (Channel.Push.state push);
  match push with
  | Channel.Push.Push { payload; _ } ->
    Alcotest.(check json_eq) "forwards payload" dummy_payload payload
  | _ -> Alcotest.fail "Expected Push from handle_out"

let test_default_terminate () =
  Channel.Default.terminate ~reason:Normal ~socket:dummy_socket ();
  Channel.Default.terminate ~reason:Left ~socket:dummy_socket ();
  Channel.Default.terminate ~reason:Closed ~socket:dummy_socket ();
  Channel.Default.terminate
    ~reason:(Error (Failure "boom"))
    ~socket:dummy_socket
    ()

let test_default_intercept () =
  Alcotest.(check (list string)) "empty" [] Channel.Default.intercept

let default_tests =
  [ "handle_info forwards payload", `Quick, test_default_handle_info
  ; "handle_out forwards payload", `Quick, test_default_handle_out
  ; "terminate is no-op", `Quick, test_default_terminate
  ; "intercept is empty", `Quick, test_default_intercept
  ]

let test_presence_state_to_json_empty () =
  let state : Presence.state = Hashtbl.create 0 in
  let json = Presence.state_to_json state in
  Alcotest.(check json_eq) "empty state" (Jsont.Json.object' []) json

let test_presence_state_to_json () =
  let state : Presence.state = Hashtbl.create 4 in
  let meta1 = Jsont.Json.(object' [ mem (name "phx_ref") (string "ref-1") ]) in
  Hashtbl.add state "user1" Presence.{ metas = [ meta1 ] };
  let json = Presence.state_to_json state in
  let encoded = encode_json Jsont.json json in
  (* Must be an object with key "user1" containing {"metas": [...]} *)
  let decoded = Jsont_bytesrw.decode_string Jsont.json_object encoded in
  match decoded with
  | Ok (Jsont.Object (mems, _)) ->
    (* Should have one member "user1" *)
    Alcotest.(check int) "one member" 1 (List.length mems);
    (* Check the structure: user1 -> {metas: [{phx_ref: "ref-1"}]} *)
    let expected_str = {|{"user1":{"metas":[{"phx_ref":"ref-1"}]}}|} in
    Alcotest.(check string) "json shape" expected_str encoded
  | Ok _ -> Alcotest.fail "Expected JSON object"
  | Error e -> Alcotest.failf "Failed to decode: %s" e

let test_presence_state_to_json_multiple_keys () =
  let state : Presence.state = Hashtbl.create 4 in
  let meta_a = Jsont.Json.(object' [ mem (name "phx_ref") (string "a") ]) in
  let meta_b = Jsont.Json.(object' [ mem (name "phx_ref") (string "b") ]) in
  Hashtbl.add state "alice" Presence.{ metas = [ meta_a ] };
  Hashtbl.add state "bob" Presence.{ metas = [ meta_b ] };
  let json = Presence.state_to_json state in
  let encoded = encode_json Jsont.json json in
  (* Decode and verify both keys exist with proper structure *)
  let decoded = Jsont_bytesrw.decode_string Jsont.json_object encoded in
  match decoded with
  | Ok (Jsont.Object (mems, _)) ->
    Alcotest.(check int) "two members" 2 (List.length mems)
  | Ok _ -> Alcotest.fail "Expected JSON object"
  | Error e -> Alcotest.failf "Failed to decode: %s" e

let test_presence_state_to_json_multiple_metas () =
  let state : Presence.state = Hashtbl.create 4 in
  let meta1 = Jsont.Json.(object' [ mem (name "phx_ref") (string "r1") ]) in
  let meta2 = Jsont.Json.(object' [ mem (name "phx_ref") (string "r2") ]) in
  Hashtbl.add state "user1" Presence.{ metas = [ meta1; meta2 ] };
  let json = Presence.state_to_json state in
  let encoded = encode_json Jsont.json json in
  let expected = {|{"user1":{"metas":[{"phx_ref":"r1"},{"phx_ref":"r2"}]}}|} in
  Alcotest.(check string) "multiple metas" expected encoded

let presence_tests =
  [ "state_to_json empty", `Quick, test_presence_state_to_json_empty
  ; "state_to_json single key", `Quick, test_presence_state_to_json
  ; ( "state_to_json multiple keys"
    , `Quick
    , test_presence_state_to_json_multiple_keys )
  ; ( "state_to_json multiple metas"
    , `Quick
    , test_presence_state_to_json_multiple_metas )
  ]

module Dummy_channel : Channel.S = struct
  type t = unit

  let init () = ()

  let join ~topic:_ ~payload:_ ~socket _ =
    Channel.Join.ok
      ~transition:{ state = (); socket }
      ~response:(Jsont.Json.object' [])

  let handle_in ~event:_ ~payload:_ ~socket state =
    Channel.Reply.noop { state; socket }

  let handle_info (msg : Channel.broadcast) ~socket state =
    Channel.Push.push ~transition:{ state; socket } ~payload:msg.payload

  let handle_out ~event:_ ~payload ~socket state =
    Channel.Push.push ~transition:{ state; socket } ~payload

  let terminate ~reason:_ ~socket:_ _ = ()
  let intercept = []
end

let test_find_channel_exact () =
  let channels =
    [ Socket.Endpoint.channel "^room:.*$" (module Dummy_channel) ]
  in
  let result = Socket.Endpoint.find_channel channels "room:lobby" in
  Alcotest.(check bool) "matches room:lobby" true (Option.is_some result)

let test_find_channel_wildcard () =
  let channels =
    [ Socket.Endpoint.channel "^chat:.*$" (module Dummy_channel) ]
  in
  Alcotest.(check bool)
    "matches chat:general"
    true
    (Option.is_some (Socket.Endpoint.find_channel channels "chat:general"));
  Alcotest.(check bool)
    "matches chat:123"
    true
    (Option.is_some (Socket.Endpoint.find_channel channels "chat:123"))

let test_find_channel_no_match () =
  let channels =
    [ Socket.Endpoint.channel "^room:.*$" (module Dummy_channel) ]
  in
  let result = Socket.Endpoint.find_channel channels "chat:lobby" in
  Alcotest.(check bool) "no match for chat:lobby" true (Option.is_none result)

let test_find_channel_multiple_routes () =
  let channels =
    [ Socket.Endpoint.channel "^room:.*$" (module Dummy_channel)
    ; Socket.Endpoint.channel "^chat:.*$" (module Dummy_channel)
    ]
  in
  Alcotest.(check bool)
    "room matches"
    true
    (Option.is_some (Socket.Endpoint.find_channel channels "room:1"));
  Alcotest.(check bool)
    "chat matches"
    true
    (Option.is_some (Socket.Endpoint.find_channel channels "chat:general"));
  Alcotest.(check bool)
    "users no match"
    true
    (Option.is_none (Socket.Endpoint.find_channel channels "users:1"))

let test_find_channel_first_match_wins () =
  let channels =
    [ Socket.Endpoint.channel "^room:lobby$" (module Dummy_channel)
    ; Socket.Endpoint.channel "^room:.*$" (module Dummy_channel)
    ]
  in
  Alcotest.(check bool)
    "matches"
    true
    (Option.is_some (Socket.Endpoint.find_channel channels "room:lobby"))

let route_tests =
  [ "exact match", `Quick, test_find_channel_exact
  ; "wildcard match", `Quick, test_find_channel_wildcard
  ; "no match", `Quick, test_find_channel_no_match
  ; "multiple routes", `Quick, test_find_channel_multiple_routes
  ; "first match wins", `Quick, test_find_channel_first_match_wins
  ]

let user_key : string Tapak.Context.key =
  Tapak.Context.Key.create { name = Some "user"; show = Some Fun.id }

let test_socket_assign () =
  let socket = Socket.assign user_key "alice" dummy_socket in
  Alcotest.(check (option string))
    "find assigned"
    (Some "alice")
    (Socket.find_assign user_key socket)

let test_socket_find_assign_missing () =
  Alcotest.(check (option string))
    "missing key"
    None
    (Socket.find_assign user_key dummy_socket)

let test_socket_accessors () =
  Alcotest.(check (option string))
    "id"
    (Some "test-socket")
    (Socket.id dummy_socket);
  Alcotest.(check string) "transport" "test" (Socket.transport dummy_socket);
  Alcotest.(check (list string))
    "joined_topics"
    []
    (Socket.joined_topics dummy_socket)

let socket_tests =
  [ "assign and find", `Quick, test_socket_assign
  ; "find missing key", `Quick, test_socket_find_assign_missing
  ; "accessors", `Quick, test_socket_accessors
  ]

let tests =
  [ "Socket Protocol", protocol_tests
  ; "Channel.Reply", reply_tests
  ; "Channel.Join", join_tests
  ; "Channel.Push", push_tests
  ; "Channel.Default", default_tests
  ; "Presence encoding", presence_tests
  ; "Channel routes", route_tests
  ; "Socket", socket_tests
  ]
