module Topic_tests = struct
  open Tapak.Channel.Topic

  let test_sprintf_literal () =
    let pattern = s "room" in
    let result = sprintf pattern in
    Alcotest.(check string) "literal" "room" result

  let test_sprintf_with_string () =
    let pattern = s "room" / str in
    let result = sprintf pattern "lobby" in
    Alcotest.(check string) "room:str" "room:lobby" result

  let test_sprintf_with_int () =
    let pattern = s "user" / int in
    let result = sprintf pattern 123 in
    Alcotest.(check string) "user:int" "user:123" result

  let test_sprintf_with_int64 () =
    let pattern = s "msg" / int64 in
    let result = sprintf pattern 9876543210L in
    Alcotest.(check string) "msg:int64" "msg:9876543210" result

  let test_sprintf_complex () =
    let pattern = s "user" / int / s "room" / str in
    let result = sprintf pattern 42 "lobby" in
    Alcotest.(check string) "complex" "user:42:room:lobby" result

  let test_sscanf_literal () =
    let pattern = s "room" in
    match sscanf pattern "room" with
    | Some f -> Alcotest.(check string) "literal match" "room" (f "room")
    | None -> Alcotest.fail "should match"

  let test_sscanf_literal_no_match () =
    let pattern = s "room" in
    match sscanf pattern "user" with
    | Some _ -> Alcotest.fail "should not match"
    | None -> ()

  let test_sscanf_with_string () =
    let pattern = s "room" / str in
    match sscanf pattern "room:lobby" with
    | Some f -> Alcotest.(check string) "extract string" "lobby" (f Fun.id)
    | None -> Alcotest.fail "should match"

  let test_sscanf_with_int () =
    let pattern = s "user" / int in
    match sscanf pattern "user:123" with
    | Some f -> Alcotest.(check int) "extract int" 123 (f Fun.id)
    | None -> Alcotest.fail "should match"

  let test_sscanf_complex () =
    let pattern = s "user" / int / s "room" / str in
    match sscanf pattern "user:42:room:lobby" with
    | Some f ->
      let user_id, room_name = f (fun uid rname -> uid, rname) in
      Alcotest.(check int) "user_id" 42 user_id;
      Alcotest.(check string) "room_name" "lobby" room_name
    | None -> Alcotest.fail "should match"

  let test_sscanf_extra_parts () =
    let pattern = s "room" / str in
    match sscanf pattern "room:lobby:extra" with
    | Some _ -> Alcotest.fail "should not match with extra parts"
    | None -> ()

  let test_sscanf_missing_parts () =
    let pattern = s "room" / str in
    match sscanf pattern "room" with
    | Some _ -> Alcotest.fail "should not match with missing parts"
    | None -> ()

  let test_matches_true () =
    let pattern = s "room" / str in
    Alcotest.(check bool) "matches" true (matches pattern "room:lobby")

  let test_matches_false () =
    let pattern = s "room" / str in
    Alcotest.(check bool) "not matches" false (matches pattern "user:123")

  let tests =
    [ "sprintf literal", `Quick, test_sprintf_literal
    ; "sprintf with string", `Quick, test_sprintf_with_string
    ; "sprintf with int", `Quick, test_sprintf_with_int
    ; "sprintf with int64", `Quick, test_sprintf_with_int64
    ; "sprintf complex", `Quick, test_sprintf_complex
    ; "sscanf literal", `Quick, test_sscanf_literal
    ; "sscanf literal no match", `Quick, test_sscanf_literal_no_match
    ; "sscanf with string", `Quick, test_sscanf_with_string
    ; "sscanf with int", `Quick, test_sscanf_with_int
    ; "sscanf complex", `Quick, test_sscanf_complex
    ; "sscanf extra parts", `Quick, test_sscanf_extra_parts
    ; "sscanf missing parts", `Quick, test_sscanf_missing_parts
    ; "matches true", `Quick, test_matches_true
    ; "matches false", `Quick, test_matches_false
    ]
end

module Protocol_tests = struct
  open Tapak.Channel.Protocol

  let test_encode_json () =
    let msg =
      { join_ref = Some "1"
      ; ref_ = Some "2"
      ; topic = "room:lobby"
      ; event = "new_msg"
      ; payload = `Assoc [ "body", `String "hello" ]
      }
    in
    let encoded = encode_json msg in
    let expected = {|["1","2","room:lobby","new_msg",{"body":"hello"}]|} in
    Alcotest.(check string) "encode" expected encoded

  let test_encode_json_with_nulls () =
    let msg =
      { join_ref = None
      ; ref_ = None
      ; topic = "room:lobby"
      ; event = "new_msg"
      ; payload = `Assoc []
      }
    in
    let encoded = encode_json msg in
    let expected = {|[null,null,"room:lobby","new_msg",{}]|} in
    Alcotest.(check string) "encode with nulls" expected encoded

  let test_decode_json () =
    let json = {|["1","2","room:lobby","new_msg",{"body":"hello"}]|} in
    match decode_json json with
    | Ok msg ->
      Alcotest.(check (option string)) "join_ref" (Some "1") msg.join_ref;
      Alcotest.(check (option string)) "ref_" (Some "2") msg.ref_;
      Alcotest.(check string) "topic" "room:lobby" msg.topic;
      Alcotest.(check string) "event" "new_msg" msg.event
    | Error err -> Alcotest.fail err

  let test_decode_json_with_nulls () =
    let json = {|[null,null,"room:lobby","new_msg",{}]|} in
    match decode_json json with
    | Ok msg ->
      Alcotest.(check (option string)) "join_ref" None msg.join_ref;
      Alcotest.(check (option string)) "ref_" None msg.ref_
    | Error err -> Alcotest.fail err

  let test_decode_invalid () =
    let json = {|{"not": "an array"}|} in
    match decode_json json with
    | Ok _ -> Alcotest.fail "should fail"
    | Error _ -> ()

  let test_make_reply () =
    let reply =
      make_reply
        ~join_ref:(Some "1")
        ~ref_:"2"
        ~topic:"room:lobby"
        ~status:Tapak.Channel.Channel.Ok
        ~payload:(`Assoc [ "data", `String "test" ])
    in
    Alcotest.(check string) "event" phx_reply reply.event;
    Alcotest.(check (option string)) "ref_" (Some "2") reply.ref_;
    match reply.payload with
    | `Assoc fields ->
      (match List.assoc_opt "status" fields with
      | Some (`String "ok") -> ()
      | _ -> Alcotest.fail "status should be ok")
    | _ -> Alcotest.fail "payload should be assoc"

  let test_make_push () =
    let push =
      make_push
        ~topic:"room:lobby"
        ~event:"user_joined"
        ~payload:(`Assoc [ "user", `String "alice" ])
    in
    Alcotest.(check (option string)) "join_ref" None push.join_ref;
    Alcotest.(check (option string)) "ref_" None push.ref_;
    Alcotest.(check string) "topic" "room:lobby" push.topic;
    Alcotest.(check string) "event" "user_joined" push.event

  let test_reserved_events () =
    Alcotest.(check string) "phx_join" "phx_join" phx_join;
    Alcotest.(check string) "phx_leave" "phx_leave" phx_leave;
    Alcotest.(check string) "phx_reply" "phx_reply" phx_reply;
    Alcotest.(check string) "phx_error" "phx_error" phx_error;
    Alcotest.(check string) "phx_close" "phx_close" phx_close;
    Alcotest.(check string) "heartbeat" "heartbeat" heartbeat

  let tests =
    [ "encode json", `Quick, test_encode_json
    ; "encode json with nulls", `Quick, test_encode_json_with_nulls
    ; "decode json", `Quick, test_decode_json
    ; "decode json with nulls", `Quick, test_decode_json_with_nulls
    ; "decode invalid", `Quick, test_decode_invalid
    ; "make reply", `Quick, test_make_reply
    ; "make push", `Quick, test_make_push
    ; "reserved events", `Quick, test_reserved_events
    ]
end

module Pubsub_tests = struct
  open Tapak.Channel.Pubsub

  let test_local_subscribe_broadcast () =
    Eio_main.run @@ fun _env ->
    Eio.Switch.run @@ fun sw ->
    let pubsub = Tapak.Channel.local_pubsub ~sw in
    let received = ref false in
    let _sub =
      subscribe pubsub "room:lobby" (fun msg ->
        if String.equal msg.topic "room:lobby" && String.equal msg.event "test"
        then received := true)
    in
    broadcast pubsub { topic = "room:lobby"; event = "test"; payload = `Null };
    Alcotest.(check bool) "received" true !received

  let test_local_unsubscribe () =
    Eio_main.run @@ fun _env ->
    Eio.Switch.run @@ fun sw ->
    let pubsub = Tapak.Channel.local_pubsub ~sw in
    let received = ref false in
    let sub = subscribe pubsub "room:lobby" (fun _msg -> received := true) in
    unsubscribe pubsub sub;
    broadcast pubsub { topic = "room:lobby"; event = "test"; payload = `Null };
    Alcotest.(check bool) "not received after unsubscribe" false !received

  let test_local_topic_filtering () =
    Eio_main.run @@ fun _env ->
    Eio.Switch.run @@ fun sw ->
    let pubsub = Tapak.Channel.local_pubsub ~sw in
    let room1_received = ref false in
    let room2_received = ref false in
    let _sub1 =
      subscribe pubsub "room:1" (fun _msg -> room1_received := true)
    in
    let _sub2 =
      subscribe pubsub "room:2" (fun _msg -> room2_received := true)
    in
    broadcast pubsub { topic = "room:1"; event = "test"; payload = `Null };
    Alcotest.(check bool) "room1 received" true !room1_received;
    Alcotest.(check bool) "room2 not received" false !room2_received

  let test_broadcast_from () =
    Eio_main.run @@ fun _env ->
    Eio.Switch.run @@ fun sw ->
    let pubsub = Tapak.Channel.local_pubsub ~sw in
    let sub1_received = ref false in
    let sub2_received = ref false in
    let sub1 = subscribe pubsub "room:1" (fun _msg -> sub1_received := true) in
    let _sub2 = subscribe pubsub "room:1" (fun _msg -> sub2_received := true) in
    broadcast_from
      pubsub
      ~self:sub1
      { topic = "room:1"; event = "test"; payload = `Null };
    Alcotest.(check bool) "sender not received" false !sub1_received;
    Alcotest.(check bool) "other received" true !sub2_received

  let tests =
    [ "local subscribe and broadcast", `Quick, test_local_subscribe_broadcast
    ; "local unsubscribe", `Quick, test_local_unsubscribe
    ; "local topic filtering", `Quick, test_local_topic_filtering
    ; "broadcast from", `Quick, test_broadcast_from
    ]
end

module Presence_tests = struct
  open Tapak.Channel.Presence

  let test_track_and_list () =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let pubsub = Tapak.Channel.local_pubsub ~sw in
    let clock = Eio.Stdenv.clock env in
    let presence =
      create ~sw ~pubsub ~node_name:"test-node" ~clock ~broadcast_period:10.0 ()
    in
    let meta = `Assoc [ "online_at", `Int 1234567890 ] in
    let _phx_ref = track presence ~topic:"room:lobby" ~key:"user:1" ~meta in
    let state = list presence ~topic:"room:lobby" in
    let json = to_json state in
    close presence;
    (* Check that user:1 is in the JSON *)
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "user:1" fields with
      | Some _ -> ()
      | None -> Alcotest.fail "user should be tracked")
    | _ -> Alcotest.fail "should be an object"

  let test_untrack () =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let pubsub = Tapak.Channel.local_pubsub ~sw in
    let clock = Eio.Stdenv.clock env in
    let presence =
      create ~sw ~pubsub ~node_name:"test-node" ~clock ~broadcast_period:10.0 ()
    in
    let meta = `Assoc [ "online_at", `Int 1234567890 ] in
    let phx_ref = track presence ~topic:"room:lobby" ~key:"user:1" ~meta in
    untrack_ref presence ~topic:"room:lobby" ~phx_ref;
    let state = list presence ~topic:"room:lobby" in
    let json = to_json state in
    close presence;
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "user:1" fields with
      | Some _ -> Alcotest.fail "user should be untracked"
      | None -> ())
    | _ -> Alcotest.fail "should be an object"

  let test_to_json () =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let pubsub = Tapak.Channel.local_pubsub ~sw in
    let clock = Eio.Stdenv.clock env in
    let presence =
      create ~sw ~pubsub ~node_name:"test-node" ~clock ~broadcast_period:10.0 ()
    in
    let meta = `Assoc [ "online_at", `Int 1234567890 ] in
    let _phx_ref = track presence ~topic:"room:lobby" ~key:"user:1" ~meta in
    let state = list presence ~topic:"room:lobby" in
    let json = to_json state in
    close presence;
    match json with
    | `Assoc _ -> () (* Just check it's valid JSON *)
    | _ -> Alcotest.fail "should be an object"

  let tests =
    [ "track and list", `Quick, test_track_and_list
    ; "untrack", `Quick, test_untrack
    ; "to_json", `Quick, test_to_json
    ]
end

module Tracker_tests = struct
  open Tapak.Channel.Presence

  let test_single_node_track () =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let pubsub = Tapak.Channel.local_pubsub ~sw in
    let clock = Eio.Stdenv.clock env in
    let tracker =
      create ~sw ~pubsub ~node_name:"node-a" ~broadcast_period:10.0 ~clock ()
    in
    let meta = `Assoc [ "online_at", `Int 1234567890 ] in
    let phx_ref = track tracker ~topic:"room:lobby" ~key:"user:1" ~meta in
    Alcotest.(check bool) "phx_ref not empty" true (String.length phx_ref > 0);
    let state = list tracker ~topic:"room:lobby" in
    let json = Tapak.Channel.Presence.to_json state in
    close tracker;
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "user:1" fields with
      | Some _ -> ()
      | None -> Alcotest.fail "user should be tracked")
    | _ -> Alcotest.fail "should be an object"

  let test_single_node_untrack () =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let pubsub = Tapak.Channel.local_pubsub ~sw in
    let clock = Eio.Stdenv.clock env in
    let tracker =
      create ~sw ~pubsub ~node_name:"node-a" ~broadcast_period:10.0 ~clock ()
    in
    let meta = `Assoc [ "online_at", `Int 1234567890 ] in
    let phx_ref = track tracker ~topic:"room:lobby" ~key:"user:1" ~meta in
    untrack_ref tracker ~topic:"room:lobby" ~phx_ref;
    let state = list tracker ~topic:"room:lobby" in
    let json = Tapak.Channel.Presence.to_json state in
    close tracker;
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "user:1" fields with
      | Some _ -> Alcotest.fail "user should be untracked"
      | None -> ())
    | _ -> Alcotest.fail "should be an object"

  let test_multi_node_sync () =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let pubsub = Tapak.Channel.local_pubsub ~sw in
    let clock = Eio.Stdenv.clock env in
    let tracker_a =
      create ~sw ~pubsub ~node_name:"node-a" ~broadcast_period:10.0 ~clock ()
    in
    let tracker_b =
      create ~sw ~pubsub ~node_name:"node-b" ~broadcast_period:10.0 ~clock ()
    in
    let meta_a = `Assoc [ "device", `String "mobile" ] in
    let _phx_ref_a =
      track tracker_a ~topic:"room:lobby" ~key:"user:1" ~meta:meta_a
    in
    let meta_b = `Assoc [ "device", `String "desktop" ] in
    let _phx_ref_b =
      track tracker_b ~topic:"room:lobby" ~key:"user:2" ~meta:meta_b
    in
    let state_a = list tracker_a ~topic:"room:lobby" in
    let state_b = list tracker_b ~topic:"room:lobby" in
    let json_a = Tapak.Channel.Presence.to_json state_a in
    let json_b = Tapak.Channel.Presence.to_json state_b in
    close tracker_a;
    close tracker_b;
    let check_both_users json label =
      match json with
      | `Assoc fields ->
        let has_user1 = Option.is_some (List.assoc_opt "user:1" fields) in
        let has_user2 = Option.is_some (List.assoc_opt "user:2" fields) in
        Alcotest.(check bool) (label ^ " has user:1") true has_user1;
        Alcotest.(check bool) (label ^ " has user:2") true has_user2
      | _ -> Alcotest.fail "should be an object"
    in
    check_both_users json_a "tracker_a";
    check_both_users json_b "tracker_b"

  let test_multi_node_untrack_sync () =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let pubsub = Tapak.Channel.local_pubsub ~sw in
    let clock = Eio.Stdenv.clock env in
    let tracker_a =
      create ~sw ~pubsub ~node_name:"node-a" ~broadcast_period:10.0 ~clock ()
    in
    let tracker_b =
      create ~sw ~pubsub ~node_name:"node-b" ~broadcast_period:10.0 ~clock ()
    in
    let meta = `Assoc [ "device", `String "mobile" ] in
    let phx_ref = track tracker_a ~topic:"room:lobby" ~key:"user:1" ~meta in
    let state_b_before = list tracker_b ~topic:"room:lobby" in
    let json_b_before = Tapak.Channel.Presence.to_json state_b_before in
    (match json_b_before with
    | `Assoc fields ->
      Alcotest.(check bool)
        "tracker_b sees user before untrack"
        true
        (Option.is_some (List.assoc_opt "user:1" fields))
    | _ -> Alcotest.fail "should be an object");
    untrack_ref tracker_a ~topic:"room:lobby" ~phx_ref;
    let state_b_after = list tracker_b ~topic:"room:lobby" in
    let json_b_after = Tapak.Channel.Presence.to_json state_b_after in
    close tracker_a;
    close tracker_b;
    match json_b_after with
    | `Assoc fields ->
      Alcotest.(check bool)
        "tracker_b sees user removed after untrack"
        false
        (Option.is_some (List.assoc_opt "user:1" fields))
    | _ -> Alcotest.fail "should be an object"

  let test_disconnect_broadcast () =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let pubsub = Tapak.Channel.local_pubsub ~sw in
    let clock = Eio.Stdenv.clock env in
    let tracker_a =
      create ~sw ~pubsub ~node_name:"node-a" ~broadcast_period:10.0 ~clock ()
    in
    let tracker_b =
      create ~sw ~pubsub ~node_name:"node-b" ~broadcast_period:10.0 ~clock ()
    in
    let disconnected_socket = ref None in
    on_disconnect tracker_b (fun socket_id ->
      disconnected_socket := Some socket_id);
    broadcast_disconnect tracker_a ~socket_id:"user:123";
    close tracker_a;
    close tracker_b;
    Alcotest.(check (option string))
      "disconnect received"
      (Some "user:123")
      !disconnected_socket

  let test_node_name () =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let pubsub = Tapak.Channel.local_pubsub ~sw in
    let clock = Eio.Stdenv.clock env in
    let tracker =
      create ~sw ~pubsub ~node_name:"my-node" ~broadcast_period:10.0 ~clock ()
    in
    let name = node_name tracker in
    close tracker;
    Alcotest.(check string) "node name" "my-node" name

  let test_multiple_metas_per_key () =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let pubsub = Tapak.Channel.local_pubsub ~sw in
    let clock = Eio.Stdenv.clock env in
    let tracker =
      create ~sw ~pubsub ~node_name:"node-a" ~broadcast_period:10.0 ~clock ()
    in
    let meta1 = `Assoc [ "device", `String "mobile" ] in
    let meta2 = `Assoc [ "device", `String "desktop" ] in
    let _phx_ref1 =
      track tracker ~topic:"room:lobby" ~key:"user:1" ~meta:meta1
    in
    let _phx_ref2 =
      track tracker ~topic:"room:lobby" ~key:"user:1" ~meta:meta2
    in
    let state = list tracker ~topic:"room:lobby" in
    let json = Tapak.Channel.Presence.to_json state in
    close tracker;
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "user:1" fields with
      | Some (`Assoc user_fields) ->
        (match List.assoc_opt "metas" user_fields with
        | Some (`List metas) ->
          Alcotest.(check int) "two metas" 2 (List.length metas)
        | _ -> Alcotest.fail "metas should be a list")
      | _ -> Alcotest.fail "user should be tracked")
    | _ -> Alcotest.fail "should be an object"

  let tests =
    [ "single node track", `Quick, test_single_node_track
    ; "single node untrack", `Quick, test_single_node_untrack
    ; "multi node sync", `Quick, test_multi_node_sync
    ; "multi node untrack sync", `Quick, test_multi_node_untrack_sync
    ; "disconnect broadcast", `Quick, test_disconnect_broadcast
    ; "node name", `Quick, test_node_name
    ; "multiple metas per key", `Quick, test_multiple_metas_per_key
    ]
end

let tests =
  [ "Topic", Topic_tests.tests
  ; "Protocol", Protocol_tests.tests
  ; "Pubsub", Pubsub_tests.tests
  ; "Presence", Presence_tests.tests
  ; "Tracker", Tracker_tests.tests
  ]
