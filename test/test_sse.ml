open Tapak

let test_simple_text_event () =
  let event =
    Sse.Event.
      { id = None
      ; data = Some (`Text "hello world")
      ; event = None
      ; comment = None
      ; retry = None
      }
  in
  let output = Sse.Event.to_string event in
  Alcotest.(check string) "simple text event" "data: hello world\n\n" output

let test_multiline_text_event () =
  let event =
    Sse.Event.
      { id = None
      ; data = Some (`Text "line1\nline2\nline3")
      ; event = None
      ; comment = None
      ; retry = None
      }
  in
  let output = Sse.Event.to_string event in
  Alcotest.(check string)
    "multiline text event"
    "data: line1\ndata: line2\ndata: line3\n\n"
    output

let test_json_event () =
  let json = `Assoc [ "message", `String "hello"; "count", `Int 42 ] in
  let event =
    Sse.Event.
      { id = None
      ; data = Some (`Json json)
      ; event = None
      ; comment = None
      ; retry = None
      }
  in
  let output = Sse.Event.to_string event in
  Alcotest.(check string)
    "json event"
    "data: {\"message\":\"hello\",\"count\":42}\n\n"
    output

let test_event_with_id () =
  let event =
    Sse.Event.
      { id = Some "123"
      ; data = Some (`Text "test")
      ; event = None
      ; comment = None
      ; retry = None
      }
  in
  let output = Sse.Event.to_string event in
  Alcotest.(check string) "event with id" "id: 123\ndata: test\n\n" output

let test_event_with_type () =
  let event =
    Sse.Event.
      { id = None
      ; data = Some (`Text "test")
      ; event = Some "custom-event"
      ; comment = None
      ; retry = None
      }
  in
  let output = Sse.Event.to_string event in
  Alcotest.(check string)
    "event with type"
    "data: test\nevent: custom-event\n\n"
    output

let test_comment_event () =
  let event =
    Sse.Event.
      { id = None
      ; data = None
      ; event = None
      ; comment = Some "keep-alive"
      ; retry = None
      }
  in
  let output = Sse.Event.to_string event in
  Alcotest.(check string) "comment event" ": keep-alive\n\n" output

let test_event_with_retry () =
  let event =
    Sse.Event.
      { id = None
      ; data = Some (`Text "test")
      ; event = None
      ; comment = None
      ; retry = Some 5000
      }
  in
  let output = Sse.Event.to_string event in
  Alcotest.(check string)
    "event with retry"
    "data: test\nretry: 5000\n\n"
    output

let test_complete_event () =
  let event =
    Sse.Event.
      { id = Some "msg-42"
      ; data = Some (`Text "complete")
      ; event = Some "notification"
      ; comment = Some "important"
      ; retry = Some 3000
      }
  in
  let output = Sse.Event.to_string event in
  Alcotest.(check string)
    "complete event with all fields"
    "id: msg-42\n\
     data: complete\n\
     event: notification\n\
     : important\n\
     retry: 3000\n\n"
    output

let tests =
  [ ( "SSE Event formatting"
    , [ Alcotest.test_case "simple text event" `Quick test_simple_text_event
      ; Alcotest.test_case
          "multiline text event"
          `Quick
          test_multiline_text_event
      ; Alcotest.test_case "json event" `Quick test_json_event
      ; Alcotest.test_case "event with id" `Quick test_event_with_id
      ; Alcotest.test_case "event with type" `Quick test_event_with_type
      ; Alcotest.test_case "comment event" `Quick test_comment_event
      ; Alcotest.test_case "event with retry" `Quick test_event_with_retry
      ; Alcotest.test_case "complete event" `Quick test_complete_event
      ] )
  ]
