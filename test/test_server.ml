let contains haystack needle =
  let n = String.length needle
  and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.sub haystack i n = needle || go (i + 1))
  in
  go 0

let outcome_name = function
  | Some `Completed -> "completed"
  | Some (`Failed _) -> "failed"
  | None -> "pending"

(* Serve [handler] on a fresh loopback port and send one request with
   [Connection: close]. Reaching EOF means the server has written the body and
   run the completion hooks. *)
let exchange net ?(meth = "GET") handler =
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen
      ~sw
      ~backlog:8
      ~reuse_addr:true
      net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> assert false
  in
  let stop, stopping = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () -> Tapak.run ~stop ~on_error:ignore socket handler);
  let response =
    Eio.Switch.run @@ fun sw ->
    let flow =
      Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
    in
    Eio.Flow.copy_string
      (Printf.sprintf
         "%s / HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"
         meth)
      flow;
    Eio.Buf_read.(parse_exn take_all) flow ~max_size:max_int
  in
  Eio.Promise.resolve stopping ();
  response

let recording handler =
  let seen = ref None in
  ( seen
  , fun request ->
      Tapak.Request.on_finished request (fun outcome -> seen := Some outcome);
      handler request )

let check_outcome expected seen =
  Alcotest.(check string) "outcome" expected (outcome_name !seen)

let test_string net () =
  let seen, handler = recording (fun _ -> Tapak.Response.plain "hello") in
  let response = exchange net handler in
  Alcotest.(check bool) "body sent" true (contains response "hello");
  check_outcome "completed" seen

let test_stream_finishes_after_body net () =
  let during = ref (Some `Completed)
  and request_ref = ref None in
  let seen, handler =
    recording (fun request ->
      request_ref := Some request;
      Tapak.Response.stream (fun write flush ->
        write "one";
        flush ();
        during := Tapak.Request.finished request;
        write "two"))
  in
  let response = exchange net handler in
  Alcotest.(check bool) "body sent" true (contains response "two");
  Alcotest.(check string)
    "pending while streaming"
    "pending"
    (outcome_name !during);
  check_outcome "completed" seen

let test_handler_raises net () =
  let seen, handler = recording (fun _ -> failwith "boom") in
  let response = exchange net handler in
  Alcotest.(check bool) "500 sent" true (contains response "500");
  check_outcome "failed" seen

let test_stream_raises net () =
  let seen, handler =
    recording (fun _ ->
      Tapak.Response.stream (fun write _ ->
        write "partial";
        failwith "boom"))
  in
  ignore (exchange net handler);
  check_outcome "failed" seen

let test_raw net () =
  let inside = ref (Some `Completed) in
  let seen, handler =
    recording (fun request ->
      Tapak.Response.make
        (Tapak.Body.raw (fun _ oc ->
           inside := Tapak.Request.finished request;
           Eio.Buf_write.string oc "raw")))
  in
  ignore (exchange net handler);
  Alcotest.(check string) "pending inside raw" "pending" (outcome_name !inside);
  check_outcome "completed" seen

let test_head_stream net () =
  let ran = ref false in
  let seen, handler =
    recording (fun _ -> Tapak.Response.stream (fun _ _ -> ran := true))
  in
  ignore (exchange net ~meth:"HEAD" handler);
  Alcotest.(check bool) "stream not run for HEAD" false !ran;
  check_outcome "completed" seen

let test_derived_request net () =
  let seen = ref None in
  let middleware next request =
    next (Tapak.Request.with_ ~target:"/rewritten" request)
  in
  let handler request =
    Tapak.Request.on_finished request (fun outcome -> seen := Some outcome);
    Tapak.Response.plain "ok"
  in
  ignore (exchange net (middleware handler));
  check_outcome "completed" seen

let test_hook_order net () =
  let calls = ref []
  and request_ref = ref None in
  let handler request =
    request_ref := Some request;
    Tapak.Request.on_finished request (fun _ -> calls := "first" :: !calls);
    Tapak.Request.on_finished request (fun _ -> failwith "ignored");
    Tapak.Request.on_finished request (fun _ -> calls := "third" :: !calls);
    Tapak.Response.plain "ok"
  in
  ignore (exchange net handler);
  Alcotest.(check (list string))
    "newest first, raising hook skipped, run once"
    [ "third"; "first" ]
    (List.rev !calls);
  let request = Option.get !request_ref in
  let late = ref false in
  Tapak.Request.on_finished request (fun _ -> late := true);
  Alcotest.(check bool) "late hook runs immediately" true !late

let test_finish_once () =
  let request = Tapak.Request.make "/" in
  let calls = ref [] in
  Tapak.Request.on_finished request (fun _ -> calls := "first" :: !calls);
  Tapak.Request.on_finished request (fun _ -> calls := "second" :: !calls);
  Tapak.Request.finish request `Completed;
  Tapak.Request.finish request (`Failed Exit);
  Alcotest.(check (list string))
    "hooks run once"
    [ "second"; "first" ]
    (List.rev !calls);
  Alcotest.(check string)
    "first outcome kept"
    "completed"
    (outcome_name (Tapak.Request.finished request))

(* Race hook registration against completion across several domains. *)
let test_finish_across_domains () =
  let rounds = 200
  and per_domain = 50
  and domains = 3 in
  for _ = 1 to rounds do
    let request = Tapak.Request.make "/" in
    let count = Atomic.make 0 in
    let register () =
      for _ = 1 to per_domain do
        Tapak.Request.on_finished request (fun _ -> Atomic.incr count)
      done
    in
    let workers = List.init domains (fun _ -> Domain.spawn register) in
    let finisher =
      Domain.spawn (fun () -> Tapak.Request.finish request `Completed)
    in
    List.iter Domain.join workers;
    Domain.join finisher;
    Alcotest.(check int)
      "every hook ran once"
      (domains * per_domain)
      (Atomic.get count)
  done

let tests net =
  [ ( "Server"
    , [ Alcotest.test_case "on_finished: string body" `Quick (test_string net)
      ; Alcotest.test_case
          "on_finished: stream fires after the body"
          `Quick
          (test_stream_finishes_after_body net)
      ; Alcotest.test_case
          "on_finished: handler raises"
          `Quick
          (test_handler_raises net)
      ; Alcotest.test_case
          "on_finished: stream raises"
          `Quick
          (test_stream_raises net)
      ; Alcotest.test_case "on_finished: raw body" `Quick (test_raw net)
      ; Alcotest.test_case
          "on_finished: HEAD with stream body"
          `Quick
          (test_head_stream net)
      ; Alcotest.test_case
          "on_finished: request derived with with_"
          `Quick
          (test_derived_request net)
      ; Alcotest.test_case
          "on_finished: newest first, late hooks"
          `Quick
          (test_hook_order net)
      ; Alcotest.test_case
          "finish: only the first call counts"
          `Quick
          test_finish_once
      ; Alcotest.test_case
          "finish: concurrent with on_finished across domains"
          `Quick
          test_finish_across_domains
      ] )
  ]
