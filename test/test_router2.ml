open Tapak
open Private.Router2

type Router.extractor_error += Unauthorized | Forbidden

type user =
  { id : int
  ; role : string
  }

let make_request ?(meth = `GET) ?auth ?content_type ?(body = "") target =
  let headers =
    List.filter_map
      Fun.id
      [ Option.map (fun v -> "authorization", v) auth
      ; Option.map (fun v -> "content-type", v) content_type
      ]
  in
  Request.make
    ~version:`HTTP_1_1
    ~meth
    ~headers:(Headers.of_list headers)
    ~body:(Bytesrw.Bytes.Reader.of_string body)
    target

let body_of response = Response.body response |> Body.to_string

let check_body name expected response =
  Alcotest.(check string) name expected (body_of response)

(* Records which effects ran, in order *)
let trace = ref []
let log name = trace := name :: !trace

let check_trace name expected =
  Alcotest.(check (list string)) name expected (List.rev !trace)

let step name =
  extract (fun _ ->
    log name;
    Ok ())

let current_user =
  extract (fun req ->
    log "auth";
    match Request.header "authorization" req with
    | Some "admin" -> Ok { id = 1; role = "admin" }
    | Some _ -> Ok { id = 2; role = "user" }
    | None -> Error Unauthorized)

let has_role role =
  current_user
  |> refine (fun u -> if u.role = role then Ok u else Error Forbidden)

let name_schema =
  Sch.Object.(define ~kind:"Name" @@ mem ~enc:Fun.id "name" Sch.string)

let limit_schema =
  Sch.Object.(define ~kind:"Limit" @@ mem ~enc:Fun.id "limit" Sch.int)

let website_schema =
  Sch.Object.(define ~kind:"Website" @@ mem ~enc:Fun.id "x-website" Sch.string)

let dispatch routes request = of_list routes request

let run_outcome routes request =
  trace := [];
  match dispatch routes request with
  | response ->
    Printf.sprintf "%d" (Http.Status.to_int (Response.status response))
  | exception Extraction_failed Unauthorized -> "unauthorized"
  | exception Extraction_failed Forbidden -> "forbidden"
  | exception Bad_request _ -> "bad_request"
  | exception Validation_failed _ -> "validation_failed"

type search =
  { limit : int
  ; website : string
  }

let user_handler search id =
  Response.of_string
    (Printf.sprintf "%Ld limit=%d website=%s" id search.limit search.website)

(* A gate's value is dropped and the rest reach the handler as one record *)
let test_gate_not_passed_to_handler () =
  let routes =
    [ (get (s "users" / int64)
      >-> let+ _ = has_role "admin"
          and+ limit = query limit_schema
          and+ website = header website_schema in
          user_handler { limit; website })
    ]
  in
  let response =
    dispatch
      routes
      (Request.make
         ~version:`HTTP_1_1
         ~meth:`GET
         ~headers:
           (Headers.of_list
              [ "authorization", "admin"; "x-website", "example.com" ])
         ~body:(Bytesrw.Bytes.Reader.empty ())
         "/users/42?limit=5")
  in
  check_body "handler output" "42 limit=5 website=example.com" response

let ordered_routes =
  [ (put (s "items" / int)
    |> gate (step "endpoint-gate-1")
    |> gate (step "endpoint-gate-2")
    >-> let+ () = step "first"
        and+ () = step "second"
        and+ () = step "third" in
        fun id ->
          log "handler";
          Response.of_string (string_of_int id))
  ]

let test_effects_run_in_declared_order () =
  let outcome =
    run_outcome ordered_routes (make_request ~meth:`PUT "/items/7")
  in
  Alcotest.(check string) "status" "200" outcome;
  check_trace
    "gates, then the block top to bottom, then the handler"
    [ "endpoint-gate-1"
    ; "endpoint-gate-2"
    ; "first"
    ; "second"
    ; "third"
    ; "handler"
    ]

let test_scope_gates_run_first () =
  let routes =
    [ scope
        ~gates:[ step "outer" ]
        (s "a")
        [ scope
            ~gates:[ step "inner" ]
            (s "b")
            [ (get (s "c")
              |> gate (step "endpoint")
              >-> let+ () = step "block" in
                  log "handler";
                  Response.of_string "ok")
            ]
        ]
    ]
  in
  let outcome = run_outcome routes (make_request "/a/b/c") in
  Alcotest.(check string) "status" "200" outcome;
  check_trace
    "outermost first"
    [ "outer"; "inner"; "endpoint"; "block"; "handler" ]

(* An unread body would raise Bad_request for the missing content type, so
   getting the gate's error shows the body was never touched. *)
let test_failed_gate_skips_body_and_handler () =
  let routes =
    [ (put (s "users" / int64)
      |> gate (has_role "admin")
      >-> let+ name = body Json name_schema in
          fun _id ->
            log "handler";
            Response.of_string name)
    ]
  in
  let outcome target ?auth () =
    run_outcome routes (make_request ~meth:`PUT ?auth target)
  in
  Alcotest.(check string) "no auth" "unauthorized" (outcome "/users/1" ());
  check_trace "stops at auth" [ "auth" ];
  Alcotest.(check string)
    "wrong role"
    "forbidden"
    (outcome "/users/1" ~auth:"bob" ());
  check_trace "stops at auth" [ "auth" ];
  Alcotest.(check string)
    "body reached after gate passes"
    "bad_request"
    (outcome "/users/1" ~auth:"admin" ())

let test_bad_capture_runs_no_gate () =
  let routes =
    [ get (s "users" / int64)
      |> gate (has_role "admin")
      >-> pure (fun id -> Response.of_string (Int64.to_string id))
    ]
  in
  let response =
    trace := [];
    dispatch routes (make_request "/users/abc")
  in
  Alcotest.(check int)
    "falls through to not_found"
    404
    (Http.Status.to_int (Response.status response));
  check_trace "gate never ran" []

let test_multiple_captures_are_curried () =
  let routes =
    [ get (s "orgs" / str / s "users" / int / splat)
      >-> pure (fun org id rest ->
        Response.of_string
          (Printf.sprintf "%s %d %s" org id (String.concat "," rest)))
    ]
  in
  check_body
    "captures in path order, percent-decoded"
    "my org 3 x,y"
    (dispatch routes (make_request "/orgs/my%20org/users/3/x/y"))

let test_refine_keeps_value () =
  let routes =
    [ (get (s "me")
      >->
      let+ admin = has_role "admin" in
      Response.of_string (string_of_int admin.id))
    ]
  in
  check_body
    "value from refine"
    "1"
    (dispatch routes (make_request ~auth:"admin" "/me"))

let test_body_request_and_response_model () =
  let routes =
    [ (post (s "names")
      |> response_model ~status:`Created ~schema:name_schema
      >->
      let+ name = body Json name_schema
      and+ req = request in
      name ^ ":" ^ Request.target req)
    ]
  in
  let response =
    dispatch
      routes
      (make_request
         ~meth:`POST
         ~content_type:"application/json"
         ~body:{|{"name":"x"}|}
         "/names")
  in
  Alcotest.(check int)
    "status from response_model"
    201
    (Http.Status.to_int (Response.status response));
  check_body "encoded" {|{"name":"x:/names"}|} response

let test_sprintf_shares_path () =
  let path () = s "users" / int64 / s "edit" in
  Alcotest.(check string)
    "reverse route"
    "/users/42/edit"
    (sprintf (path ()) 42L);
  let routes =
    [ get (path ()) >-> pure (fun id -> Response.of_string (Int64.to_string id))
    ]
  in
  check_body
    "same path dispatches"
    "42"
    (dispatch routes (make_request "/users/42/edit"))

let test_recover_catches_gate_failure () =
  let route =
    get (s "me")
    |> gate (has_role "admin")
    >-> pure (Response.of_string "ok")
    |> recover (fun _req -> function
      | Extraction_failed Unauthorized ->
        Some (Response.of_string ~status:`Unauthorized "denied")
      | _ -> None)
  in
  let response = dispatch [ route ] (make_request "/me") in
  Alcotest.(check int)
    "recovered status"
    401
    (Http.Status.to_int (Response.status response));
  check_body "recovered body" "denied" response

let test_match_returns_none () =
  let routes = [ get (s "a") >-> pure (Response.of_string "a") ] in
  Alcotest.(check bool)
    "unknown path"
    true
    (Option.is_none (match' routes (make_request "/b")));
  Alcotest.(check bool)
    "wrong method"
    true
    (Option.is_none (match' routes (make_request ~meth:`POST "/a")))

(* Mirrors a real API: every route needs the guard, few care about the user *)
let test_gated_scope_with_recover () =
  let auth_runs = ref 0 in
  let guard =
    extract (fun req ->
      incr auth_runs;
      match Request.header "authorization" req with
      | Some "ok" -> Ok { id = 1; role = "user" }
      | _ -> Error Unauthorized)
  in
  let not_authorized _req = function
    | Extraction_failed Unauthorized ->
      Some (Response.of_string ~status:`Unauthorized "denied")
    | _ -> None
  in
  let create name = Response.of_string ("created " ^ name) in
  let list _req = Response.of_string "listed" in
  let delete_website id =
    Response.of_string ("deleted " ^ Int64.to_string id)
  in
  let routes =
    [ scope
        ~gates:[ void guard ]
        (s "api" / s "websites")
        [ post (s "") >-> map create (body Urlencoded name_schema)
        ; get (s "") >> list
        ; delete int64 >-> pure delete_website
        ]
      |> recover not_authorized
    ]
  in
  let call ?meth ?auth ?content_type ?body target =
    dispatch routes (make_request ?meth ?auth ?content_type ?body target)
  in
  check_body
    "post"
    "created x"
    (call
       ~meth:`POST
       ~auth:"ok"
       ~content_type:"application/x-www-form-urlencoded"
       ~body:"name=x"
       "/api/websites");
  check_body "get" "listed" (call ~auth:"ok" "/api/websites");
  check_body
    "delete"
    "deleted 9"
    (call ~meth:`DELETE ~auth:"ok" "/api/websites/9");
  Alcotest.(check int) "guard ran once per request" 3 !auth_runs;
  List.iter
    (fun (meth, target) ->
       let response = call ~meth target in
       Alcotest.(check int)
         target
         401
         (Http.Status.to_int (Response.status response));
       check_body target "denied" response)
    [ `POST, "/api/websites"
    ; `GET, "/api/websites"
    ; `DELETE, "/api/websites/9"
    ]

let test_handler_operator_runs_per_request () =
  let calls = ref 0 in
  let handler _req =
    incr calls;
    Response.of_string (string_of_int !calls)
  in
  let routes = [ get (s "x") >> handler ] in
  Alcotest.(check int) "not called when the route is built" 0 !calls;
  check_body "first" "1" (dispatch routes (make_request "/x"));
  check_body "second" "2" (dispatch routes (make_request "/x"))

let test_handler_operator_with_gate_and_capture () =
  let routes =
    [ ( get (s "x" / int) |> gate (has_role "admin") >> fun req id ->
        Response.of_string (Printf.sprintf "%s %d" (Request.target req) id) )
    ]
  in
  check_body
    "request then captures"
    "/x/4 4"
    (dispatch routes (make_request ~auth:"admin" "/x/4"));
  Alcotest.(check string)
    "gate still applies"
    "forbidden"
    (run_outcome routes (make_request ~auth:"bob" "/x/4"))

let counting_auth runs =
  extract (fun req ->
    incr runs;
    match Request.header "authorization" req with
    | Some "admin" -> Ok { id = 1; role = "admin" }
    | Some _ -> Ok { id = 2; role = "user" }
    | None -> Error Unauthorized)

let test_memo_runs_once_per_request () =
  let runs = ref 0 in
  let auth = memo (counting_auth runs) in
  let is_admin =
    auth |> refine (fun u -> if u.role = "admin" then Ok u else Error Forbidden)
  in
  let routes =
    [ scope
        ~gates:[ void auth ]
        (s "api")
        [ get (s "me")
          |> gate is_admin
          >-> map (fun me -> Response.of_string (string_of_int me.id)) auth
        ]
    ]
  in
  check_body
    "value"
    "1"
    (dispatch routes (make_request ~auth:"admin" "/api/me"));
  Alcotest.(check int) "scope, endpoint and block share one run" 1 !runs;
  ignore (dispatch routes (make_request ~auth:"admin" "/api/me"));
  Alcotest.(check int) "the next request runs it again" 2 !runs

let test_without_memo_runs_each_time () =
  let runs = ref 0 in
  let auth = counting_auth runs in
  let routes =
    [ get (s "me")
      |> gate auth
      >-> map (fun me -> Response.of_string (string_of_int me.id)) auth
    ]
  in
  ignore (dispatch routes (make_request ~auth:"admin" "/me"));
  Alcotest.(check int) "ran twice" 2 !runs

let test_memo_instances_are_separate () =
  let runs = ref 0 in
  let source = counting_auth runs in
  let routes =
    [ get (s "me")
      |> gate (memo source)
      >-> map (fun me -> Response.of_string (string_of_int me.id)) (memo source)
    ]
  in
  ignore (dispatch routes (make_request ~auth:"admin" "/me"));
  Alcotest.(check int) "two memo calls, two runs" 2 !runs

let test_memo_failure_stops_the_request () =
  let runs = ref 0 in
  let auth = memo (counting_auth runs) in
  let routes =
    [ get (s "me")
      |> gate auth
      >-> map (fun me -> Response.of_string (string_of_int me.id)) auth
    ]
  in
  Alcotest.(check string)
    "gate error"
    "unauthorized"
    (run_outcome routes (make_request "/me"));
  Alcotest.(check int) "only the gate ran" 1 !runs

let test_memo_lets_gate_and_block_share_body () =
  let input = memo (body Json name_schema) in
  let routes =
    [ post (s "x") |> gate input >-> map Response.of_string input ]
  in
  check_body
    "the body stream is only consumed once"
    "x"
    (dispatch
       routes
       (make_request
          ~meth:`POST
          ~content_type:"application/json"
          ~body:{|{"name":"x"}|}
          "/x"))

(* A scope gate whose parsed value every route under it reads *)
let test_memo_scope_value_reaches_routes () =
  let search =
    memo
      (let+ () = step "parse"
       and+ limit = query limit_schema in
       limit)
  in
  let show prefix limit = Response.of_string (prefix ^ string_of_int limit) in
  let routes =
    [ scope
        ~gates:[ void search ]
        (s "websites")
        [ get (s "") >-> map (show "list ") search
        ; get (s "top") >-> map (show "top ") search
        ; (get (s "ids") >> fun _req -> Response.of_string "ids")
        ]
    ]
  in
  trace := [];
  check_body
    "first route"
    "list 5"
    (dispatch routes (make_request "/websites?limit=5"));
  check_trace "parsed once" [ "parse" ];
  check_body
    "second route"
    "top 7"
    (dispatch routes (make_request "/websites/top?limit=7"));
  Alcotest.(check string)
    "a route ignoring the value is still gated"
    "validation_failed"
    (run_outcome routes (make_request "/websites/ids"))

let tests =
  [ "Gate is not passed to handler", `Quick, test_gate_not_passed_to_handler
  ; "Effects run in declared order", `Quick, test_effects_run_in_declared_order
  ; "Scope gates run first", `Quick, test_scope_gates_run_first
  ; ( "Failed gate skips body and handler"
    , `Quick
    , test_failed_gate_skips_body_and_handler )
  ; "Bad capture runs no gate", `Quick, test_bad_capture_runs_no_gate
  ; "Multiple captures are curried", `Quick, test_multiple_captures_are_curried
  ; "Refine keeps value", `Quick, test_refine_keeps_value
  ; ( "Body, request and response_model"
    , `Quick
    , test_body_request_and_response_model )
  ; "Sprintf shares path", `Quick, test_sprintf_shares_path
  ; "Recover catches gate failure", `Quick, test_recover_catches_gate_failure
  ; "match' returns None", `Quick, test_match_returns_none
  ; "Gated scope with recover", `Quick, test_gated_scope_with_recover
  ; "Memo runs once per request", `Quick, test_memo_runs_once_per_request
  ; "Without memo runs each time", `Quick, test_without_memo_runs_each_time
  ; "Memo instances are separate", `Quick, test_memo_instances_are_separate
  ; ( "Memo failure stops the request"
    , `Quick
    , test_memo_failure_stops_the_request )
  ; ( "Memo lets gate and block share body"
    , `Quick
    , test_memo_lets_gate_and_block_share_body )
  ; ( "Memo scope value reaches routes"
    , `Quick
    , test_memo_scope_value_reaches_routes )
  ; ( ">> runs the handler per request"
    , `Quick
    , test_handler_operator_runs_per_request )
  ; ( ">> with gate and capture"
    , `Quick
    , test_handler_operator_with_gate_and_capture )
  ]
  |> List.map (fun (name, speed, fn) -> Alcotest.test_case name speed fn)
  |> fun tests -> [ "Router2", tests ]
