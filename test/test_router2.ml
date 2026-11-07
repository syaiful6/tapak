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
      >> let+ _ = has_role "admin"
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
    >> let+ () = step "first"
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
              >> let+ () = step "block" in
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
      >> let+ name = body Json name_schema in
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
      >> pure (fun id -> Response.of_string (Int64.to_string id))
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
      >> pure (fun org id rest ->
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
      >>
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
      >>
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
    [ get (path ()) >> pure (fun id -> Response.of_string (Int64.to_string id))
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
    >> pure (Response.of_string "ok")
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
  let routes = [ get (s "a") >> pure (Response.of_string "a") ] in
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
        [ post (s "") >> map create (body Urlencoded name_schema)
        ; get (s "") |> handle_request list
        ; delete int64 >> pure delete_website
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
  let routes = [ get (s "x") |> handle_request handler ] in
  Alcotest.(check int) "not called when the route is built" 0 !calls;
  check_body "first" "1" (dispatch routes (make_request "/x"));
  check_body "second" "2" (dispatch routes (make_request "/x"))

let test_handler_operator_with_gate_and_capture () =
  let routes =
    [ get (s "x" / int)
      |> gate (has_role "admin")
      |> handle_request (fun req id ->
        Response.of_string (Printf.sprintf "%s %d" (Request.target req) id))
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
          >> map (fun me -> Response.of_string (string_of_int me.id)) auth
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
      >> map (fun me -> Response.of_string (string_of_int me.id)) auth
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
      >> map (fun me -> Response.of_string (string_of_int me.id)) (memo source)
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
      >> map (fun me -> Response.of_string (string_of_int me.id)) auth
    ]
  in
  Alcotest.(check string)
    "gate error"
    "unauthorized"
    (run_outcome routes (make_request "/me"));
  Alcotest.(check int) "only the gate ran" 1 !runs

let test_memo_lets_gate_and_block_share_body () =
  let input = memo (body Json name_schema) in
  let routes = [ post (s "x") |> gate input >> map Response.of_string input ] in
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
        [ get (s "") >> map (show "list ") search
        ; get (s "top") >> map (show "top ") search
        ; get (s "ids") |> handle_request (fun _req -> Response.of_string "ids")
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

let key_schema =
  Sch.Object.(define ~kind:"Key" @@ mem ~enc:Fun.id "x-key" Sch.string)

let token_schema =
  Sch.Object.(define ~kind:"Token" @@ mem ~enc:Fun.id "x-token" Sch.string)

let json_request ?(body = {|{"name":"x"}|}) target =
  make_request ~meth:`POST ~content_type:"application/json" ~body target

let test_alt_uses_first_success () =
  let either =
    map (fun _ -> "key") (query key_schema)
    <|> map (fun _ -> "token") (query token_schema)
  in
  let routes = [ get (s "x") >> map Response.of_string either ] in
  check_body "left" "key" (dispatch routes (make_request "/x?x-key=a"));
  check_body "right" "token" (dispatch routes (make_request "/x?x-token=a"));
  check_body
    "left wins when both match"
    "key"
    (dispatch routes (make_request "/x?x-key=a&x-token=b"))

let test_alt_raises_right_error () =
  let routes =
    [ get (s "x")
      >> map
           (fun _ -> Response.of_string "ok")
           (fail Forbidden <|> fail Unauthorized)
    ]
  in
  Alcotest.(check string)
    "right error"
    "unauthorized"
    (run_outcome routes (make_request "/x"))

let test_fail_replaces_validation_error () =
  let routes =
    [ get (s "x")
      >> map Response.of_string (query key_schema <|> fail Unauthorized)
    ]
  in
  check_body "valid" "a" (dispatch routes (make_request "/x?x-key=a"));
  Alcotest.(check string)
    "custom error"
    "unauthorized"
    (run_outcome routes (make_request "/x"))

let test_alt_keeps_other_exceptions () =
  let routes =
    [ get (s "x")
      >> map
           Response.of_string
           (extract (fun _ -> failwith "db down") <|> pure "fallback")
    ]
  in
  Alcotest.check_raises "not caught" (Failure "db down") (fun () ->
    ignore (dispatch routes (make_request "/x")))

let test_alt_right_branch_is_not_run_on_success () =
  let routes =
    [ get (s "x")
      >> map (fun () -> Response.of_string "ok") (step "left" <|> step "right")
    ]
  in
  ignore (run_outcome routes (make_request "/x"));
  check_trace "only left" [ "left" ]

let test_two_bodies_are_rejected () =
  let rejected name schema =
    match schema () with
    | () -> Alcotest.failf "%s: expected the schema to be rejected" name
    | exception Invalid_argument _ -> ()
  in
  let json = body Json name_schema in
  rejected "same media type" (fun () -> ignore (json <|> json));
  rejected "different media types" (fun () ->
    ignore (json <|> body Urlencoded name_schema));
  rejected "through memo" (fun () -> ignore (memo json <|> json));
  rejected "through refine" (fun () ->
    ignore (json <|> (json |> refine (fun v -> Ok v))));
  rejected "inside let+" (fun () ->
    ignore
      ((let+ _ = request
        and+ v = json in
        v)
      <|> json));
  rejected "nested alternative" (fun () ->
    ignore (json <|> (fail Forbidden <|> json)))

let test_one_body_in_alt () =
  let with_custom_error =
    [ post (s "x")
      >> map Response.of_string (body Json name_schema <|> fail Unauthorized)
    ]
  in
  check_body "valid" "x" (dispatch with_custom_error (json_request "/x"));
  Alcotest.(check string)
    "invalid body gets the custom error"
    "unauthorized"
    (run_outcome with_custom_error (json_request ~body:"{}" "/x"));
  let body_on_right =
    [ post (s "x")
      >> map Response.of_string (fail Forbidden <|> body Json name_schema)
    ]
  in
  check_body
    "body is unread when the left branch fails first"
    "x"
    (dispatch body_on_right (json_request "/x"))

type shape =
  | Circle of float
  | Square of float

(* The supported way to accept several body shapes *)
let test_union_body () =
  let side name =
    Sch.Object.(define ~kind:name @@ mem ~enc:Fun.id "size" Sch.float)
  in
  let shape =
    Sch.Union.(
      define
        ~discriminator:"type"
        [ case
            ~tag:"circle"
            ~inj:(fun v -> Circle v)
            ~proj:(function Circle v -> Some v | _ -> None)
            (side "Circle")
        ; case
            ~tag:"square"
            ~inj:(fun v -> Square v)
            ~proj:(function Square v -> Some v | _ -> None)
            (side "Square")
        ])
  in
  let show = function
    | Circle v -> Response.of_string (Printf.sprintf "circle %g" v)
    | Square v -> Response.of_string (Printf.sprintf "square %g" v)
  in
  let routes = [ post (s "x") >> map show (body Json shape) ] in
  check_body
    "first case"
    "circle 2"
    (dispatch routes (json_request ~body:{|{"type":"circle","size":2}|} "/x"));
  check_body
    "second case"
    "square 3"
    (dispatch routes (json_request ~body:{|{"type":"square","size":3}|} "/x"))

let form_request ?(body = "name=x") target =
  make_request
    ~meth:`POST
    ~content_type:"application/x-www-form-urlencoded"
    ~body
    target

let test_body_of_selects_by_content_type () =
  let routes =
    [ post (s "x")
      >> map
           Response.of_string
           (Private.Router2.body_of [ Json; Urlencoded ] name_schema)
    ]
  in
  check_body "json" "x" (dispatch routes (json_request "/x"));
  check_body "form" "x" (dispatch routes (form_request "/x"));
  check_body
    "content type parameters are ignored"
    "x"
    (dispatch
       routes
       (make_request
          ~meth:`POST
          ~content_type:"application/json; charset=utf-8"
          ~body:{|{"name":"x"}|}
          "/x"));
  Alcotest.(check string)
    "other content type"
    "bad_request"
    (run_outcome
       routes
       (make_request ~meth:`POST ~content_type:"text/plain" ~body:"x" "/x"));
  Alcotest.(check string)
    "missing content type"
    "bad_request"
    (run_outcome routes (make_request ~meth:`POST ~body:"x" "/x"));
  Alcotest.(check string)
    "invalid body for the selected type"
    "validation_failed"
    (run_outcome routes (json_request ~body:"{}" "/x"))

let test_body_of_rejects_misuse () =
  Alcotest.check_raises
    "no media types"
    (Invalid_argument "Router2.body_of: expected at least one media type")
    (fun () -> ignore (Private.Router2.body_of [] name_schema));
  let many = Private.Router2.body_of [ Json; Urlencoded ] name_schema in
  match many <|> body Multipart name_schema with
  | _ -> Alcotest.fail "expected two bodies in <|> to be rejected"
  | exception Invalid_argument _ -> ()

let check_unauthorized ?challenge name response =
  Alcotest.(check int)
    (name ^ " status")
    401
    (Http.Status.to_int (Response.status response));
  Alcotest.(check (option string))
    (name ^ " challenge")
    challenge
    (Response.header "WWW-Authenticate" response)

let basic_auth ~username ~password =
  match username, password with
  | "alice", "secret" -> Some "alice"
  | "colon", "a:b" -> Some "colon"
  | _ -> None

(* alice:secret, colon:a:b and wrong:wrong *)
let alice = "Basic YWxpY2U6c2VjcmV0"
let colon = "Basic Y29sb246YTpi"
let wrong = "Basic d3Jvbmc6d3Jvbmc="
let basic_challenge = {|Basic realm="Restricted"|}

let test_basic_auth () =
  let routes =
    [ get (s "me") >> map Response.of_string (secured (basic basic_auth)) ]
  in
  let call ?auth () = dispatch routes (make_request ?auth "/me") in
  let rejected name ?auth () =
    check_unauthorized ~challenge:basic_challenge name (call ?auth ())
  in
  check_body "valid" "alice" (call ~auth:alice ());
  check_body "only the first colon splits" "colon" (call ~auth:colon ());
  check_body "lowercase scheme" "alice" (call ~auth:"basic YWxpY2U6c2VjcmV0" ());
  check_body "uppercase scheme" "alice" (call ~auth:"BASIC YWxpY2U6c2VjcmV0" ());
  rejected "wrong password" ~auth:wrong ();
  rejected "missing" ();
  rejected "other scheme" ~auth:"Bearer token" ();
  rejected "invalid base64" ~auth:"Basic !!!" ()

let test_secured_as_gate_or_value () =
  let runs = ref 0 in
  let auth =
    memo
      (secured
         (basic (fun ~username ~password ->
            incr runs;
            basic_auth ~username ~password)))
  in
  let routes =
    [ scope
        ~gates:[ void auth ]
        (s "api")
        [ get (s "gate")
          |> handle_request (fun _req -> Response.of_string "gated")
        ; get (s "value") >> map Response.of_string auth
        ]
    ]
  in
  check_body
    "gate"
    "gated"
    (dispatch routes (make_request ~auth:alice "/api/gate"));
  Alcotest.(check int) "verified once" 1 !runs;
  check_body
    "value"
    "alice"
    (dispatch routes (make_request ~auth:alice "/api/value"));
  Alcotest.(check int) "gate and value share one run" 2 !runs;
  check_unauthorized
    ~challenge:basic_challenge
    "gate rejects"
    (dispatch routes (make_request "/api/gate"))

let test_bearer_auth () =
  let verify = function "token" -> Some "alice" | _ -> None in
  let routes =
    [ get (s "me") >> map Response.of_string (secured (bearer verify)) ]
  in
  let call ?auth () = dispatch routes (make_request ?auth "/me") in
  let rejected name ?auth () =
    check_unauthorized ~challenge:"Bearer" name (call ?auth ())
  in
  check_body "valid" "alice" (call ~auth:"Bearer token" ());
  check_body "scheme is case-insensitive" "alice" (call ~auth:"bearer token" ());
  rejected "wrong token" ~auth:"Bearer nope" ();
  rejected "missing" ();
  rejected "other scheme" ~auth:alice ();
  rejected "empty token" ~auth:"Bearer " ()

let test_api_key_locations () =
  let verify = function "k" -> Some "ok" | _ -> None in
  let call in_ ?(headers = []) target =
    let routes =
      [ get (s "x")
        >> map Response.of_string (secured (api_key ~in_ "key" verify))
      ]
    in
    dispatch
      routes
      (Request.make
         ~version:`HTTP_1_1
         ~meth:`GET
         ~headers:(Headers.of_list headers)
         ~body:(Bytesrw.Bytes.Reader.empty ())
         target)
  in
  check_body "header" "ok" (call `Header ~headers:[ "key", "k" ] "/x");
  check_body "query" "ok" (call `Query "/x?key=k");
  check_body "cookie" "ok" (call `Cookie ~headers:[ "cookie", "key=k" ] "/x");
  check_unauthorized "wrong key" (call `Query "/x?key=z");
  check_unauthorized "missing" (call `Query "/x");
  check_unauthorized "empty" (call `Query "/x?key=")

let test_alternative_schemes () =
  let auth =
    secured (basic basic_auth)
    <|> secured (bearer (function "token" -> Some "bob" | _ -> None))
  in
  let routes = [ get (s "me") >> map Response.of_string auth ] in
  let call auth = dispatch routes (make_request ~auth "/me") in
  check_body "basic" "alice" (call alice);
  check_body "bearer" "bob" (call "Bearer token")

let test_challenges () =
  let challenge t = Security.challenge (t : _ Security.t).scheme in
  let verify _ = Some () in
  let login ~username:_ ~password:_ = Some () in
  Alcotest.(check (option string))
    "default realm"
    (Some basic_challenge)
    (challenge (basic login));
  Alcotest.(check (option string))
    "custom realm"
    (Some {|Basic realm="Admin"|})
    (challenge (basic ~realm:"Admin" login));
  Alcotest.(check (option string))
    "bearer"
    (Some "Bearer")
    (challenge (bearer verify));
  Alcotest.(check (option string))
    "api key has none"
    None
    (challenge (api_key ~in_:`Header "k" verify))

let protected = secured (basic basic_auth)
let protected_routes = [ get (s "me") >> map Response.of_string protected ]

let test_custom_unauthorized_response () =
  let reasons = ref [] in
  let unauthorized _req _scheme (reason : Security.reason) =
    reasons := reason :: !reasons;
    Response.of_string ~status:`Unauthorized "Forbidden access"
  in
  let handler = of_list ~unauthorized protected_routes in
  let call ?auth () = handler (make_request ?auth "/me") in
  check_body "custom body" "Forbidden access" (call ());
  check_unauthorized
    ~challenge:basic_challenge
    "challenge is added for us"
    (call ());
  ignore (call ~auth:"Bearer x" ());
  ignore (call ~auth:wrong ());
  Alcotest.(check bool)
    "reason tells the cases apart"
    true
    (List.rev !reasons
    = [ Security.Missing
      ; Security.Missing
      ; Security.Malformed
      ; Security.Invalid
      ])

let test_handler_can_set_its_own_challenge () =
  let unauthorized _req _scheme _reason =
    Response.of_string
      ~status:`Unauthorized
      ~headers:(Headers.of_list [ "WWW-Authenticate", "Custom" ])
      ""
  in
  check_unauthorized
    ~challenge:"Custom"
    "kept"
    (of_list ~unauthorized protected_routes (make_request "/me"))

let test_recover_runs_before_default_401 () =
  let routes =
    [ get (s "me")
      >> map Response.of_string protected
      |> recover (fun _req -> function
        | Extraction_failed (Security.Unauthorized _) ->
          Some (Response.of_string ~status:`Unauthorized "from recover")
        | _ -> None)
    ]
  in
  check_body
    "recover wins"
    "from recover"
    (dispatch routes (make_request "/me"))

let test_authorization_stays_with_the_app () =
  let not_alice =
    protected |> refine (function "alice" -> Error Forbidden | who -> Ok who)
  in
  let routes = [ get (s "me") >> map Response.of_string not_alice ] in
  Alcotest.(check string)
    "refine errors aren't turned into a 401"
    "forbidden"
    (run_outcome routes (make_request ~auth:alice "/me"))

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
  ; "<|> uses first success", `Quick, test_alt_uses_first_success
  ; "<|> raises right error", `Quick, test_alt_raises_right_error
  ; ( "fail replaces validation error"
    , `Quick
    , test_fail_replaces_validation_error )
  ; "<|> keeps other exceptions", `Quick, test_alt_keeps_other_exceptions
  ; ( "<|> skips right on success"
    , `Quick
    , test_alt_right_branch_is_not_run_on_success )
  ; "Two bodies are rejected", `Quick, test_two_bodies_are_rejected
  ; "One body in <|>", `Quick, test_one_body_in_alt
  ; "Union body", `Quick, test_union_body
  ; ( "body_of selects by content type"
    , `Quick
    , test_body_of_selects_by_content_type )
  ; "body_of rejects misuse", `Quick, test_body_of_rejects_misuse
  ; "Basic auth", `Quick, test_basic_auth
  ; "Secured as gate or value", `Quick, test_secured_as_gate_or_value
  ; "Bearer auth", `Quick, test_bearer_auth
  ; "API key locations", `Quick, test_api_key_locations
  ; "Alternative schemes", `Quick, test_alternative_schemes
  ; "Challenges", `Quick, test_challenges
  ; "Custom unauthorized response", `Quick, test_custom_unauthorized_response
  ; ( "Handler can set its own challenge"
    , `Quick
    , test_handler_can_set_its_own_challenge )
  ; ( "recover runs before default 401"
    , `Quick
    , test_recover_runs_before_default_401 )
  ; ( "Authorization stays with the app"
    , `Quick
    , test_authorization_stays_with_the_app )
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
