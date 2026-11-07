open Tapak
open Router

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
           (Router.body_of [ Json; Urlencoded ] name_schema)
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
    (Invalid_argument "Router.body_of: expected at least one media type")
    (fun () -> ignore (Router.body_of [] name_schema));
  let many = Router.body_of [ Json; Urlencoded ] name_schema in
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
  let challenge t = Security.(challenge (scheme t)) in
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

let is_hex_color ~off ~len s =
  if len <> 6
  then false
  else
    let loop i =
      let c = String.get s (off + i) in
      (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    in
    loop 0

let test_simple_route () =
  let route =
    get (s "users")
    |> handle_request (fun request ->
      Alcotest.(check string)
        "method should be GET"
        "GET"
        (Http.Method.to_string (Request.meth request));
      Response.of_string "users list")
  in
  let request = make_request "/users" in
  match match' [ route ] request with
  | Some response ->
    Alcotest.(check bool)
      "route should match"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "route should have matched"

let test_int64_param () =
  let route =
    get (s "users" / int64)
    >> pure (fun id ->
      Alcotest.(check int64) "id should be 42" 42L id;
      Response.of_string (Printf.sprintf "User %Ld" id))
  in
  let request = make_request "/users/42" in
  match match' [ route ] request with
  | Some response ->
    Alcotest.(check bool)
      "route should match"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "route should have matched"

let test_multiple_params () =
  let route =
    get (s "users" / int64 / s "posts" / str)
    >> pure (fun user_id slug ->
      Alcotest.(check int64) "user_id should be 42" 42L user_id;
      Alcotest.(check string) "slug should be 'hello'" "hello" slug;
      Response.of_string (Printf.sprintf "User %Ld post %s" user_id slug))
  in
  let request = make_request "/users/42/posts/hello" in
  match match' [ route ] request with
  | Some response ->
    Alcotest.(check bool)
      "route should match"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "route should have matched"

let test_no_match () =
  let route =
    get (s "users" / int64) >> pure (fun _id -> Response.of_string "user")
  in
  let request = make_request "/posts/42" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "route should not have matched"
  | None -> ()

let test_post_method () =
  let route =
    post (s "users")
    |> handle_request (fun _ ->
      Response.of_string ~status:`Created "user created")
  in
  let request = make_request ~meth:`POST "/users" in
  match match' [ route ] request with
  | Some response ->
    Alcotest.(check bool)
      "route should match POST"
      true
      (Response.status response = `Created)
  | None -> Alcotest.fail "POST route should have matched"

let test_method_mismatch () =
  let route =
    post (s "users")
    |> handle_request (fun _ ->
      Response.of_string ~status:`Created "user created")
  in
  let request = make_request ~meth:`GET "/users" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "GET should not match POST route"
  | None -> ()

let test_sprintf_simple () =
  let pattern = s "users" in
  let url = sprintf pattern in
  Alcotest.(check string) "url should be /users" "/users" url

let test_sprintf_int64 () =
  let pattern = s "users" / int64 in
  let url = sprintf pattern 42L in
  Alcotest.(check string) "url should be /users/42" "/users/42" url

let test_sprintf_multiple () =
  let pattern = s "users" / int64 / s "posts" / str in
  let url = sprintf pattern 42L "hello-world" in
  Alcotest.(check string)
    "url should be /users/42/posts/hello-world"
    "/users/42/posts/hello-world"
    url

let test_int_param () =
  let route =
    get (s "page" / int)
    >> pure (fun page_num ->
      Alcotest.(check int) "page should be 5" 5 page_num;
      Response.of_string (Printf.sprintf "Page %d" page_num))
  in
  let request = make_request "/page/5" in
  match match' [ route ] request with
  | Some response ->
    Alcotest.(check bool)
      "route should match"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "route should have matched"

let test_invalid_int () =
  let route =
    get (s "page" / int) >> pure (fun _page -> Response.of_string "page")
  in
  let request = make_request "/page/not-a-number" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "route should not match invalid int"
  | None -> ()

let test_bool_param () =
  let route =
    get (s "published" / bool)
    >> pure (fun is_published ->
      Alcotest.(check bool) "should be true" true is_published;
      Response.of_string (string_of_bool is_published))
  in
  let request = make_request "/published/true" in
  match match' [ route ] request with
  | Some _ -> ()
  | None -> Alcotest.fail "route should have matched"

let test_scope () =
  let route =
    scope
      (s "api" / s "v1")
      [ get (s "users") |> handle_request (fun _ -> Response.of_string "users")
      ]
  in
  let request = make_request "/api/v1/users" in
  match match' [ route ] request with
  | Some _ -> ()
  | None -> Alcotest.fail "scoped route should have matched"

let test_url_generation () =
  let user_url_pattern = s "users" / int64 in
  let user_posts_url_pattern = s "users" / int64 / s "posts" / str in

  let url = sprintf user_url_pattern 42L in
  Alcotest.(check string) "URL should be /users/42" "/users/42" url;

  let url2 = sprintf user_posts_url_pattern 42L "hello-world" in
  Alcotest.(check string)
    "URL should be /users/42/posts/hello-world"
    "/users/42/posts/hello-world"
    url2

let test_scope_with_middlewares () =
  let module M = struct
    type t = unit

    let call () next req =
      let resp = next req in
      Response.with_
        ~headers:(Tapak.Headers.add (Response.headers resp) "X-Test" "true")
        resp
  end
  in
  let test_middleware = Middleware.use (module M) () in
  let route =
    scope
      ~middlewares:[ test_middleware ]
      (s "api")
      [ get (s "test") |> handle_request (fun _ -> Response.of_string "ok") ]
  in
  let request = make_request "/api/test" in
  match match' [ route ] request with
  | Some response ->
    (match Response.header "x-test" response with
    | Some value -> Alcotest.(check string) "middleware should run" "true" value
    | None -> Alcotest.fail "middleware header not found")
  | None -> Alcotest.fail "scoped route should have matched"

let test_splat () =
  let route =
    get (s "files" / splat)
    >> pure (fun segments ->
      let path = String.concat "/" segments in
      Response.of_string (Printf.sprintf "Files: %s" path))
  in
  let request = make_request "/files/docs/readme.txt" in
  match match' [ route ] request with
  | Some response ->
    Alcotest.(check bool)
      "splat route should match"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "splat route should have matched"

let test_splat_segments () =
  let captured_segments = ref [] in
  let route =
    get (s "static" / splat)
    >> pure (fun segments ->
      captured_segments := segments;
      Response.of_string "ok")
  in
  let request = make_request "/static/js/app/bundle.js" in
  match match' [ route ] request with
  | Some _ ->
    Alcotest.(check (list string))
      "should capture all segments"
      [ "js"; "app"; "bundle.js" ]
      !captured_segments
  | None -> Alcotest.fail "splat route should have matched"

let test_splat_empty () =
  let captured_segments = ref [] in
  let route =
    get (s "spa" / splat)
    >> pure (fun segments ->
      captured_segments := segments;
      Response.of_string "ok")
  in
  let request = make_request "/spa" in
  match match' [ route ] request with
  | Some _ ->
    Alcotest.(check (list string))
      "should capture empty list"
      []
      !captured_segments
  | None -> Alcotest.fail "splat route should have matched"

let test_sprintf_splat () =
  let pattern = s "files" / splat in
  let url = sprintf pattern [ "docs"; "readme.txt" ] in
  Alcotest.(check string)
    "url should be /files/docs/readme.txt"
    "/files/docs/readme.txt"
    url

let test_sprintf_splat_empty () =
  let pattern = s "spa" / splat in
  let url = sprintf pattern [] in
  Alcotest.(check string) "url should be /spa" "/spa" url

let test_splat_with_prefix () =
  let route =
    get (s "api" / s "v1" / splat)
    >> pure (fun segments ->
      let path = String.concat "/" segments in
      Response.of_string (Printf.sprintf "API: %s" path))
  in
  let request = make_request "/api/v1/users/42/posts" in
  match match' [ route ] request with
  | Some response ->
    Alcotest.(check bool)
      "splat with prefix should match"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "splat route should have matched"

let test_slug () =
  let route =
    get (s "posts" / slug)
    >> pure (fun slug_str ->
      Response.of_string (Printf.sprintf "Post: %s" slug_str))
  in
  let request = make_request "/posts/my-awesome-post-123" in
  match match' [ route ] request with
  | Some response ->
    Alcotest.(check bool)
      "slug route should match"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "slug route should have matched"

let test_slug_invalid () =
  let route =
    get (s "posts" / slug) >> pure (fun _slug -> Response.of_string "ok")
  in
  let request = make_request "/posts/My_Post" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "slug should not match invalid format"
  | None -> ()

let test_slug_valid () =
  let captured_slug = ref "" in
  let route =
    get (s "posts" / slug)
    >> pure (fun slug_str ->
      captured_slug := slug_str;
      Response.of_string "ok")
  in
  let request = make_request "/posts/hello-world-123" in
  match match' [ route ] request with
  | Some _ ->
    Alcotest.(check string)
      "should capture slug"
      "hello-world-123"
      !captured_slug
  | None -> Alcotest.fail "slug route should have matched"

let test_custom () =
  let hex_color =
    custom
      ~parse:(fun ~off ~len s ->
        if is_hex_color ~off ~len s then Some s else None)
      ~format:Fun.id
      ~type_name:"string"
  in
  let route =
    get (s "color" / hex_color ())
    >> pure (fun color ->
      Response.of_string (Printf.sprintf "Color: #%s" color))
  in
  let request = make_request "/color/ff5733" in
  match match' [ route ] request with
  | Some response ->
    Alcotest.(check bool)
      "custom hex color should match"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "custom route should have matched"

let test_custom_invalid () =
  let hex_color =
    custom
      ~parse:(fun ~off ~len s ->
        if is_hex_color ~off ~len s then Some s else None)
      ~format:Fun.id
      ~type_name:"string"
  in
  let route =
    get (s "color" / hex_color ())
    >> pure (fun _color -> Response.of_string "ok")
  in
  let request = make_request "/color/gggggg" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "custom should not match invalid"
  | None -> ()

let test_sprintf_slug () =
  let pattern = s "posts" / slug in
  let url = sprintf pattern "my-great-post-2024" in
  Alcotest.(check string)
    "url should be /posts/my-great-post-2024"
    "/posts/my-great-post-2024"
    url

let test_sprintf_custom () =
  let hex_color =
    custom
      ~parse:(fun ~off ~len s -> String.sub s off len |> Option.some)
      ~format:(fun s -> String.uppercase_ascii s)
      ~type_name:"string"
  in
  let pattern = s "color" / hex_color () in
  let url = sprintf pattern "ff5733" in
  Alcotest.(check string) "url should be /color/FF5733" "/color/FF5733" url

let test_root_route () =
  let route =
    get (s "") |> handle_request (fun _ -> Response.of_string "home page")
  in
  let request = make_request "/" in
  match match' [ route ] request with
  | Some response ->
    Alcotest.(check bool)
      "root route should match"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "root route should have matched"

let test_sprintf_root () =
  let pattern = s "" in
  let url = sprintf pattern in
  Alcotest.(check string) "url should be /" "/" url

let test_root_with_method () =
  let route =
    post (s "")
    |> handle_request (fun _ ->
      Response.of_string ~status:`Created "posted to root")
  in
  let request = make_request ~meth:`POST "/" in
  match match' [ route ] request with
  | Some response ->
    Alcotest.(check bool)
      "root POST route should match"
      true
      (Response.status response = `Created)
  | None -> Alcotest.fail "root POST route should have matched"

let test_root_doesnt_match_subpaths () =
  let route =
    get (s "") |> handle_request (fun _ -> Response.of_string "root")
  in
  let request = make_request "/users" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "root should not match /users"
  | None -> ()

let test_scoped_empty_literal () =
  let route =
    scope
      (s "users")
      [ get (s "") |> handle_request (fun _ -> Response.of_string "user list") ]
  in
  let request = make_request "/users" in
  match match' [ route ] request with
  | Some response ->
    Alcotest.(check bool)
      "scoped empty literal should match /users"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "scoped empty literal should have matched /users"

let test_scoped_empty_literal_no_trailing_slash () =
  let route =
    scope
      (s "api" / s "v1")
      [ get (s "") |> handle_request (fun _ -> Response.of_string "api index") ]
  in
  let request = make_request "/api/v1" in
  match match' [ route ] request with
  | Some response ->
    Alcotest.(check bool)
      "nested scoped empty literal should match /api/v1"
      true
      (Response.status response = `OK)
  | None ->
    Alcotest.fail "nested scoped empty literal should have matched /api/v1"

let test_scoped_with_trailing_route () =
  let route =
    scope
      (s "users")
      [ get (s "") |> handle_request (fun _ -> Response.of_string "user list")
      ; get (s "new") |> handle_request (fun _ -> Response.of_string "new user")
      ]
  in
  let request1 = make_request "/users" in
  let request2 = make_request "/users/new" in
  (match match' [ route ] request1 with
  | Some _ -> ()
  | None -> Alcotest.fail "/users should match");
  match match' [ route ] request2 with
  | Some _ -> ()
  | None -> Alcotest.fail "/users/new should match"

let test_exact_path_matching () =
  let route =
    get (s "users" / int64)
    >> pure (fun id -> Response.of_string (Printf.sprintf "User %Ld" id))
  in
  let request1 = make_request "/users/123" in
  (match match' [ route ] request1 with
  | Some response ->
    Alcotest.(check bool)
      "/users/123 should match"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "/users/123 should have matched");

  let request2 = make_request "/users/123/" in
  (match match' [ route ] request2 with
  | Some response ->
    Alcotest.(check bool)
      "/users/123/ should match (trailing slash normalized)"
      true
      (Response.status response = `OK)
  | None ->
    Alcotest.fail "/users/123/ should have matched (trailing slash normalized)");

  let request3 = make_request "/users/123/posts" in
  match match' [ route ] request3 with
  | Some _ -> Alcotest.fail "/users/123/posts should not match (extra segments)"
  | None -> ()

let test_any_method () =
  let route =
    any (s "api" / s "webhook")
    |> handle_request (fun _ -> Response.of_string "webhook received")
  in
  let request1 = make_request ~meth:`GET "/api/webhook" in
  (match match' [ route ] request1 with
  | Some response ->
    Alcotest.(check bool)
      "GET should match 'any' route"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "GET should have matched 'any' route");

  let request2 = make_request ~meth:`POST "/api/webhook" in
  (match match' [ route ] request2 with
  | Some response ->
    Alcotest.(check bool)
      "POST should match 'any' route"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "POST should have matched 'any' route");

  let request3 = make_request ~meth:`PUT "/api/webhook" in
  (match match' [ route ] request3 with
  | Some response ->
    Alcotest.(check bool)
      "PUT should match 'any' route"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "PUT should have matched 'any' route");

  let request4 = make_request ~meth:`DELETE "/api/webhook" in
  match match' [ route ] request4 with
  | Some response ->
    Alcotest.(check bool)
      "DELETE should match 'any' route"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "DELETE should have matched 'any' route"

let test_any_method_with_params () =
  let route =
    any (s "resources" / int64)
    >> pure (fun id -> Response.of_string (Printf.sprintf "Resource %Ld" id))
  in
  let request1 = make_request ~meth:`GET "/resources/42" in
  (match match' [ route ] request1 with
  | Some response ->
    Alcotest.(check bool)
      "GET should match 'any' with params"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "GET should have matched 'any' with params");

  let request2 = make_request ~meth:`POST "/resources/42" in
  match match' [ route ] request2 with
  | Some response ->
    Alcotest.(check bool)
      "POST should match 'any' with params"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "POST should have matched 'any' with params"

let test_response_model () =
  let string_schema =
    Sch.Object.define @@ Sch.Object.mem ~enc:Fun.id "message" Sch.string
  in
  let route =
    get (s "users" / int64)
    |> response_model ~status:`OK ~schema:string_schema
    |> handle_request (fun request id ->
      let ua =
        Request.header "user-agent" request |> Option.value ~default:"unknown"
      in
      Printf.sprintf "User %Ld with ua: %s" id ua)
  in
  let request =
    Request.make
      ~headers:(Headers.of_list [ "user-agent", "alcotest" ])
      ~meth:`GET
      ~body:(Bytesrw.Bytes.Reader.empty ())
      "/users/100"
  in
  match match' [ route ] request with
  | Some response ->
    let body = Response.body response |> Body.to_string in
    let expected = {|{"message":"User 100 with ua: alcotest"}|} in
    Alcotest.(check string)
      "response body should be the user id and ua as JSON string"
      expected
      body
  | None -> Alcotest.fail "Route should have matched"

let test_route_with_header () =
  let header_schema = Sch.Object.(define @@ mem "X-API-Key" Sch.string) in
  let route =
    get (s "api" / s "data")
    >> map
         (fun api_key ->
            Alcotest.(check string)
              "api key should be secret123"
              "secret123"
              api_key;
            Response.of_string "authenticated")
         (header header_schema)
  in
  let request =
    Request.make
      ~headers:(Headers.of_list [ "x-api-key", "secret123" ])
      ~meth:`GET
      ~body:(Bytesrw.Bytes.Reader.empty ())
      "/api/data"
  in
  match match' [ route ] request with
  | Some response ->
    Alcotest.(check bool)
      "route with header should match"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "route with valid header should have matched"

let test_route_header_case_insensitive () =
  let header_schema =
    Sch.Object.define @@ Sch.Object.mem "Content-Type" Sch.string
  in
  let route =
    post (s "api" / s "upload")
    >> map
         (fun content_type ->
            Response.of_string (Printf.sprintf "Content-Type: %s" content_type))
         (header header_schema)
  in
  let request =
    Request.make
      ~headers:(Headers.of_list [ "CONTENT-TYPE", "application/json" ])
      ~meth:`POST
      ~body:(Bytesrw.Bytes.Reader.empty ())
      "/api/upload"
  in
  match match' [ route ] request with
  | Some response ->
    let body = Response.body response |> Body.to_string in
    Alcotest.(check string)
      "should match header case-insensitively"
      "Content-Type: application/json"
      body
  | None -> Alcotest.fail "route should match with case-insensitive header"

let test_route_header_missing () =
  let header_schema =
    Sch.Object.define @@ Sch.Object.mem "Authorization" Sch.string
  in
  let route =
    get (s "api" / s "protected")
    >> map (fun _auth -> Response.of_string "ok") (header header_schema)
  in
  let request = make_request "/api/protected" in
  try
    let _ = match' [ route ] request in
    Alcotest.fail "route should raise Validation_failed without required header"
  with
  | Router.Validation_failed _ -> ()

let test_route_multiple_headers () =
  let header_schema =
    Sch.Object.(
      define ~kind:"Header_schema"
      @@
      let+ api_key = mem ~enc:Stdlib.fst "X-API-Key" Sch.string
      and+ user_id = mem ~enc:Stdlib.snd "X-User-ID" Sch.string in
      api_key, user_id)
  in
  let route =
    get (s "api" / s "user-data")
    >> map
         (fun (api_key, user_id) ->
            Response.of_string
              (Printf.sprintf "Key: %s, User: %s" api_key user_id))
         (header header_schema)
  in
  let request =
    Request.make
      ~headers:
        (Headers.of_list [ "x-api-key", "secret123"; "x-user-id", "user42" ])
      ~meth:`GET
      ~body:(Bytesrw.Bytes.Reader.empty ())
      "/api/user-data"
  in
  match match' [ route ] request with
  | Some response ->
    let body = Response.body response |> Body.to_string in
    Alcotest.(check string)
      "should extract multiple headers"
      "Key: secret123, User: user42"
      body
  | None -> Alcotest.fail "route with multiple headers should have matched"

let test_route_optional_header () =
  let header_schema =
    Sch.Object.define @@ Sch.Object.mem_opt "X-Request-ID" Sch.string
  in
  let route =
    get (s "api" / s "endpoint")
    >> map
         (fun request_id ->
            let msg =
              match request_id with
              | Some id -> Printf.sprintf "Request ID: %s" id
              | None -> "No request ID"
            in
            Response.of_string msg)
         (header header_schema)
  in
  let request_with =
    Request.make
      ~headers:(Headers.of_list [ "x-request-id", "req-123" ])
      ~meth:`GET
      ~body:(Bytesrw.Bytes.Reader.empty ())
      "/api/endpoint"
  in
  let request_without = make_request "/api/endpoint" in
  (match match' [ route ] request_with with
  | Some response ->
    let body = Response.body response |> Body.to_string in
    Alcotest.(check string)
      "should use header when present"
      "Request ID: req-123"
      body
  | None -> Alcotest.fail "route should match with optional header present");
  match match' [ route ] request_without with
  | Some response ->
    let body = Response.body response |> Body.to_string in
    Alcotest.(check string)
      "should work without optional header"
      "No request ID"
      body
  | None -> Alcotest.fail "route should match without optional header"

let test_route_header_validation () =
  let header_schema =
    Sch.Object.define
    @@ Sch.Object.mem
         "Authorization"
         Sch.(with_ ~constraint_:(Constraint.pattern "^Bearer .+$") string)
  in
  let route =
    get (s "api" / s "secure")
    >> map (fun _auth -> Response.of_string "authorized") (header header_schema)
  in
  let valid_request =
    Request.make
      ~headers:(Headers.of_list [ "authorization", "Bearer token123" ])
      ~meth:`GET
      ~body:(Bytesrw.Bytes.Reader.empty ())
      "/api/secure"
  in
  let invalid_request =
    Request.make
      ~headers:(Headers.of_list [ "authorization", "Basic token123" ])
      ~meth:`GET
      ~body:(Bytesrw.Bytes.Reader.empty ())
      "/api/secure"
  in
  (match match' [ route ] valid_request with
  | Some _ -> ()
  | None -> Alcotest.fail "route should match valid Authorization header");
  try
    let _ = match' [ route ] invalid_request in
    Alcotest.fail "route should raise Validation_failed for invalid header"
  with
  | Router.Validation_failed _ -> ()

let test_url_encoded_path_space () =
  let captured = ref "" in
  let route =
    get (s "files" / str)
    >> pure (fun filename ->
      captured := filename;
      Response.of_string filename)
  in
  let request = make_request "/files/hello%20world.txt" in
  match match' [ route ] request with
  | Some _ ->
    Alcotest.(check string)
      "URL-encoded %20 should be decoded to space"
      "hello world.txt"
      !captured
  | None -> Alcotest.fail "route should have matched URL-encoded path"

let test_url_encoded_path_unicode () =
  let captured = ref "" in
  let route =
    get (s "posts" / str)
    >> pure (fun slug ->
      captured := slug;
      Response.of_string slug)
  in
  let request = make_request "/posts/caf%C3%A9" in
  match match' [ route ] request with
  | Some _ ->
    Alcotest.(check string)
      "URL-encoded UTF-8 should be decoded to café"
      "café"
      !captured
  | None -> Alcotest.fail "route should have matched URL-encoded UTF-8 path"

let test_url_encoded_path_special_chars () =
  let captured = ref "" in
  let route =
    get (s "api" / str)
    >> pure (fun value ->
      captured := value;
      Response.of_string value)
  in
  let request = make_request "/api/hello%2Fworld" in
  match match' [ route ] request with
  | Some _ ->
    Alcotest.(check string)
      "URL-encoded %2F should be decoded to /"
      "hello/world"
      !captured
  | None -> Alcotest.fail "route should have matched URL-encoded path"

let test_url_encoded_multiple_params () =
  let captured1 = ref "" in
  let captured2 = ref "" in
  let route =
    get (s "users" / str / s "files" / str)
    >> pure (fun username filename ->
      captured1 := username;
      captured2 := filename;
      Response.of_string "ok")
  in
  let request = make_request "/users/john%20doe/files/my%20document.pdf" in
  match match' [ route ] request with
  | Some _ ->
    Alcotest.(check string)
      "First param should be decoded"
      "john doe"
      !captured1;
    Alcotest.(check string)
      "Second param should be decoded"
      "my document.pdf"
      !captured2
  | None -> Alcotest.fail "route should have matched"

let test_url_encoded_literal_match () =
  let route =
    get (s "api" / s "test route")
    |> handle_request (fun _ -> Response.of_string "ok")
  in
  let request = make_request "/api/test%20route" in
  match match' [ route ] request with
  | Some _ -> ()
  | None -> Alcotest.fail "encoded literal should match decoded route literal"

let test_url_encoded_international_chars () =
  let captured = ref "" in
  let route =
    get (s "search" / str)
    >> pure (fun query ->
      captured := query;
      Response.of_string query)
  in
  let request = make_request "/search/%E6%97%A5%E6%9C%AC%E8%AA%9E" in
  match match' [ route ] request with
  | Some _ ->
    Alcotest.(check string)
      "Japanese characters should be decoded"
      "日本語"
      !captured
  | None -> Alcotest.fail "route should have matched international characters"

let test_url_encoded_mixed_encoded_unencoded () =
  let captured = ref "" in
  let route =
    get (s "items" / str)
    >> pure (fun item ->
      captured := item;
      Response.of_string item)
  in
  let request = make_request "/items/hello%20world-123_test" in
  match match' [ route ] request with
  | Some _ ->
    Alcotest.(check string)
      "Mixed encoded/unencoded should work"
      "hello world-123_test"
      !captured
  | None -> Alcotest.fail "route should have matched"

let test_url_encoded_plus_sign () =
  let captured = ref "" in
  let route =
    get (s "data" / str)
    >> pure (fun value ->
      captured := value;
      Response.of_string value)
  in
  let request = make_request "/data/a%2Bb" in
  match match' [ route ] request with
  | Some _ ->
    Alcotest.(check string) "Plus sign should be decoded" "a+b" !captured
  | None -> Alcotest.fail "route should have matched"

let test_url_no_encoding_needed () =
  let captured = ref "" in
  let route =
    get (s "files" / str)
    >> pure (fun filename ->
      captured := filename;
      Response.of_string filename)
  in
  let request = make_request "/files/normal-file_name.txt" in
  match match' [ route ] request with
  | Some _ ->
    Alcotest.(check string)
      "Unencoded path should work normally"
      "normal-file_name.txt"
      !captured
  | None -> Alcotest.fail "route should have matched"

let test_recover_validation_failed () =
  let schema =
    Sch.Object.(
      define
      @@ mem "age" Sch.(with_ ~constraint_:(Constraint.int_range 18 100) int))
  in
  let route =
    post (s "users")
    >> map
         (fun age -> Response.of_string ~status:`Created (string_of_int age))
         (body Json schema)
    |> recover (fun _request -> function
      | Validation_failed errors ->
        let error_msg =
          errors
          |> List.map (fun (field, msg) -> Printf.sprintf "%s: %s" field msg)
          |> String.concat ", "
        in
        Some (Response.of_string ~status:`Bad_request error_msg)
      | _ -> None)
  in
  let request =
    Request.make
      ~headers:(Headers.of_list [ "content-type", "application/json" ])
      ~meth:`POST
      ~body:(Bytesrw.Bytes.Reader.of_string {|{"age": 15}|})
      "/users"
  in
  match match' [ route ] request with
  | Some response ->
    Alcotest.(check bool)
      "recover should catch validation error"
      true
      (Response.status response = `Bad_request)
  | None -> Alcotest.fail "route should have matched"

let test_recover_bad_request () =
  let schema = Sch.Object.define @@ Sch.Object.mem "value" Sch.int in
  let route =
    post (s "data")
    >> map
         (fun value -> Response.of_string (string_of_int value))
         (body Json schema)
    |> recover (fun _request -> function
      | Bad_request msg ->
        Some
          (Response.of_string
             ~status:`Bad_request
             (Printf.sprintf "Error: %s" msg))
      | _ -> None)
  in
  let request =
    Request.make
      ~headers:(Headers.of_list [ "content-type", "text/plain" ])
      ~meth:`POST
      ~body:(Bytesrw.Bytes.Reader.of_string "invalid")
      "/data"
  in
  match match' [ route ] request with
  | Some response ->
    Alcotest.(check bool)
      "recover should catch bad request error"
      true
      (Response.status response = `Bad_request)
  | None -> Alcotest.fail "route should have matched"

let test_recover_reraises_when_none () =
  let schema =
    Sch.Object.define
    @@ Sch.Object.mem
         "num"
         Sch.(with_ ~constraint_:(Constraint.int_range 1 10) int)
  in
  let route =
    post (s "test")
    >> map
         (fun num -> Response.of_string (string_of_int num))
         (body Json schema)
    |> recover (fun _request -> function
      | Bad_request _ ->
        Some (Response.of_string ~status:`Bad_request "handled")
      | _ -> None (* Don't handle Validation_failed *))
  in
  let request =
    Request.make
      ~headers:(Headers.of_list [ "content-type", "application/json" ])
      ~meth:`POST
      ~body:(Bytesrw.Bytes.Reader.of_string {|{"num": 99}|})
      "/test"
  in
  try
    let _ = match' [ route ] request in
    Alcotest.fail "should have re-raised Validation_failed"
  with
  | Router.Validation_failed _ -> ()

let test_recover_has_request_access () =
  let schema = Sch.Object.define @@ Sch.Object.mem "value" Sch.int in
  let captured_path = ref "" in
  let route =
    post (s "api" / s "data")
    >> map
         (fun value -> Response.of_string (string_of_int value))
         (body Json schema)
    |> recover (fun request -> function
      | Validation_failed _ ->
        captured_path := Request.target request;
        Some (Response.of_string ~status:`Bad_request "error")
      | _ -> None)
  in
  let request =
    Request.make
      ~headers:(Headers.of_list [ "content-type", "application/json" ])
      ~meth:`POST
      ~body:(Bytesrw.Bytes.Reader.of_string {|{"value": "invalid"}|})
      "/api/data"
  in
  (match match' [ route ] request with
  | Some _ -> ()
  | None -> Alcotest.fail "route should have matched");
  Alcotest.(check string)
    "recover handler should have access to request"
    "/api/data"
    !captured_path

let test_recover_on_scope () =
  let schema =
    Sch.Object.define
    @@ Sch.Object.mem
         "age"
         Sch.(with_ ~constraint_:(Constraint.int_range 1 100) int)
  in
  let route =
    scope
      (s "api")
      [ post (s "users")
        >> map
             (fun age ->
                Response.of_string ~status:`Created (string_of_int age))
             (body Json schema)
      ]
    |> recover (fun _request -> function
      | Validation_failed _ ->
        Some (Response.of_string ~status:`Bad_request "scope error handler")
      | _ -> None)
  in
  let request =
    Request.make
      ~headers:(Headers.of_list [ "content-type", "application/json" ])
      ~meth:`POST
      ~body:(Bytesrw.Bytes.Reader.of_string {|{"age": 150}|})
      "/api/users"
  in
  match match' [ route ] request with
  | Some response ->
    let body = Response.body response |> Body.to_string in
    Alcotest.(check string)
      "scope-level recover should catch errors"
      "scope error handler"
      body
  | None -> Alcotest.fail "route should have matched"

let test_recover_success_path () =
  let schema = Sch.Object.define @@ Sch.Object.mem "value" Sch.int in
  let route =
    post (s "data")
    >> map
         (fun value -> Response.of_string (string_of_int value))
         (body Json schema)
    |> recover (fun _request -> function
      | Validation_failed _ ->
        Some (Response.of_string ~status:`Bad_request "error")
      | _ -> None)
  in
  let request =
    Request.make
      ~headers:(Headers.of_list [ "content-type", "application/json" ])
      ~meth:`POST
      ~body:(Bytesrw.Bytes.Reader.of_string {|{"value": 42}|})
      "/data"
  in
  match match' [ route ] request with
  | Some response ->
    let body = Response.body response |> Body.to_string in
    Alcotest.(check string)
      "recover should not interfere with successful requests"
      "42"
      body
  | None -> Alcotest.fail "route should have matched"

let test_multiple_routes_in_nested_scope () =
  let routes =
    [ scope
        (s "api" / s "v1")
        [ get (s "users")
          |> handle_request (fun _ -> Response.of_string "users")
        ; get (s "profile")
          |> handle_request (fun _ -> Response.of_string "profile")
        ]
    ]
  in
  let request1 = make_request "/api/v1/users" in
  (match match' routes request1 with
  | Some _ -> ()
  | None -> Alcotest.fail "/api/v1/users should match with multiple routes");
  let request2 = make_request "/api/v1/profile" in
  match match' routes request2 with
  | Some _ -> ()
  | None -> Alcotest.fail "/api/v1/profile should match with multiple routes"

let test_nested_scopes_multiple_routes () =
  let routes =
    [ scope
        (s "api")
        [ scope
            (s "v1")
            [ get (s "users")
              |> handle_request (fun _ -> Response.of_string "users")
            ; get (s "profile")
              |> handle_request (fun _ -> Response.of_string "profile")
            ]
        ]
    ]
  in
  let request1 = make_request "/api/v1/users" in
  (match match' routes request1 with
  | Some _ -> ()
  | None -> Alcotest.fail "/api/v1/users should match with nested scopes");
  let request2 = make_request "/api/v1/profile" in
  match match' routes request2 with
  | Some _ -> ()
  | None -> Alcotest.fail "/api/v1/profile should match with nested scopes"

let tag_schema =
  Sch.Object.(
    define ~kind:"Tag" @@ mem ~enc:Fun.id "x-tag" (Sch.list Sch.string))

let test_declared_headers_keep_value_order () =
  let headers =
    Headers.of_list [ "x-tag", "a"; "host", "h"; "X-TAG", "b"; "x-tag", "c" ]
  in
  let decode json =
    match Sch.Json.decode ~lookup:Sch.mem_ci tag_schema json with
    | Sch.Validation.Success v -> v
    | Sch.Validation.Error _ -> Alcotest.fail "expected the tags to decode"
  in
  Alcotest.(check (list string))
    "same as converting every header"
    (decode (Sch_ext.Header.to_json headers))
    (decode (Sch_ext.Header.to_json_of_names [ "x-tag" ] headers))

let test_declared_names () =
  let check name expected schema =
    Alcotest.(check (option (list string)))
      name
      expected
      (Sch_ext.Declared.names schema)
  in
  check "single field" (Some [ "limit" ]) limit_schema;
  check
    "fields in declaration order"
    (Some [ "a"; "b" ])
    Sch.Object.(
      define ~kind:"Pair"
      @@
      let+ a = mem ~enc:fst "a" Sch.int
      and+ b = mem ~enc:snd "b" Sch.int in
      a, b);
  check
    "rejecting unknown members needs every member"
    None
    Sch.Object.(
      define ~kind:"Strict" ~unknown:Sch.Error_on_unknown
      @@ mem ~enc:Fun.id "a" Sch.int);
  check
    "a union reads a discriminator"
    None
    Sch.Union.(
      define
        ~discriminator:"kind"
        [ case ~tag:"a" ~inj:Fun.id ~proj:Option.some limit_schema ]);
  check "not an object" None Sch.int

let test_strict_header_schema_sees_all_headers () =
  let strict =
    Sch.Object.(
      define ~kind:"Strict" ~unknown:Sch.Error_on_unknown
      @@ mem ~enc:Fun.id "x-key" Sch.string)
  in
  let routes =
    [ (get (s "a")
      >>
      let+ key = header strict in
      Response.of_string key)
    ]
  in
  let request headers =
    Request.make
      ~version:`HTTP_1_1
      ~meth:`GET
      ~headers:(Headers.of_list headers)
      ~body:(Bytesrw.Bytes.Reader.empty ())
      "/a"
  in
  check_body
    "only the declared header"
    "k"
    (dispatch routes (request [ "X-Key", "k" ]));
  Alcotest.(check string)
    "another header is rejected"
    "validation_failed"
    (run_outcome routes (request [ "x-key", "k"; "x-other", "v" ]))

let test_inputs_read_twice_agree () =
  let session =
    Sch.Object.(define ~kind:"Session" @@ mem ~enc:Fun.id "session" Sch.string)
  in
  let routes =
    [ (get (s "a")
      >>
      let+ l1 = query limit_schema
      and+ l2 = query limit_schema
      and+ c1 = cookie session
      and+ c2 = cookie session in
      Response.of_string (Printf.sprintf "%d %d %s %s" l1 l2 c1 c2))
    ]
  in
  let request =
    Request.make
      ~version:`HTTP_1_1
      ~meth:`GET
      ~headers:(Headers.of_list [ "cookie", "session=abc; theme=dark" ])
      ~body:(Bytesrw.Bytes.Reader.empty ())
      "/a?limit=5"
  in
  check_body
    "both readers see the value"
    "5 5 abc abc"
    (dispatch routes request)

let test_parsed_inputs_are_not_shared_between_requests () =
  let routes =
    [ (get (s "a")
      >>
      let+ limit = query limit_schema in
      Response.of_string (string_of_int limit))
    ]
  in
  let handler = of_list routes in
  check_body "first" "1" (handler (make_request "/a?limit=1"));
  check_body "second" "2" (handler (make_request "/a?limit=2"))

(* Captures stay with the target the route matched. Everything the schema reads
   comes from the request as middleware left it. *)
let test_schema_sees_request_from_middleware () =
  let session =
    Sch.Object.(define ~kind:"Session" @@ mem ~enc:Fun.id "session" Sch.string)
  in
  let rewrite next request =
    next
      (Request.with_
         ~target:"/m/999?limit=7"
         ~headers:
           (Headers.of_list [ "x-website", "new"; "cookie", "session=new" ])
         request)
  in
  let routes =
    [ scope
        ~middlewares:[ rewrite ]
        (s "m")
        [ (get int
          >>
          let+ limit = query limit_schema
          and+ website = header website_schema
          and+ session = cookie session in
          fun id ->
            Response.of_string
              (Printf.sprintf "%d %d %s %s" id limit website session))
        ]
    ]
  in
  check_body
    "capture from the matched target, the rest from the new request"
    "5 7 new new"
    (dispatch routes (make_request "/m/5?limit=1"))

let contains ~sub text =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length text && (String.sub text i n = sub || at (i + 1))
  in
  at 0

let check_rejected name fragments routes =
  match (of_list routes : Handler.t) with
  | _ -> Alcotest.failf "%s: expected the routes to be rejected" name
  | exception Invalid_argument message ->
    List.iter
      (fun sub ->
         if not (contains ~sub message)
         then Alcotest.failf "%s: %S doesn't mention %S" name message sub)
      fragments

let check_accepted name routes =
  match (of_list routes : Handler.t) with
  | _ -> ()
  | exception Invalid_argument message -> Alcotest.failf "%s: %s" name message

let reply text = pure (fun _ -> Response.of_string text)
let constant text = pure (Response.of_string text)

let test_same_shape_and_method_is_rejected () =
  check_rejected
    "different parsers"
    [ "GET /u/<integer>"; "GET /u/<string:slug>" ]
    [ get (s "u" / int) >> reply "int"; get (s "u" / slug) >> reply "slug" ];
  check_rejected
    "same parser"
    [ "GET /u/<integer>" ]
    [ get (s "u" / int) >> reply "a"; get (s "u" / int) >> reply "b" ];
  check_rejected
    "literal duplicate"
    [ "POST /a/b" ]
    [ post (s "a" / s "b") >> constant "x"
    ; post (s "a" / s "b") >> constant "y"
    ];
  check_rejected
    "duplicate through a scope"
    [ "GET /a/b" ]
    [ scope (s "a") [ get (s "b") >> constant "x" ]
    ; get (s "a" / s "b") >> constant "y"
    ];
  check_rejected
    "two any routes"
    [ "ANY /a" ]
    [ any (s "a") >> constant "x"; any (s "a") >> constant "y" ]

let test_any_with_other_parser_is_rejected () =
  check_rejected
    "any after get"
    [ "ANY /v/<string:slug>"; "GET /v/<integer>" ]
    [ get (s "v" / int) >> reply "int"; any (s "v" / slug) >> reply "slug" ];
  check_rejected
    "get after any"
    [ "ANY /v/<string:slug>"; "GET /v/<integer>" ]
    [ any (s "v" / slug) >> reply "slug"; get (s "v" / int) >> reply "int" ]

let test_specific_method_beats_any () =
  let routes =
    [ any (s "a" / str) >> reply "any"; put (s "a" / str) >> reply "put" ]
  in
  check_body "specific" "put" (dispatch routes (make_request ~meth:`PUT "/a/x"));
  check_body "fallback" "any" (dispatch routes (make_request "/a/x"))

let test_capture_next_to_splat_is_rejected () =
  check_rejected
    "same position"
    [ "GET /x/<integer>"; "GET /x/*" ]
    [ get (s "x" / int) >> reply "int"; get (s "x" / splat) >> reply "splat" ];
  check_rejected
    "order doesn't matter"
    [ "GET /x/<integer>"; "GET /x/*" ]
    [ get (s "x" / splat) >> reply "splat"; get (s "x" / int) >> reply "int" ];
  check_rejected
    "capture deeper in the path"
    [ "GET /x/<integer>/y"; "GET /x/*" ]
    [ get (s "x" / int / s "y") >> reply "int"
    ; get (s "x" / splat) >> reply "splat"
    ];
  check_rejected
    "a capture that can't fail"
    [ "GET /x/<string>"; "GET /x/*" ]
    [ get (s "x" / str) >> reply "str"; get (s "x" / splat) >> reply "splat" ];
  check_rejected
    "any overlaps every method"
    [ "POST /x/<integer>"; "ANY /x/*" ]
    [ post (s "x" / int) >> reply "int"; any (s "x" / splat) >> reply "splat" ]

let test_segments_after_splat_are_rejected () =
  check_rejected
    "literal"
    [ "GET /s/*/tail" ]
    [ get (s "s" / splat / s "tail") >> reply "x" ];
  check_rejected
    "capture"
    [ "GET /s/*/<integer>" ]
    [ get (s "s" / splat / int) >> pure (fun _ _ -> Response.of_string "x") ]

let test_routes_that_do_not_conflict_are_accepted () =
  check_accepted
    "different methods, different parsers"
    [ get (s "u" / int) >> reply "int"; post (s "u" / slug) >> reply "slug" ];
  check_accepted
    "different methods, capture and splat"
    [ get (s "x" / int) >> reply "int"; post (s "x" / splat) >> reply "splat" ];
  check_accepted
    "different shapes after a capture"
    [ get (s "u" / int / s "a") >> reply "int"
    ; get (s "u" / slug / s "b") >> reply "slug"
    ];
  check_accepted
    "literal next to a capture"
    [ get (s "x" / s "new") >> constant "new"
    ; get (s "x" / int) >> reply "int"
    ];
  check_accepted
    "literal next to a splat"
    [ get (s "x" / s "new") >> constant "new"
    ; get (s "x" / splat) >> reply "splat"
    ];
  check_accepted
    "root catch-all next to literals"
    [ get (s "api" / s "hello") >> constant "hi"
    ; get (s "api" / int) >> reply "int"
    ; get splat >> reply "rest"
    ];
  check_accepted
    "any next to a method with the same parser"
    [ any (s "a" / str) >> reply "any"; put (s "a" / str) >> reply "put" ];
  check_accepted
    "any next to a method on a literal path"
    [ any (s "a") >> constant "any"; put (s "a") >> constant "put" ]

let test_failed_capture_under_literal_skips_outer_splat () =
  let routes =
    [ get (s "a" / s "new" / int) >> reply "int"
    ; get (s "a" / splat) >> reply "splat"
    ]
  in
  check_body "capture" "int" (dispatch routes (make_request "/a/new/1"));
  check_body "splat" "splat" (dispatch routes (make_request "/a/other/1"));
  Alcotest.(check string)
    "a failed capture doesn't reach the splat"
    "404"
    (run_outcome routes (make_request "/a/new/abc"))

let test_match_rejects_when_built () =
  match
    (match' [ get (s "a") >> constant "x"; get (s "a") >> constant "y" ]
     : Request.t -> Response.t option)
  with
  | _ -> Alcotest.fail "expected the routes to be rejected"
  | exception Invalid_argument _ -> ()

let test_strict_header_schema_with_uppercase_field () =
  let strict =
    Sch.Object.(
      define ~kind:"Strict" ~unknown:Sch.Error_on_unknown
      @@ mem ~enc:Fun.id "X-Key" Sch.string)
  in
  let routes =
    [ (get (s "a")
      >>
      let+ key = header strict in
      Response.of_string key)
    ]
  in
  let request headers =
    Request.make
      ~version:`HTTP_1_1
      ~meth:`GET
      ~headers:(Headers.of_list headers)
      ~body:(Bytesrw.Bytes.Reader.empty ())
      "/a"
  in
  check_body "declared header" "k" (dispatch routes (request [ "x-key", "k" ]));
  Alcotest.(check string)
    "other headers are still rejected"
    "validation_failed"
    (run_outcome routes (request [ "x-key", "k"; "x-other", "v" ]))

let tests =
  [ "Simple route", `Quick, test_simple_route
  ; "Int64 parameter", `Quick, test_int64_param
  ; "Multiple parameters", `Quick, test_multiple_params
  ; "No match", `Quick, test_no_match
  ; "POST method", `Quick, test_post_method
  ; "Method mismatch", `Quick, test_method_mismatch
  ; "sprintf simple", `Quick, test_sprintf_simple
  ; "sprintf int64", `Quick, test_sprintf_int64
  ; "sprintf multiple", `Quick, test_sprintf_multiple
  ; "Int parameter", `Quick, test_int_param
  ; "Invalid int", `Quick, test_invalid_int
  ; "Bool parameter", `Quick, test_bool_param
  ; "Scope prefix", `Quick, test_scope
  ; "URL generation", `Quick, test_url_generation
  ; "Scope with middlewares", `Quick, test_scope_with_middlewares
  ; "Splat matching", `Quick, test_splat
  ; "Splat captures segments", `Quick, test_splat_segments
  ; "Splat with empty path", `Quick, test_splat_empty
  ; "sprintf with splat", `Quick, test_sprintf_splat
  ; "sprintf with empty splat", `Quick, test_sprintf_splat_empty
  ; "Splat with prefix", `Quick, test_splat_with_prefix
  ; "Slug parameter", `Quick, test_slug
  ; "Slug rejects invalid", `Quick, test_slug_invalid
  ; "Slug valid characters", `Quick, test_slug_valid
  ; "Custom parameter", `Quick, test_custom
  ; "Custom rejects invalid", `Quick, test_custom_invalid
  ; "sprintf with slug", `Quick, test_sprintf_slug
  ; "sprintf with custom", `Quick, test_sprintf_custom
  ; "Root route", `Quick, test_root_route
  ; "sprintf root route", `Quick, test_sprintf_root
  ; "Root route with POST method", `Quick, test_root_with_method
  ; "Root route doesn't match subpaths", `Quick, test_root_doesnt_match_subpaths
  ; "Scoped empty literal", `Quick, test_scoped_empty_literal
  ; ( "Nested scoped empty literal"
    , `Quick
    , test_scoped_empty_literal_no_trailing_slash )
  ; "Scoped with multiple routes", `Quick, test_scoped_with_trailing_route
  ; "Exact path matching", `Quick, test_exact_path_matching
  ; "Any method matches all HTTP methods", `Quick, test_any_method
  ; "Any method with parameters", `Quick, test_any_method_with_params
  ; "Response model", `Quick, test_response_model
  ; "Route with header", `Quick, test_route_with_header
  ; "Route header case-insensitive", `Quick, test_route_header_case_insensitive
  ; "Route header missing fails", `Quick, test_route_header_missing
  ; "Route multiple headers", `Quick, test_route_multiple_headers
  ; "Route optional header", `Quick, test_route_optional_header
  ; "Route header validation", `Quick, test_route_header_validation
  ; "URL-encoded path with space", `Quick, test_url_encoded_path_space
  ; "URL-encoded path with unicode", `Quick, test_url_encoded_path_unicode
  ; ( "URL-encoded path with special chars"
    , `Quick
    , test_url_encoded_path_special_chars )
  ; "URL-encoded multiple parameters", `Quick, test_url_encoded_multiple_params
  ; "URL-encoded literal matching", `Quick, test_url_encoded_literal_match
  ; ( "URL-encoded international chars"
    , `Quick
    , test_url_encoded_international_chars )
  ; ( "URL-encoded mixed encoded/unencoded"
    , `Quick
    , test_url_encoded_mixed_encoded_unencoded )
  ; "URL-encoded plus sign", `Quick, test_url_encoded_plus_sign
  ; "URL no encoding needed", `Quick, test_url_no_encoding_needed
  ; "Recover catches Validation_failed", `Quick, test_recover_validation_failed
  ; "Recover catches Bad_request", `Quick, test_recover_bad_request
  ; "Recover re-raises when None", `Quick, test_recover_reraises_when_none
  ; "Recover has request access", `Quick, test_recover_has_request_access
  ; "Recover on scope", `Quick, test_recover_on_scope
  ; "Recover doesn't affect success", `Quick, test_recover_success_path
  ; ( "Multiple routes in nested scope"
    , `Quick
    , test_multiple_routes_in_nested_scope )
  ; "Nested scopes multiple routes", `Quick, test_nested_scopes_multiple_routes
  ; "Gate is not passed to handler", `Quick, test_gate_not_passed_to_handler
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
  ; ( "Declared headers keep value order"
    , `Quick
    , test_declared_headers_keep_value_order )
  ; "Declared names", `Quick, test_declared_names
  ; ( "Strict header schema sees all headers"
    , `Quick
    , test_strict_header_schema_sees_all_headers )
  ; "Inputs read twice agree", `Quick, test_inputs_read_twice_agree
  ; ( "Parsed inputs aren't shared between requests"
    , `Quick
    , test_parsed_inputs_are_not_shared_between_requests )
  ; ( "Schema sees request from middleware"
    , `Quick
    , test_schema_sees_request_from_middleware )
  ; ( "Same shape and method is rejected"
    , `Quick
    , test_same_shape_and_method_is_rejected )
  ; ( "any with another parser is rejected"
    , `Quick
    , test_any_with_other_parser_is_rejected )
  ; "Specific method beats any", `Quick, test_specific_method_beats_any
  ; ( "Capture next to splat is rejected"
    , `Quick
    , test_capture_next_to_splat_is_rejected )
  ; ( "Segments after splat are rejected"
    , `Quick
    , test_segments_after_splat_are_rejected )
  ; ( "Routes that don't conflict are accepted"
    , `Quick
    , test_routes_that_do_not_conflict_are_accepted )
  ; ( "Failed capture under a literal skips outer splat"
    , `Quick
    , test_failed_capture_under_literal_skips_outer_splat )
  ; "match' rejects when built", `Quick, test_match_rejects_when_built
  ; ( "Strict header schema with uppercase field"
    , `Quick
    , test_strict_header_schema_with_uppercase_field )
  ]
  |> List.map (fun (name, speed, fn) -> Alcotest.test_case name speed fn)
  |> fun tests -> [ "Router", tests ]
