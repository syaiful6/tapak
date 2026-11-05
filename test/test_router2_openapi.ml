open Tapak
module Openapi = Private.Openapi
module Spec = Openapi.Spec
module R2 = Private.Router2

let page =
  Sch.Object.(
    define ~kind:"Page"
    @@
    let+ limit = mem ~doc:"max" ~default:10 ~enc:fst "limit" Sch.int
    and+ q = mem_opt ~enc:snd "q" Sch.string in
    limit, q)

let api_key_schema =
  Sch.Object.(define ~kind:"Key" @@ mem ~enc:Fun.id "x-api-key" Sch.string)

let api_key_opt =
  Sch.Object.(
    define ~kind:"KeyOpt" @@ mem_opt ~enc:Fun.id "X-API-Key" Sch.string)

let session =
  Sch.Object.(define ~kind:"Session" @@ mem ~enc:Fun.id "session" Sch.string)

let name = Sch.Object.(define ~kind:"Name" @@ mem ~enc:Fun.id "name" Sch.string)

let other_name =
  Sch.Object.(define ~kind:"Other" @@ mem ~enc:Fun.id "other" Sch.string)

let ok = Response.of_string "ok"

type Router.extractor_error += Denied

let color_path () =
  Router.enum
    ~parse:(fun ~off ~len s -> Some (String.sub s off len))
    ~format:Fun.id
    ~type_name:"string"
    ~values:[ "red"; "blue" ]
    ()

(* The same API written against both routers must produce the same document *)
let router_routes =
  let open Router in
  [ get (s "users" / p "id" int64 / s "posts" / int)
    |> query page
    |> header api_key_schema
    |> cookie session
    |> summary "summary"
    |> description "description"
    |> tags [ "a" ]
    |> tag "b"
    |> operation_id "getPost"
    |> into (fun _ _ _ _ _ -> ok)
  ; post (s "users")
    |> body Json name
    |> response_model ~status:`Created ~schema:name
    |> into Fun.id
  ; put (s "form" / ann ("c", "colour") (color_path ()))
    |> body Urlencoded name
    |> into (fun _ _ -> ok)
  ; patch (s "upload") |> body Multipart name |> into (fun _ -> ok)
  ; delete (s "files" / splat) |> into (fun _ -> ok)
  ; any (s "any") |> extract (fun _ -> Ok ()) |> into (fun () -> ok)
  ; head (s "hidden") |> include_in_schema false |> into ok
  ; scope (s "api" / s "v1") [ get (s "x" / slug) |> into (fun _ -> ok) ]
  ]

let router2_routes =
  let open R2 in
  [ (get (s "users" / p "id" int64 / s "posts" / int)
    |> summary "summary"
    |> description "description"
    |> tags [ "a" ]
    |> tag "b"
    |> operation_id "getPost"
    >->
    let+ _ = query page
    and+ _ = header api_key_schema
    and+ _ = cookie session in
    fun _ _ -> ok)
  ; post (s "users")
    |> response_model ~status:`Created ~schema:name
    >-> body Json name
  ; (put (s "form" / ann ("c", "colour") (color_path ()))
    >->
    let+ _ = body Urlencoded name in
    fun _ -> ok)
  ; (patch (s "upload")
    >->
    let+ _ = body Multipart name in
    ok)
  ; delete (s "files" / splat) >-> pure (fun _ -> ok)
  ; (any (s "any")
    >->
    let+ () = extract (fun _ -> Ok ()) in
    ok)
  ; head (s "hidden") |> include_in_schema false >-> pure ok
  ; scope (s "api" / s "v1") [ get (s "x" / slug) >-> pure (fun _ -> ok) ]
  ]

let test_matches_router_output () =
  Alcotest.(check string)
    "same document"
    (Openapi.generate_string
       ~title:"T"
       ~version:"1"
       ~description:"D"
       ~base_path:"/b"
       router_routes)
    (Openapi.generate_router2_string
       ~title:"T"
       ~version:"1"
       ~description:"D"
       ~base_path:"/b"
       router2_routes)

let operation path method_ routes =
  let spec = Openapi.generate_router2 routes in
  match List.assoc path spec.paths with
  | Spec.Json_schema.Or_ref.Value item -> Option.get (method_ item)
  | _ -> Alcotest.fail "expected an inline path item"

let get_op item = item.Spec.Path_item.get

let parameters (op : Spec.Operation.t) =
  List.filter_map
    (function
      | Spec.Json_schema.Or_ref.Value (p : Spec.Parameter.t) -> Some p
      | _ -> None)
    op.parameters

let names op = List.map (fun (p : Spec.Parameter.t) -> p.name) (parameters op)

let test_gates_documented_in_run_order () =
  let open R2 in
  let routes =
    [ scope
        ~gates:[ void (cookie session) ]
        (s "api")
        [ (get (s "x" / p "id" int)
          |> gate (header api_key_schema)
          >->
          let+ _ = query page in
          fun _ -> ok)
        ]
    ]
  in
  Alcotest.(check (list string))
    "path, scope gate, endpoint gate, block"
    [ "id"; "session"; "x-api-key"; "limit"; "q" ]
    (names (operation "/api/x/{id}" get_op routes))

let test_refine_documents_its_source () =
  let open R2 in
  let routes =
    [ get (s "x")
      |> gate (header api_key_schema |> refine (fun key -> Ok key))
      >-> pure ok
    ]
  in
  Alcotest.(check (list string))
    "header seen through refine"
    [ "x-api-key" ]
    (names (operation "/x" get_op routes))

let test_extract_gate_is_invisible () =
  let open R2 in
  let routes = [ get (s "x") |> gate (extract (fun _ -> Ok ())) >-> pure ok ] in
  Alcotest.(check (list string))
    "no parameters"
    []
    (names (operation "/x" get_op routes))

let test_generation_runs_nothing () =
  let open R2 in
  let fail_if_run _ = Alcotest.fail "effect ran during generation" in
  let routes =
    [ (get (s "x")
      |> gate (extract fail_if_run)
      |> gate (header api_key_schema |> refine fail_if_run)
      >->
      let+ _ = extract fail_if_run in
      ok)
    ]
  in
  Alcotest.(check (list string))
    "only the static parts"
    [ "x-api-key" ]
    (names (operation "/x" get_op routes))

let test_shared_parameter_is_merged () =
  let open R2 in
  let routes =
    [ (get (s "x")
      |> gate (header api_key_opt)
      |> gate (header api_key_schema)
      >->
      let+ _ = header api_key_schema
      and+ _ = cookie session in
      ok)
    ]
  in
  match parameters (operation "/x" get_op routes) with
  | [ key; cookie ] ->
    Alcotest.(check string) "first spelling kept" "X-API-Key" key.name;
    Alcotest.(check bool) "required wins" true key.required;
    Alcotest.(check string) "other parameter kept" "session" cookie.name
  | params ->
    Alcotest.failf "expected 2 parameters, got %d" (List.length params)

let test_same_name_in_other_location_is_kept () =
  let open R2 in
  let as_query =
    Sch.Object.(define ~kind:"Q" @@ mem ~enc:Fun.id "x-api-key" Sch.string)
  in
  let routes =
    [ (get (s "x")
      >->
      let+ _ = header api_key_schema
      and+ _ = query as_query in
      ok)
    ]
  in
  Alcotest.(check int)
    "both kept"
    2
    (List.length (parameters (operation "/x" get_op routes)))

let test_first_body_is_documented () =
  let open R2 in
  let routes =
    [ (post (s "x")
      >->
      let+ _ = body Json name
      and+ _ = body Json other_name in
      ok)
    ]
  in
  let document = Openapi.generate_router2_string routes in
  let contains needle =
    let n = String.length needle in
    let rec go i =
      i + n <= String.length document
      && (String.equal (String.sub document i n) needle || go (i + 1))
    in
    go 0
  in
  Alcotest.(check bool) "first body present" true (contains {|"name"|});
  Alcotest.(check bool) "second body absent" false (contains {|"other"|})

let test_memo_documents_its_source_once () =
  let open R2 in
  let key = memo (header api_key_schema) in
  let routes =
    [ scope
        ~gates:[ void key ]
        (s "api")
        [ get (s "x") >-> map (fun _ -> ok) key ]
    ]
  in
  Alcotest.(check (list string))
    "listed once"
    [ "x-api-key" ]
    (names (operation "/api/x" get_op routes))

let required_flags op =
  List.map (fun (p : Spec.Parameter.t) -> p.name, p.required) (parameters op)

let flags = Alcotest.(list (pair string bool))

let test_fail_keeps_parameter_required () =
  let open R2 in
  let routes =
    [ get (s "x") >-> map (fun _ -> ok) (header api_key_schema <|> fail Denied)
    ]
  in
  Alcotest.check
    flags
    "required"
    [ "x-api-key", true ]
    (required_flags (operation "/x" get_op routes))

let test_alt_makes_one_sided_parameter_optional () =
  let open R2 in
  let routes =
    [ get (s "x")
      >-> map
            (fun _ -> ok)
            (map ignore (header api_key_schema) <|> map ignore (cookie session))
    ]
  in
  Alcotest.check
    flags
    "both optional"
    [ "x-api-key", false; "session", false ]
    (required_flags (operation "/x" get_op routes))

let test_alt_keeps_parameter_required_in_both () =
  let open R2 in
  let both =
    (let+ _ = header api_key_schema
     and+ _ = cookie session in
     ())
    <|> map ignore (header api_key_schema)
  in
  let routes = [ get (s "x") >-> map (fun () -> ok) both ] in
  Alcotest.check
    flags
    "only the shared one"
    [ "x-api-key", true; "session", false ]
    (required_flags (operation "/x" get_op routes))

let test_gate_requires_what_alt_leaves_optional () =
  let open R2 in
  let routes =
    [ get (s "x")
      |> gate (header api_key_schema)
      >-> map
            (fun _ -> ok)
            (map Option.some (header api_key_schema) <|> pure None)
    ]
  in
  Alcotest.check
    flags
    "required wins"
    [ "x-api-key", true ]
    (required_flags (operation "/x" get_op routes))

let body_required op =
  match op.Spec.Operation.request_body with
  | Some (Spec.Json_schema.Or_ref.Value body) ->
    Option.value body.Spec.Request_body.required ~default:false
  | _ -> Alcotest.fail "expected an inline request body"

let post_op item = item.Spec.Path_item.post

let test_body_requirement_follows_alt () =
  let open R2 in
  let required =
    [ post (s "x") >-> map (fun _ -> ok) (body Json name <|> fail Denied) ]
  in
  Alcotest.(check bool)
    "with fail"
    true
    (body_required (operation "/x" post_op required));
  let optional =
    [ post (s "x")
      >-> map (fun _ -> ok) (map Option.some (body Json name) <|> pure None)
    ]
  in
  Alcotest.(check bool)
    "with a fallback"
    false
    (body_required (operation "/x" post_op optional))

let test_root_path_is_documented_as_slash () =
  let open R2 in
  let routes = [ get (s "") >-> pure ok ] in
  ignore (operation "/" get_op routes)

let security_of path routes =
  (operation path get_op routes).Spec.Operation.security

let requirements = Alcotest.(option (list (list (pair string (list string)))))
let no_login ~username:_ ~password:_ = Some ()
let basic_auth = R2.(secured (basic no_login))
let bearer_auth = R2.(void (secured (bearer ~format:"JWT" Option.some)))

let api_key_auth =
  R2.(void (secured (api_key ~in_:`Header "x-api-key" Option.some)))

let test_no_security_by_default () =
  let open R2 in
  Alcotest.check
    requirements
    "absent, including for plain extract"
    None
    (security_of
       "/x"
       [ get (s "x") |> gate (extract (fun _ -> Ok ())) >-> pure ok ])

let test_security_follows_schema_shape () =
  let open R2 in
  let check name expected gates =
    Alcotest.check
      requirements
      name
      expected
      (security_of "/x" [ get (s "x") |> gates >-> pure ok ])
  in
  check "single" (Some [ [ "basicAuth", [] ] ]) (gate basic_auth);
  check
    "sequence is AND"
    (Some [ [ "basicAuth", []; "bearerAuth", [] ] ])
    (fun e -> e |> gate basic_auth |> gate bearer_auth);
  check
    "<|> is OR"
    (Some [ [ "basicAuth", [] ]; [ "bearerAuth", [] ] ])
    (gate (basic_auth <|> bearer_auth));
  check
    "AND distributes over OR"
    (Some
       [ [ "basicAuth", []; "x-api-key", [] ]
       ; [ "bearerAuth", []; "x-api-key", [] ]
       ])
    (fun e -> e |> gate (basic_auth <|> bearer_auth) |> gate api_key_auth);
  check
    "a fallback makes it optional"
    (Some [ [ "basicAuth", [] ]; [] ])
    (gate (basic_auth <|> pure ()));
  check
    "fail adds nothing"
    (Some [ [ "basicAuth", [] ] ])
    (gate (basic_auth <|> fail Denied));
  check
    "reading a scheme twice is one requirement"
    (Some [ [ "basicAuth", [] ] ])
    (fun e -> e |> gate basic_auth |> gate basic_auth);
  check
    "seen through refine and memo"
    (Some [ [ "basicAuth", [] ] ])
    (gate (memo basic_auth |> refine (fun () -> Ok ())))

let test_scope_gate_secures_routes () =
  let open R2 in
  let routes =
    [ scope
        ~gates:[ void basic_auth ]
        (s "api")
        [ get (s "x") >-> pure ok; get (s "y") |> gate bearer_auth >-> pure ok ]
    ; get (s "public") >-> pure ok
    ]
  in
  Alcotest.check
    requirements
    "inherited"
    (Some [ [ "basicAuth", [] ] ])
    (security_of "/api/x" routes);
  Alcotest.check
    requirements
    "combined with the endpoint"
    (Some [ [ "basicAuth", []; "bearerAuth", [] ] ])
    (security_of "/api/y" routes);
  Alcotest.check requirements "outside" None (security_of "/public" routes)

let test_security_schemes_are_documented () =
  let open R2 in
  let routes =
    [ get (s "a") |> gate basic_auth >-> pure ok
    ; get (s "b") |> gate bearer_auth >-> pure ok
    ; get (s "c") |> gate api_key_auth >-> pure ok
    ; get (s "d") |> gate basic_auth >-> pure ok
    ; get (s "hidden")
      |> gate (secured ~name:"hidden" (basic no_login))
      |> include_in_schema false
      >-> pure ok
    ]
  in
  let spec = Openapi.generate_router2 routes in
  let schemes =
    (Option.get spec.components).Spec.Component.security_schemes
    |> List.map (fun (name, scheme) ->
      match scheme with
      | Spec.Json_schema.Or_ref.Value (v : Spec.Security_scheme.t) -> name, v
      | _ -> Alcotest.fail "expected an inline scheme")
  in
  Alcotest.(check (list string))
    "each once, hidden routes excluded"
    [ "basicAuth"; "bearerAuth"; "x-api-key" ]
    (List.map fst schemes);
  let scheme name = List.assoc name schemes in
  Alcotest.(check (option string))
    "basic"
    (Some "basic")
    (scheme "basicAuth").scheme;
  Alcotest.(check (option string))
    "bearer format"
    (Some "JWT")
    (scheme "bearerAuth").bearer_format;
  Alcotest.(check (option string))
    "api key name"
    (Some "x-api-key")
    (scheme "x-api-key").name;
  (* The whole document still encodes *)
  ignore (Openapi.generate_router2_string routes)

let test_conflicting_scheme_names_are_rejected () =
  let open R2 in
  let routes =
    [ get (s "a") |> gate (secured ~name:"auth" (basic no_login)) >-> pure ok
    ; get (s "b")
      |> gate (secured ~name:"auth" (bearer Option.some))
      >-> pure ok
    ]
  in
  match Openapi.generate_router2 routes with
  | _ -> Alcotest.fail "expected the conflict to be rejected"
  | exception Invalid_argument _ -> ()

let test_generating_security_runs_no_extractor () =
  let open R2 in
  let routes =
    [ get (s "x")
      |> gate
           (secured
              (basic (fun ~username:_ ~password:_ ->
                 Alcotest.fail "verify ran during generation")))
      >-> pure ok
    ]
  in
  ignore (Openapi.generate_router2 routes)

let test_realm_is_not_a_conflict () =
  let open R2 in
  let routes =
    [ get (s "a") |> gate (secured (basic ~realm:"One" no_login)) >-> pure ok
    ; get (s "b") |> gate (secured (basic ~realm:"Two" no_login)) >-> pure ok
    ]
  in
  let spec = Openapi.generate_router2 routes in
  Alcotest.(check int)
    "OpenAPI has no realm, so one scheme"
    1
    (List.length (Option.get spec.components).Spec.Component.security_schemes)

let response_401 (op : Spec.Operation.t) =
  match List.assoc_opt "401" op.responses.responses with
  | Some (Spec.Json_schema.Or_ref.Value (r : Spec.Response.t)) -> Some r
  | Some _ -> Alcotest.fail "expected an inline response"
  | None -> None

(* What the router really answers when no credentials are sent *)
let answers_401 routes =
  let request =
    Request.make
      ~version:`HTTP_1_1
      ~meth:`GET
      ~headers:(Headers.of_list [])
      ~body:(Bytesrw.Bytes.Reader.of_string "")
      "/x"
  in
  match R2.of_list routes request with
  | response -> Response.status response = `Unauthorized
  | exception Router.Extraction_failed _ -> false

let test_401_matches_what_the_router_answers () =
  let open R2 in
  let check name expected gates =
    let routes = [ get (s "x") |> gates >-> pure ok ] in
    Alcotest.(check bool)
      (name ^ ": documented")
      expected
      (Option.is_some (response_401 (operation "/x" get_op routes)));
    Alcotest.(check bool) (name ^ ": answered") expected (answers_401 routes)
  in
  let denied = extract (fun _ -> Error Denied) in
  check "no security" false Fun.id;
  check "plain extract" false (gate denied);
  check "gate" true (gate basic_auth);
  check "second gate" true (fun e ->
    e |> gate (extract (fun _ -> Ok ())) |> gate basic_auth);
  check
    "through memo and refine"
    true
    (gate (memo basic_auth |> refine (fun () -> Ok ())));
  check "either scheme" true (gate (basic_auth <|> bearer_auth));
  check "a fallback never fails" false (gate (basic_auth <|> pure ()));
  check "fail replaces the error" false (gate (basic_auth <|> fail Denied));
  check
    "the right error wins over a scheme"
    false
    (gate (basic_auth <|> denied));
  check
    "the right scheme wins over an error"
    true
    (gate (denied <|> basic_auth));
  check "fail on the left is ignored" true (gate (fail Denied <|> basic_auth))

let test_401_documents_the_challenge () =
  let open R2 in
  let header gates =
    let routes = [ get (s "x") |> gates >-> pure ok ] in
    let response = Option.get (response_401 (operation "/x" get_op routes)) in
    match List.assoc_opt "WWW-Authenticate" response.headers with
    | Some (Spec.Json_schema.Or_ref.Value (h : Spec.Header.t)) ->
      Some (Option.get h.description, Option.get h.required)
    | Some _ -> Alcotest.fail "expected an inline header"
    | None -> None
  in
  let check = Alcotest.(check (option (pair string bool))) in
  check
    "basic has its realm"
    (Some ({|Basic realm="Staff"|}, true))
    (header (gate (secured (basic ~realm:"Staff" no_login))));
  check "bearer" (Some ("Bearer", true)) (header (gate bearer_auth));
  check "api key has no challenge" None (header (gate api_key_auth));
  check
    "only the right branch can answer"
    (Some ("Bearer", true))
    (header (gate (basic_auth <|> bearer_auth)));
  check
    "the right branch has no challenge"
    None
    (header (gate (bearer_auth <|> api_key_auth)));
  check
    "not guaranteed when another scheme fails first"
    (Some ("Bearer", false))
    (header (fun e -> e |> gate api_key_auth |> gate bearer_auth))

let test_scope_gate_documents_401 () =
  let open R2 in
  let routes =
    [ scope ~gates:[ void basic_auth ] (s "api") [ get (s "x") >-> pure ok ]
    ; get (s "public") >-> pure ok
    ]
  in
  Alcotest.(check bool)
    "inherited"
    true
    (Option.is_some (response_401 (operation "/api/x" get_op routes)));
  Alcotest.(check bool)
    "outside"
    false
    (Option.is_some (response_401 (operation "/public" get_op routes)))

let test_declared_401_and_success_are_kept () =
  let open R2 in
  let routes =
    [ get (s "x")
      |> gate basic_auth
      |> response_model ~status:`Unauthorized ~schema:name
      >-> pure "denied"
    ; get (s "y") |> gate basic_auth >-> pure ok
    ]
  in
  Alcotest.(check string)
    "declared 401"
    ""
    (Option.get (response_401 (operation "/x" get_op routes))).description;
  Alcotest.(check (list string))
    "success first"
    [ "200"; "401" ]
    (List.map fst (operation "/y" get_op routes).responses.responses);
  (* The whole document still encodes *)
  ignore (Openapi.generate_router2_string routes)

let test_body_of_documents_each_content_type () =
  let open R2 in
  let routes =
    [ post (s "x") >-> map (fun _ -> ok) (body_of [ Json; Urlencoded ] name) ]
  in
  match (operation "/x" post_op routes).request_body with
  | Some (Spec.Json_schema.Or_ref.Value body) ->
    Alcotest.(check (list string))
      "both"
      [ "application/json"; "application/x-www-form-urlencoded" ]
      (List.map fst body.content)
  | _ -> Alcotest.fail "expected an inline request body"

let tests =
  [ "Matches Router output", `Quick, test_matches_router_output
  ; "Gates documented in run order", `Quick, test_gates_documented_in_run_order
  ; "Refine documents its source", `Quick, test_refine_documents_its_source
  ; "Extract gate is invisible", `Quick, test_extract_gate_is_invisible
  ; "Generation runs nothing", `Quick, test_generation_runs_nothing
  ; "Shared parameter is merged", `Quick, test_shared_parameter_is_merged
  ; ( "Same name in other location is kept"
    , `Quick
    , test_same_name_in_other_location_is_kept )
  ; "First body is documented", `Quick, test_first_body_is_documented
  ; "fail keeps parameter required", `Quick, test_fail_keeps_parameter_required
  ; ( "<|> makes one-sided parameter optional"
    , `Quick
    , test_alt_makes_one_sided_parameter_optional )
  ; ( "<|> keeps parameter required in both"
    , `Quick
    , test_alt_keeps_parameter_required_in_both )
  ; ( "Gate requires what <|> leaves optional"
    , `Quick
    , test_gate_requires_what_alt_leaves_optional )
  ; "Body requirement follows <|>", `Quick, test_body_requirement_follows_alt
  ; "No security by default", `Quick, test_no_security_by_default
  ; "Security follows schema shape", `Quick, test_security_follows_schema_shape
  ; "Scope gate secures routes", `Quick, test_scope_gate_secures_routes
  ; ( "Security schemes are documented"
    , `Quick
    , test_security_schemes_are_documented )
  ; ( "Conflicting scheme names are rejected"
    , `Quick
    , test_conflicting_scheme_names_are_rejected )
  ; ( "Generating security runs no extractor"
    , `Quick
    , test_generating_security_runs_no_extractor )
  ; "Realm is not a conflict", `Quick, test_realm_is_not_a_conflict
  ; ( "401 matches what the router answers"
    , `Quick
    , test_401_matches_what_the_router_answers )
  ; "401 documents the challenge", `Quick, test_401_documents_the_challenge
  ; "Scope gate documents 401", `Quick, test_scope_gate_documents_401
  ; ( "Declared 401 and success are kept"
    , `Quick
    , test_declared_401_and_success_are_kept )
  ; ( "body_of documents each content type"
    , `Quick
    , test_body_of_documents_each_content_type )
  ; ( "Root path is documented as /"
    , `Quick
    , test_root_path_is_documented_as_slash )
  ; ( "Memo documents its source once"
    , `Quick
    , test_memo_documents_its_source_once )
  ]
  |> List.map (fun (name, speed, fn) -> Alcotest.test_case name speed fn)
  |> fun tests -> [ "Router2 OpenAPI", tests ]
