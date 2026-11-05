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

let api_key =
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
    |> header api_key
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
    and+ _ = header api_key
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
          |> gate (header api_key)
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
      |> gate (header api_key |> refine (fun key -> Ok key))
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
      |> gate (header api_key |> refine fail_if_run)
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
      |> gate (header api_key)
      >->
      let+ _ = header api_key
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
      let+ _ = header api_key
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
  let key = memo (header api_key) in
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
    [ get (s "x") >-> map (fun _ -> ok) (header api_key <|> fail Denied) ]
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
            (map ignore (header api_key) <|> map ignore (cookie session))
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
    (let+ _ = header api_key
     and+ _ = cookie session in
     ())
    <|> map ignore (header api_key)
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
      |> gate (header api_key)
      >-> map (fun _ -> ok) (map Option.some (header api_key) <|> pure None)
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
  ; ( "Root path is documented as /"
    , `Quick
    , test_root_path_is_documented_as_slash )
  ; ( "Memo documents its source once"
    , `Quick
    , test_memo_documents_its_source_once )
  ]
  |> List.map (fun (name, speed, fn) -> Alcotest.test_case name speed fn)
  |> fun tests -> [ "Router2 OpenAPI", tests ]
