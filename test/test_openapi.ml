open Tapak

let simple_handler () = Response.of_string ~body:"OK" `OK

let test_basic_generation () =
  let open Router in
  let routes =
    [ get (s "users")
      |> summary "List users"
      |> operation_id "listUsers"
      |> unit
      |> into simple_handler
    ; post (s "users")
      |> unit
      |> summary "Create user"
      |> operation_id "createUser"
      |> into simple_handler
    ]
  in
  let spec = Openapi.generate ~title:"Test API" ~version:"1.0.0" routes in
  match spec with
  | `Assoc fields ->
    let openapi_version = List.assoc "openapi" fields in
    Alcotest.(check string)
      "openapi version"
      "3.0.0"
      (match openapi_version with `String v -> v | _ -> "")
  | _ -> Alcotest.fail "Expected object"

let test_path_parameters () =
  let open Router in
  let routes =
    [ get (s "users" / int)
      |> summary "Get user by ID"
      |> operation_id "getUser"
      |> into (fun _id -> Response.of_string ~body:"User" `OK)
    ]
  in
  let spec = Openapi.generate routes in
  match spec with
  | `Assoc fields ->
    (match List.assoc "paths" fields with
    | `Assoc paths ->
      Alcotest.(check bool)
        "has /users/{param0} path"
        true
        (List.mem_assoc "/users/{param0}" paths)
    | _ -> Alcotest.fail "Expected paths object")
  | _ -> Alcotest.fail "Expected object"

let test_annotated_parameters () =
  let open Router in
  let routes =
    [ get (s "users" / p "userId" int)
      |> summary "Get user by ID"
      |> into (fun _id -> Response.of_string ~body:"User" `OK)
    ]
  in
  let spec = Openapi.generate routes in
  match spec with
  | `Assoc fields ->
    (match List.assoc "paths" fields with
    | `Assoc paths ->
      Alcotest.(check bool)
        "has /users/{userId} path"
        true
        (List.mem_assoc "/users/{userId}" paths)
    | _ -> Alcotest.fail "Expected paths object")
  | _ -> Alcotest.fail "Expected object"

let test_request_body () =
  let open Router in
  let open Schema.Syntax in
  let user_schema =
    let+ name = Schema.str "name"
    and+ age = Schema.int "age" in
    name, age
  in
  let routes =
    [ post (s "users")
      |> body Schema.Json user_schema
      |> summary "Create user"
      |> into (fun (_name, _age) -> Response.of_string ~body:"Created" `Created)
    ]
  in
  let spec = Openapi.generate routes in
  try
    let operation =
      List.fold_left
        (fun acc name -> Yojson.Safe.Util.member name acc)
        spec
        [ "paths"; "/users"; "post" ]
    in
    match operation with
    | `Assoc op_fields ->
      Alcotest.(check bool)
        "has requestBody"
        true
        (List.mem_assoc "requestBody" op_fields)
    | _ -> Alcotest.fail "Expected post operation object"
  with
  | Yojson.Safe.Util.Type_error _ ->
    Alcotest.fail "Expected post operation object"

let test_scoped_routes () =
  let open Router in
  let routes =
    [ scope
        (s "api" / s "v1")
        [ get (s "users") |> unit |> into simple_handler
        ; get (s "posts") |> unit |> into simple_handler
        ]
    ]
  in
  let spec = Openapi.generate routes in
  match spec with
  | `Assoc fields ->
    (match List.assoc "paths" fields with
    | `Assoc paths ->
      Alcotest.(check bool)
        "has /api/v1/users path"
        true
        (List.mem_assoc "/api/v1/users" paths);
      Alcotest.(check bool)
        "has /api/v1/posts path"
        true
        (List.mem_assoc "/api/v1/posts" paths)
    | _ -> Alcotest.fail "Expected paths object")
  | _ -> Alcotest.fail "Expected object"

let test_base_path () =
  let open Router in
  let routes = [ get (s "users") |> unit |> into simple_handler ] in
  let spec = Openapi.generate ~base_path:"/api" routes in
  match spec with
  | `Assoc fields ->
    (match List.assoc "paths" fields with
    | `Assoc paths ->
      Alcotest.(check bool)
        "has /api/users path"
        true
        (List.mem_assoc "/api/users" paths)
    | _ -> Alcotest.fail "Expected paths object")
  | _ -> Alcotest.fail "Expected object"

let test_tags () =
  let open Router in
  let routes =
    [ get (s "users")
      |> tags [ "Users"; "Management" ]
      |> unit
      |> into simple_handler
    ]
  in
  let spec = Openapi.generate routes in
  try
    let operation =
      List.fold_left
        (fun acc name -> Yojson.Safe.Util.member name acc)
        spec
        [ "paths"; "/users"; "get" ]
    in
    match operation with
    | `Assoc op_fields ->
      (match List.assoc "tags" op_fields with
      | `List tags -> Alcotest.(check int) "has 2 tags" 2 (List.length tags)
      | _ -> Alcotest.fail "Expected tags array")
    | _ -> Alcotest.fail "Expected get operation object"
  with
  | Yojson.Safe.Util.Type_error _ ->
    Alcotest.fail "Expected get operation object"

let test_include_in_schema () =
  let open Router in
  let routes =
    [ get (s "users")
      |> summary "List users"
      |> operation_id "listUsers"
      |> unit
      |> into simple_handler
    ; get (s "internal" / s "health")
      |> summary "Internal health check"
      |> include_in_schema false
      |> unit
      |> into simple_handler
    ; post (s "users")
      |> summary "Create user"
      |> operation_id "createUser"
      |> unit
      |> into simple_handler
    ]
  in
  let spec = Openapi.generate routes in
  match spec with
  | `Assoc fields ->
    (match List.assoc "paths" fields with
    | `Assoc paths ->
      Alcotest.(check bool)
        "has /users path"
        true
        (List.mem_assoc "/users" paths);
      Alcotest.(check bool)
        "does not have /internal/health path"
        false
        (List.mem_assoc "/internal/health" paths);
      Alcotest.(check int) "has 1 path" 1 (List.length paths)
    | _ -> Alcotest.fail "Expected paths object")
  | _ -> Alcotest.fail "Expected object"

let test_query_parameters () =
  let open Router in
  let open Schema.Syntax in
  let search_schema =
    let+ query = Schema.str "q"
    and+ limit = Schema.int ~default:10 "limit"
    and+ tags = Schema.list "tags" (Schema.Field.str ()) in
    query, limit, tags
  in
  let routes =
    [ get (s "search")
      |> query search_schema
      |> summary "Search"
      |> into (fun (_q, _limit, _tags) ->
        Response.of_string ~body:"Results" `OK)
    ]
  in
  let spec = Openapi.generate routes in
  try
    let operation =
      List.fold_left
        (fun acc name -> Yojson.Safe.Util.member name acc)
        spec
        [ "paths"; "/search"; "get" ]
    in
    match operation with
    | `Assoc op_fields ->
      (match List.assoc "parameters" op_fields with
      | `List params ->
        Alcotest.(check int) "has 3 query parameters" 3 (List.length params);
        (* Check first parameter (q - required string) *)
        (match List.nth params 0 with
        | `Assoc param_fields ->
          Alcotest.(check string)
            "q parameter name"
            "q"
            (match List.assoc "name" param_fields with
            | `String n -> n
            | _ -> "");
          Alcotest.(check string)
            "q parameter in query"
            "query"
            (match List.assoc "in" param_fields with `String i -> i | _ -> "");
          Alcotest.(check bool)
            "q parameter required"
            true
            (match List.assoc "required" param_fields with
            | `Bool r -> r
            | _ -> false)
        | _ -> Alcotest.fail "Expected parameter object");
        (* Check second parameter (limit - optional with default) *)
        (match List.nth params 1 with
        | `Assoc param_fields ->
          Alcotest.(check string)
            "limit parameter name"
            "limit"
            (match List.assoc "name" param_fields with
            | `String n -> n
            | _ -> "");
          Alcotest.(check bool)
            "limit parameter not required (has default)"
            false
            (match List.assoc "required" param_fields with
            | `Bool r -> r
            | _ -> true)
        | _ -> Alcotest.fail "Expected parameter object");
        (* Check third parameter (tags - array with explode) *)
        (match List.nth params 2 with
        | `Assoc param_fields ->
          Alcotest.(check string)
            "tags parameter name"
            "tags"
            (match List.assoc "name" param_fields with
            | `String n -> n
            | _ -> "");
          Alcotest.(check string)
            "tags has form style"
            "form"
            (match List.assoc_opt "style" param_fields with
            | Some (`String s) -> s
            | _ -> "");
          Alcotest.(check bool)
            "tags has explode true"
            true
            (match List.assoc_opt "explode" param_fields with
            | Some (`Bool e) -> e
            | _ -> false);
          (* Check schema is array *)
          (match List.assoc "schema" param_fields with
          | `Assoc schema_fields ->
            Alcotest.(check string)
              "tags schema is array"
              "array"
              (match List.assoc "type" schema_fields with
              | `String t -> t
              | _ -> "")
          | _ -> Alcotest.fail "Expected schema object")
        | _ -> Alcotest.fail "Expected parameter object")
      | _ -> Alcotest.fail "Expected parameters array")
    | _ -> Alcotest.fail "Expected get operation object"
  with
  | Not_found -> Alcotest.fail "Missing required field in spec"

let test_query_with_constraints () =
  let open Router in
  let open Schema in
  let search_schema =
    let open Syntax in
    let+ page = int ~default:1 ~constraint_:(Constraint.int_range 1 100) "page"
    and+ per_page =
      int ~default:20 ~constraint_:(Constraint.int_range 1 100) "per_page"
    in
    page, per_page
  in
  let routes =
    [ get (s "items")
      |> query search_schema
      |> into (fun (_page, _per_page) -> Response.of_string ~body:"Items" `OK)
    ]
  in
  let spec = Openapi.generate routes in
  try
    let operation =
      List.fold_left
        (fun acc name -> Yojson.Safe.Util.member name acc)
        spec
        [ "paths"; "/items"; "get" ]
    in
    match operation with
    | `Assoc op_fields ->
      (match List.assoc "parameters" op_fields with
      | `List params ->
        (* Check page parameter has constraints *)
        (match List.nth params 0 with
        | `Assoc param_fields ->
          (match List.assoc "schema" param_fields with
          | `Assoc schema_fields ->
            Alcotest.(check bool)
              "page has minimum constraint"
              true
              (List.mem_assoc "minimum" schema_fields);
            Alcotest.(check bool)
              "page has maximum constraint"
              true
              (List.mem_assoc "maximum" schema_fields)
          | _ -> Alcotest.fail "Expected schema object")
        | _ -> Alcotest.fail "Expected parameter object")
      | _ -> Alcotest.fail "Expected parameters array")
    | _ -> Alcotest.fail "Expected get operation object"
  with
  | Not_found -> Alcotest.fail "Missing required field in spec"

let tests =
  [ ( "OpenAPI"
    , [ Alcotest.test_case "Basic generation" `Quick test_basic_generation
      ; Alcotest.test_case "Path parameters" `Quick test_path_parameters
      ; Alcotest.test_case
          "Annotated parameters"
          `Quick
          test_annotated_parameters
      ; Alcotest.test_case "Request body" `Quick test_request_body
      ; Alcotest.test_case "Scoped routes" `Quick test_scoped_routes
      ; Alcotest.test_case "Base path" `Quick test_base_path
      ; Alcotest.test_case "Tags" `Quick test_tags
      ; Alcotest.test_case "Include in schema" `Quick test_include_in_schema
      ; Alcotest.test_case "Query parameters" `Quick test_query_parameters
      ; Alcotest.test_case
          "Query with constraints"
          `Quick
          test_query_with_constraints
      ] )
  ]
