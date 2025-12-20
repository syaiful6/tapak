open Tapak_kernel

let make_request ?(meth = `GET) target =
  Request.create
    ~scheme:`HTTP
    ~version:Piaf.Versions.HTTP.HTTP_1_1
    ~meth
    ~body:Piaf.Body.empty
    target

let test_simple_route () =
  let open Router in
  let route =
    get (s "users")
    |> request
    |> into (fun request ->
      Alcotest.(check string)
        "method should be GET"
        "GET"
        (Piaf.Method.to_string (Request.meth request));
      Response.of_string ~body:"users list" `OK)
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
  let open Router in
  let route =
    get (s "users" / int64)
    |> into (fun id ->
      Alcotest.(check int64) "id should be 42" 42L id;
      Response.of_string ~body:(Printf.sprintf "User %Ld" id) `OK)
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
  let open Router in
  let route =
    get (s "users" / int64 / s "posts" / str)
    |> into (fun user_id slug ->
      Alcotest.(check int64) "user_id should be 42" 42L user_id;
      Alcotest.(check string) "slug should be 'hello'" "hello" slug;
      Response.of_string
        ~body:(Printf.sprintf "User %Ld post %s" user_id slug)
        `OK)
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
  let open Router in
  let route =
    get (s "users" / int64)
    |> into (fun _id -> Response.of_string ~body:"user" `OK)
  in
  let request = make_request "/posts/42" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "route should not have matched"
  | None -> ()

let test_post_method () =
  let open Router in
  let route =
    post (s "users")
    |> unit
    |> into (fun () -> Response.of_string ~body:"user created" `Created)
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
  let open Router in
  let route =
    post (s "users")
    |> unit
    |> into (fun () -> Response.of_string ~body:"user created" `Created)
  in
  let request = make_request ~meth:`GET "/users" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "GET should not match POST route"
  | None -> ()

let test_sprintf_simple () =
  let open Router in
  let pattern = s "users" in
  let url = sprintf pattern in
  Alcotest.(check string) "url should be /users" "/users" url

let test_sprintf_int64 () =
  let open Router in
  let pattern = s "users" / int64 in
  let url = sprintf pattern 42L in
  Alcotest.(check string) "url should be /users/42" "/users/42" url

let test_sprintf_multiple () =
  let open Router in
  let pattern = s "users" / int64 / s "posts" / str in
  let url = sprintf pattern 42L "hello-world" in
  Alcotest.(check string)
    "url should be /users/42/posts/hello-world"
    "/users/42/posts/hello-world"
    url

let test_int_param () =
  let open Router in
  let route =
    get (s "page" / int)
    |> into (fun page_num ->
      Alcotest.(check int) "page should be 5" 5 page_num;
      Response.of_string ~body:(Printf.sprintf "Page %d" page_num) `OK)
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
  let open Router in
  let route =
    get (s "page" / int)
    |> into (fun _page -> Response.of_string ~body:"page" `OK)
  in
  let request = make_request "/page/not-a-number" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "route should not match invalid int"
  | None -> ()

let test_bool_param () =
  let open Router in
  let route =
    get (s "published" / bool)
    |> into (fun is_published ->
      Alcotest.(check bool) "should be true" true is_published;
      Response.of_string ~body:(string_of_bool is_published) `OK)
  in
  let request = make_request "/published/true" in
  match match' [ route ] request with
  | Some _ -> ()
  | None -> Alcotest.fail "route should have matched"

let test_scope () =
  let open Router in
  let route =
    scope
      (s "api" / s "v1")
      [ get (s "users")
        |> unit
        |> into (fun () -> Response.of_string ~body:"users" `OK)
      ]
  in
  let request = make_request "/api/v1/users" in
  match match' [ route ] request with
  | Some _ -> ()
  | None -> Alcotest.fail "scoped route should have matched"

let test_url_generation () =
  let open Router in
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
  let open Router in
  let module M = struct
    type t = unit

    let call () next req =
      let resp = next req in
      Response.with_
        ~headers:(Piaf.Headers.add (Response.headers resp) "X-Test" "true")
        resp
  end
  in
  let test_middleware = Middleware.use (module M) () in
  let route =
    scope
      ~middlewares:[ test_middleware ]
      (s "api")
      [ get (s "test")
        |> unit
        |> into (fun () -> Response.of_string ~body:"ok" `OK)
      ]
  in
  let request = make_request "/api/test" in
  match match' [ route ] request with
  | Some response ->
    (match Piaf.Headers.get (Response.headers response) "x-test" with
    | Some value -> Alcotest.(check string) "middleware should run" "true" value
    | None -> Alcotest.fail "middleware header not found")
  | None -> Alcotest.fail "scoped route should have matched"

let test_splat () =
  let open Router in
  let route =
    get (s "files" / splat)
    |> into (fun segments ->
      let path = String.concat "/" segments in
      Response.of_string ~body:(Printf.sprintf "Files: %s" path) `OK)
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
  let open Router in
  let captured_segments = ref [] in
  let route =
    get (s "static" / splat)
    |> into (fun segments ->
      captured_segments := segments;
      Response.of_string ~body:"ok" `OK)
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
  let open Router in
  let captured_segments = ref [] in
  let route =
    get (s "spa" / splat)
    |> into (fun segments ->
      captured_segments := segments;
      Response.of_string ~body:"ok" `OK)
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
  let open Router in
  let pattern = s "files" / splat in
  let url = sprintf pattern [ "docs"; "readme.txt" ] in
  Alcotest.(check string)
    "url should be /files/docs/readme.txt"
    "/files/docs/readme.txt"
    url

let test_sprintf_splat_empty () =
  let open Router in
  let pattern = s "spa" / splat in
  let url = sprintf pattern [] in
  Alcotest.(check string) "url should be /spa" "/spa" url

let test_splat_with_prefix () =
  let open Router in
  let route =
    get (s "api" / s "v1" / splat)
    |> into (fun segments ->
      let path = String.concat "/" segments in
      Response.of_string ~body:(Printf.sprintf "API: %s" path) `OK)
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
  let open Router in
  let route =
    get (s "posts" / slug)
    |> into (fun slug_str ->
      Response.of_string ~body:(Printf.sprintf "Post: %s" slug_str) `OK)
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
  let open Router in
  let route =
    get (s "posts" / slug)
    |> into (fun _slug -> Response.of_string ~body:"ok" `OK)
  in
  let request = make_request "/posts/My_Post" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "slug should not match invalid format"
  | None -> ()

let test_slug_valid () =
  let open Router in
  let captured_slug = ref "" in
  let route =
    get (s "posts" / slug)
    |> into (fun slug_str ->
      captured_slug := slug_str;
      Response.of_string ~body:"ok" `OK)
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
  let open Router in
  let is_hex_color s =
    String.length s = 6
    && String.for_all
         (fun c ->
            (c >= '0' && c <= '9')
            || (c >= 'a' && c <= 'f')
            || (c >= 'A' && c <= 'F'))
         s
  in
  let hex_color =
    custom
      ~parse:(fun s -> if is_hex_color s then Some s else None)
      ~format:Fun.id
      ~type_name:"string"
  in
  let route =
    get (s "color" / hex_color ())
    |> into (fun color ->
      Response.of_string ~body:(Printf.sprintf "Color: #%s" color) `OK)
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
  let open Router in
  let is_hex_color s =
    String.length s = 6
    && String.for_all
         (fun c ->
            (c >= '0' && c <= '9')
            || (c >= 'a' && c <= 'f')
            || (c >= 'A' && c <= 'F'))
         s
  in
  let hex_color =
    custom
      ~parse:(fun s -> if is_hex_color s then Some s else None)
      ~format:Fun.id
      ~type_name:"string"
  in
  let route =
    get (s "color" / hex_color ())
    |> into (fun _color -> Response.of_string ~body:"ok" `OK)
  in
  let request = make_request "/color/gggggg" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "custom should not match invalid"
  | None -> ()

let test_sprintf_slug () =
  let open Router in
  let pattern = s "posts" / slug in
  let url = sprintf pattern "my-great-post-2024" in
  Alcotest.(check string)
    "url should be /posts/my-great-post-2024"
    "/posts/my-great-post-2024"
    url

let test_sprintf_custom () =
  let open Router in
  let hex_color =
    custom
      ~parse:(fun s -> Some s)
      ~format:(fun s -> String.uppercase_ascii s)
      ~type_name:"string"
  in
  let pattern = s "color" / hex_color () in
  let url = sprintf pattern "ff5733" in
  Alcotest.(check string) "url should be /color/FF5733" "/color/FF5733" url

let test_root_route () =
  let open Router in
  let route =
    get (s "")
    |> unit
    |> into (fun () -> Response.of_string ~body:"home page" `OK)
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
  let open Router in
  let pattern = s "" in
  let url = sprintf pattern in
  Alcotest.(check string) "url should be /" "/" url

let test_root_with_method () =
  let open Router in
  let route =
    post (s "")
    |> unit
    |> into (fun () -> Response.of_string ~body:"posted to root" `Created)
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
  let open Router in
  let route =
    get (s "") |> unit |> into (fun () -> Response.of_string ~body:"root" `OK)
  in
  let request = make_request "/users" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "root should not match /users"
  | None -> ()

let test_scoped_empty_literal () =
  let open Router in
  let route =
    scope
      (s "users")
      [ get (s "")
        |> unit
        |> into (fun () -> Response.of_string ~body:"user list" `OK)
      ]
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
  let open Router in
  let route =
    scope
      (s "api" / s "v1")
      [ get (s "")
        |> unit
        |> into (fun () -> Response.of_string ~body:"api index" `OK)
      ]
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
  let open Router in
  let route =
    scope
      (s "users")
      [ get (s "")
        |> unit
        |> into (fun () -> Response.of_string ~body:"user list" `OK)
      ; get (s "new")
        |> unit
        |> into (fun () -> Response.of_string ~body:"new user" `OK)
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
  let open Router in
  let route =
    get (s "users" / int64)
    |> into (fun id ->
      Response.of_string ~body:(Printf.sprintf "User %Ld" id) `OK)
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
  let open Router in
  let route =
    any (s "api" / s "webhook")
    |> unit
    |> into (fun () -> Response.of_string ~body:"webhook received" `OK)
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
  let open Router in
  let route =
    any (s "resources" / int64)
    |> into (fun id ->
      Response.of_string ~body:(Printf.sprintf "Resource %Ld" id) `OK)
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
  let open Router in
  let route =
    get (s "users" / int64)
    |> request
    |> response_model (fun body -> Response.of_string ~body `OK)
    |> into (fun request id ->
      let headers = Request.headers request in
      let ua =
        Piaf.Headers.get headers "user-agent" |> Option.value ~default:"unknown"
      in
      Printf.sprintf "User %Ld with ua: %s" id ua)
  in
  let request =
    Request.create
      ~headers:(Piaf.Headers.of_list [ "user-agent", "alcotest" ])
      ~scheme:`HTTP
      ~version:Piaf.Versions.HTTP.HTTP_1_1
      ~meth:`GET
      ~body:Piaf.Body.empty
      "/users/100"
  in
  match match' [ route ] request with
  | Some response ->
    let body = Response.body response |> Piaf.Body.to_string |> Result.get_ok in
    Alcotest.(check string)
      "response body should be the user id and ua"
      "User 100 with ua: alcotest"
      body
  | None -> Alcotest.fail "Route should have matched"

let test_route_with_header () =
  let open Router in
  let open Schema in
  let header_schema = str "X-API-Key" in
  let route =
    get (s "api" / s "data")
    |> header header_schema
    |> into (fun api_key ->
      Alcotest.(check string) "api key should be secret123" "secret123" api_key;
      Response.of_string ~body:"authenticated" `OK)
  in
  let request =
    Request.create
      ~headers:(Piaf.Headers.of_list [ "x-api-key", "secret123" ])
      ~scheme:`HTTP
      ~version:Piaf.Versions.HTTP.HTTP_1_1
      ~meth:`GET
      ~body:Piaf.Body.empty
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
  let open Router in
  let open Schema in
  let header_schema = str "Content-Type" in
  let route =
    post (s "api" / s "upload")
    |> header header_schema
    |> into (fun content_type ->
      Response.of_string
        ~body:(Printf.sprintf "Content-Type: %s" content_type)
        `OK)
  in
  let request =
    Request.create
      ~headers:(Piaf.Headers.of_list [ "CONTENT-TYPE", "application/json" ])
      ~scheme:`HTTP
      ~version:Piaf.Versions.HTTP.HTTP_1_1
      ~meth:`POST
      ~body:Piaf.Body.empty
      "/api/upload"
  in
  match match' [ route ] request with
  | Some response ->
    let body = Response.body response |> Piaf.Body.to_string |> Result.get_ok in
    Alcotest.(check string)
      "should match header case-insensitively"
      "Content-Type: application/json"
      body
  | None -> Alcotest.fail "route should match with case-insensitive header"

let test_route_header_missing () =
  let open Router in
  let open Schema in
  let header_schema = str "Authorization" in
  let route =
    get (s "api" / s "protected")
    |> header header_schema
    |> into (fun _auth -> Response.of_string ~body:"ok" `OK)
  in
  let request = make_request "/api/protected" in
  try
    let _ = match' [ route ] request in
    Alcotest.fail "route should raise Validation_failed without required header"
  with
  | Router.Validation_failed _ -> ()

let test_route_multiple_headers () =
  let open Router in
  let open Schema in
  let header_schema =
    let open Schema.Syntax in
    let+ api_key = str "X-API-Key"
    and+ user_id = str "X-User-ID" in
    api_key, user_id
  in
  let route =
    get (s "api" / s "user-data")
    |> header header_schema
    |> into (fun (api_key, user_id) ->
      Response.of_string
        ~body:(Printf.sprintf "Key: %s, User: %s" api_key user_id)
        `OK)
  in
  let request =
    Request.create
      ~headers:
        (Piaf.Headers.of_list
           [ "x-api-key", "secret123"; "x-user-id", "user42" ])
      ~scheme:`HTTP
      ~version:Piaf.Versions.HTTP.HTTP_1_1
      ~meth:`GET
      ~body:Piaf.Body.empty
      "/api/user-data"
  in
  match match' [ route ] request with
  | Some response ->
    let body = Response.body response |> Piaf.Body.to_string |> Result.get_ok in
    Alcotest.(check string)
      "should extract multiple headers"
      "Key: secret123, User: user42"
      body
  | None -> Alcotest.fail "route with multiple headers should have matched"

let test_route_optional_header () =
  let open Router in
  let open Schema in
  let header_schema = option "X-Request-ID" (Field.str ()) in
  let route =
    get (s "api" / s "endpoint")
    |> header header_schema
    |> into (fun request_id ->
      let msg =
        match request_id with
        | Some id -> Printf.sprintf "Request ID: %s" id
        | None -> "No request ID"
      in
      Response.of_string ~body:msg `OK)
  in
  let request_with =
    Request.create
      ~headers:(Piaf.Headers.of_list [ "x-request-id", "req-123" ])
      ~scheme:`HTTP
      ~version:Piaf.Versions.HTTP.HTTP_1_1
      ~meth:`GET
      ~body:Piaf.Body.empty
      "/api/endpoint"
  in
  let request_without = make_request "/api/endpoint" in
  (match match' [ route ] request_with with
  | Some response ->
    let body = Response.body response |> Piaf.Body.to_string |> Result.get_ok in
    Alcotest.(check string)
      "should use header when present"
      "Request ID: req-123"
      body
  | None -> Alcotest.fail "route should match with optional header present");
  match match' [ route ] request_without with
  | Some response ->
    let body = Response.body response |> Piaf.Body.to_string |> Result.get_ok in
    Alcotest.(check string)
      "should work without optional header"
      "No request ID"
      body
  | None -> Alcotest.fail "route should match without optional header"

let test_route_header_validation () =
  let open Router in
  let open Schema in
  let header_schema =
    str ~constraint_:(Constraint.pattern "^Bearer .+$") "Authorization"
  in
  let route =
    get (s "api" / s "secure")
    |> header header_schema
    |> into (fun _auth -> Response.of_string ~body:"authorized" `OK)
  in
  let valid_request =
    Request.create
      ~headers:(Piaf.Headers.of_list [ "authorization", "Bearer token123" ])
      ~scheme:`HTTP
      ~version:Piaf.Versions.HTTP.HTTP_1_1
      ~meth:`GET
      ~body:Piaf.Body.empty
      "/api/secure"
  in
  let invalid_request =
    Request.create
      ~headers:(Piaf.Headers.of_list [ "authorization", "Basic token123" ])
      ~scheme:`HTTP
      ~version:Piaf.Versions.HTTP.HTTP_1_1
      ~meth:`GET
      ~body:Piaf.Body.empty
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
  let open Router in
  let captured = ref "" in
  let route =
    get (s "files" / str)
    |> into (fun filename ->
      captured := filename;
      Response.of_string ~body:filename `OK)
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
  let open Router in
  let captured = ref "" in
  let route =
    get (s "posts" / str)
    |> into (fun slug ->
      captured := slug;
      Response.of_string ~body:slug `OK)
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
  let open Router in
  let captured = ref "" in
  let route =
    get (s "api" / str)
    |> into (fun value ->
      captured := value;
      Response.of_string ~body:value `OK)
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
  let open Router in
  let captured1 = ref "" in
  let captured2 = ref "" in
  let route =
    get (s "users" / str / s "files" / str)
    |> into (fun username filename ->
      captured1 := username;
      captured2 := filename;
      Response.of_string ~body:"ok" `OK)
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
  let open Router in
  let route =
    get (s "api" / s "test route")
    |> unit
    |> into (fun () -> Response.of_string ~body:"ok" `OK)
  in
  let request = make_request "/api/test%20route" in
  match match' [ route ] request with
  | Some _ -> ()
  | None -> Alcotest.fail "encoded literal should match decoded route literal"

let test_url_encoded_international_chars () =
  let open Router in
  let captured = ref "" in
  let route =
    get (s "search" / str)
    |> into (fun query ->
      captured := query;
      Response.of_string ~body:query `OK)
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
  let open Router in
  let captured = ref "" in
  let route =
    get (s "items" / str)
    |> into (fun item ->
      captured := item;
      Response.of_string ~body:item `OK)
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
  let open Router in
  let captured = ref "" in
  let route =
    get (s "data" / str)
    |> into (fun value ->
      captured := value;
      Response.of_string ~body:value `OK)
  in
  let request = make_request "/data/a%2Bb" in
  match match' [ route ] request with
  | Some _ ->
    Alcotest.(check string) "Plus sign should be decoded" "a+b" !captured
  | None -> Alcotest.fail "route should have matched"

let test_url_no_encoding_needed () =
  let open Router in
  let captured = ref "" in
  let route =
    get (s "files" / str)
    |> into (fun filename ->
      captured := filename;
      Response.of_string ~body:filename `OK)
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
  let open Router in
  let open Schema in
  let schema =
    let open Schema.Syntax in
    let+ age = int ~constraint_:(Constraint.int_range 18 100) "age" in
    age
  in
  let route =
    post (s "users")
    |> body Schema.Json schema
    |> into (fun age -> Response.of_string ~body:(string_of_int age) `Created)
    |> recover (fun _request -> function
      | Validation_failed errors ->
        let error_msg =
          errors
          |> List.map (fun (field, msg) -> Printf.sprintf "%s: %s" field msg)
          |> String.concat ", "
        in
        Some (Response.of_string ~body:error_msg `Bad_request)
      | _ -> None)
  in
  let request =
    Request.create
      ~headers:(Piaf.Headers.of_list [ "content-type", "application/json" ])
      ~scheme:`HTTP
      ~version:Piaf.Versions.HTTP.HTTP_1_1
      ~meth:`POST
      ~body:(Piaf.Body.of_string {|{"age": 15}|})
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
  let open Router in
  let open Schema in
  let schema = int "value" in
  let route =
    post (s "data")
    |> body Schema.Json schema
    |> into (fun value -> Response.of_string ~body:(string_of_int value) `OK)
    |> recover (fun _request -> function
      | Bad_request msg ->
        Some
          (Response.of_string
             ~body:(Printf.sprintf "Error: %s" msg)
             `Bad_request)
      | _ -> None)
  in
  let request =
    Request.create
      ~headers:(Piaf.Headers.of_list [ "content-type", "text/plain" ])
      ~scheme:`HTTP
      ~version:Piaf.Versions.HTTP.HTTP_1_1
      ~meth:`POST
      ~body:(Piaf.Body.of_string "invalid")
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
  let open Router in
  let open Schema in
  let schema = int ~constraint_:(Constraint.int_range 1 10) "num" in
  let route =
    post (s "test")
    |> body Schema.Json schema
    |> into (fun num -> Response.of_string ~body:(string_of_int num) `OK)
    |> recover (fun _request -> function
      | Bad_request _ -> Some (Response.of_string ~body:"handled" `Bad_request)
      | _ -> None (* Don't handle Validation_failed *))
  in
  let request =
    Request.create
      ~headers:(Piaf.Headers.of_list [ "content-type", "application/json" ])
      ~scheme:`HTTP
      ~version:Piaf.Versions.HTTP.HTTP_1_1
      ~meth:`POST
      ~body:(Piaf.Body.of_string {|{"num": 99}|})
      "/test"
  in
  try
    let _ = match' [ route ] request in
    Alcotest.fail "should have re-raised Validation_failed"
  with
  | Router.Validation_failed _ -> ()

let test_recover_has_request_access () =
  let open Router in
  let open Schema in
  let schema = int "value" in
  let captured_path = ref "" in
  let route =
    post (s "api" / s "data")
    |> body Schema.Json schema
    |> into (fun value -> Response.of_string ~body:(string_of_int value) `OK)
    |> recover (fun request -> function
      | Validation_failed _ ->
        captured_path := Request.target request;
        Some (Response.of_string ~body:"error" `Bad_request)
      | _ -> None)
  in
  let request =
    Request.create
      ~headers:(Piaf.Headers.of_list [ "content-type", "application/json" ])
      ~scheme:`HTTP
      ~version:Piaf.Versions.HTTP.HTTP_1_1
      ~meth:`POST
      ~body:(Piaf.Body.of_string {|{"value": "invalid"}|})
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
  let open Router in
  let open Schema in
  let schema = int ~constraint_:(Constraint.int_range 1 100) "age" in
  let route =
    scope
      (s "api")
      [ post (s "users")
        |> body Schema.Json schema
        |> into (fun age ->
          Response.of_string ~body:(string_of_int age) `Created)
      ]
    |> recover (fun _request -> function
      | Validation_failed _ ->
        Some (Response.of_string ~body:"scope error handler" `Bad_request)
      | _ -> None)
  in
  let request =
    Request.create
      ~headers:(Piaf.Headers.of_list [ "content-type", "application/json" ])
      ~scheme:`HTTP
      ~version:Piaf.Versions.HTTP.HTTP_1_1
      ~meth:`POST
      ~body:(Piaf.Body.of_string {|{"age": 150}|})
      "/api/users"
  in
  match match' [ route ] request with
  | Some response ->
    let body = Response.body response |> Piaf.Body.to_string |> Result.get_ok in
    Alcotest.(check string)
      "scope-level recover should catch errors"
      "scope error handler"
      body
  | None -> Alcotest.fail "route should have matched"

let test_recover_success_path () =
  let open Router in
  let open Schema in
  let schema = int "value" in
  let route =
    post (s "data")
    |> body Schema.Json schema
    |> into (fun value -> Response.of_string ~body:(string_of_int value) `OK)
    |> recover (fun _request -> function
      | Validation_failed _ ->
        Some (Response.of_string ~body:"error" `Bad_request)
      | _ -> None)
  in
  let request =
    Request.create
      ~headers:(Piaf.Headers.of_list [ "content-type", "application/json" ])
      ~scheme:`HTTP
      ~version:Piaf.Versions.HTTP.HTTP_1_1
      ~meth:`POST
      ~body:(Piaf.Body.of_string {|{"value": 42}|})
      "/data"
  in
  match match' [ route ] request with
  | Some response ->
    let body = Response.body response |> Piaf.Body.to_string |> Result.get_ok in
    Alcotest.(check string)
      "recover should not interfere with successful requests"
      "42"
      body
  | None -> Alcotest.fail "route should have matched"

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
  ]
  |> List.map (fun (name, speed, fn) -> Alcotest.test_case name speed fn)
  |> fun tests -> "Router", tests
