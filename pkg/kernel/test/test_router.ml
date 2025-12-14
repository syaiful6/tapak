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
    |> into (fun id _request ->
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
    |> into (fun user_id slug _request ->
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
    |> into (fun _id _request -> Response.of_string ~body:"user" `OK)
  in
  let request = make_request "/posts/42" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "route should not have matched"
  | None -> ()

let test_post_method () =
  let open Router in
  let route =
    post (s "users")
    |> into (fun _request -> Response.of_string ~body:"user created" `Created)
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
    |> into (fun _request -> Response.of_string ~body:"user created" `Created)
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
    |> into (fun page_num _request ->
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
    |> into (fun _page _request -> Response.of_string ~body:"page" `OK)
  in
  let request = make_request "/page/not-a-number" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "route should not match invalid int"
  | None -> ()

let test_bool_param () =
  let open Router in
  let route =
    get (s "published" / bool)
    |> into (fun is_published _request ->
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
        |> into (fun _req -> Response.of_string ~body:"users" `OK)
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
      [ get (s "test") |> into (fun _req -> Response.of_string ~body:"ok" `OK) ]
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
    |> into (fun segments _request ->
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
    |> into (fun segments _request ->
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
    |> into (fun segments _request ->
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
    |> into (fun segments _request ->
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
    |> into (fun slug_str _request ->
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
    |> into (fun _slug _request -> Response.of_string ~body:"ok" `OK)
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
    |> into (fun slug_str _request ->
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
    |> into (fun color _request ->
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
    |> into (fun _color _request -> Response.of_string ~body:"ok" `OK)
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
    |> into (fun _request -> Response.of_string ~body:"home page" `OK)
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
    |> into (fun _request -> Response.of_string ~body:"posted to root" `Created)
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
    get (s "") |> into (fun _request -> Response.of_string ~body:"root" `OK)
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
        |> into (fun _req -> Response.of_string ~body:"user list" `OK)
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
        |> into (fun _req -> Response.of_string ~body:"api index" `OK)
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
        |> into (fun _req -> Response.of_string ~body:"user list" `OK)
      ; get (s "new")
        |> into (fun _req -> Response.of_string ~body:"new user" `OK)
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
    |> into (fun id _request ->
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
    |> into (fun _request -> Response.of_string ~body:"webhook received" `OK)
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
    |> into (fun id _request ->
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
    |> response_model (fun body -> Response.of_string ~body `OK)
    |> into (fun id request ->
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
    |> into (fun api_key _request ->
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
    |> into (fun content_type _request ->
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
    |> into (fun _auth _request -> Response.of_string ~body:"ok" `OK)
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
    |> into (fun (api_key, user_id) _request ->
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
    |> into (fun request_id _request ->
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
    |> into (fun _auth _request -> Response.of_string ~body:"authorized" `OK)
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
  ]
  |> List.map (fun (name, speed, fn) -> Alcotest.test_case name speed fn)
  |> fun tests -> "Router", tests
