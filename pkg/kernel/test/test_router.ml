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
    get (s "users") @-> fun request ->
    Alcotest.(check string)
      "method should be GET"
      "GET"
      (Piaf.Method.to_string (Request.meth request));
    Response.of_string ~body:"users list" `OK
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
    get (s "users" / int64) @-> fun id _request ->
    Alcotest.(check int64) "id should be 42" 42L id;
    Response.of_string ~body:(Printf.sprintf "User %Ld" id) `OK
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
    get (s "users" / int64 / s "posts" / str) @-> fun user_id slug _request ->
    Alcotest.(check int64) "user_id should be 42" 42L user_id;
    Alcotest.(check string) "slug should be 'hello'" "hello" slug;
    Response.of_string
      ~body:(Printf.sprintf "User %Ld post %s" user_id slug)
      `OK
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
    get (s "users" / int64) @-> fun _id _request ->
    Response.of_string ~body:"user" `OK
  in
  let request = make_request "/posts/42" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "route should not have matched"
  | None -> ()

let test_post_method () =
  let open Router in
  let route =
    post (s "users") @-> fun _request ->
    Response.of_string ~body:"user created" `Created
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
    post (s "users") @-> fun _request ->
    Response.of_string ~body:"user created" `Created
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
    get (s "page" / int) @-> fun page_num _request ->
    Alcotest.(check int) "page should be 5" 5 page_num;
    Response.of_string ~body:(Printf.sprintf "Page %d" page_num) `OK
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
    get (s "page" / int) @-> fun _page _request ->
    Response.of_string ~body:"page" `OK
  in
  let request = make_request "/page/not-a-number" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "route should not match invalid int"
  | None -> ()

let test_bool_param () =
  let open Router in
  let route =
    get (s "published" / bool) @-> fun is_published _request ->
    Alcotest.(check bool) "should be true" true is_published;
    Response.of_string ~body:(string_of_bool is_published) `OK
  in
  let request = make_request "/published/true" in
  match match' [ route ] request with
  | Some _ -> ()
  | None -> Alcotest.fail "route should have matched"

let test_scope () =
  let open Router in
  let routes =
    scope
      (s "api" / s "v1")
      [ (get (s "users") @-> fun _req -> Response.of_string ~body:"users" `OK) ]
  in
  let request = make_request "/api/v1/users" in
  match match' routes request with
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
    type args = unit
    type state = unit

    let init () = ()

    let call () next req =
      let resp = next req in
      Response.with_
        ~headers:(Piaf.Headers.add (Response.headers resp) "X-Test" "true")
        resp
  end
  in
  let test_middleware = Middleware.use ~name:"test" (module M) () in
  let routes =
    scope
      ~middlewares:[ test_middleware ]
      (s "api")
      [ (get (s "test") @-> fun _req -> Response.of_string ~body:"ok" `OK) ]
  in
  let request = make_request "/api/test" in
  match match' routes request with
  | Some response ->
    (match Piaf.Headers.get (Response.headers response) "x-test" with
    | Some value -> Alcotest.(check string) "middleware should run" "true" value
    | None -> Alcotest.fail "middleware header not found")
  | None -> Alcotest.fail "scoped route should have matched"

let test_splat () =
  let open Router in
  let route =
    get (s "files" / splat) @-> fun segments _request ->
    let path = String.concat "/" segments in
    Response.of_string ~body:(Printf.sprintf "Files: %s" path) `OK
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
    get (s "static" / splat) @-> fun segments _request ->
    captured_segments := segments;
    Response.of_string ~body:"ok" `OK
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
    get (s "spa" / splat) @-> fun segments _request ->
    captured_segments := segments;
    Response.of_string ~body:"ok" `OK
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
    get (s "api" / s "v1" / splat) @-> fun segments _request ->
    let path = String.concat "/" segments in
    Response.of_string ~body:(Printf.sprintf "API: %s" path) `OK
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
    get (s "posts" / slug) @-> fun slug_str _request ->
    Response.of_string ~body:(Printf.sprintf "Post: %s" slug_str) `OK
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
    get (s "posts" / slug) @-> fun _slug _request ->
    Response.of_string ~body:"ok" `OK
  in
  let request = make_request "/posts/My_Post" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "slug should not match invalid format"
  | None -> ()

let test_slug_valid () =
  let open Router in
  let captured_slug = ref "" in
  let route =
    get (s "posts" / slug) @-> fun slug_str _request ->
    captured_slug := slug_str;
    Response.of_string ~body:"ok" `OK
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
  in
  let route =
    get (s "color" / hex_color) @-> fun color _request ->
    Response.of_string ~body:(Printf.sprintf "Color: #%s" color) `OK
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
  in
  let route =
    get (s "color" / hex_color) @-> fun _color _request ->
    Response.of_string ~body:"ok" `OK
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
    custom ~parse:(fun s -> Some s) ~format:(fun s -> String.uppercase_ascii s)
  in
  let pattern = s "color" / hex_color in
  let url = sprintf pattern "ff5733" in
  Alcotest.(check string) "url should be /color/FF5733" "/color/FF5733" url

let test_root_route () =
  let open Router in
  let route =
    get (s "") @-> fun _request -> Response.of_string ~body:"home page" `OK
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
    post (s "") @-> fun _request ->
    Response.of_string ~body:"posted to root" `Created
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
    get (s "") @-> fun _request -> Response.of_string ~body:"root" `OK
  in
  let request = make_request "/users" in
  match match' [ route ] request with
  | Some _ -> Alcotest.fail "root should not match /users"
  | None -> ()

let test_scoped_empty_literal () =
  let open Router in
  let routes =
    scope
      (s "users")
      [ (get (s "") @-> fun _req -> Response.of_string ~body:"user list" `OK) ]
  in
  let request = make_request "/users" in
  match match' routes request with
  | Some response ->
    Alcotest.(check bool)
      "scoped empty literal should match /users"
      true
      (Response.status response = `OK)
  | None -> Alcotest.fail "scoped empty literal should have matched /users"

let test_scoped_empty_literal_no_trailing_slash () =
  let open Router in
  let routes =
    scope
      (s "api" / s "v1")
      [ (get (s "") @-> fun _req -> Response.of_string ~body:"api index" `OK) ]
  in
  let request = make_request "/api/v1" in
  match match' routes request with
  | Some response ->
    Alcotest.(check bool)
      "nested scoped empty literal should match /api/v1"
      true
      (Response.status response = `OK)
  | None ->
    Alcotest.fail "nested scoped empty literal should have matched /api/v1"

let test_scoped_with_trailing_route () =
  let open Router in
  let routes =
    scope
      (s "users")
      [ (get (s "") @-> fun _req -> Response.of_string ~body:"user list" `OK)
      ; (get (s "new") @-> fun _req -> Response.of_string ~body:"new user" `OK)
      ]
  in
  let request1 = make_request "/users" in
  let request2 = make_request "/users/new" in
  (match match' routes request1 with
  | Some _ -> ()
  | None -> Alcotest.fail "/users should match");
  match match' routes request2 with
  | Some _ -> ()
  | None -> Alcotest.fail "/users/new should match"

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
  ]
  |> List.map (fun (name, speed, fn) -> Alcotest.test_case name speed fn)
  |> fun tests -> "Router", tests
