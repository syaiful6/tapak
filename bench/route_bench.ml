open Tapak

let create_mock_request ~meth ~path =
  Request.create
    ~scheme:`HTTP
    ~version:Piaf.Versions.HTTP.HTTP_1_1
    ~meth
    ~body:Piaf.Body.empty
    path

let ok body = Response.of_string ~body `OK

let simple_routes =
  let open Router in
  [ get (s "api" / s "v1" / s "users") |> unit |> into (fun () -> ok "users")
  ; get (s "api" / s "v1" / s "posts") |> unit |> into (fun () -> ok "posts")
  ; get (s "api" / s "v1" / s "comments")
    |> unit
    |> into (fun () -> ok "comments")
  ; get (s "api" / s "v1" / s "tags") |> unit |> into (fun () -> ok "tags")
  ; get (s "api" / s "v1" / s "categories")
    |> unit
    |> into (fun () -> ok "categories")
  ; get (s "api" / s "v2" / s "users") |> unit |> into (fun () -> ok "users v2")
  ; get (s "api" / s "v2" / s "posts") |> unit |> into (fun () -> ok "posts v2")
  ; get (s "api" / s "v2" / s "comments")
    |> unit
    |> into (fun () -> ok "comments v2")
  ; post (s "api" / s "v1" / s "users")
    |> unit
    |> into (fun () -> ok "create user")
  ; put (s "api" / s "v1" / s "users")
    |> unit
    |> into (fun () -> ok "update user")
  ]

let capture_routes =
  let open Router in
  [ get (s "users" / int) |> into (fun id -> ok (string_of_int id))
  ; get (s "users" / int / s "posts") |> into (fun id -> ok (string_of_int id))
  ; get (s "users" / int / s "posts" / int)
    |> into (fun user_id post_id ->
      ok (string_of_int user_id ^ "/" ^ string_of_int post_id))
  ; get (s "products" / str) |> into (fun slug -> ok slug)
  ; get (s "categories" / str / s "items") |> into (fun cat -> ok cat)
  ]

let mixed_routes =
  let open Router in
  [ get (s "api" / s "users") |> unit |> into (fun () -> ok "all users")
  ; get (s "api" / s "users" / int) |> into (fun id -> ok (string_of_int id))
  ; get (s "api" / s "users" / int / s "profile")
    |> into (fun id -> ok (string_of_int id))
  ; get (s "api" / s "posts") |> unit |> into (fun () -> ok "all posts")
  ; get (s "api" / s "posts" / int) |> into (fun id -> ok (string_of_int id))
  ; get (s "api" / s "posts" / int / s "comments")
    |> into (fun id -> ok (string_of_int id))
  ; post (s "api" / s "users") |> unit |> into (fun () -> ok "create user")
  ; put (s "api" / s "users" / int) |> into (fun id -> ok (string_of_int id))
  ; delete (s "api" / s "users" / int) |> into (fun id -> ok (string_of_int id))
  ; get (s "api" / s "search") |> unit |> into (fun () -> ok "search")
  ]

let large_routes =
  let open Router in
  [ (* Public API *)
    get (s "api" / s "v1" / s "health") |> unit |> into (fun () -> ok "healthy")
  ; get (s "api" / s "v1" / s "status") |> unit |> into (fun () -> ok "ok")
  ; (* User management *)
    get (s "api" / s "v1" / s "users") |> unit |> into (fun () -> ok "users")
  ; get (s "api" / s "v1" / s "users" / int)
    |> into (fun id -> ok (string_of_int id))
  ; post (s "api" / s "v1" / s "users") |> unit |> into (fun () -> ok "create")
  ; put (s "api" / s "v1" / s "users" / int)
    |> into (fun id -> ok (string_of_int id))
  ; delete (s "api" / s "v1" / s "users" / int)
    |> into (fun id -> ok (string_of_int id))
  ; get (s "api" / s "v1" / s "users" / int / s "profile")
    |> into (fun id -> ok (string_of_int id))
  ; get (s "api" / s "v1" / s "users" / int / s "settings")
    |> into (fun id -> ok (string_of_int id))
  ; (* Posts *)
    get (s "api" / s "v1" / s "posts") |> unit |> into (fun () -> ok "posts")
  ; get (s "api" / s "v1" / s "posts" / int)
    |> into (fun id -> ok (string_of_int id))
  ; post (s "api" / s "v1" / s "posts") |> unit |> into (fun () -> ok "create")
  ; put (s "api" / s "v1" / s "posts" / int)
    |> into (fun id -> ok (string_of_int id))
  ; delete (s "api" / s "v1" / s "posts" / int)
    |> into (fun id -> ok (string_of_int id))
  ; get (s "api" / s "v1" / s "posts" / int / s "comments")
    |> into (fun id -> ok (string_of_int id))
  ; post (s "api" / s "v1" / s "posts" / int / s "comments")
    |> into (fun id -> ok (string_of_int id))
  ; (* Comments *)
    get (s "api" / s "v1" / s "comments" / int)
    |> into (fun id -> ok (string_of_int id))
  ; put (s "api" / s "v1" / s "comments" / int)
    |> into (fun id -> ok (string_of_int id))
  ; delete (s "api" / s "v1" / s "comments" / int)
    |> into (fun id -> ok (string_of_int id))
  ; (* Tags *)
    get (s "api" / s "v1" / s "tags") |> unit |> into (fun () -> ok "tags")
  ; get (s "api" / s "v1" / s "tags" / str) |> into (fun tag -> ok tag)
  ; post (s "api" / s "v1" / s "tags") |> unit |> into (fun () -> ok "create")
  ; (* Categories *)
    get (s "api" / s "v1" / s "categories")
    |> unit
    |> into (fun () -> ok "categories")
  ; get (s "api" / s "v1" / s "categories" / str) |> into (fun cat -> ok cat)
  ; get (s "api" / s "v1" / s "categories" / str / s "posts")
    |> into (fun cat -> ok cat)
  ; (* Admin *)
    get (s "admin" / s "dashboard") |> unit |> into (fun () -> ok "dashboard")
  ; get (s "admin" / s "users") |> unit |> into (fun () -> ok "admin users")
  ; get (s "admin" / s "settings") |> unit |> into (fun () -> ok "settings")
  ; (* Static-like paths *)
    get (s "assets" / s "css" / s "main.css")
    |> unit
    |> into (fun () -> ok "css")
  ; get (s "assets" / s "js" / s "main.js") |> unit |> into (fun () -> ok "js")
  ; get (s "assets" / s "images" / s "logo.png")
    |> unit
    |> into (fun () -> ok "image")
  ]

let bench_router (router, req) =
  let _resp = router req in
  ()

let bench_methods (router, get_req, post_req, put_req, delete_req) =
  let _ = router get_req in
  let _ = router post_req in
  let _ = router put_req in
  let _ = router delete_req in
  ()

let () =
  let open Benchmark in
  Printf.printf "\n========================================\n";
  Printf.printf "Router Performance Benchmarks\n";
  Printf.printf "========================================\n\n";

  Printf.printf "--- Simple Routes (10 routes, literals only) ---\n";
  let router = Router.router simple_routes in
  let req = create_mock_request ~meth:`GET ~path:"/api/v1/users" in
  let results =
    throughputN ~repeat:3 2 [ "Router", bench_router, (router, req) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf "--- Routes with Captures (5 routes) ---\n";
  let router = Router.router capture_routes in
  let req = create_mock_request ~meth:`GET ~path:"/users/123/posts/456" in
  let results =
    throughputN ~repeat:3 2 [ "Router", bench_router, (router, req) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf "--- Mixed Routes (10 routes, literals + captures) ---\n";
  let router = Router.router mixed_routes in
  let req = create_mock_request ~meth:`GET ~path:"/api/users/123/profile" in
  let results =
    throughputN ~repeat:3 2 [ "Router", bench_router, (router, req) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf "--- Large Route Set (30 routes) - Early Match ---\n";
  let router = Router.router large_routes in
  let req = create_mock_request ~meth:`GET ~path:"/api/v1/users" in
  let results =
    throughputN ~repeat:3 2 [ "Router", bench_router, (router, req) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf "--- Large Route Set - Middle Match ---\n";
  let router = Router.router large_routes in
  let req = create_mock_request ~meth:`GET ~path:"/api/v1/posts/456/comments" in
  let results =
    throughputN ~repeat:3 2 [ "Router", bench_router, (router, req) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf "--- Large Route Set - Late Match (Worst Case) ---\n";
  let router = Router.router large_routes in
  let req = create_mock_request ~meth:`GET ~path:"/assets/images/logo.png" in
  let results =
    throughputN ~repeat:3 2 [ "Router", bench_router, (router, req) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf "--- Multiple HTTP Methods (4 requests) ---\n";
  let router = Router.router mixed_routes in
  let get_req = create_mock_request ~meth:`GET ~path:"/api/users/123" in
  let post_req = create_mock_request ~meth:`POST ~path:"/api/users" in
  let put_req = create_mock_request ~meth:`PUT ~path:"/api/users/123" in
  let delete_req = create_mock_request ~meth:`DELETE ~path:"/api/users/123" in
  let results =
    throughputN
      ~repeat:3
      2
      [ "Router", bench_methods, (router, get_req, post_req, put_req, delete_req)
      ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf "--- Deep Path (5 segments) ---\n";
  let router = Router.router large_routes in
  let req = create_mock_request ~meth:`GET ~path:"/api/v1/users/123/profile" in
  let results =
    throughputN ~repeat:3 2 [ "Router", bench_router, (router, req) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf "========================================\n";
  Printf.printf "Benchmark Complete\n";
  Printf.printf "========================================\n"
