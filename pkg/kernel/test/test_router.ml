open Tapak_kernel

let params_equal =
  List.equal (fun a b ->
    String.equal (fst a) (fst b) && String.equal (snd a) (snd b))

let params =
  Alcotest.testable Router.pp_matches (fun a b ->
    params_equal a.Router.params b.Router.params)

let route_matches target route =
  let path = Router.Matcher.of_path target `GET in
  let matcher = Router.Matcher.of_router route in
  match Router.Matcher.try_match matcher path with
  | None -> None
  | Some (_, params, splat) -> Some { Router.params; splat = List.rev splat }

let simple_app _ = Response.create `OK

let test_simple_route () =
  let route = Router.get "/test/:id" simple_app in
  Alcotest.(check (option params))
    "No match on different path"
    None
    (route_matches "/different/123" route);
  Alcotest.(check (option params))
    "Simple route match"
    (Some { Router.params = [ "id", "123" ]; splat = [] })
    (route_matches "/test/123" route)

let test_with_scoped_route () =
  let route = Router.scope "/" [ Router.get "/test/:id" simple_app ] in
  Alcotest.(check (option params))
    "No match on different path"
    None
    (route_matches "/different/123" route);
  Alcotest.(check (option params))
    "Simple route match"
    (Some { Router.params = [ "id", "123" ]; splat = [] })
    (route_matches "/test/123" route)

let test_simple_route_2 () =
  let route = Router.get "/test/:format/:name" simple_app in
  Alcotest.(check (option params))
    "No match on different path"
    None
    (route_matches "/test/bar" route)

(* Middleware tests *)
let execution_order = ref []
let reset_execution_order () = execution_order := []
let get_execution_order () = List.rev !execution_order

let make_test_request path =
  Request.create
    ~scheme:`HTTP
    ~version:Piaf.Versions.HTTP.HTTP_1_1
    ~meth:`GET
    ~body:Piaf.Body.empty
    path

let test_middleware name =
  let module M = struct
    type args = unit
    type state = unit

    let init () = ()

    let call () next request =
      execution_order := name :: !execution_order;
      next request
  end
  in
  Middleware.use ~name (module M) ()

let test_scoped_middleware_execution () =
  reset_execution_order ();
  let middleware1 = test_middleware "middleware1" in
  let middleware2 = test_middleware "middleware2" in
  let handler _request =
    execution_order := "handler" :: !execution_order;
    Response.create `OK
  in
  let route =
    Router.scope
      ~middlewares:[ middleware1; middleware2 ]
      "/"
      [ Router.get "/test" handler ]
  in
  let request = make_test_request "/test" in
  let _ = Router.make route request in
  Alcotest.(check (list string))
    "Middleware executed in order before handler"
    [ "middleware1"; "middleware2"; "handler" ]
    (get_execution_order ())

let test_nested_scoped_middleware () =
  reset_execution_order ();
  let parent_mw = test_middleware "parent" in
  let child_mw = test_middleware "child" in
  let handler _request =
    execution_order := "handler" :: !execution_order;
    Response.create `OK
  in
  let route =
    Router.scope
      ~middlewares:[ parent_mw ]
      "/"
      [ Router.scope
          ~middlewares:[ child_mw ]
          "/api"
          [ Router.get "/test" handler ]
      ]
  in
  let request = make_test_request "/api/test" in
  let _ = Router.make route request in
  Alcotest.(check (list string))
    "Parent middleware executes before child middleware"
    [ "parent"; "child"; "handler" ]
    (get_execution_order ())

let test_middleware_isolation () =
  reset_execution_order ();
  let mw1 = test_middleware "scope1_mw" in
  let mw2 = test_middleware "scope2_mw" in
  let handler1 _request =
    execution_order := "handler1" :: !execution_order;
    Response.create `OK
  in
  let handler2 _request =
    execution_order := "handler2" :: !execution_order;
    Response.create `OK
  in
  let route =
    Router.scope
      "/"
      [ Router.scope
          ~middlewares:[ mw1 ]
          "/scope1"
          [ Router.get "/test" handler1 ]
      ; Router.scope
          ~middlewares:[ mw2 ]
          "/scope2"
          [ Router.get "/test" handler2 ]
      ]
  in
  reset_execution_order ();
  let request1 = make_test_request "/scope1/test" in
  let _ = Router.make route request1 in
  Alcotest.(check (list string))
    "Scope1 middleware only executes for scope1 routes"
    [ "scope1_mw"; "handler1" ]
    (get_execution_order ());
  reset_execution_order ();
  let request2 = make_test_request "/scope2/test" in
  let _ = Router.make route request2 in
  Alcotest.(check (list string))
    "Scope2 middleware only executes for scope2 routes"
    [ "scope2_mw"; "handler2" ]
    (get_execution_order ())

let test_multiple_middlewares_in_scope () =
  reset_execution_order ();
  let mw1 = test_middleware "mw1" in
  let mw2 = test_middleware "mw2" in
  let mw3 = test_middleware "mw3" in
  let handler _request =
    execution_order := "handler" :: !execution_order;
    Response.create `OK
  in
  let route =
    Router.scope
      ~middlewares:[ mw1; mw2; mw3 ]
      "/"
      [ Router.get "/test" handler ]
  in
  let request = make_test_request "/test" in
  let _ = Router.make route request in
  Alcotest.(check (list string))
    "All middlewares execute in order"
    [ "mw1"; "mw2"; "mw3"; "handler" ]
    (get_execution_order ())

let test_deeply_nested_scopes () =
  reset_execution_order ();
  let mw_level1 = test_middleware "level1" in
  let mw_level2 = test_middleware "level2" in
  let mw_level3 = test_middleware "level3" in
  let handler _request =
    execution_order := "handler" :: !execution_order;
    Response.create `OK
  in
  let route =
    Router.scope
      ~middlewares:[ mw_level1 ]
      "/"
      [ Router.scope
          ~middlewares:[ mw_level2 ]
          "/api"
          [ Router.scope
              ~middlewares:[ mw_level3 ]
              "/v1"
              [ Router.get "/test" handler ]
          ]
      ]
  in
  let request = make_test_request "/api/v1/test" in
  let _ = Router.make route request in
  Alcotest.(check (list string))
    "Middlewares execute from outermost to innermost scope"
    [ "level1"; "level2"; "level3"; "handler" ]
    (get_execution_order ())

(* Splat tests *)
let test_single_splat () =
  let route = Router.get "/users/*" simple_app in
  Alcotest.(check (option params))
    "Single splat matches /users/123"
    (Some { Router.params = []; splat = [ "123" ] })
    (route_matches "/users/123" route);
  Alcotest.(check (option params))
    "Single splat doesn't match /users/123/profile"
    None
    (route_matches "/users/123/profile" route)

let test_full_splat () =
  let route = Router.get "/admin/**" simple_app in
  Alcotest.(check (option params))
    "Full splat matches /admin/settings"
    (Some { Router.params = []; splat = [ "settings" ] })
    (route_matches "/admin/settings" route);
  Alcotest.(check (option params))
    "Full splat matches /admin/posts/1"
    (Some { Router.params = []; splat = [ "posts"; "1" ] })
    (route_matches "/admin/posts/1" route);
  Alcotest.(check (option params))
    "Full splat matches /admin/posts/1/edit"
    (Some { Router.params = []; splat = [ "posts"; "1"; "edit" ] })
    (route_matches "/admin/posts/1/edit" route)

let test_mixed_splat_and_params () =
  let route = Router.get "/admin/*/:object_id" simple_app in
  Alcotest.(check (option params))
    "Mixed splat+param matches /admin/12/posts"
    (Some { Router.params = [ "object_id", "posts" ]; splat = [ "12" ] })
    (route_matches "/admin/12/posts" route)

let tests =
  List.map
    (fun (name, cases) -> Format.asprintf "Router: %s" name, cases)
    [ ( "test match 1"
      , [ Alcotest.test_case "Simple route match" `Quick test_simple_route ] )
    ; ( "test match 2"
      , [ Alcotest.test_case "Scoped route match" `Quick test_with_scoped_route
        ] )
    ; ( "test match 3"
      , [ Alcotest.test_case "No match" `Quick test_simple_route_2 ] )
    ; ( "middleware tests"
      , [ Alcotest.test_case
            "Scoped middleware execution"
            `Quick
            test_scoped_middleware_execution
        ; Alcotest.test_case
            "Nested scoped middleware"
            `Quick
            test_nested_scoped_middleware
        ; Alcotest.test_case
            "Middleware isolation between scopes"
            `Quick
            test_middleware_isolation
        ; Alcotest.test_case
            "Multiple middlewares in scope"
            `Quick
            test_multiple_middlewares_in_scope
        ; Alcotest.test_case
            "Deeply nested scopes"
            `Quick
            test_deeply_nested_scopes
        ] )
    ; ( "splat tests"
      , [ Alcotest.test_case "Single splat" `Quick test_single_splat
        ; Alcotest.test_case "Full splat" `Quick test_full_splat
        ; Alcotest.test_case
            "Mixed splat and params"
            `Quick
            test_mixed_splat_and_params
        ] )
    ]
