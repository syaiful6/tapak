let headers = Alcotest.of_pp Tapak.Headers.pp_hum
let status = Alcotest.of_pp Piaf.Status.pp_hum

let with_app ?middlewares ?handler f =
  let hd = Option.value handler ~default:(fun _ -> Tapak.html "") in
  let middlewares = Option.value middlewares ~default:[] in
  let service = Tapak.Filter.apply_all middlewares hd in
  f service

let make_request
      ?(meth = `GET)
      ?(origin = None)
      ?(request_method = None)
      ?(request_headers = None)
      ()
  =
  let hdrs =
    List.filter_map
      Fun.id
      [ Option.map (fun o -> "Origin", o) origin
      ; Option.map (fun m -> "Access-Control-Request-Method", m) request_method
      ; Option.map
          (fun h -> "Access-Control-Request-Headers", h)
          request_headers
      ]
  in
  Tapak.Request.create
    ~scheme:`HTTP
    ~version:Piaf.Versions.HTTP.HTTP_1_1
    ~meth
    ~headers:(Tapak.Headers.of_list hdrs)
    ~body:Tapak.Body.empty
    "/"

let test_no_origin_header_passes_through () =
  (* Per spec: requests without Origin header are not CORS requests *)
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args ())
        ]
      (fun service ->
         let req = make_request () in
         service req)
  in
  Alcotest.(check (option string))
    "no CORS headers added"
    None
    (Tapak.Response.header "Access-Control-Allow-Origin" res)

let test_simple_request_gets_cors_headers () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args ())
        ]
      (fun service ->
         let req = make_request ~origin:(Some "http://example.com") () in
         service req)
  in
  Alcotest.(check (option string))
    "allows all origins with wildcard"
    (Some "*")
    (Tapak.Response.header "Access-Control-Allow-Origin" res)

let test_get_request_with_permissive_option () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.permissive ())
        ]
      (fun service ->
         let req = make_request ~origin:(Some "http://example.com") () in
         service req)
  in
  Alcotest.(check headers)
    "same headers"
    (Tapak.Headers.of_list
       [ "Content-Type", "text/html; charset=utf-8"
       ; "Access-Control-Allow-Origin", "*"
       ])
    (Tapak.Response.headers res)

let test_permissive_preflight () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.permissive ())
        ]
      (fun service ->
         let req =
           make_request
             ~meth:`OPTIONS
             ~origin:(Some "http://example.com")
             ~request_method:(Some "POST")
             ~request_headers:(Some "X-Custom-Header, Content-Type")
             ()
         in
         service req)
  in
  Alcotest.(check status)
    "204 No Content"
    `No_content
    (Tapak.Response.status res);
  Alcotest.(check (option string))
    "allows all origins"
    (Some "*")
    (Tapak.Response.header "Access-Control-Allow-Origin" res);
  (* When headers is ["*"], it should reflect the requested headers *)
  Alcotest.(check (option string))
    "reflects requested headers"
    (Some "X-Custom-Header, Content-Type")
    (Tapak.Response.header "Access-Control-Allow-Headers" res);
  Alcotest.(check (option string))
    "includes max-age"
    (Some "86400")
    (Tapak.Response.header "Access-Control-Max-Age" res)

let test_allow_list_accepts_listed_origin () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args
               ~origins:
                 (`Allow_list
                     [ "http://allowed.com"; "http://also-allowed.com" ])
               ())
        ]
      (fun service ->
         let req = make_request ~origin:(Some "http://allowed.com") () in
         service req)
  in
  Alcotest.(check (option string))
    "echoes the specific origin"
    (Some "http://allowed.com")
    (Tapak.Response.header "Access-Control-Allow-Origin" res);
  (* Must include Vary: Origin when origin is dynamic *)
  Alcotest.(check (option string))
    "includes Vary: Origin"
    (Some "Origin")
    (Tapak.Response.header "Vary" res)

let test_allow_list_rejects_unlisted_origin () =
  let handler_called = ref false in
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args
               ~origins:(`Allow_list [ "http://allowed.com" ])
               ())
        ]
      ~handler:(fun _ ->
        handler_called := true;
        Tapak.html "OK")
      (fun service ->
         let req = make_request ~origin:(Some "http://evil.com") () in
         service req)
  in
  (* For simple requests, disallowed origins still pass through but without CORS
     headers *)
  Alcotest.(check bool) "handler is called" true !handler_called;
  Alcotest.(check (option string))
    "no CORS headers for disallowed origin"
    None
    (Tapak.Response.header "Access-Control-Allow-Origin" res)

let test_allow_list_rejects_preflight_for_unlisted_origin () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args
               ~origins:(`Allow_list [ "http://allowed.com" ])
               ())
        ]
      (fun service ->
         let req =
           make_request
             ~meth:`OPTIONS
             ~origin:(Some "http://evil.com")
             ~request_method:(Some "POST")
             ()
         in
         service req)
  in
  Alcotest.(check status) "403 Forbidden" `Forbidden (Tapak.Response.status res)

let test_allow_predicate () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args
               ~origins:
                 (`Allow_predicate (fun origin -> String.length origin > 10))
               ())
        ]
      (fun service ->
         let req = make_request ~origin:(Some "http://long-origin.com") () in
         service req)
  in
  Alcotest.(check (option string))
    "echoes origin when predicate passes"
    (Some "http://long-origin.com")
    (Tapak.Response.header "Access-Control-Allow-Origin" res)

let test_allow_predicate_rejects () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args
               ~origins:
                 (`Allow_predicate (fun origin -> String.length origin > 100))
               ())
        ]
      (fun service ->
         let req = make_request ~origin:(Some "http://short.com") () in
         service req)
  in
  Alcotest.(check (option string))
    "no CORS headers when predicate fails"
    None
    (Tapak.Response.header "Access-Control-Allow-Origin" res)

let test_credentials_with_allow_all_echoes_origin () =
  (* Per spec: credentials:true + Allow_all MUST echo the origin, not "*" *)
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args
               ~origins:`Allow_all
               ~credentials:true
               ())
        ]
      (fun service ->
         let req = make_request ~origin:(Some "http://example.com") () in
         service req)
  in
  Alcotest.(check (option string))
    "echoes origin instead of wildcard"
    (Some "http://example.com")
    (Tapak.Response.header "Access-Control-Allow-Origin" res);
  Alcotest.(check (option string))
    "includes credentials header"
    (Some "true")
    (Tapak.Response.header "Access-Control-Allow-Credentials" res);
  (* Must vary by origin when credentials enabled with Allow_all *)
  Alcotest.(check (option string))
    "includes Vary: Origin"
    (Some "Origin")
    (Tapak.Response.header "Vary" res)

let test_credentials_with_allow_list () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args
               ~origins:(`Allow_list [ "http://trusted.com" ])
               ~credentials:true
               ())
        ]
      (fun service ->
         let req = make_request ~origin:(Some "http://trusted.com") () in
         service req)
  in
  Alcotest.(check (option string))
    "echoes origin"
    (Some "http://trusted.com")
    (Tapak.Response.header "Access-Control-Allow-Origin" res);
  Alcotest.(check (option string))
    "includes credentials header"
    (Some "true")
    (Tapak.Response.header "Access-Control-Allow-Credentials" res)

let test_no_credentials_header_when_disabled () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args ~credentials:false ())
        ]
      (fun service ->
         let req = make_request ~origin:(Some "http://example.com") () in
         service req)
  in
  Alcotest.(check (option string))
    "no credentials header"
    None
    (Tapak.Response.header "Access-Control-Allow-Credentials" res)

let test_no_credentials_header_with_wildcard_origin () =
  (* Per spec: credentials header must not be sent when origin is "*" *)
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (* credentials:false with Allow_all returns "*" for origin *)
            (Tapak.Middleware.CORS.args
               ~origins:`Allow_all
               ~credentials:false
               ())
        ]
      (fun service ->
         let req = make_request ~origin:(Some "http://example.com") () in
         service req)
  in
  Alcotest.(check (option string))
    "origin is wildcard"
    (Some "*")
    (Tapak.Response.header "Access-Control-Allow-Origin" res);
  Alcotest.(check (option string))
    "no credentials header with wildcard"
    None
    (Tapak.Response.header "Access-Control-Allow-Credentials" res)

let test_preflight_detection_requires_request_method_header () =
  (* OPTIONS without Access-Control-Request-Method is NOT a preflight *)
  let handler_called = ref false in
  let _res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args ())
        ]
      ~handler:(fun _ ->
        handler_called := true;
        Tapak.html "OK")
      (fun service ->
         let req =
           make_request ~meth:`OPTIONS ~origin:(Some "http://example.com") ()
         in
         service req)
  in
  Alcotest.(check bool)
    "handler is called for non-preflight OPTIONS"
    true
    !handler_called

let test_preflight_returns_204 () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args ())
        ]
      (fun service ->
         let req =
           make_request
             ~meth:`OPTIONS
             ~origin:(Some "http://example.com")
             ~request_method:(Some "POST")
             ()
         in
         service req)
  in
  Alcotest.(check status)
    "204 No Content"
    `No_content
    (Tapak.Response.status res)

let test_preflight_includes_allowed_methods () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args ~methods:[ `GET; `POST; `DELETE ] ())
        ]
      (fun service ->
         let req =
           make_request
             ~meth:`OPTIONS
             ~origin:(Some "http://example.com")
             ~request_method:(Some "POST")
             ()
         in
         service req)
  in
  Alcotest.(check (option string))
    "includes allowed methods"
    (Some "GET, POST, DELETE")
    (Tapak.Response.header "Access-Control-Allow-Methods" res)

let test_preflight_includes_allowed_headers () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args
               ~headers:[ "Content-Type"; "Authorization"; "X-Custom" ]
               ())
        ]
      (fun service ->
         let req =
           make_request
             ~meth:`OPTIONS
             ~origin:(Some "http://example.com")
             ~request_method:(Some "POST")
             ()
         in
         service req)
  in
  Alcotest.(check (option string))
    "includes allowed headers"
    (Some "Content-Type, Authorization, X-Custom")
    (Tapak.Response.header "Access-Control-Allow-Headers" res)

let test_preflight_wildcard_headers_reflects_request () =
  (* When headers is ["*"], reflect the Access-Control-Request-Headers back *)
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args ~headers:[ "*" ] ())
        ]
      (fun service ->
         let req =
           make_request
             ~meth:`OPTIONS
             ~origin:(Some "http://example.com")
             ~request_method:(Some "POST")
             ~request_headers:(Some "X-Foo, X-Bar")
             ()
         in
         service req)
  in
  Alcotest.(check (option string))
    "reflects requested headers"
    (Some "X-Foo, X-Bar")
    (Tapak.Response.header "Access-Control-Allow-Headers" res)

let test_preflight_wildcard_headers_without_request_headers () =
  (* When headers is ["*"] but no request headers, return "*" *)
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args ~headers:[ "*" ] ())
        ]
      (fun service ->
         let req =
           make_request
             ~meth:`OPTIONS
             ~origin:(Some "http://example.com")
             ~request_method:(Some "POST")
             ()
         in
         service req)
  in
  Alcotest.(check (option string))
    "returns wildcard when no request headers"
    (Some "*")
    (Tapak.Response.header "Access-Control-Allow-Headers" res)

let test_preflight_max_age () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args ~max_age:3600 ())
        ]
      (fun service ->
         let req =
           make_request
             ~meth:`OPTIONS
             ~origin:(Some "http://example.com")
             ~request_method:(Some "POST")
             ()
         in
         service req)
  in
  Alcotest.(check (option string))
    "includes max-age"
    (Some "3600")
    (Tapak.Response.header "Access-Control-Max-Age" res)

let test_preflight_no_max_age_when_not_configured () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args ())
        ]
      (fun service ->
         let req =
           make_request
             ~meth:`OPTIONS
             ~origin:(Some "http://example.com")
             ~request_method:(Some "POST")
             ()
         in
         service req)
  in
  Alcotest.(check (option string))
    "no max-age header"
    None
    (Tapak.Response.header "Access-Control-Max-Age" res)

let test_preflight_with_credentials () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args
               ~origins:(`Allow_list [ "http://example.com" ])
               ~credentials:true
               ())
        ]
      (fun service ->
         let req =
           make_request
             ~meth:`OPTIONS
             ~origin:(Some "http://example.com")
             ~request_method:(Some "POST")
             ()
         in
         service req)
  in
  Alcotest.(check (option string))
    "echoes origin"
    (Some "http://example.com")
    (Tapak.Response.header "Access-Control-Allow-Origin" res);
  Alcotest.(check (option string))
    "includes credentials"
    (Some "true")
    (Tapak.Response.header "Access-Control-Allow-Credentials" res)

let test_exposed_headers () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args
               ~exposed_headers:[ "X-Request-Id"; "X-RateLimit-Remaining" ]
               ())
        ]
      (fun service ->
         let req = make_request ~origin:(Some "http://example.com") () in
         service req)
  in
  Alcotest.(check (option string))
    "includes exposed headers"
    (Some "X-Request-Id, X-RateLimit-Remaining")
    (Tapak.Response.header "Access-Control-Expose-Headers" res)

let test_no_exposed_headers_when_empty () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args ~exposed_headers:[] ())
        ]
      (fun service ->
         let req = make_request ~origin:(Some "http://example.com") () in
         service req)
  in
  Alcotest.(check (option string))
    "no exposed headers header"
    None
    (Tapak.Response.header "Access-Control-Expose-Headers" res)

let test_vary_origin_with_allow_list () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args
               ~origins:(`Allow_list [ "http://example.com" ])
               ())
        ]
      (fun service ->
         let req = make_request ~origin:(Some "http://example.com") () in
         service req)
  in
  Alcotest.(check (option string))
    "includes Vary: Origin"
    (Some "Origin")
    (Tapak.Response.header "Vary" res)

let test_vary_origin_with_predicate () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args
               ~origins:(`Allow_predicate (fun _ -> true))
               ())
        ]
      (fun service ->
         let req = make_request ~origin:(Some "http://example.com") () in
         service req)
  in
  Alcotest.(check (option string))
    "includes Vary: Origin"
    (Some "Origin")
    (Tapak.Response.header "Vary" res)

let test_no_vary_with_allow_all_no_credentials () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args
               ~origins:`Allow_all
               ~credentials:false
               ())
        ]
      (fun service ->
         let req = make_request ~origin:(Some "http://example.com") () in
         service req)
  in
  (* When Allow_all without credentials, response is always "*", so no Vary
     needed *)
  Alcotest.(check (option string))
    "no Vary header needed"
    None
    (Tapak.Response.header "Vary" res)

let test_vary_with_allow_all_and_credentials () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args
               ~origins:`Allow_all
               ~credentials:true
               ())
        ]
      (fun service ->
         let req = make_request ~origin:(Some "http://example.com") () in
         service req)
  in
  (* When Allow_all with credentials, origin varies, so Vary is needed *)
  Alcotest.(check (option string))
    "includes Vary: Origin"
    (Some "Origin")
    (Tapak.Response.header "Vary" res)

let test_send_preflight_disabled_passes_through () =
  let handler_called = ref false in
  let _res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args ~send_preflight:false ())
        ]
      ~handler:(fun _ ->
        handler_called := true;
        Tapak.html "OK")
      (fun service ->
         let req =
           make_request
             ~meth:`OPTIONS
             ~origin:(Some "http://example.com")
             ~request_method:(Some "POST")
             ()
         in
         service req)
  in
  Alcotest.(check bool)
    "handler is called when send_preflight is false"
    true
    !handler_called

let test_send_preflight_disabled_rejected_origin_passes_through () =
  let handler_called = ref false in
  let _res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args
               ~origins:(`Allow_list [ "http://allowed.com" ])
               ~send_preflight:false
               ())
        ]
      ~handler:(fun _ ->
        handler_called := true;
        Tapak.html "OK")
      (fun service ->
         let req =
           make_request
             ~meth:`OPTIONS
             ~origin:(Some "http://evil.com")
             ~request_method:(Some "POST")
             ()
         in
         service req)
  in
  Alcotest.(check bool)
    "handler called for rejected origin when send_preflight is false"
    true
    !handler_called

let test_strict_mode () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.strict ~origins:[ "http://trusted.com" ])
        ]
      (fun service ->
         let req = make_request ~origin:(Some "http://trusted.com") () in
         service req)
  in
  Alcotest.(check (option string))
    "echoes trusted origin"
    (Some "http://trusted.com")
    (Tapak.Response.header "Access-Control-Allow-Origin" res);
  Alcotest.(check (option string))
    "credentials enabled in strict mode"
    (Some "true")
    (Tapak.Response.header "Access-Control-Allow-Credentials" res)

let test_strict_mode_preflight () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.strict ~origins:[ "http://trusted.com" ])
        ]
      (fun service ->
         let req =
           make_request
             ~meth:`OPTIONS
             ~origin:(Some "http://trusted.com")
             ~request_method:(Some "POST")
             ()
         in
         service req)
  in
  (* strict mode only allows GET and POST *)
  Alcotest.(check (option string))
    "only GET and POST allowed"
    (Some "GET, POST")
    (Tapak.Response.header "Access-Control-Allow-Methods" res);
  (* strict mode only allows Content-Type and Authorization headers *)
  Alcotest.(check (option string))
    "limited headers allowed"
    (Some "Content-Type, Authorization")
    (Tapak.Response.header "Access-Control-Allow-Headers" res)

let test_strict_mode_rejects_untrusted_origin () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.strict ~origins:[ "http://trusted.com" ])
        ]
      (fun service ->
         let req =
           make_request
             ~meth:`OPTIONS
             ~origin:(Some "http://evil.com")
             ~request_method:(Some "POST")
             ()
         in
         service req)
  in
  Alcotest.(check status) "403 Forbidden" `Forbidden (Tapak.Response.status res)

let test_multiple_origins_in_allow_list () =
  let config =
    Tapak.Middleware.CORS.args
      ~origins:
        (`Allow_list
            [ "http://first.com"; "http://second.com"; "http://third.com" ])
      ()
  in
  (* Test first origin *)
  let res1 =
    with_app
      ~middlewares:[ Tapak.use (module Tapak.Middleware.CORS) config ]
      (fun service ->
         let req = make_request ~origin:(Some "http://first.com") () in
         service req)
  in
  Alcotest.(check (option string))
    "accepts first origin"
    (Some "http://first.com")
    (Tapak.Response.header "Access-Control-Allow-Origin" res1);
  (* Test second origin *)
  let res2 =
    with_app
      ~middlewares:[ Tapak.use (module Tapak.Middleware.CORS) config ]
      (fun service ->
         let req = make_request ~origin:(Some "http://second.com") () in
         service req)
  in
  Alcotest.(check (option string))
    "accepts second origin"
    (Some "http://second.com")
    (Tapak.Response.header "Access-Control-Allow-Origin" res2)

let test_all_default_methods_included () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args ())
        ]
      (fun service ->
         let req =
           make_request
             ~meth:`OPTIONS
             ~origin:(Some "http://example.com")
             ~request_method:(Some "PATCH")
             ()
         in
         service req)
  in
  Alcotest.(check (option string))
    "includes all default methods"
    (Some "GET, POST, PUT, PATCH, DELETE, HEAD, OPTIONS")
    (Tapak.Response.header "Access-Control-Allow-Methods" res)

let test_preflight_vary_header () =
  let res =
    with_app
      ~middlewares:
        [ Tapak.use
            (module Tapak.Middleware.CORS)
            (Tapak.Middleware.CORS.args
               ~origins:(`Allow_list [ "http://example.com" ])
               ())
        ]
      (fun service ->
         let req =
           make_request
             ~meth:`OPTIONS
             ~origin:(Some "http://example.com")
             ~request_method:(Some "POST")
             ()
         in
         service req)
  in
  (* Preflight responses should also have Vary header when origin is dynamic *)
  Alcotest.(check (option string))
    "preflight includes Vary: Origin"
    (Some "Origin")
    (Tapak.Response.header "Vary" res)

let tests =
  [ ( "CORS - Basic"
    , [ Alcotest.test_case
          "No Origin header passes through"
          `Quick
          test_no_origin_header_passes_through
      ; Alcotest.test_case
          "Simple request gets CORS headers"
          `Quick
          test_simple_request_gets_cors_headers
      ] )
  ; ( "CORS - Permissive Mode"
    , [ Alcotest.test_case
          "GET request with permissive CORS"
          `Quick
          test_get_request_with_permissive_option
      ; Alcotest.test_case
          "Permissive preflight"
          `Quick
          test_permissive_preflight
      ] )
  ; ( "CORS - Origin Policy"
    , [ Alcotest.test_case
          "Allow list accepts listed origin"
          `Quick
          test_allow_list_accepts_listed_origin
      ; Alcotest.test_case
          "Allow list rejects unlisted origin"
          `Quick
          test_allow_list_rejects_unlisted_origin
      ; Alcotest.test_case
          "Allow list rejects preflight for unlisted origin"
          `Quick
          test_allow_list_rejects_preflight_for_unlisted_origin
      ; Alcotest.test_case "Allow predicate accepts" `Quick test_allow_predicate
      ; Alcotest.test_case
          "Allow predicate rejects"
          `Quick
          test_allow_predicate_rejects
      ; Alcotest.test_case
          "Multiple origins in allow list"
          `Quick
          test_multiple_origins_in_allow_list
      ] )
  ; ( "CORS - Credentials"
    , [ Alcotest.test_case
          "Credentials with Allow_all echoes origin"
          `Quick
          test_credentials_with_allow_all_echoes_origin
      ; Alcotest.test_case
          "Credentials with allow list"
          `Quick
          test_credentials_with_allow_list
      ; Alcotest.test_case
          "No credentials header when disabled"
          `Quick
          test_no_credentials_header_when_disabled
      ; Alcotest.test_case
          "No credentials header with wildcard origin"
          `Quick
          test_no_credentials_header_with_wildcard_origin
      ] )
  ; ( "CORS - Preflight"
    , [ Alcotest.test_case
          "Preflight detection requires request method header"
          `Quick
          test_preflight_detection_requires_request_method_header
      ; Alcotest.test_case
          "Preflight returns 204"
          `Quick
          test_preflight_returns_204
      ; Alcotest.test_case
          "Preflight includes allowed methods"
          `Quick
          test_preflight_includes_allowed_methods
      ; Alcotest.test_case
          "Preflight includes allowed headers"
          `Quick
          test_preflight_includes_allowed_headers
      ; Alcotest.test_case
          "Preflight wildcard headers reflects request"
          `Quick
          test_preflight_wildcard_headers_reflects_request
      ; Alcotest.test_case
          "Preflight wildcard headers without request headers"
          `Quick
          test_preflight_wildcard_headers_without_request_headers
      ; Alcotest.test_case "Preflight max-age" `Quick test_preflight_max_age
      ; Alcotest.test_case
          "Preflight no max-age when not configured"
          `Quick
          test_preflight_no_max_age_when_not_configured
      ; Alcotest.test_case
          "Preflight with credentials"
          `Quick
          test_preflight_with_credentials
      ; Alcotest.test_case
          "Preflight vary header"
          `Quick
          test_preflight_vary_header
      ; Alcotest.test_case
          "All default methods included"
          `Quick
          test_all_default_methods_included
      ] )
  ; ( "CORS - Exposed Headers"
    , [ Alcotest.test_case
          "Exposed headers included"
          `Quick
          test_exposed_headers
      ; Alcotest.test_case
          "No exposed headers when empty"
          `Quick
          test_no_exposed_headers_when_empty
      ] )
  ; ( "CORS - Vary Header"
    , [ Alcotest.test_case
          "Vary Origin with allow list"
          `Quick
          test_vary_origin_with_allow_list
      ; Alcotest.test_case
          "Vary Origin with predicate"
          `Quick
          test_vary_origin_with_predicate
      ; Alcotest.test_case
          "No Vary with Allow_all no credentials"
          `Quick
          test_no_vary_with_allow_all_no_credentials
      ; Alcotest.test_case
          "Vary with Allow_all and credentials"
          `Quick
          test_vary_with_allow_all_and_credentials
      ] )
  ; ( "CORS - send_preflight Option"
    , [ Alcotest.test_case
          "send_preflight disabled passes through"
          `Quick
          test_send_preflight_disabled_passes_through
      ; Alcotest.test_case
          "send_preflight disabled rejected origin passes through"
          `Quick
          test_send_preflight_disabled_rejected_origin_passes_through
      ] )
  ; ( "CORS - Strict Mode"
    , [ Alcotest.test_case "Strict mode basic" `Quick test_strict_mode
      ; Alcotest.test_case
          "Strict mode preflight"
          `Quick
          test_strict_mode_preflight
      ; Alcotest.test_case
          "Strict mode rejects untrusted origin"
          `Quick
          test_strict_mode_rejects_untrusted_origin
      ] )
  ]
