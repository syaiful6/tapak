let test_set_cookie () =
  let sc =
    Tapak.Cookies.Set_cookie.make
      ~secure:true
      ~http_only:true
      ("session_id", "abc123")
  in
  let name, value = Tapak.Cookies.Set_cookie.serialize sc in
  Alcotest.(check string) "Cookie name should be correct" "Set-Cookie" name;
  Alcotest.(check string)
    "Cookie value should be correct"
    "session_id=abc123; Secure; HttpOnly"
    value

let test_set_cookie_with_expires () =
  let expiration = `Max_age 3600L in
  let sc =
    Tapak.Cookies.Set_cookie.make
      ~secure:true
      ~http_only:true
      ~expiration
      ("session_id", "abc123")
  in
  let _, value = Tapak.Cookies.Set_cookie.serialize sc in
  Alcotest.(check string)
    "Cookie value should include Max-Age"
    "session_id=abc123; Secure; HttpOnly; Max-Age=3600"
    value

let test_set_cookie_with_path () =
  let sc =
    Tapak.Cookies.Set_cookie.make
      ~secure:true
      ~http_only:true
      ~path:"/app"
      ("session_id", "abc123")
  in
  let _, value = Tapak.Cookies.Set_cookie.serialize sc in
  Alcotest.(check string)
    "Cookie value should include Path"
    "session_id=abc123; Secure; HttpOnly; Path=/app"
    value

let test_set_cookie_with_domain () =
  let sc =
    Tapak.Cookies.Set_cookie.make
      ~secure:true
      ~http_only:true
      ~domain:"example.com"
      ("session_id", "abc123")
  in
  let _, value = Tapak.Cookies.Set_cookie.serialize sc in
  Alcotest.(check string)
    "Cookie value should include Domain"
    "session_id=abc123; Secure; HttpOnly; Domain=example.com"
    value

let test_set_cookie_with_same_site () =
  let sc =
    Tapak.Cookies.Set_cookie.make
      ~secure:true
      ~http_only:true
      ~same_site:`Strict
      ("session_id", "abc123")
  in
  let _, value = Tapak.Cookies.Set_cookie.serialize sc in
  Alcotest.(check string)
    "Cookie value should include SameSite"
    "session_id=abc123; Secure; HttpOnly; SameSite=Strict"
    value

let test_set_cookie_with_same_site_none () =
  let sc =
    Tapak.Cookies.Set_cookie.make
      ~secure:true
      ~http_only:true
      ~same_site:`None
      ("session_id", "abc123")
  in
  let _, value = Tapak.Cookies.Set_cookie.serialize sc in
  Alcotest.(check string)
    "Cookie value should include SameSite=None"
    "session_id=abc123; Secure; HttpOnly; SameSite=None"
    value

let test_set_cookie_with_same_site_lax () =
  let sc =
    Tapak.Cookies.Set_cookie.make
      ~secure:true
      ~http_only:true
      ~same_site:`Lax
      ("session_id", "abc123")
  in
  let _, value = Tapak.Cookies.Set_cookie.serialize sc in
  Alcotest.(check string)
    "Cookie value should include SameSite=Lax"
    "session_id=abc123; Secure; HttpOnly; SameSite=Lax"
    value

let test_set_cookie_with_same_site_none_and_secure_false () =
  let sc =
    Tapak.Cookies.Set_cookie.make
      ~secure:false
      ~http_only:true
      ~same_site:`None
      ("session_id", "abc123")
  in
  let _, value = Tapak.Cookies.Set_cookie.serialize sc in
  Alcotest.(check string)
    "Cookie with SameSite=None should coerce Secure even when secure=false"
    "session_id=abc123; Secure; HttpOnly; SameSite=None"
    value

let test_cookie_parse_matches_extract () =
  let open Tapak.Cookies.Cookie in
  let check header_values =
    let headers =
      Tapak.Headers.of_list (List.map (fun v -> "cookie", v) header_values)
    in
    Alcotest.(check (list (pair string string)))
      (String.concat " | " header_values)
      (extract headers)
      (parse headers)
  in
  List.iter
    check
    [ []
    ; [ "" ]
    ; [ "a=1" ]
    ; [ "a=1; b=2" ]
    ; [ "a=1;b=2;" ]
    ; [ "a" ]
    ; [ "a=" ]
    ; [ "=b" ]
    ; [ "a=b=c" ]
    ; [ " \t a=1 ; b=2 " ]
    ; [ ";;" ]
    ; [ "  ;  " ]
    ; [ "$Version=1; a=1; $Path=/" ]
    ; [ "a=1"; "b=2" ]
    ; [ "a=1"; ""; "a=2; c" ]
    ];
  let state = Random.State.make [| 42 |] in
  let tokens = [| "a"; "bc"; "="; ";"; " "; "\t"; "$"; "%20"; "\""; "," |] in
  for _ = 1 to 2000 do
    let value () =
      String.concat
        ""
        (List.init (Random.State.int state 8) (fun _ ->
           tokens.(Random.State.int state (Array.length tokens))))
    in
    check (List.init (1 + Random.State.int state 2) (fun _ -> value ()))
  done

let tests =
  [ ( "Cookies"
    , [ ( "Cookie.parse matches extract"
        , `Quick
        , test_cookie_parse_matches_extract )
      ; "Set-Cookie header generation", `Quick, test_set_cookie
      ; "Set-Cookie with expiration", `Quick, test_set_cookie_with_expires
      ; "Set-Cookie with path", `Quick, test_set_cookie_with_path
      ; "Set-Cookie with domain", `Quick, test_set_cookie_with_domain
      ; "Set-Cookie with SameSite", `Quick, test_set_cookie_with_same_site
      ; ( "Set-Cookie with SameSite=None"
        , `Quick
        , test_set_cookie_with_same_site_none )
      ; ( "Set-Cookie with SameSite=Lax"
        , `Quick
        , test_set_cookie_with_same_site_lax )
      ; ( "Set-Cookie with SameSite=None and Secure=false"
        , `Quick
        , test_set_cookie_with_same_site_none_and_secure_false )
      ] )
  ]
