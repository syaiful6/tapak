let make_request ?cookie () =
  let headers =
    match cookie with
    | None -> Tapak.Headers.empty
    | Some ck ->
      let cookie_header = Tapak.Cookies.Cookie.serialize ck in
      Tapak.Headers.add
        Tapak.Headers.empty
        (fst cookie_header)
        (snd cookie_header)
  in
  Tapak.Request.create
    ~scheme:`HTTP
    ~version:Piaf.Versions.HTTP.HTTP_1_1
    ~headers
    ~meth:`GET
    ~body:Tapak.Body.empty
    "/test"

let make_response () = Tapak.html "<h1>Test</h1>"

let string_contains ~substring s =
  let sub_len = String.length substring in
  let s_len = String.length s in
  if sub_len > s_len
  then false
  else
    let rec check i =
      if i > s_len - sub_len
      then false
      else if String.sub s i sub_len = substring
      then true
      else check (i + 1)
    in
    check 0

let test_generate_secret () =
  let secret1 = Tapak.Csrf.generate_secret () in
  let secret2 = Tapak.Csrf.generate_secret () in
  Alcotest.(check int)
    "the length of the secret is 32 bytes"
    (String.length secret1)
    32;
  Alcotest.(check bool)
    "The generated secrets are different"
    (secret1 <> secret2)
    true

let test_request_empty_cookie_csrf_input () =
  let request = make_request () in
  let token, secret = Tapak.Csrf.input request in
  Alcotest.(check int)
    "the length of the secret is 32 bytes"
    (String.length secret)
    32;
  Alcotest.(check int)
    "the length of the token is 64 bytes"
    (String.length token)
    64

let test_input_reuses_existing_secret () =
  let existing_secret = Tapak.Csrf.generate_secret () in
  let cookie = [ "XSRF-TOKEN", existing_secret ] in
  let request = make_request ~cookie () in
  let _token, secret = Tapak.Csrf.input request in
  Alcotest.(check string)
    "input reuses existing secret from cookie"
    existing_secret
    secret

let test_input_custom_cookie_name () =
  let existing_secret = Tapak.Csrf.generate_secret () in
  let cookie = [ "MY-CSRF-TOKEN", existing_secret ] in
  let request = make_request ~cookie () in
  let _token, secret = Tapak.Csrf.input ~cookie_name:"MY-CSRF-TOKEN" request in
  Alcotest.(check string) "input uses custom cookie name" existing_secret secret

let test_input_generates_different_masked_tokens () =
  let existing_secret = Tapak.Csrf.generate_secret () in
  let cookie = [ "XSRF-TOKEN", existing_secret ] in
  let request = make_request ~cookie () in
  let token1, _ = Tapak.Csrf.input request in
  let token2, _ = Tapak.Csrf.input request in
  Alcotest.(check bool)
    "multiple calls produce different masked tokens (BREACH protection)"
    (token1 <> token2)
    true

let test_verify_token_no_cookie () =
  let request = make_request () in
  let token, _secret = Tapak.Csrf.input request in
  let is_valid = Tapak.Csrf.verify_token ~token request in
  Alcotest.(check bool)
    "verify_token returns false when no cookie is present"
    is_valid
    false

let test_verify_token_with_cookie () =
  let request = make_request () in
  let token, secret = Tapak.Csrf.input request in
  let cookie = [ "XSRF-TOKEN", secret ] in
  let request_with_cookie = make_request ~cookie () in
  let is_valid = Tapak.Csrf.verify_token ~token request_with_cookie in
  Alcotest.(check bool)
    "verify_token returns true when valid token and cookie are present"
    is_valid
    true

let test_verify_token_with_cookie_mismatched () =
  let request = make_request () in
  let valid_secret = Tapak.Csrf.generate_secret () in
  let token, _secret = Tapak.Csrf.input request in
  let cookie = [ "XSRF-TOKEN", valid_secret ] in
  let request_with_cookie = make_request ~cookie () in
  let is_valid = Tapak.Csrf.verify_token ~token request_with_cookie in
  Alcotest.(check bool)
    "verify_token returns false when token and cookie do not match"
    is_valid
    false

let test_verify_token_empty_token () =
  let secret = Tapak.Csrf.generate_secret () in
  let cookie = [ "XSRF-TOKEN", secret ] in
  let request = make_request ~cookie () in
  let is_valid = Tapak.Csrf.verify_token ~token:"" request in
  Alcotest.(check bool)
    "verify_token returns false for empty token"
    is_valid
    false

let test_verify_token_invalid_length () =
  let secret = Tapak.Csrf.generate_secret () in
  let cookie = [ "XSRF-TOKEN", secret ] in
  let request = make_request ~cookie () in
  (* Test with token that's neither 32 nor 64 bytes *)
  let is_valid_short = Tapak.Csrf.verify_token ~token:"tooshort" request in
  let is_valid_wrong =
    Tapak.Csrf.verify_token ~token:(String.make 50 'a') request
  in
  Alcotest.(check bool)
    "verify_token returns false for short token"
    is_valid_short
    false;
  Alcotest.(check bool)
    "verify_token returns false for wrong length token"
    is_valid_wrong
    false

let test_verify_token_tampered () =
  let request = make_request () in
  let token, secret = Tapak.Csrf.input request in
  let cookie = [ "XSRF-TOKEN", secret ] in
  let request_with_cookie = make_request ~cookie () in
  (* Tamper with the token by changing a character *)
  let tampered_token =
    let bytes = Bytes.of_string token in
    Bytes.set bytes 0 (if token.[0] = 'a' then 'b' else 'a');
    Bytes.to_string bytes
  in
  let is_valid =
    Tapak.Csrf.verify_token ~token:tampered_token request_with_cookie
  in
  Alcotest.(check bool)
    "verify_token returns false for tampered token"
    is_valid
    false

let test_verify_token_unmasked () =
  let secret = Tapak.Csrf.generate_secret () in
  let cookie = [ "XSRF-TOKEN", secret ] in
  let request = make_request ~cookie () in
  let is_valid = Tapak.Csrf.verify_token ~token:secret request in
  Alcotest.(check bool)
    "verify_token accepts unmasked 32-byte token"
    is_valid
    true

let test_verify_token_custom_cookie_name () =
  let request = make_request () in
  let token, secret = Tapak.Csrf.input ~cookie_name:"MY-CSRF" request in
  let cookie = [ "MY-CSRF", secret ] in
  let request_with_cookie = make_request ~cookie () in
  let is_valid =
    Tapak.Csrf.verify_token ~cookie_name:"MY-CSRF" ~token request_with_cookie
  in
  Alcotest.(check bool)
    "verify_token works with custom cookie name"
    is_valid
    true

let test_verify_token_mismatched_cookie_names () =
  let secret = Tapak.Csrf.generate_secret () in
  let cookie = [ "XSRF-TOKEN", secret ] in
  let request = make_request ~cookie () in
  (* Use a different cookie name for verification *)
  let is_valid =
    Tapak.Csrf.verify_token ~cookie_name:"OTHER-TOKEN" ~token:secret request
  in
  Alcotest.(check bool)
    "verify_token returns false when cookie names don't match"
    is_valid
    false

let test_verify_token_malformed_cookie () =
  (* Cookie exists but has invalid/non-alphanumeric characters *)
  let cookie = [ "XSRF-TOKEN", "invalid!@#$%^&*()secret" ] in
  let request = make_request ~cookie () in
  let token = String.make 64 'a' in
  (* This should not crash, just return false *)
  let is_valid = Tapak.Csrf.verify_token ~token request in
  Alcotest.(check bool)
    "verify_token handles malformed cookie gracefully"
    is_valid
    false

let test_with_cookie_adds_header () =
  let secret = Tapak.Csrf.generate_secret () in
  let response = make_response () in
  let response_with_cookie = Tapak.Csrf.with_cookie secret response in
  let set_cookie_header =
    Tapak.Response.header "set-cookie" response_with_cookie
  in
  Alcotest.(check bool)
    "with_cookie adds Set-Cookie header"
    (Option.is_some set_cookie_header)
    true;
  let header_value = Option.get set_cookie_header in
  Alcotest.(check bool)
    "Set-Cookie header contains XSRF-TOKEN"
    (String.starts_with ~prefix:"XSRF-TOKEN=" header_value)
    true;
  Alcotest.(check bool)
    "Set-Cookie header contains the secret"
    (string_contains ~substring:secret header_value)
    true

let test_with_cookie_default_settings () =
  let secret = Tapak.Csrf.generate_secret () in
  let response = make_response () in
  let response_with_cookie = Tapak.Csrf.with_cookie secret response in
  let header_value =
    Tapak.Response.header "set-cookie" response_with_cookie |> Option.get
  in
  Alcotest.(check bool)
    "Set-Cookie has path=/"
    (string_contains ~substring:"path=/" header_value)
    true;
  Alcotest.(check bool)
    "Set-Cookie has SameSite=Lax"
    (string_contains ~substring:"SameSite=Lax" header_value)
    true;
  Alcotest.(check bool)
    "Set-Cookie has Max-Age"
    (string_contains ~substring:"Max-Age=" header_value)
    true

let test_with_cookie_custom_settings () =
  let secret = Tapak.Csrf.generate_secret () in
  let response = make_response () in
  let settings : Tapak.Csrf.t =
    { cookie_name = Some "MY-CSRF-TOKEN"
    ; expiration = Some (`Max_age 3600L)
    ; domain = Some "example.com"
    ; path = Some "/api"
    ; secure = true
    ; http_only = true
    ; same_site = Some `Strict
    }
  in
  let response_with_cookie = Tapak.Csrf.with_cookie ~settings secret response in
  let header_value =
    Tapak.Response.header "set-cookie" response_with_cookie |> Option.get
  in
  Alcotest.(check bool)
    "Set-Cookie uses custom cookie name"
    (String.starts_with ~prefix:"MY-CSRF-TOKEN=" header_value)
    true;
  Alcotest.(check bool)
    "Set-Cookie has custom path"
    (string_contains ~substring:"path=/api" header_value)
    true;
  Alcotest.(check bool)
    "Set-Cookie has custom domain"
    (string_contains ~substring:"domain=example.com" header_value)
    true;
  Alcotest.(check bool)
    "Set-Cookie has secure flag"
    (string_contains ~substring:"secure" header_value)
    true;
  Alcotest.(check bool)
    "Set-Cookie has httponly flag"
    (string_contains ~substring:"httponly" header_value)
    true;
  Alcotest.(check bool)
    "Set-Cookie has SameSite=Strict"
    (string_contains ~substring:"SameSite=Strict" header_value)
    true

let tests =
  [ ( "CSRF"
    , [ Alcotest.test_case
          "generate_secret produces unique 32-byte secrets"
          `Quick
          test_generate_secret
      ; Alcotest.test_case
          "input produces token and secret when no cookie is present"
          `Quick
          test_request_empty_cookie_csrf_input
      ; Alcotest.test_case
          "input reuses existing secret from cookie"
          `Quick
          test_input_reuses_existing_secret
      ; Alcotest.test_case
          "input uses custom cookie name"
          `Quick
          test_input_custom_cookie_name
      ; Alcotest.test_case
          "input generates different masked tokens (BREACH protection)"
          `Quick
          test_input_generates_different_masked_tokens
      ; Alcotest.test_case
          "verify_token returns false when no cookie is present"
          `Quick
          test_verify_token_no_cookie
      ; Alcotest.test_case
          "verify_token returns true when valid token and cookie are present"
          `Quick
          test_verify_token_with_cookie
      ; Alcotest.test_case
          "verify_token returns false when token and cookie do not match"
          `Quick
          test_verify_token_with_cookie_mismatched
      ; Alcotest.test_case
          "verify_token returns false for empty token"
          `Quick
          test_verify_token_empty_token
      ; Alcotest.test_case
          "verify_token returns false for invalid length tokens"
          `Quick
          test_verify_token_invalid_length
      ; Alcotest.test_case
          "verify_token returns false for tampered token"
          `Quick
          test_verify_token_tampered
      ; Alcotest.test_case
          "verify_token accepts unmasked 32-byte token"
          `Quick
          test_verify_token_unmasked
      ; Alcotest.test_case
          "verify_token works with custom cookie name"
          `Quick
          test_verify_token_custom_cookie_name
      ; Alcotest.test_case
          "verify_token returns false when cookie names don't match"
          `Quick
          test_verify_token_mismatched_cookie_names
      ; Alcotest.test_case
          "verify_token handles malformed cookie gracefully"
          `Quick
          test_verify_token_malformed_cookie
      ; Alcotest.test_case
          "with_cookie adds Set-Cookie header"
          `Quick
          test_with_cookie_adds_header
      ; Alcotest.test_case
          "with_cookie uses default settings"
          `Quick
          test_with_cookie_default_settings
      ; Alcotest.test_case
          "with_cookie uses custom settings"
          `Quick
          test_with_cookie_custom_settings
      ] )
  ]
