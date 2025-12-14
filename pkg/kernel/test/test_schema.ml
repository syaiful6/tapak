open Tapak_kernel

let test_schema_single_field () =
  let schema = Schema.str "name" in
  let json = `Assoc [ "name", `String "Alice" ] in
  Alcotest.(check (result string (list (pair string string))))
    "parse single field"
    (Ok "Alice")
    (Schema.eval Schema.Json schema json)

let test_schema_multiple_fields () =
  let open Schema in
  let schema =
    let open Syntax in
    let+ name = str "name"
    and+ age = int "age"
    and+ active = bool "active" in
    name, age, active
  in
  let json =
    `Assoc [ "name", `String "Bob"; "age", `Int 30; "active", `Bool true ]
  in
  Alcotest.(check (result (triple string int bool) (list (pair string string))))
    "parse multiple fields"
    (Ok ("Bob", 30, true))
    (eval Schema.Json schema json)

let test_schema_with_defaults () =
  let open Schema in
  let schema =
    let open Syntax in
    let+ name = str "name"
    and+ role = str ~default:"user" "role" in
    name, role
  in
  let json = `Assoc [ "name", `String "Charlie" ] in
  Alcotest.(check (result (pair string string) (list (pair string string))))
    "use default for missing field"
    (Ok ("Charlie", "user"))
    (eval Schema.Json schema json)

let test_schema_validation () =
  let open Schema in
  let schema =
    int "age"
    |> validate (fun a ->
      if a >= 0 && a <= 120 then Ok a else Error [ "Invalid age" ])
  in
  let valid_json = `Assoc [ "age", `Int 25 ] in
  let invalid_json = `Assoc [ "age", `Int 150 ] in
  Alcotest.(check (result int (list (pair string string))))
    "accept valid age"
    (Ok 25)
    (eval Schema.Json schema valid_json);
  Alcotest.(check (result int (list (pair string string))))
    "reject invalid age"
    (Error [ "age", "Invalid age" ])
    (eval Schema.Json schema invalid_json)

let test_schema_cross_field_validation () =
  let open Schema in
  let schema =
    let open Syntax in
    map (fun a b -> a, b) (str "password")
    <*> str "password_confirm"
    |> validate (fun (p1, p2) ->
      if p1 = p2 then Ok p1 else Error [ "Passwords do not match" ])
  in
  let valid_json =
    `Assoc
      [ "password", `String "secret"; "password_confirm", `String "secret" ]
  in
  let invalid_json =
    `Assoc
      [ "password", `String "secret"; "password_confirm", `String "different" ]
  in
  Alcotest.(check (result string (list (pair string string))))
    "accept matching passwords"
    (Ok "secret")
    (eval Schema.Json schema valid_json);
  Alcotest.(check (result string (list (pair string string))))
    "reject mismatched passwords"
    (Error [ "", "Passwords do not match" ])
    (eval Schema.Json schema invalid_json)

let test_schema_urlencoded () =
  let open Schema in
  let schema =
    let open Syntax in
    let+ name = str "name"
    and+ age = int "age" in
    name, age
  in
  let form = [ "name", [ "David" ]; "age", [ "35" ] ] in
  Alcotest.(check (result (pair string int) (list (pair string string))))
    "parse URL-encoded form"
    (Ok ("David", 35))
    (eval Schema.Urlencoded schema form)

let test_schema_urlencoded_list () =
  let open Schema in
  let schema = list "tags" (Field.str ()) in
  let form = [ "tags", [ "ocaml"; "web"; "framework" ] ] in
  Alcotest.(check (result (list string) (list (pair string string))))
    "parse URL-encoded list"
    (Ok [ "ocaml"; "web"; "framework" ])
    (eval Schema.Urlencoded schema form)

type user =
  { name : string
  ; email : string
  ; age : int
  ; bio : string option
  ; tags : string list
  }

let test_schema_complex () =
  let open Schema in
  let schema =
    let open Syntax in
    let+ name = str "name"
    and+ email = str "email"
    and+ age =
      int ~default:18 "age"
      |> validate (fun age ->
        if age < 13 then Error [ "Must be 13 or older" ] else Ok age)
    and+ bio = option "bio" (Field.str ())
    and+ tags = list ~default:[] "tags" (Field.str ()) in
    { name; email; age; bio; tags }
  in
  let json =
    `Assoc
      [ "name", `String "Eve"
      ; "email", `String "eve@example.com"
      ; "age", `Int 25
      ; "bio", `String "Developer"
      ; "tags", `List [ `String "ocaml"; `String "functional" ]
      ]
  in
  let result = eval Schema.Json schema json in
  match result with
  | Ok user ->
    Alcotest.(check string) "name" "Eve" user.name;
    Alcotest.(check int) "age" 25 user.age;
    Alcotest.(check (option string)) "bio" (Some "Developer") user.bio;
    Alcotest.(check (list string)) "tags" [ "ocaml"; "functional" ] user.tags
  | Error errs ->
    let msg = String.concat "; " (List.map (fun (f, m) -> f ^ ": " ^ m) errs) in
    Alcotest.fail msg

let test_validator_usage () =
  let open Schema in
  let schema =
    let open Syntax in
    let+ name =
      str
        ~constraint_:
          (Constraint.all_of
             [ Constraint.min_length 2; Constraint.max_length 50 ])
        "name"
    and+ email = str ~constraint_:(Constraint.min_length 1) "email" in
    name, email
  in
  let valid_json =
    `Assoc [ "name", `String "Frank"; "email", `String "frank@example.com" ]
  in
  let invalid_json = `Assoc [ "name", `String "F"; "email", `String "" ] in
  Alcotest.(check bool)
    "accept valid data"
    true
    (Result.is_ok (eval Schema.Json schema valid_json));
  Alcotest.(check bool)
    "reject invalid data"
    true
    (Result.is_error (eval Schema.Json schema invalid_json))

let test_error_accumulation () =
  let open Schema in
  let schema =
    let open Syntax in
    let+ name = str ~constraint_:(Constraint.min_length 2) "name"
    and+ email = str ~constraint_:(Constraint.min_length 1) "email"
    and+ age = int ~constraint_:(Constraint.int_range 0 120) "age" in
    name, email, age
  in
  let invalid_json =
    `Assoc [ "name", `String "A"; "email", `String ""; "age", `Int 150 ]
  in
  match eval Schema.Json schema invalid_json with
  | Ok _ -> Alcotest.fail "Expected validation to fail"
  | Error errs ->
    let has_name_error = List.exists (fun (field, _) -> field = "name") errs in
    let has_email_error =
      List.exists (fun (field, _) -> field = "email") errs
    in
    let has_age_error = List.exists (fun (field, _) -> field = "age") errs in
    Alcotest.(check bool) "has name error with field name" true has_name_error;
    Alcotest.(check bool) "has email error with field name" true has_email_error;
    Alcotest.(check bool) "has age error with field name" true has_age_error

let test_urlencoded_repeated_keys () =
  let open Schema in
  let schema = list "tags" (Field.str ()) in
  (* Standard format: ?tags=ocaml&tags=web&tags=framework *)
  let form =
    [ "tags", [ "ocaml" ]; "tags", [ "web" ]; "tags", [ "framework" ] ]
  in
  let result = eval Schema.Urlencoded schema form in
  Alcotest.(check (result (list string) (list (pair string string))))
    "parse array with repeated keys (standard OpenAPI format)"
    (Ok [ "ocaml"; "web"; "framework" ])
    result

let test_urlencoded_repeated_keys_mixed () =
  let open Schema in
  let schema =
    let open Syntax in
    let+ tags = list "tags" (Field.str ())
    and+ category = str "category"
    and+ active = bool "active" in
    tags, category, active
  in
  (* Standard format: ?tags=ocaml&tags=web&category=programming&active=true *)
  let form =
    [ "tags", [ "ocaml" ]
    ; "tags", [ "web" ]
    ; "category", [ "programming" ]
    ; "active", [ "true" ]
    ]
  in
  let result = eval Schema.Urlencoded schema form in
  Alcotest.(
    check
      (result (triple (list string) string bool) (list (pair string string))))
    "parse mixed repeated keys and single values"
    (Ok ([ "ocaml"; "web" ], "programming", true))
    result

let test_urlencoded_repeated_keys_integers () =
  let open Schema in
  let schema = list "ids" (Field.int ()) in
  (* Standard format: ?ids=1&ids=2&ids=3 *)
  let form = [ "ids", [ "1" ]; "ids", [ "2" ]; "ids", [ "3" ] ] in
  let result = eval Schema.Urlencoded schema form in
  Alcotest.(check (result (list int) (list (pair string string))))
    "parse integer array with repeated keys"
    (Ok [ 1; 2; 3 ])
    result

let test_urlencoded_repeated_keys_empty () =
  let open Schema in
  let schema = list ~default:[] "tags" (Field.str ()) in
  (* No tags parameter provided *)
  let form = [ "category", [ "test" ] ] in
  let result = eval Schema.Urlencoded schema form in
  Alcotest.(check (result (list string) (list (pair string string))))
    "use default for missing array parameter"
    (Ok [])
    result

let test_urlencoded_array_bracket_notation () =
  let open Schema in
  let schema = list "tags" (Field.str ()) in
  let form = [ "tags[]", [ "ocaml"; "web"; "framework" ] ] in
  let result = eval Schema.Urlencoded schema form in
  Alcotest.(check (result (list string) (list (pair string string))))
    "parse array with bracket notation"
    (Ok [ "ocaml"; "web"; "framework" ])
    result

let test_urlencoded_nested_object () =
  let open Schema in
  let user_schema =
    let open Syntax in
    let+ name = str "name"
    and+ email = str "email" in
    name, email
  in
  let schema = obj "user" user_schema in
  let form =
    [ "user[name]", [ "John" ]; "user[email]", [ "john@example.com" ] ]
  in
  let result = eval Schema.Urlencoded schema form in
  Alcotest.(check (result (pair string string) (list (pair string string))))
    "parse nested object with bracket notation"
    (Ok ("John", "john@example.com"))
    result

let test_urlencoded_deep_nesting () =
  let open Schema in
  let settings_schema =
    let open Syntax in
    let+ theme = str "theme"
    and+ notifications = bool "notifications" in
    theme, notifications
  in
  let profile_schema = obj "settings" settings_schema in
  let schema = obj "user" (obj "profile" profile_schema) in
  let form =
    [ "user[profile][settings][theme]", [ "dark" ]
    ; "user[profile][settings][notifications]", [ "true" ]
    ]
  in
  let result = eval Schema.Urlencoded schema form in
  Alcotest.(check (result (pair string bool) (list (pair string string))))
    "parse deeply nested objects"
    (Ok ("dark", true))
    result

let test_urlencoded_indexed_array () =
  let open Schema in
  let schema = list "items" (Field.str ()) in
  let form =
    [ "items[0]", [ "first" ]
    ; "items[1]", [ "second" ]
    ; "items[2]", [ "third" ]
    ]
  in
  let result = eval Schema.Urlencoded schema form in
  Alcotest.(check (result (list string) (list (pair string string))))
    "parse indexed array"
    (Ok [ "first"; "second"; "third" ])
    result

let test_urlencoded_array_of_objects () =
  let open Schema in
  let item_schema =
    let open Syntax in
    let+ id = int "id"
    and+ name = str "name" in
    id, name
  in
  let schema = list "items" (Object item_schema) in
  let form =
    [ "items[0][id]", [ "1" ]
    ; "items[0][name]", [ "First" ]
    ; "items[1][id]", [ "2" ]
    ; "items[1][name]", [ "Second" ]
    ]
  in
  let result = eval Schema.Urlencoded schema form in
  match result with
  | Ok items ->
    Alcotest.(check int) "number of items" 2 (List.length items);
    let first_id, first_name = List.nth items 0 in
    let second_id, second_name = List.nth items 1 in
    Alcotest.(check int) "first item id" 1 first_id;
    Alcotest.(check string) "first item name" "First" first_name;
    Alcotest.(check int) "second item id" 2 second_id;
    Alcotest.(check string) "second item name" "Second" second_name
  | Error errs ->
    let msg = String.concat "; " (List.map (fun (f, m) -> f ^ ": " ^ m) errs) in
    Alcotest.fail ("Expected successful parse but got errors: " ^ msg)

let test_urlencoded_complex_nested () =
  let open Schema in
  let address_schema =
    let open Syntax in
    let+ street = str "street"
    and+ city = str "city" in
    street, city
  in
  let schema =
    let open Syntax in
    let+ name = str "name"
    and+ age = int "age"
    and+ tags = list "tags" (Field.str ())
    and+ address = obj "address" address_schema in
    name, age, tags, address
  in
  let form =
    [ "name", [ "Alice" ]
    ; "age", [ "30" ]
    ; "tags[]", [ "dev"; "ocaml" ]
    ; "address[street]", [ "Main St" ]
    ; "address[city]", [ "NYC" ]
    ]
  in
  let result = eval Schema.Urlencoded schema form in
  match result with
  | Ok (name, age, tags, (street, city)) ->
    Alcotest.(check string) "name" "Alice" name;
    Alcotest.(check int) "age" 30 age;
    Alcotest.(check (list string)) "tags" [ "dev"; "ocaml" ] tags;
    Alcotest.(check string) "street" "Main St" street;
    Alcotest.(check string) "city" "NYC" city
  | Error errs ->
    let msg = String.concat "; " (List.map (fun (f, m) -> f ^ ": " ^ m) errs) in
    Alcotest.fail ("Expected successful parse but got errors: " ^ msg)

let test_urlencoded_empty_array () =
  let open Schema in
  let schema = list ~default:[] "tags" (Field.str ()) in
  let form = [] in
  let result = eval Schema.Urlencoded schema form in
  Alcotest.(check (result (list string) (list (pair string string))))
    "use default for missing array"
    (Ok [])
    result

let test_urlencoded_object_with_optional () =
  let open Schema in
  let user_schema =
    let open Syntax in
    let+ name = str "name"
    and+ email = str "email"
    and+ bio = option "bio" (Field.str ()) in
    name, email, bio
  in
  let schema = obj "user" user_schema in
  let form =
    [ "user[name]", [ "Bob" ]; "user[email]", [ "bob@example.com" ] ]
  in
  let result = eval Schema.Urlencoded schema form in
  Alcotest.(
    check
      (result
         (triple string string (option string))
         (list (pair string string))))
    "parse object with missing optional field"
    (Ok ("Bob", "bob@example.com", None))
    result

let test_pattern_constraint () =
  let open Schema in
  let schema =
    str ~constraint_:(Constraint.pattern "^[A-Z][a-z]+$") "username"
  in
  let valid_json = `Assoc [ "username", `String "Alice" ] in
  let invalid_json = `Assoc [ "username", `String "alice123" ] in
  Alcotest.(check (result string (list (pair string string))))
    "accept string matching pattern"
    (Ok "Alice")
    (eval Schema.Json schema valid_json);
  Alcotest.(check bool)
    "reject string not matching pattern"
    true
    (Result.is_error (eval Schema.Json schema invalid_json))

let test_format_email () =
  let open Schema in
  let schema = str ~constraint_:(Constraint.format `Email) "email" in
  let valid_json = `Assoc [ "email", `String "test@example.com" ] in
  let invalid_json = `Assoc [ "email", `String "not-an-email" ] in
  Alcotest.(check (result string (list (pair string string))))
    "accept valid email"
    (Ok "test@example.com")
    (eval Schema.Json schema valid_json);
  Alcotest.(check bool)
    "reject invalid email"
    true
    (Result.is_error (eval Schema.Json schema invalid_json))

let test_format_uri () =
  let open Schema in
  let schema = str ~constraint_:(Constraint.format `Uri) "website" in
  let valid_json = `Assoc [ "website", `String "https://example.com" ] in
  let invalid_json = `Assoc [ "website", `String "not a uri" ] in
  Alcotest.(check (result string (list (pair string string))))
    "accept valid URI"
    (Ok "https://example.com")
    (eval Schema.Json schema valid_json);
  Alcotest.(check bool)
    "reject invalid URI"
    true
    (Result.is_error (eval Schema.Json schema invalid_json))

let test_format_uuid () =
  let open Schema in
  let schema = str ~constraint_:(Constraint.format `Uuid) "id" in
  let valid_json =
    `Assoc [ "id", `String "550e8400-e29b-41d4-a716-446655440000" ]
  in
  let invalid_json = `Assoc [ "id", `String "not-a-uuid" ] in
  Alcotest.(check (result string (list (pair string string))))
    "accept valid UUID"
    (Ok "550e8400-e29b-41d4-a716-446655440000")
    (eval Schema.Json schema valid_json);
  Alcotest.(check bool)
    "reject invalid UUID"
    true
    (Result.is_error (eval Schema.Json schema invalid_json))

let test_format_date () =
  let open Schema in
  let schema = str ~constraint_:(Constraint.format `Date) "birthdate" in
  let valid_json = `Assoc [ "birthdate", `String "2024-03-15" ] in
  let invalid_date_json = `Assoc [ "birthdate", `String "2024-02-30" ] in
  let invalid_format_json = `Assoc [ "birthdate", `String "15-03-2024" ] in
  Alcotest.(check (result string (list (pair string string))))
    "accept valid date"
    (Ok "2024-03-15")
    (eval Schema.Json schema valid_json);
  Alcotest.(check bool)
    "reject invalid date (Feb 30)"
    true
    (Result.is_error (eval Schema.Json schema invalid_date_json));
  Alcotest.(check bool)
    "reject invalid date format"
    true
    (Result.is_error (eval Schema.Json schema invalid_format_json))

let test_format_datetime () =
  let open Schema in
  let schema = str ~constraint_:(Constraint.format `Date_time) "created_at" in
  let valid_json = `Assoc [ "created_at", `String "2024-03-15T14:30:00Z" ] in
  let invalid_json = `Assoc [ "created_at", `String "2024-03-15 14:30:00" ] in
  Alcotest.(check (result string (list (pair string string))))
    "accept valid datetime"
    (Ok "2024-03-15T14:30:00Z")
    (eval Schema.Json schema valid_json);
  Alcotest.(check bool)
    "reject invalid datetime format"
    true
    (Result.is_error (eval Schema.Json schema invalid_json))

let test_format_ipv4 () =
  let open Schema in
  let schema = str ~constraint_:(Constraint.format `Ipv4) "ip_address" in
  let valid_json = `Assoc [ "ip_address", `String "192.168.1.1" ] in
  let invalid_json = `Assoc [ "ip_address", `String "999.999.999.999" ] in
  Alcotest.(check (result string (list (pair string string))))
    "accept valid IPv4"
    (Ok "192.168.1.1")
    (eval Schema.Json schema valid_json);
  Alcotest.(check bool)
    "reject invalid IPv4"
    true
    (Result.is_error (eval Schema.Json schema invalid_json))

let test_format_ipv6 () =
  let open Schema in
  let schema = str ~constraint_:(Constraint.format `Ipv6) "ip_address" in
  let valid_json = `Assoc [ "ip_address", `String "2001:0db8::1" ] in
  let invalid_json = `Assoc [ "ip_address", `String "not-an-ipv6" ] in
  Alcotest.(check (result string (list (pair string string))))
    "accept valid IPv6"
    (Ok "2001:0db8::1")
    (eval Schema.Json schema valid_json);
  Alcotest.(check bool)
    "reject invalid IPv6"
    true
    (Result.is_error (eval Schema.Json schema invalid_json))

let test_date_leap_year () =
  let open Schema in
  let schema = str ~constraint_:(Constraint.format `Date) "date" in
  let leap_year_json = `Assoc [ "date", `String "2024-02-29" ] in
  let non_leap_year_json = `Assoc [ "date", `String "2023-02-29" ] in
  Alcotest.(check (result string (list (pair string string))))
    "accept Feb 29 in leap year"
    (Ok "2024-02-29")
    (eval Schema.Json schema leap_year_json);
  Alcotest.(check bool)
    "reject Feb 29 in non-leap year"
    true
    (Result.is_error (eval Schema.Json schema non_leap_year_json))

let test_time_validation () =
  let open Schema in
  let schema = str ~constraint_:(Constraint.format `Date_time) "timestamp" in
  let valid_json = `Assoc [ "timestamp", `String "2024-01-01T23:59:59Z" ] in
  let invalid_hour_json =
    `Assoc [ "timestamp", `String "2024-01-01T25:00:00Z" ]
  in
  let invalid_minute_json =
    `Assoc [ "timestamp", `String "2024-01-01T12:60:00Z" ]
  in
  Alcotest.(check (result string (list (pair string string))))
    "accept valid time"
    (Ok "2024-01-01T23:59:59Z")
    (eval Schema.Json schema valid_json);
  Alcotest.(check bool)
    "reject invalid hour (25)"
    true
    (Result.is_error (eval Schema.Json schema invalid_hour_json));
  Alcotest.(check bool)
    "reject invalid minute (60)"
    true
    (Result.is_error (eval Schema.Json schema invalid_minute_json))

let test_multiple_constraints () =
  let open Schema in
  let schema =
    str
      ~constraint_:
        (Constraint.all_of
           [ Constraint.min_length 5
           ; Constraint.max_length 50
           ; Constraint.pattern
               "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
           ])
      "email"
  in
  let valid_json = `Assoc [ "email", `String "user@example.com" ] in
  let too_short_json = `Assoc [ "email", `String "a@b" ] in
  let no_pattern_json = `Assoc [ "email", `String "not-an-email" ] in
  Alcotest.(check (result string (list (pair string string))))
    "accept valid with all constraints"
    (Ok "user@example.com")
    (eval Schema.Json schema valid_json);
  Alcotest.(check bool)
    "reject too short email"
    true
    (Result.is_error (eval Schema.Json schema too_short_json));
  Alcotest.(check bool)
    "reject invalid pattern"
    true
    (Result.is_error (eval Schema.Json schema no_pattern_json))

let test_header_case_insensitive () =
  let open Schema in
  let schema = str "X-API-Key" in
  let headers_upper = `Assoc [ "X-API-KEY", `String "secret123" ] in
  let headers_lower = `Assoc [ "x-api-key", `String "secret123" ] in
  let headers_mixed = `Assoc [ "X-Api-Key", `String "secret123" ] in
  Alcotest.(check (result string (list (pair string string))))
    "match uppercase header"
    (Ok "secret123")
    (evaluate (module Header_interpreter) schema headers_upper);
  Alcotest.(check (result string (list (pair string string))))
    "match lowercase header"
    (Ok "secret123")
    (evaluate (module Header_interpreter) schema headers_lower);
  Alcotest.(check (result string (list (pair string string))))
    "match mixed case header"
    (Ok "secret123")
    (evaluate (module Header_interpreter) schema headers_mixed)

let test_header_multiple_fields () =
  let open Schema in
  let schema =
    let open Syntax in
    let+ api_key = str "X-API-Key"
    and+ content_type = str "Content-Type"
    and+ user_agent = option "User-Agent" (Field.str ()) in
    api_key, content_type, user_agent
  in
  let headers =
    `Assoc
      [ "x-api-key", `String "secret123"
      ; "content-type", `String "application/json"
      ; "user-agent", `String "test-client/1.0"
      ]
  in
  match evaluate (module Header_interpreter) schema headers with
  | Ok (key, ct, ua) ->
    Alcotest.(check string) "api key" "secret123" key;
    Alcotest.(check string) "content type" "application/json" ct;
    Alcotest.(check (option string)) "user agent" (Some "test-client/1.0") ua
  | Error errs ->
    let msg = String.concat "; " (List.map (fun (f, m) -> f ^ ": " ^ m) errs) in
    Alcotest.fail msg

let test_header_missing_required () =
  let open Schema in
  let schema = str "Authorization" in
  let headers = `Assoc [ "Content-Type", `String "application/json" ] in
  match evaluate (module Header_interpreter) schema headers with
  | Ok _ -> Alcotest.fail "Should fail when required header is missing"
  | Error errs ->
    let has_auth_error =
      List.exists (fun (field, _) -> field = "Authorization") errs
    in
    Alcotest.(check bool) "has Authorization error" true has_auth_error

let test_header_optional () =
  let open Schema in
  let schema = option "X-Custom-Header" (Field.str ()) in
  let with_header = `Assoc [ "x-custom-header", `String "value" ] in
  let without_header = `Assoc [] in
  Alcotest.(check (result (option string) (list (pair string string))))
    "parse present optional header"
    (Ok (Some "value"))
    (evaluate (module Header_interpreter) schema with_header);
  Alcotest.(check (result (option string) (list (pair string string))))
    "parse missing optional header"
    (Ok None)
    (evaluate (module Header_interpreter) schema without_header)

let test_header_with_constraint () =
  let open Schema in
  let schema =
    str ~constraint_:(Constraint.pattern "^Bearer .+$") "Authorization"
  in
  let valid_header = `Assoc [ "authorization", `String "Bearer token123" ] in
  let invalid_header = `Assoc [ "authorization", `String "Basic token123" ] in
  Alcotest.(check (result string (list (pair string string))))
    "accept valid Authorization header"
    (Ok "Bearer token123")
    (evaluate (module Header_interpreter) schema valid_header);
  Alcotest.(check bool)
    "reject invalid Authorization header"
    true
    (Result.is_error
       (evaluate (module Header_interpreter) schema invalid_header))

let test_header_multiple_values () =
  let open Schema in
  let schema = list "Accept" (Field.str ()) in
  let headers =
    `Assoc
      [ "accept", `List [ `String "application/json"; `String "text/html" ] ]
  in
  Alcotest.(check (result (list string) (list (pair string string))))
    "parse header with multiple values"
    (Ok [ "application/json"; "text/html" ])
    (evaluate (module Header_interpreter) schema headers)

let tests =
  [ ( "Schema"
    , [ Alcotest.test_case "Single field" `Quick test_schema_single_field
      ; Alcotest.test_case "Multiple fields" `Quick test_schema_multiple_fields
      ; Alcotest.test_case "With defaults" `Quick test_schema_with_defaults
      ; Alcotest.test_case "Validation" `Quick test_schema_validation
      ; Alcotest.test_case
          "Cross-field validation"
          `Quick
          test_schema_cross_field_validation
      ; Alcotest.test_case "URL-encoded" `Quick test_schema_urlencoded
      ; Alcotest.test_case "URL-encoded list" `Quick test_schema_urlencoded_list
      ; Alcotest.test_case "Complex schema" `Quick test_schema_complex
      ; Alcotest.test_case "Validator usage" `Quick test_validator_usage
      ; Alcotest.test_case "Error accumulation" `Quick test_error_accumulation
      ; Alcotest.test_case
          "URL-encoded repeated keys (standard)"
          `Quick
          test_urlencoded_repeated_keys
      ; Alcotest.test_case
          "URL-encoded repeated keys mixed with singles"
          `Quick
          test_urlencoded_repeated_keys_mixed
      ; Alcotest.test_case
          "URL-encoded repeated keys (integers)"
          `Quick
          test_urlencoded_repeated_keys_integers
      ; Alcotest.test_case
          "URL-encoded repeated keys empty (default)"
          `Quick
          test_urlencoded_repeated_keys_empty
      ; Alcotest.test_case
          "URL-encoded array bracket notation"
          `Quick
          test_urlencoded_array_bracket_notation
      ; Alcotest.test_case
          "URL-encoded nested object"
          `Quick
          test_urlencoded_nested_object
      ; Alcotest.test_case
          "URL-encoded deep nesting"
          `Quick
          test_urlencoded_deep_nesting
      ; Alcotest.test_case
          "URL-encoded indexed array"
          `Quick
          test_urlencoded_indexed_array
      ; Alcotest.test_case
          "URL-encoded array of objects"
          `Quick
          test_urlencoded_array_of_objects
      ; Alcotest.test_case
          "URL-encoded complex nested"
          `Quick
          test_urlencoded_complex_nested
      ; Alcotest.test_case
          "URL-encoded empty array"
          `Quick
          test_urlencoded_empty_array
      ; Alcotest.test_case
          "URL-encoded object with optional"
          `Quick
          test_urlencoded_object_with_optional
      ; Alcotest.test_case "Pattern constraint" `Quick test_pattern_constraint
      ; Alcotest.test_case "Format: Email" `Quick test_format_email
      ; Alcotest.test_case "Format: URI" `Quick test_format_uri
      ; Alcotest.test_case "Format: UUID" `Quick test_format_uuid
      ; Alcotest.test_case "Format: Date" `Quick test_format_date
      ; Alcotest.test_case "Format: Date_time" `Quick test_format_datetime
      ; Alcotest.test_case "Format: IPv4" `Quick test_format_ipv4
      ; Alcotest.test_case "Format: IPv6" `Quick test_format_ipv6
      ; Alcotest.test_case
          "Date leap year validation"
          `Quick
          test_date_leap_year
      ; Alcotest.test_case "Time validation" `Quick test_time_validation
      ; Alcotest.test_case
          "Multiple constraints"
          `Quick
          test_multiple_constraints
      ; Alcotest.test_case
          "Header case-insensitive"
          `Quick
          test_header_case_insensitive
      ; Alcotest.test_case
          "Header multiple fields"
          `Quick
          test_header_multiple_fields
      ; Alcotest.test_case
          "Header missing required"
          `Quick
          test_header_missing_required
      ; Alcotest.test_case "Header optional" `Quick test_header_optional
      ; Alcotest.test_case
          "Header with constraint"
          `Quick
          test_header_with_constraint
      ; Alcotest.test_case
          "Header multiple values"
          `Quick
          test_header_multiple_values
      ] )
  ]
