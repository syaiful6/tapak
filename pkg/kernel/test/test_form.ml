open Tapak_kernel

let urlencoded_testable =
  Alcotest.testable
    (fun ppf lst ->
       Format.fprintf ppf "[";
       List.iter
         (fun (k, vs) ->
            Format.fprintf ppf "%s: [%s]; " k (String.concat ", " vs))
         lst;
       Format.fprintf ppf "]")
    (fun a b ->
       (* Simple structural equality for lists with duplicate keys *)
       List.length a = List.length b
       && List.for_all2
            (fun (k1, vs1) (k2, vs2) ->
               String.equal k1 k2
               && List.length vs1 = List.length vs2
               && List.for_all2 String.equal vs1 vs2)
            a
            b)

let test_of_string_empty () =
  let result = Form.Urlencoded.of_string "" in
  Alcotest.(check urlencoded_testable)
    "Empty string returns empty list"
    []
    result

let test_of_string_single_param () =
  let result = Form.Urlencoded.of_string "name=value" in
  Alcotest.(check urlencoded_testable)
    "Single parameter"
    [ "name", [ "value" ] ]
    result

let test_of_string_multiple_params () =
  let result = Form.Urlencoded.of_string "name=value&foo=bar" in
  Alcotest.(check urlencoded_testable)
    "Multiple parameters"
    [ "name", [ "value" ]; "foo", [ "bar" ] ]
    result

let test_of_string_multiple_values () =
  let result = Form.Urlencoded.of_string "color=red&color=blue&color=green" in
  (* Uri.query_of_encoded returns duplicate keys as separate entries, not merged *)
  Alcotest.(check urlencoded_testable)
    "Multiple values for same key"
    [ "color", [ "red" ]; "color", [ "blue" ]; "color", [ "green" ] ]
    result

let test_of_string_url_encoded_chars () =
  let result =
    Form.Urlencoded.of_string "message=hello%20world&special=%21%40%23"
  in
  Alcotest.(check urlencoded_testable)
    "URL-encoded special characters"
    [ "message", [ "hello world" ]; "special", [ "!@#" ] ]
    result

let test_of_string_empty_value () =
  let result = Form.Urlencoded.of_string "key=&other=value" in
  Alcotest.(check urlencoded_testable)
    "Empty value"
    [ "key", [ "" ]; "other", [ "value" ] ]
    result

let test_of_string_no_value () =
  let result = Form.Urlencoded.of_string "flag&other=value" in
  (* Uri.query_of_encoded treats keys without values as having an empty list *)
  Alcotest.(check urlencoded_testable)
    "Key without value"
    [ "flag", []; "other", [ "value" ] ]
    result

let test_of_string_plus_as_space () =
  let result = Form.Urlencoded.of_string "name=hello+world" in
  Alcotest.(check urlencoded_testable)
    "Plus sign decoded as space"
    [ "name", [ "hello world" ] ]
    result

let test_of_string_complex () =
  let result =
    Form.Urlencoded.of_string
      "user=john&email=john%40example.com&tags=ocaml&tags=web&active="
  in
  Alcotest.(check urlencoded_testable)
    "Complex form with mixed types"
    [ "user", [ "john" ]
    ; "email", [ "john@example.com" ]
    ; "tags", [ "ocaml" ]
    ; "tags", [ "web" ]
    ; "active", [ "" ]
    ]
    result

let test_of_body_success () =
  let body_content = "name=value&foo=bar" in
  let body = Piaf.Body.of_string body_content in
  let result = Form.Urlencoded.of_body body in
  Alcotest.(
    check
      (result
         urlencoded_testable
         (of_pp (fun ppf _ -> Format.fprintf ppf "Bad_request"))))
    "Parse body successfully"
    (Ok [ "name", [ "value" ]; "foo", [ "bar" ] ])
    result

let test_of_body_empty () =
  let body = Piaf.Body.empty in
  let result = Form.Urlencoded.of_body body in
  Alcotest.(
    check
      (result
         urlencoded_testable
         (of_pp (fun ppf _ -> Format.fprintf ppf "Bad_request"))))
    "Parse empty body"
    (Ok [])
    result

let make_test_request_with_query query =
  let uri = Uri.of_string ("http://example.com/test" ^ query) in
  Request.create
    ~scheme:`HTTP
    ~version:Piaf.Versions.HTTP.HTTP_1_1
    ~meth:`GET
    ~body:Piaf.Body.empty
    (Uri.path_and_query uri)

let test_of_query_empty () =
  let request = make_test_request_with_query "" in
  let result = Form.Urlencoded.of_query request in
  Alcotest.(check urlencoded_testable) "No query string" [] result

let test_of_query_single_param () =
  let request = make_test_request_with_query "?name=value" in
  let result = Form.Urlencoded.of_query request in
  Alcotest.(check urlencoded_testable)
    "Single query parameter"
    [ "name", [ "value" ] ]
    result

let test_of_query_multiple_params () =
  let request = make_test_request_with_query "?name=value&foo=bar&baz=qux" in
  let result = Form.Urlencoded.of_query request in
  Alcotest.(check urlencoded_testable)
    "Multiple query parameters"
    [ "name", [ "value" ]; "foo", [ "bar" ]; "baz", [ "qux" ] ]
    result

let test_of_query_multiple_values () =
  let request =
    make_test_request_with_query "?tag=ocaml&tag=web&tag=framework"
  in
  let result = Form.Urlencoded.of_query request in
  Alcotest.(check urlencoded_testable)
    "Multiple values for same parameter"
    [ "tag", [ "ocaml" ]; "tag", [ "web" ]; "tag", [ "framework" ] ]
    result

let test_of_query_encoded_chars () =
  let request =
    make_test_request_with_query
      "?message=hello%20world&email=test%40example.com"
  in
  let result = Form.Urlencoded.of_query request in
  Alcotest.(check urlencoded_testable)
    "URL-encoded characters in query"
    [ "message", [ "hello world" ]; "email", [ "test@example.com" ] ]
    result

let test_normalize_empty () =
  let result = Form.Urlencoded.normalize [] in
  Alcotest.(check urlencoded_testable) "Normalize empty list" [] result

let test_normalize_single_key () =
  let params = [ "name", [ "value" ]; "foo", [ "bar" ] ] in
  let result = Form.Urlencoded.normalize params in
  Alcotest.(check urlencoded_testable)
    "Normalize with no duplicates"
    [ "foo", [ "bar" ]; "name", [ "value" ] ]
    result

let test_normalize_duplicate_keys () =
  let params =
    [ "color", [ "red" ]
    ; "name", [ "John" ]
    ; "color", [ "blue" ]
    ; "color", [ "green" ]
    ]
  in
  let result = Form.Urlencoded.normalize params in
  Alcotest.(check urlencoded_testable)
    "Normalize duplicate keys"
    [ "color", [ "red"; "blue"; "green" ]; "name", [ "John" ] ]
    result

let test_normalize_preserves_order_within_key () =
  let params =
    [ "tag", [ "first" ]; "tag", [ "second" ]; "tag", [ "third" ] ]
  in
  let result = Form.Urlencoded.normalize params in
  Alcotest.(check urlencoded_testable)
    "Normalize preserves order of values"
    [ "tag", [ "first"; "second"; "third" ] ]
    result

let test_get_existing_key () =
  let params = [ "name", [ "John" ]; "age", [ "30" ] ] in
  let result = Form.Urlencoded.get "name" params in
  Alcotest.(check (option string)) "Get existing key" (Some "John") result

let test_get_missing_key () =
  let params = [ "name", [ "John" ] ] in
  let result = Form.Urlencoded.get "age" params in
  Alcotest.(check (option string)) "Get missing key" None result

let test_get_empty_value () =
  let params = [ "flag", [] ] in
  let result = Form.Urlencoded.get "flag" params in
  Alcotest.(check (option string)) "Get key with empty value list" None result

let test_get_first_of_multiple () =
  let params = [ "color", [ "red" ]; "color", [ "blue" ] ] in
  let result = Form.Urlencoded.get "color" params in
  Alcotest.(check (option string))
    "Get first value when multiple exist"
    (Some "red")
    result

let test_get_list_single_value () =
  let params = [ "name", [ "John" ]; "age", [ "30" ] ] in
  let result = Form.Urlencoded.get_list "name" params in
  Alcotest.(check (list string)) "Get list with single value" [ "John" ] result

let test_get_list_multiple_values () =
  let params =
    [ "color", [ "red" ]
    ; "name", [ "John" ]
    ; "color", [ "blue" ]
    ; "color", [ "green" ]
    ]
  in
  let result = Form.Urlencoded.get_list "color" params in
  Alcotest.(check (list string))
    "Get list with multiple values across entries"
    [ "red"; "blue"; "green" ]
    result

let test_get_list_missing_key () =
  let params = [ "name", [ "John" ] ] in
  let result = Form.Urlencoded.get_list "age" params in
  Alcotest.(check (list string)) "Get list for missing key" [] result

let test_get_list_normalized () =
  let params =
    [ "color", [ "red" ]; "color", [ "blue" ] ] |> Form.Urlencoded.normalize
  in
  let result = Form.Urlencoded.get_list "color" params in
  Alcotest.(check (list string))
    "Get list from normalized params"
    [ "red"; "blue" ]
    result

let test_django_style_workflow () =
  (* Simulate receiving ?color=red&color=blue&name=John&tags=ocaml&tags=web *)
  let raw_params =
    Form.Urlencoded.of_string
      "color=red&color=blue&name=John&tags=ocaml&tags=web"
  in
  let normalized = Form.Urlencoded.normalize raw_params in

  let name = Form.Urlencoded.get "name" normalized in
  let colors = Form.Urlencoded.get_list "color" normalized in
  let tags = Form.Urlencoded.get_list "tags" normalized in

  Alcotest.(check (option string)) "Get single value" (Some "John") name;
  Alcotest.(check (list string)) "Get color list" [ "red"; "blue" ] colors;
  Alcotest.(check (list string)) "Get tags list" [ "ocaml"; "web" ] tags

(* Multipart tests *)

let test_multipart_get_part_from_empty () =
  let fields : Form.Multipart.t = [] in
  let result = Form.Multipart.get_part "name" fields in
  Alcotest.(check (option reject))
    "Get part from empty list"
    None
    (Option.map (fun _ -> ()) result)

let test_multipart_get_part_existing () =
  let fields : Form.Multipart.t =
    [ ( "name"
      , { name = "name"
        ; filename = None
        ; content_type = "text/plain"
        ; body = Piaf.Body.of_string "John"
        } )
    ]
  in
  let result = Form.Multipart.get_part "name" fields in
  match result with
  | Some part ->
    Alcotest.(check string) "Name matches" "name" part.name;
    Alcotest.(check (option string)) "No filename" None part.filename
  | None -> Alcotest.fail "Expected to find part"

let test_multipart_get_field_from_empty () =
  let fields : Form.Multipart.t = [] in
  let result = Form.Multipart.get_field "name" fields in
  Alcotest.(check (option reject))
    "Get field from empty list"
    None
    (Option.map (fun _ -> ()) result)

let test_multipart_get_field_existing () =
  let fields : Form.Multipart.t =
    [ ( "name"
      , { name = "name"
        ; filename = None
        ; content_type = "text/plain"
        ; body = Piaf.Body.of_string "John"
        } )
    ]
  in
  let result = Form.Multipart.get_field "name" fields in
  match result with
  | Some (Ok value) -> Alcotest.(check string) "Value matches" "John" value
  | Some (Error _) -> Alcotest.fail "Failed to read field"
  | None -> Alcotest.fail "Expected to find field"

let test_multipart_get_field_missing () =
  let fields : Form.Multipart.t =
    [ ( "name"
      , { name = "name"
        ; filename = None
        ; content_type = "text/plain"
        ; body = Piaf.Body.of_string "John"
        } )
    ]
  in
  let result = Form.Multipart.get_field "age" fields in
  Alcotest.(check (option reject))
    "Get missing field"
    None
    (Option.map (fun _ -> ()) result)

let test_multipart_get_all_parts () =
  let fields : Form.Multipart.t =
    [ ( "tag"
      , { name = "tag"
        ; filename = None
        ; content_type = "text/plain"
        ; body = Piaf.Body.of_string "ocaml"
        } )
    ; ( "name"
      , { name = "name"
        ; filename = None
        ; content_type = "text/plain"
        ; body = Piaf.Body.of_string "John"
        } )
    ; ( "tag"
      , { name = "tag"
        ; filename = None
        ; content_type = "text/plain"
        ; body = Piaf.Body.of_string "web"
        } )
    ]
  in
  let result = Form.Multipart.get_all_parts "tag" fields in
  Alcotest.(check int) "Number of parts" 2 (List.length result)

let test_multipart_get_all_fields () =
  let fields : Form.Multipart.t =
    [ ( "tag"
      , { name = "tag"
        ; filename = None
        ; content_type = "text/plain"
        ; body = Piaf.Body.of_string "ocaml"
        } )
    ; ( "name"
      , { name = "name"
        ; filename = None
        ; content_type = "text/plain"
        ; body = Piaf.Body.of_string "John"
        } )
    ; ( "tag"
      , { name = "tag"
        ; filename = None
        ; content_type = "text/plain"
        ; body = Piaf.Body.of_string "web"
        } )
    ]
  in
  let result = Form.Multipart.get_all_fields "tag" fields in
  match result with
  | Ok values ->
    Alcotest.(check (list string)) "Values match" [ "ocaml"; "web" ] values
  | Error _ -> Alcotest.fail "Failed to read fields"

let test_multipart_get_all_fields_empty () =
  let fields : Form.Multipart.t =
    [ ( "name"
      , { name = "name"
        ; filename = None
        ; content_type = "text/plain"
        ; body = Piaf.Body.of_string "John"
        } )
    ]
  in
  let result = Form.Multipart.get_all_fields "tag" fields in
  match result with
  | Ok values -> Alcotest.(check (list string)) "Empty list" [] values
  | Error _ -> Alcotest.fail "Should return empty list"

let test_multipart_drain_empty () =
  let fields : Form.Multipart.t = [] in
  let result = Form.Multipart.drain fields in
  Alcotest.(check (result unit (of_pp (fun ppf _ -> Format.fprintf ppf "Msg"))))
    "Drain empty list"
    (Ok ())
    result

let test_multipart_drain_with_parts () =
  let fields : Form.Multipart.t =
    [ ( "name"
      , { name = "name"
        ; filename = None
        ; content_type = "text/plain"
        ; body = Piaf.Body.of_string "John"
        } )
    ; ( "avatar"
      , { name = "avatar"
        ; filename = Some "photo.jpg"
        ; content_type = "image/jpeg"
        ; body = Piaf.Body.of_string "fake image data"
        } )
    ]
  in
  let result = Form.Multipart.drain fields in
  Alcotest.(check (result unit (of_pp (fun ppf _ -> Format.fprintf ppf "Msg"))))
    "Drain successfully"
    (Ok ())
    result

let tests =
  List.map
    (fun (name, cases) -> Format.asprintf "Form: %s" name, cases)
    [ ( "of_string tests"
      , [ Alcotest.test_case "Empty string" `Quick test_of_string_empty
        ; Alcotest.test_case
            "Single parameter"
            `Quick
            test_of_string_single_param
        ; Alcotest.test_case
            "Multiple parameters"
            `Quick
            test_of_string_multiple_params
        ; Alcotest.test_case
            "Multiple values for same key"
            `Quick
            test_of_string_multiple_values
        ; Alcotest.test_case
            "URL-encoded characters"
            `Quick
            test_of_string_url_encoded_chars
        ; Alcotest.test_case "Empty value" `Quick test_of_string_empty_value
        ; Alcotest.test_case "Key without value" `Quick test_of_string_no_value
        ; Alcotest.test_case "Plus as space" `Quick test_of_string_plus_as_space
        ; Alcotest.test_case "Complex form" `Quick test_of_string_complex
        ] )
    ; ( "of_body tests"
      , [ Alcotest.test_case
            "Parse body successfully"
            `Quick
            test_of_body_success
        ; Alcotest.test_case "Parse empty body" `Quick test_of_body_empty
        ] )
    ; ( "of_query tests"
      , [ Alcotest.test_case "No query string" `Quick test_of_query_empty
        ; Alcotest.test_case
            "Single query parameter"
            `Quick
            test_of_query_single_param
        ; Alcotest.test_case
            "Multiple query parameters"
            `Quick
            test_of_query_multiple_params
        ; Alcotest.test_case
            "Multiple values for same parameter"
            `Quick
            test_of_query_multiple_values
        ; Alcotest.test_case
            "Encoded characters"
            `Quick
            test_of_query_encoded_chars
        ] )
    ; ( "normalize tests"
      , [ Alcotest.test_case "Empty list" `Quick test_normalize_empty
        ; Alcotest.test_case "No duplicates" `Quick test_normalize_single_key
        ; Alcotest.test_case
            "Duplicate keys"
            `Quick
            test_normalize_duplicate_keys
        ; Alcotest.test_case
            "Preserves value order"
            `Quick
            test_normalize_preserves_order_within_key
        ] )
    ; ( "get tests"
      , [ Alcotest.test_case "Existing key" `Quick test_get_existing_key
        ; Alcotest.test_case "Missing key" `Quick test_get_missing_key
        ; Alcotest.test_case "Empty value" `Quick test_get_empty_value
        ; Alcotest.test_case
            "First of multiple"
            `Quick
            test_get_first_of_multiple
        ] )
    ; ( "get_list tests"
      , [ Alcotest.test_case "Single value" `Quick test_get_list_single_value
        ; Alcotest.test_case
            "Multiple values"
            `Quick
            test_get_list_multiple_values
        ; Alcotest.test_case "Missing key" `Quick test_get_list_missing_key
        ; Alcotest.test_case
            "From normalized params"
            `Quick
            test_get_list_normalized
        ] )
    ; ( "integration tests"
      , [ Alcotest.test_case
            "Django-style workflow"
            `Quick
            test_django_style_workflow
        ] )
    ; ( "multipart tests"
      , [ Alcotest.test_case
            "Get part from empty"
            `Quick
            test_multipart_get_part_from_empty
        ; Alcotest.test_case
            "Get part existing"
            `Quick
            test_multipart_get_part_existing
        ; Alcotest.test_case
            "Get field from empty"
            `Quick
            test_multipart_get_field_from_empty
        ; Alcotest.test_case
            "Get field existing"
            `Quick
            test_multipart_get_field_existing
        ; Alcotest.test_case
            "Get field missing"
            `Quick
            test_multipart_get_field_missing
        ; Alcotest.test_case "Get all parts" `Quick test_multipart_get_all_parts
        ; Alcotest.test_case
            "Get all fields"
            `Quick
            test_multipart_get_all_fields
        ; Alcotest.test_case
            "Get all fields empty"
            `Quick
            test_multipart_get_all_fields_empty
        ; Alcotest.test_case "Drain empty" `Quick test_multipart_drain_empty
        ; Alcotest.test_case
            "Drain with parts"
            `Quick
            test_multipart_drain_with_parts
        ] )
    ]
