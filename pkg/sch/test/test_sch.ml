module Json_schema = Sch.Json_schema

(* schema tests *)
let test_schema_single_field () =
  let schema = Sch.Object.(define @@ Sch.Object.mem "name" Sch.string) in
  let result =
    Sch.Json_decoder.decode_string schema {|{"name": "Alice"}|}
    |> Sch.Validation.to_result
  in
  Alcotest.(check (result string (list (pair string string))))
    "parse single field"
    (Ok "Alice")
    result

let test_schema_multiple_fields () =
  let schema =
    Sch.Object.(
      define
      @@ let+ name = mem "name" Sch.string
         and+ age = mem "age" Sch.int in
         name, age)
  in
  let result =
    Sch.Json_decoder.decode_string schema {|{"name": "Alice", "age": 30}|}
    |> Sch.Validation.to_result
  in
  Alcotest.(check (result (pair string int) (list (pair string string))))
    "parse multiple fields"
    (Ok ("Alice", 30))
    result

(* json schema *)
let test_json_type () =
  let value = Json_schema.Json_type.(union string number) in
  Alcotest.(check bool)
    "Json_type.contains"
    true
    (Json_schema.Json_type.contains value String)

let geoschema =
  {|
{
  "$id": "https://example.com/geographical-location.schema.json",
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "title": "Longitude and Latitude Values",
  "description": "A geographical coordinate.",
  "required": [ "latitude", "longitude" ],
  "type": "object",
  "properties": {
    "latitude": {
      "type": "number",
      "minimum": -90,
      "maximum": 90
    },
    "longitude": {
      "type": "number",
      "minimum": -180,
      "maximum": 180
    }
  }
}
|}

let test_jsonschema_parsing () =
  let v = Json_schema.of_string geoschema in
  Alcotest.(check bool) "Json_schema.parse" true (Result.is_ok v);
  match v with
  | Ok schema ->
    Alcotest.(check (option string))
      "Json_schema.id"
      (Some "https://example.com/geographical-location.schema.json")
      schema.Json_schema.id;
    Alcotest.(check (option string))
      "Json_schema.title"
      (Some "Longitude and Latitude Values")
      schema.Json_schema.title
  | Error _ -> Alcotest.fail "Failed to parse schema"

(** {1 Encoder tests} *)

let test_encode_string () =
  let result = Sch.Json_encoder.encode_string Sch.string "hello" in
  Alcotest.(check string) "encode string" {|"hello"|} result

let test_encode_int () =
  let result = Sch.Json_encoder.encode_string Sch.int 42 in
  Alcotest.(check string) "encode int" "42" result

let test_encode_bool () =
  let result = Sch.Json_encoder.encode_string Sch.bool true in
  Alcotest.(check string) "encode bool" "true" result

let test_encode_list () =
  let result = Sch.Json_encoder.encode_string (Sch.list Sch.int) [ 1; 2; 3 ] in
  Alcotest.(check string) "encode list" "[1,2,3]" result

let test_encode_object_minified () =
  let schema =
    Sch.Object.(
      define
      @@ let+ name = mem "name" ~enc:Stdlib.fst Sch.string
         and+ age = mem "age" ~enc:Stdlib.snd Sch.int in
         name, age)
  in
  let result = Sch.Json_encoder.encode_string schema ("Alice", 30) in
  Alcotest.(check string)
    "encode object minified"
    {|{"name":"Alice","age":30}|}
    result

let test_encode_object_indented () =
  let schema =
    Sch.Object.(
      define
      @@ let+ name = mem "name" ~enc:Stdlib.fst Sch.string
         and+ age = mem "age" ~enc:Stdlib.snd Sch.int in
         name, age)
  in
  let result =
    Sch.Json_encoder.encode_string ~format:(Indent 2) schema ("Alice", 30)
  in
  let expected = "{\n  \"name\": \"Alice\",\n  \"age\": 30\n}" in
  Alcotest.(check string) "encode object indented" expected result

let test_encode_nested_indented () =
  let inner =
    Sch.Object.(
      define
      @@ let+ x = mem "x" ~enc:Stdlib.fst Sch.int
         and+ y = mem "y" ~enc:Stdlib.snd Sch.int in
         x, y)
  in
  let outer =
    Sch.Object.(
      define
      @@ let+ name = mem "name" ~enc:Stdlib.fst Sch.string
         and+ pos = mem "pos" ~enc:Stdlib.snd inner in
         name, pos)
  in
  let result =
    Sch.Json_encoder.encode_string ~format:(Indent 2) outer ("thing", (1, 2))
  in
  let expected =
    "{\n\
    \  \"name\": \"thing\",\n\
    \  \"pos\": {\n\
    \    \"x\": 1,\n\
    \    \"y\": 2\n\
    \  }\n\
     }"
  in
  Alcotest.(check string) "encode nested indented" expected result

let test_encode_empty_list () =
  let result =
    Sch.Json_encoder.encode_string ~format:(Indent 2) (Sch.list Sch.int) []
  in
  Alcotest.(check string) "encode empty list" "[]" result

let test_encode_list_indented () =
  let result =
    Sch.Json_encoder.encode_string
      ~format:(Indent 2)
      (Sch.list Sch.int)
      [ 1; 2; 3 ]
  in
  let expected = "[1, 2, 3]" in
  Alcotest.(check string) "encode list indented" expected result

let test_encode_list_of_objects_indented () =
  let item =
    Sch.Object.(
      define
      @@ let+ x = mem "x" ~enc:Stdlib.fst Sch.int
         and+ y = mem "y" ~enc:Stdlib.snd Sch.int in
         x, y)
  in
  let result =
    Sch.Json_encoder.encode_string
      ~format:(Indent 2)
      (Sch.list item)
      [ 1, 2; 3, 4 ]
  in
  let expected =
    "[\n\
    \  {\n\
    \    \"x\": 1,\n\
    \    \"y\": 2\n\
    \  },\n\
    \  {\n\
    \    \"x\": 3,\n\
    \    \"y\": 4\n\
    \  }\n\
     ]"
  in
  Alcotest.(check string) "list of objects indented" expected result

(** Float / Double precision *)

let test_encode_float () =
  let result = Sch.Json_encoder.encode_string Sch.float 42.5 in
  Alcotest.(check string) "encode float" "42.5" result

let test_encode_double () =
  let result = Sch.Json_encoder.encode_string Sch.double 42.5 in
  Alcotest.(check string) "encode double" "42.5" result

let test_encode_float_precision () =
  let result = Sch.Json_encoder.encode_string Sch.float Float.pi in
  Alcotest.(check string)
    "float uses %.7g (7 significant digits)"
    "3.141593"
    result

let test_encode_double_precision () =
  let result = Sch.Json_encoder.encode_string Sch.double Float.pi in
  Alcotest.(check string)
    "double uses %.15g (15 significant digits)"
    "3.14159265358979"
    result

let test_encode_float_nonfinite () =
  let check_null label v =
    let result = Sch.Json_encoder.encode_string Sch.float v in
    Alcotest.(check string) label "null" result
  in
  check_null "float infinity" infinity;
  check_null "float neg_infinity" neg_infinity;
  check_null "float nan" nan

let test_encode_double_nonfinite () =
  let check_null label v =
    let result = Sch.Json_encoder.encode_string Sch.double v in
    Alcotest.(check string) label "null" result
  in
  check_null "double infinity" infinity;
  check_null "double neg_infinity" neg_infinity;
  check_null "double nan" nan

(** Int32 / Int64 *)

let test_encode_int32 () =
  let result = Sch.Json_encoder.encode_string Sch.int32 42l in
  Alcotest.(check string) "encode int32" "42" result

let test_encode_int64 () =
  let result = Sch.Json_encoder.encode_string Sch.int64 42L in
  Alcotest.(check string) "int64 encodes as JSON string" {|"42"|} result

let test_encode_int64_large () =
  let result = Sch.Json_encoder.encode_string Sch.int64 9223372036854775807L in
  Alcotest.(check string)
    "large int64 safe as JSON string"
    {|"9223372036854775807"|}
    result

(** Option *)

let test_encode_option_none () =
  let result = Sch.Json_encoder.encode_string (Sch.option Sch.int) None in
  Alcotest.(check string) "none encodes as null" "null" result

let test_encode_option_some () =
  let result = Sch.Json_encoder.encode_string (Sch.option Sch.int) (Some 42) in
  Alcotest.(check string) "some encodes inner value" "42" result

(** Password *)

let test_encode_password () =
  let result = Sch.Json_encoder.encode_string Sch.password "secret" in
  Alcotest.(check string) "encode password" {|"secret"|} result

(** Iso (custom codec) *)

let test_encode_iso () =
  let bool_str =
    Sch.custom
      ~enc:(fun b -> if b then "yes" else "no")
      ~dec:(fun s ->
        match s with
        | "yes" -> Ok true
        | "no" -> Ok false
        | _ -> Error [ "invalid" ])
      Sch.string
  in
  let result_t = Sch.Json_encoder.encode_string bool_str true in
  let result_f = Sch.Json_encoder.encode_string bool_str false in
  Alcotest.(check string) "iso true -> yes" {|"yes"|} result_t;
  Alcotest.(check string) "iso false -> no" {|"no"|} result_f

(** Rec *)

let test_encode_rec () =
  (* TODO: use a more complex recursive schema *)
  let codec = Sch.rec' (lazy Sch.int) in
  let result = Sch.Json_encoder.encode_string codec 42 in
  Alcotest.(check string) "rec forces and encodes" "42" result

(** String escaping *)

let test_encode_string_escaping () =
  let check label input expected =
    let result = Sch.Json_encoder.encode_string Sch.string input in
    Alcotest.(check string) label expected result
  in
  check "newline" "hello\nworld" {|"hello\nworld"|};
  check "tab" "a\tb" {|"a\tb"|};
  check "carriage return" "a\rb" {|"a\rb"|};
  check "backslash" "a\\b" {|"a\\b"|};
  check "quotes" "he said \"hi\"" {|"he said \"hi\""|};
  check "control \\x00" "\x00" "\"\\u0000\"";
  check "control \\x1f" "\x1f" "\"\\u001f\"";
  check "DEL \\x7f" "\x7f" "\"\\u007f\""

(** Omit *)

let omit_schema =
  Sch.Object.(
    define
    @@ let+ name = mem "name" ~enc:Stdlib.fst Sch.string
       and+ tag =
         mem "tag" ~enc:Stdlib.snd ~omit:Option.is_none (Sch.option Sch.string)
       in
       name, tag)

let test_encode_omit_skips () =
  let result = Sch.Json_encoder.encode_string omit_schema ("Alice", None) in
  Alcotest.(check string) "omitted field absent" {|{"name":"Alice"}|} result

let test_encode_omit_present () =
  let result =
    Sch.Json_encoder.encode_string omit_schema ("Alice", Some "admin")
  in
  Alcotest.(check string)
    "non-omitted field present"
    {|{"name":"Alice","tag":"admin"}|}
    result

(** { 1 Schema Decoder Test Suites } *)
let schema_decoder_tests =
  [ "schema single field", `Quick, test_schema_single_field
  ; "schema multiple fields", `Quick, test_schema_multiple_fields
  ]

(** { 1 Test Suites } *)
let json_schema_tests =
  [ "json type", `Quick, test_json_type
  ; "Parsing", `Quick, test_jsonschema_parsing
  ]

(** { 1 Schema Encoder Test Suites } *)
let encoder_tests =
  [ "encode string", `Quick, test_encode_string
  ; "encode int", `Quick, test_encode_int
  ; "encode bool", `Quick, test_encode_bool
  ; "encode float", `Quick, test_encode_float
  ; "encode double", `Quick, test_encode_double
  ; "float precision", `Quick, test_encode_float_precision
  ; "double precision", `Quick, test_encode_double_precision
  ; "float non-finite", `Quick, test_encode_float_nonfinite
  ; "double non-finite", `Quick, test_encode_double_nonfinite
  ; "encode int32", `Quick, test_encode_int32
  ; "encode int64", `Quick, test_encode_int64
  ; "int64 large value", `Quick, test_encode_int64_large
  ; "encode password", `Quick, test_encode_password
  ; "option none", `Quick, test_encode_option_none
  ; "option some", `Quick, test_encode_option_some
  ; "iso codec", `Quick, test_encode_iso
  ; "rec codec", `Quick, test_encode_rec
  ; "string escaping", `Quick, test_encode_string_escaping
  ; "encode list", `Quick, test_encode_list
  ; "encode object minified", `Quick, test_encode_object_minified
  ; "encode object indented", `Quick, test_encode_object_indented
  ; "encode nested indented", `Quick, test_encode_nested_indented
  ; "encode empty list", `Quick, test_encode_empty_list
  ; "encode list indented", `Quick, test_encode_list_indented
  ; "list of objects indented", `Quick, test_encode_list_of_objects_indented
  ; "omit field skipped", `Quick, test_encode_omit_skips
  ; "omit field present", `Quick, test_encode_omit_present
  ]

let () =
  Alcotest.run
    "Sch"
    [ "Json_schema", json_schema_tests
    ; "Decoder", schema_decoder_tests
    ; "Encoder", encoder_tests
    ]
