module Json_schema = Sch.Json_schema

(* schema tests *)
let decode_str_result schema str =
  Sch.Json_decoder.decode_string schema str |> Sch.Validation.to_result

let test_decoder_schema_single_field () =
  let schema = Sch.Object.(define @@ Sch.Object.mem "name" Sch.string) in
  let result = decode_str_result schema {|{"name": "Alice"}|} in
  Alcotest.(check (result string (list (pair string string))))
    "parse single field"
    (Ok "Alice")
    result

let test_decoder_schema_multiple_fields () =
  let schema =
    Sch.Object.(
      define
      @@ let+ name = mem "name" Sch.string
         and+ age = mem "age" Sch.int in
         name, age)
  in
  let result = decode_str_result schema {|{"name": "Alice", "age": 30}|} in
  Alcotest.(check (result (pair string int) (list (pair string string))))
    "parse multiple fields"
    (Ok ("Alice", 30))
    result

let test_decoder_schema_with_defaults () =
  let schema =
    Sch.Object.(
      define
      @@ let+ name = mem "name" Sch.string
         and+ role = mem ~default:"customer" "role" Sch.string in
         name, role)
  in
  let result = decode_str_result schema {|{"name": "Bob"}|} in
  Alcotest.(check (result (pair string string) (list (pair string string))))
    "parse with default"
    (Ok ("Bob", "customer"))
    result

let test_decoder_cross_field_validation () =
  let schema_with_confirmation =
    Sch.Object.(
      define
      @@ let+ password = mem "password" Sch.string
         and+ confirm_password = mem "confirm_password" Sch.string in
         password, confirm_password)
  in
  let schema =
    Sch.custom
      ~enc:(fun pwd -> pwd, pwd)
      ~dec:(fun (pwd, conf) ->
        if String.equal pwd conf
        then Ok pwd
        else Error [ "passwords do not match" ])
      schema_with_confirmation
  in
  let valid_json = {|{"password": "secret", "confirm_password": "secret"}|} in
  let invalid_json =
    {|{"password": "secret", "confirm_password": "not_secret"}|}
  in
  Alcotest.(check (result string (list (pair string string))))
    "valid passwords confirmation"
    (Result.ok "secret")
    (decode_str_result schema valid_json);
  Alcotest.(check (result string (list (pair string string))))
    "invalid passwords confirmation"
    (Result.error [ "", "passwords do not match" ])
    (decode_str_result schema invalid_json)

let test_decoder_error_accum () =
  let schema =
    Sch.Object.(
      define ~kind:"user"
      @@
      let+ name =
        mem "name" Sch.(with_ ~constraint_:(Constraint.min_length 2) string)
      and+ email =
        mem "email" Sch.(with_ ~constraint_:(Constraint.format `Email) string)
      and+ age =
        mem "age" Sch.(with_ ~constraint_:(Constraint.int_range 17 120) int)
      in
      name, email, age)
  in
  let invalid_json =
    {|{
      "name": "A",
      "email": "invalid-email",
      "age": 15
    }|}
  in
  match decode_str_result schema invalid_json with
  | Ok _ -> Alcotest.fail "Expected validation errors"
  | Error errors ->
    let has_error field = List.exists (fun (f, _) -> f = field) errors in
    Alcotest.(check bool) "name error present" true (has_error "name");
    Alcotest.(check bool) "email error present" true (has_error "email");
    Alcotest.(check bool) "age error present" true (has_error "age")

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

(** { 1 To_json_schema tests } *)

let schema_to_json s =
  match Json_schema.to_string s with Ok s -> s | Error _ -> failwith "encode"

let test_to_json_schema_string () =
  let schema = Sch.to_json_schema Sch.string in
  let json = schema_to_json schema in
  let parsed = Json_schema.of_string json |> Result.get_ok in
  Alcotest.(check bool)
    "string type"
    true
    (Json_schema.Json_type.contains parsed.type_ String)

let test_to_json_schema_int () =
  let schema = Sch.to_json_schema Sch.int in
  let json = schema_to_json schema in
  let parsed = Json_schema.of_string json |> Result.get_ok in
  Alcotest.(check bool)
    "integer type"
    true
    (Json_schema.Json_type.contains parsed.type_ Integer);
  Alcotest.(check (option string)) "int32 format" (Some "int32") parsed.format

let test_to_json_schema_object () =
  let codec =
    Sch.Object.(
      define ~kind:"user"
      @@ let+ name = mem "name" Sch.string
         and+ age = mem "age" Sch.int in
         name, age)
  in
  let schema = Sch.to_json_schema codec in
  Alcotest.(check bool)
    "object type"
    true
    (Json_schema.Json_type.contains schema.type_ Object);
  Alcotest.(check (option (list string)))
    "required fields"
    (Some [ "name"; "age" ])
    schema.required;
  let prop_names = Option.map (List.map fst) schema.properties in
  Alcotest.(check (option (list string)))
    "property names"
    (Some [ "name"; "age" ])
    prop_names

let test_to_json_schema_optional_field () =
  let codec =
    Sch.Object.(
      define
      @@ let+ name = mem "name" Sch.string
         and+ tag = mem_opt Sch.string "tag" in
         name, tag)
  in
  let schema = Sch.to_json_schema codec in
  Alcotest.(check (option (list string)))
    "only name required"
    (Some [ "name" ])
    schema.required

let test_to_json_schema_additional_properties () =
  let codec =
    Sch.Object.(define ~unknown:Error_on_unknown @@ mem "name" Sch.string)
  in
  let schema = Sch.to_json_schema codec in
  let additional =
    match schema.additional_properties with
    | Some (Json_schema.Or_bool.Bool false) -> true
    | _ -> false
  in
  Alcotest.(check bool) "additionalProperties is false" true additional

let test_to_json_schema_list () =
  let codec = Sch.list Sch.string in
  let schema = Sch.to_json_schema codec in
  Alcotest.(check bool)
    "array type"
    true
    (Json_schema.Json_type.contains schema.type_ Array);
  let has_items = Option.is_some schema.items in
  Alcotest.(check bool) "has items" true has_items

let test_to_json_schema_option () =
  let codec = Sch.option Sch.string in
  let schema = Sch.to_json_schema codec in
  Alcotest.(check bool)
    "allows string"
    true
    (Json_schema.Json_type.contains schema.type_ String);
  Alcotest.(check bool)
    "allows null"
    true
    (Json_schema.Json_type.contains schema.type_ Null)

let test_to_json_schema_roundtrip () =
  let codec =
    Sch.Object.(
      define ~kind:"person" ~doc:"A person"
      @@ let+ name = mem "name" ~doc:"The name" Sch.string
         and+ age = mem "age" Sch.int
         and+ email = mem_opt Sch.string "email" in
         name, age, email)
  in
  let schema = Sch.to_json_schema codec in
  let json = schema_to_json schema in
  let reparsed = Json_schema.of_string json in
  Alcotest.(check bool) "roundtrip succeeds" true (Result.is_ok reparsed)

type tree =
  { value : int
  ; children : tree list
  }

let test_to_json_schema_rec () =
  let rec tree_codec =
    lazy
      Sch.Object.(
        define ~kind:"tree"
        @@ let+ value = mem "value" ~enc:(fun t -> t.value) Sch.int
           and+ children =
             mem
               "children"
               ~enc:(fun t -> t.children)
               (Sch.list (Sch.rec' tree_codec))
           in
           { value; children })
  in
  let schema = Sch.to_json_schema (Sch.rec' tree_codec) in
  (* Top level should be a $ref *)
  Alcotest.(check (option string))
    "top-level ref"
    (Some "#/$defs/tree")
    schema.ref_;
  (* Should have $defs with tree *)
  let has_tree_def =
    match schema.defs with
    | Some defs -> List.exists (fun (name, _) -> name = "tree") defs
    | None -> false
  in
  Alcotest.(check bool) "has tree in $defs" true has_tree_def

(** { 1 Schema Decoder Test Suites } *)
let schema_decoder_tests =
  [ "schema single field", `Quick, test_decoder_schema_single_field
  ; "schema multiple fields", `Quick, test_decoder_schema_multiple_fields
  ; "schema with defaults", `Quick, test_decoder_schema_with_defaults
  ; "cross-field validation", `Quick, test_decoder_cross_field_validation
  ; "schema error accumulation", `Quick, test_decoder_error_accum
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

(* { 1 Json schema generation test suites }*)
let to_json_schema_tests =
  [ "string schema", `Quick, test_to_json_schema_string
  ; "int schema", `Quick, test_to_json_schema_int
  ; "object schema", `Quick, test_to_json_schema_object
  ; "optional field", `Quick, test_to_json_schema_optional_field
  ; "additionalProperties", `Quick, test_to_json_schema_additional_properties
  ; "list schema", `Quick, test_to_json_schema_list
  ; "option schema", `Quick, test_to_json_schema_option
  ; "roundtrip", `Quick, test_to_json_schema_roundtrip
  ; "recursive schema", `Quick, test_to_json_schema_rec
  ]

let () =
  Alcotest.run
    "Sch"
    [ "Json_schema", json_schema_tests
    ; "Decoder", schema_decoder_tests
    ; "Encoder", encoder_tests
    ; "To_json_schema", to_json_schema_tests
    ]
