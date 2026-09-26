# Sch

**Sch** is a schema definition and validation library for OCaml. You define schemas once as values, and get validation,
JSON encoding/decoding, and [JSON Schema](https://json-schema.org/) output for free.

Schemas describe the *structure and constraints* of your data, independently of any serialisation format. 
JSON support is built-in, but the schema layer itself is not JSON-specific.

## Features

- **Validation**: structured error accumulation with field paths
- **JSON codec**: encode and decode OCaml values to/from JSON
- **JSON Schema output**: generate standard JSON Schema (Draft4 – Draft2020-12) from any schema
- **Constraints**: min/max length, numeric ranges, regex patterns, email/UUID/date formats, and more
- **Composable**: records, unions, lists, maps, optional fields, recursive types, custom codecs

## Installation

```bash
opam install sch
```

## Quick Start

### 1. Define a schema

```ocaml
type user =
  { name : string
  ; email : string
  ; age : int
  }

let user_schema =
  Sch.Object.(
    define
    @@ let+ name =
         mem
           ~enc:(fun u -> u.name)
           "name"
           Sch.(with_ ~constraint_:(Constraint.min_length 1) string)
       and+ email =
         mem
           ~enc:(fun u -> u.email)
           "email"
           Sch.(with_ ~constraint_:(Constraint.format `Email) string)
       and+ age =
         mem
           ~enc:(fun u -> u.age)
           "age"
           Sch.(with_ ~constraint_:(Constraint.int_min 18) int)
       in
       { name; email; age })
```

### 2. Validate and decode

```ocaml
let () =
  let json = {|{"name": "Alice", "email": "alice@example.com", "age": 30}|} in
  match Sch.Json.decode_string user_schema json |> Sch.Validation.to_result with
  | Ok user -> Printf.printf "Hello, %s!\n" user.name
  | Error errors ->
    List.iter (fun (path, msg) -> Printf.printf "%s: %s\n" path msg) errors
```

Validation errors accumulate across all fields. You get every problem in one pass, not just the first failure:

```ocaml
let bad_json = {|{"name": "", "email": "not-an-email", "age": 16}|}
(* Error:
   name: String length must be at least 1
   email: Invalid email format
   age: Value must be at least 18  *)
```

### 3. Encode to JSON

```ocaml
let alice = { name = "Alice"; email = "alice@example.com"; age = 30 }
let json_string = Sch.Json.encode_string user_schema alice
(* {"name":"Alice","email":"alice@example.com","age":30} *)
```

### 4. Generate JSON Schema

```ocaml
let () =
  match Sch.to_json_schema user_schema |> Sch.Json_schema.to_string with
  | Ok s -> print_string s
  | Error e -> print_string (Jsont.Error.to_string e)
```

A specific draft can be requested:

```ocaml
let json_schema =
  Sch.to_json_schema ~draft:Sch.Json_schema.Draft.Draft2020_12 user_schema
```

The generated schema can be served as an API contract or used with any standard JSON Schema validator.

## Constraints

Constraints are composable and type-safe:

```ocaml
(* Strings *)
Sch.(with_ ~constraint_:(Constraint.min_length 3) string)
Sch.(with_ ~constraint_:(Constraint.format `Email) string)
Sch.(with_ ~constraint_:(Constraint.pattern "^[a-z]+$") string)

(* Numbers *)
Sch.(with_ ~constraint_:(Constraint.int_min 0) int)
Sch.(with_ ~constraint_:(Constraint.int_range 1 100) int)
Sch.(with_ ~constraint_:(Constraint.float_max 1.0) float)

(* Lists *)
Sch.(with_ ~constraint_:(Constraint.min_items 1) (list string))
Sch.(with_ ~constraint_:(Constraint.unique_items) (list int))

(* Combining *)
Sch.(with_ ~constraint_:(Constraint.all_of [min_length 8; max_length 64]) string)
```

## Unions

`Sch.Union` defines schemas for sum types. Each `Sch.Union.case` maps one
constructor to its payload schema:

```ocaml
type shape =
  | Circle of float
  | Rectangle of float * float

let circle_schema = Sch.Object.(define @@ mem "radius" Sch.float)

let rectangle_schema =
  Sch.Object.(
    define
    @@ let+ width = mem "width" Sch.float
       and+ height = mem "height" Sch.float in
       width, height)

let shape_schema =
  Sch.Union.(
    define
      ~discriminator:"type"
      [ case
          ~tag:"circle"
          ~inj:(fun r -> Circle r)
          ~proj:(function Circle r -> Some r | _ -> None)
          circle_schema
      ; case
          ~tag:"rectangle"
          ~inj:(fun (w, h) -> Rectangle (w, h))
          ~proj:(function Rectangle (w, h) -> Some (w, h) | _ -> None)
          rectangle_schema
      ])
```

`Sch.Union.define` writes a discriminator field (`"type"` above) alongside the
case payload:

```json
{ "type": "rectangle", "width": 3, "height": 4 }
```

### Tagless unions

Use `Sch.Union.tagless` when the JSON shape identifies the alternative. For
example, a value may be either a string or a list of strings. The `~tag` names
appear in errors but not in the encoded JSON:

```ocaml
type payload =
  | Text of string
  | Items of string list

let payload_schema =
  Sch.Union.(
    tagless
      [ case
          ~tag:"text"
          ~inj:(fun s -> Text s)
          ~proj:(function Text s -> Some s | _ -> None)
          Sch.string
      ; case
          ~tag:"items"
          ~inj:(fun xs -> Items xs)
          ~proj:(function Items xs -> Some xs | _ -> None)
          Sch.(list string)
      ])
```

```ocaml
Sch.Json.encode_string payload_schema (Text "hello")        (* "hello" *)
Sch.Json.encode_string payload_schema (Items [ "a"; "b" ])   (* ["a","b"] *)
```

The decoder tries every case and accepts one match, following JSON Schema and
OpenAPI `oneOf` semantics. When no case matches, it reports each case's errors
under that case's tag. Multiple matches produce an ambiguity error such as
`Ambiguous tagless union: matched cases user, admin`. The encoder also requires
one matching projection. Like `Sch.Union.define`, `Sch.Union.tagless` rejects
empty case lists and duplicate tags.

The generated JSON Schema contains `oneOf` without a discriminator or top-level
`type`:

```json
{
  "oneOf": [
    { "type": "string" },
    { "type": "array", "items": { "type": "string" } }
  ]
}
```

## Custom codecs

Use `Sch.custom` to adapt any schema to a different OCaml type:

```ocaml
(* Parse a "YYYY-MM-DD" string into a Ptime.t *)
let date_schema =
  Sch.custom
    ~enc:Ptime.to_rfc3339
    ~dec:(fun s ->
      match Ptime.of_rfc3339 s with
      | Ok (t, _, _) -> Ok t
      | Error _ -> Error ["Invalid date format"])
    Sch.string
```

## Used with Tapak

Sch is the validation engine for [Tapak](../../README.md), an OCaml web framework. Route definitions use Sch schemas for
request body parsing, query parameters, and OpenAPI generation.

See the Tapak examples for real-world usage:

- [`examples/body-parsing/`](../../examples/body-parsing/main.ml) — JSON and form body validation in HTTP handlers
- [`examples/openapi/`](../../examples/openapi/main.ml) — OpenAPI spec generation from Sch schemas
