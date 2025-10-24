# tapak-ppx

PPX extension for type-safe route generation in the Tapak web framework.

## Status

⚠️ **Work in Progress** - This PPX is in early development.

## Installation

```bash
opam install tapak-ppx
```

## Usage

Add `tapak-ppx` to your dune preprocess field:

```dune
(executable
 (name app)
 (libraries tapak)
 (preprocess
  (pps tapak-ppx)))
```

## Example

Define handlers with route annotations:

```ocaml
open Tapak

let home _request =
  Response.of_string' "Welcome!"
[@@route GET, "/"]

let get_user ~id _request =
  Response.of_string' (Printf.sprintf "User ID: %d" id)
[@@route GET, "/users/:id"]

(* tapak-ppx generates both path and route variables automatically *)

(* Generated path variables for URL generation: *)
let user_url = Router.sprintf get_user_path 123  (* => "/users/123" *)

(* Generated route variables for routing: *)
let routes = [
  home_route;
  get_user_route;
]
```

### What Gets Generated

For each handler with a `[@route]` annotation, the PPX generates:

1. **Path variable** (`handler_name_path`) - A polymorphic path for URL generation with `Router.sprintf`
2. **Route variable** (`handler_name_route`) - A complete route with the handler attached
3. **Handler wrapper** - Automatically wraps your handler to call it with named parameters

Example:

```ocaml
(* Your code: *)
let get_user ~id _request = Response.of_string' (Printf.sprintf "User %d" id)
[@@route GET, "/users/:id"]

(* Generated: *)
let get_user_path = Tapak.Router.(/) (Tapak.Router.s "users") Tapak.Router.int

let get_user_route =
  Tapak.Router.(@->)
    (Tapak.Router.get (Tapak.Router.(/) (Tapak.Router.s "users") Tapak.Router.int))
    (fun id request -> get_user ~id request)
```

## Supported HTTP Methods

- `GET`
- `POST`
- `PUT`
- `DELETE`
- `PATCH`
- `HEAD`

## Current Implementation

✅ Basic route attribute parsing
✅ HTTP method validation
✅ Route pattern validation
✅ Route generation with fully qualified names
✅ Path variable generation for URL generation
✅ Handler wrapper generation with named parameters
✅ Parameter extraction (`:param` syntax)
✅ Splat support (`*path` syntax)

## Roadmap

See [PPX_TAPAK_DESIGN.md](../../PPX_TAPAK_DESIGN.md) for the complete design.

- [ ] Type inference from function signatures (currently defaults to `int`)
- [ ] Parameter type annotations with `[@param type]`
- [ ] Multiple parameter types (int32, int64, string, bool, slug, custom)
- [ ] Middleware support with `[@middleware]`
- [ ] Better error messages with location context
- [ ] Scope support

## License

MIT
