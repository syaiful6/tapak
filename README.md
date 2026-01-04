# Tapak

> ⚠️ **Under Heavy Development**: This library is in active development and
> the API is not yet stable. Breaking changes may occur between releases.
> Not recommended for production use at this time.

Tapak is a modern, type-safe, contract-first web framework for OCaml 5,
built on EIO. It combines direct-style concurrency with uncompromising type safety
and OpenAPI generation.

## Documentation

📖 [Read the full documentation](https://syaiful6.github.io/tapak/)

## Rationale and Motivation

The OCaml ecosystem has several excellent web frameworks, each with different design philosophies:

| Framework                                              | Async Runtime     | Routing            | Input Validation        | OpenAPI Generation  | Status           |
| ------------------------------------------------------ | ----------------- | ------------------ | ----------------------- | ------------------- | ---------------- |
| **[Dream](https://camlworks.github.io/dream/)**        | Lwt               | String patterns    | Manual                  | No                  | Stable\*         |
| **[Opium](https://github.com/rgrinberg/opium)**        | Lwt               | String patterns    | Manual                  | No                  | Stable           |
| **[Eliom](https://ocsigen.org/eliom/)**                | Lwt               | Type-safe          | Yes                     | No                  | Stable           |
| **[tiny_httpd](https://github.com/c-cube/tiny_httpd)** | Threads           | GADT type-safe     | Manual                  | No                  | Stable\*         |
| **Tapak**                                              | **EIO (OCaml 5)** | **GADT type-safe** | **Yes, contract-first** | Derived from routes | **Experimental** |

- - Dream is stable but still evolving, it hasn't reached v1.0 yet.
- - Tiny_httpd looks stable, but I haven't used it personally.

### What you get

- [Type-level routing](./examples/showcase/main.ml): Define your handlers as taking inputs, we will
  extract and parse them from the request path, query parameters, cookies and headers for you, all without
  sacrificing type-safety.
- [Request validation](./examples/body-parsing/main.ml): Define a schema once, get parsing, validation and error responses
  for free.
- [Realtime communication](./examples/live-cursors/main.ml): Built-in support for WebSockets, and [Server-Sent Events](./examples/sse-chat/main.ml)
  (SSE) for building interactive, real-time web applications.
- [OpenAPI documentation](./examples/openapi/main.ml): Tapak generates OpenAPI documentation from
  your route definitions. Ensuring your API documentation are always accurate and up-to-date.

## Installation

Tapak is not on Opam yet (soon!). Installl it via pinning or use Nix.

```bash
# Pin from the main branch
opam pin add tapak https://github.com/syaiful6/tapak.git

# Or pin from a specific branch
opam pin add tapak https://github.com/syaiful6/tapak.git#branch-name

# Install dependencies and build
opam install tapak --deps-only
```

### Using Nix Flakes

Add Tapak as a flake input in your `flake.nix`:

```nix
{
  inputs = {
    tapak.url = "github:syaiful6/tapak";
    # Or specify a branch:
    # tapak.url = "github:syaiful6/tapak/main";
  };

  outputs = { self, tapak, ... }: {
    # Use tapak in your development environment or package build
    # you know how to do this, you use nix btw.
  };
}
```

## Quick Start

Here's a simple example demonstrating type-safe routing, middleware composition, and EIO integration:

```ocaml
let home_handler () =
  Tapak.html ~status:`OK "<h1>Welcome to Tapak!</h1>"

let user_handler user_id =
  (* user_id is automatically parsed as int64 *)
  let html = Printf.sprintf "<h1>User %Ld</h1>" user_id in
  Tapak.html ~status:`OK html

let api_handler id name =
  (* Both id (int64) and name (string) are type-safe *)
  let json =
    `Assoc [("user_id", `String (Int64.to_string id)); ("name", `String name)]
  in
  Tapak.json ~status:`OK json

let app env =
  let now () = Eio.Time.now (Eio.Stdenv.clock env) in
  Tapak.(
    Router.(
      routes
        [ get (s "") |> unit |> into home_handler
        ; get (s "users" / int64) |> into user_handler
        ; get (s "api" / s "users" / int64 / str) |> into api_handler
        ])
    |> use
         (module Middleware.Request_logger)
         (Middleware.Request_logger.args ~now ()))

let () =
  Eio_main.run @@ fun env ->
  let port = 3000 in
  let address = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let config = Piaf.Server.Config.create address in
  Printf.printf "Server running on http://localhost:%d\n" port;
  ignore (Tapak.run_with ~config ~env (app env))
```

For more examples, see the [`examples/`](./examples) directory.
