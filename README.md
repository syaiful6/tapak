# Tapak

> ⚠️ **Under Heavy Development**: This library is in active development and
the API is not yet stable. Breaking changes may occur between releases.
Not recommended for production use at this time.

Tapak is a composable web framework for OCaml 5, built on EIO (effect-based I/O)
and inspired by [Twitter's Finagle](https://twitter.github.io/finagle/) architecture.

## Documentation

📖 [Read the full documentation](https://syaiful6.github.io/tapak/)

## Rationale and Motivation

The OCaml ecosystem has several excellent web frameworks, each with different design philosophies:

| Framework | Async Runtime | Routing | Philosophy | Status |
|-----------|---------------|---------|------------|--------|
| **[Dream](https://camlworks.github.io/dream/)** | Lwt | String patterns | Batteries-included, tidy | Production-ready |
| **[Opium](https://github.com/rgrinberg/opium)** | Lwt | String patterns | Sinatra-like micro-framework | Mature |
| **[Eliom](https://ocsigen.org/eliom/)** | Lwt | Type-safe | Multi-tier, full-stack | Production-ready |
| **[tiny_httpd](https://github.com/c-cube/tiny_httpd)** | Threads | Basic (Scanf) | Minimalist, zero dependencies | Stable |
| **Tapak** | **EIO (OCaml 5)** | **GADT type-safe** | Composable Service/Filter | **Experimental** |


### Why Tapak?

With OCaml 5's effect handlers and **EIO** (effect-based I/O), we now have a foundation
for writing concurrent code that is both ergonomic (direct style, no monads) and performant
(no heap allocations for context switching).

Tapak explores what a **modern, practical web framework** looks like when built on this foundation:

1. **Embraces modern OCaml**: OCaml 5 effects + EIO for direct-style concurrent code
2. **Type-safe from the ground up**: GADT-based routing with compile-time parameter checking
3. **Composable architecture**: Finagle's proven Service/Filter pattern for modularity
4. **Focused core**: Not batteries-included, compose what you need

Unlike **Dream** or **Opium** (Lwt-based with monadic code), Tapak uses EIO for direct-style
concurrency. Unlike **Eliom** (full-stack with client/server shared code), Tapak focuses on
server-side HTTP services with maximum composability.

This is an experiment in pushing the boundaries of typed functional web development
while keeping pragmatism and real-world usage in mind.

## Features

- **Composable Architecture**: Build applications from small, reusable components using the Service/Filter pattern
- **Type-Safe Routing**: GADT-based router with compile-time parameter type checking and URL generation
- **Request Guards**: Composable request validation and data extraction with extensible error types
- **Middleware Pipeline**: Composable middleware for cross-cutting concerns (logging, compression, CSRF, etc.)
- **Request/Response Compression**: Built-in support for gzip, deflate, brotli, and zstd
- **Built on EIO**: Uses OCaml 5's effect-based I/O for direct-style concurrent code
- **Powered by Piaf**: Currently uses [Piaf](https://github.com/anmonteiro/piaf) as the HTTP server (EIO-native, HTTP/1.1 & HTTP/2)
- **Finagle-Inspired**: Adopts proven patterns from Twitter's battle-tested framework
- **PPX Support**: Optional PPX for Django/Flask-style route annotations

### Roadmap

- **Static middleware**: Static file serving with caching and range requests
- **Backend abstraction**: Make the HTTP server pluggable (currently coupled to Piaf)
- More built-in middleware and utilities as real-world usage demands emerge

## Installation

Since Tapak is still in development and the API is being designed,
it's not yet published to Opam. You can still install it using one of the following methods:

### Using Opam Pin

Pin the repository directly from GitHub:

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
open Tapak

let home_handler _req =
  Response.of_html ~status:`OK
    "<h1>Welcome to Tapak!</h1>"

let user_handler user_id _req =
  (* user_id is automatically parsed as int64 *)
  let html = Printf.sprintf "<h1>User %Ld</h1>" user_id in
  Response.of_html ~status:`OK html

let api_handler id name _req =
  (* Both id (int64) and name (string) are type-safe *)
  let json =
    Printf.sprintf {|{"user_id": %Ld, "name": "%s"}|} id name
  in
  Response.of_string ~status:`OK json

let app env =
  let open Middleware in
  let open Router in
  App.(
    routes
      [ get (s "") @-> home_handler
      ; get (s "users" / int64) @-> user_handler
      ; get (s "api" / s "users" / int64 / str) @-> api_handler
      ]
      ()
    <++> [ use ~name:"Logger" (module Request_logger)
             (Request_logger.args
               ~now:(fun () -> Eio.Time.now (Eio.Stdenv.clock env))
               ())
         ])

let () =
  Eio_main.run @@ fun env ->
  let port = 3000 in
  let address = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let config = Piaf.Server.Config.create address in
  Printf.printf "Server running on http://localhost:%d\n" port;
  ignore (Server.run_with ~config ~env (app env))
```

**Key points:**

- **Type-safe parameters**: `int64`, `str`, `bool`, `int32`, `int`, and custom types
- **Clean syntax**: Routes look like the URLs they match
- **No runtime parsing errors**: Parameter types are checked at compile time
- **Composable middleware**: Use `<++>` to add middleware chains
- **EIO integration**: Native support for OCaml 5's effect-based I/O

For more examples, see the [`examples/`](./examples) directory.
