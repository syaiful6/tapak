# Tapak

> ⚠️ **Under Heavy Development**: This library is in active development and
the API is not yet stable. Breaking changes may occur between releases.
Not recommended for production use at this time.

Tapak is a composable framework for building web applications in OCaml.
Inspired by [Twitter's Finagle](https://twitter.github.io/finagle/), it provides a modular and
extensible architecture for building scalable web services.

## Documentation

📖 [Read the full documentation](https://syaiful6.github.io/tapak/)

## Rationale and Motivation

Most web frameworks in the OCaml ecosystem are built on Lwt, an older async I/O model.
With OCaml 5's effect handlers and the introduction of **EIO** (effect-based I/O),
we now have a more ergonomic and performant foundation for concurrent programming.

Tapak was created to explore whether we can build a **practical, production-ready web framework** in OCaml that:

1. **Embraces modern OCaml**: Leverages OCaml 5's effect handlers and EIO
2. **Maintains type safety**: Provides compile-time guarantees without sacrificing expressiveness
3. **Stays composable**: Adopts Finagle's proven Service/Filter pattern for maximum modularity

This is an experiment in pushing the boundaries of what's possible with typed
functional programming for web development, while keeping pragmatism in mind.

## Features

- **Composable Architecture**: Build applications from small, reusable components using the Service/Filter pattern
- **Type-Safe Routing**: GADT-based router with compile-time parameter type checking
- **Middleware Pipeline**: Composable middlewares for cross-cutting concerns
- **Request/Response Compression**: Built-in support for gzip, deflate, brotli, and zstd
- **Built on EIO**: Uses OCaml 5's effect-based I/O for excellent performance
- **Finagle-Inspired**: Adopts proven patterns from Twitter's battle-tested framework
- **PPX Support**: Optional PPX for Django/Flask-style route annotations

## Quick Start

Here's a simple example demonstrating type-safe routing, middleware composition, and EIO integration:

```ocaml
open Tapak

(* Define handlers with typed parameters *)
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

(* Run with EIO *)
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
- **Composable middlewares**: Use `<++>` to add middleware chains
- **EIO integration**: Native support for OCaml 5's effect-based I/O

For more examples, see the [`examples/`](./examples) directory.
