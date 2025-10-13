# Tapak

Tapak is a composable framework for building web applications in OCaml. Inspired by [Twitter's Finagle](https://twitter.github.io/finagle/), it provides a modular and extensible architecture for building scalable web services.

## Documentation

📖 [Read the full documentation](https://syaiful6.github.io/tapak/)

## Features

- **Composable Architecture**: Build applications from small, reusable components
- **Type-Safe Routing**: Pattern matching based router with parameter extraction and splat matching
- **Middleware Pipeline**: Chain middlewares to handle cross-cutting concerns
- **Built on EIO**: Uses OCaml 5's effect-based I/O for excellent performance
- **Finagle-Inspired**: Adopts proven patterns from Twitter's battle-tested framework

## Quick Start

```ocaml
open Tapak_kernel

let hello_handler _request =
  Response.create
    ~body:(Piaf.Body.of_string "Hello, World!")
    `OK

let routes =
  Router.router
    [ Router.get "/" hello_handler
    ; Router.get "/hello/:name" (fun request ->
        match Router.route_params request with
        | Some params ->
          let name = List.assoc "name" params in
          Response.create
            ~body:(Piaf.Body.of_string ("Hello, " ^ name ^ "!"))
            `OK
        | None -> Response.create `Not_found)
    ]
```

## Building Documentation Locally

```bash
dune build @doc
```

Then open `_build/default/_doc/_html/index.html` in your browser.

## License

MIT
