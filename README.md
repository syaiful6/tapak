# Tapak

Tapak is a composable framework for building web applications in OCaml. Inspired by [Twitter's Finagle](https://twitter.github.io/finagle/), it provides a modular and extensible architecture for building scalable web services.

## Documentation

📖 [Read the full documentation](https://syaiful6.github.io/tapak/)

## Features

- **Composable Architecture**: Build applications from small, reusable components
- **Type-Safe Routing**: Pattern matching based router with parameter extraction and splat matching
- **Middleware Pipeline**: Chain middlewares to handle cross-cutting concerns
- **Request/Response Compression**: Built-in support for gzip, deflate, brotli, and zstd
- **Built on EIO**: Uses OCaml 5's effect-based I/O for excellent performance
- **Finagle-Inspired**: Adopts proven patterns from Twitter's battle-tested framework

## Quick Start

```ocaml
open Tapak

let hello_handler _request =
  Response.create
    ~body:(Body.of_string "Hello, World!")
    `OK

let routes =
  Router.router
    [ Router.get "/" hello_handler
    ; Router.get "/hello/:name" (fun request ->
        match Router.route_params request with
        | Some params ->
          let name = List.assoc "name" params in
          Response.create
            ~body:(Body.of_string ("Hello, " ^ name ^ "!"))
            `OK
        | None -> Response.create `Not_found)
    ]

(* Create an app with compression middleware *)
let () =
  let app =
    App.create routes
    |> App.(<+>) (Middleware.Compression.middleware
                    ~predicate:Tapak_compressions.Predicate.default_predicate
                    ~preferred_encodings:[`Br; `Zstd; `Gzip]
                    Tapak_compressions.encoder)
  in
  (* Run your app with EIO... *)
```

## Building Documentation Locally

```bash
dune build @doc
```

Then open `_build/default/_doc/_html/index.html` in your browser.

## License

MIT
