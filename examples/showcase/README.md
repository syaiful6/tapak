# Tapak Showcase Example

This example demonstrates all the major features of the Tapak web framework.

## Features Demonstrated

### 1. **Routing**
- Simple routes (`/`)
- Named parameters (`/users/:id`)
- Wildcard matching (`/files/**`)
- HTTP method routing (GET, POST, PUT)

### 2. **Request Body Decompression**
- Automatic decompression of compressed request bodies
- Support for gzip, deflate, brotli, zstd (with tapak-compressions)
- Proper HTTP status codes (415 for unsupported, 400 for errors)

### 3. **Hot Reload** (Development)
- Zero-downtime rebuilds using socket activation
- Works with `dune --watch` for automatic rebuilds
- No dropped connections during reload

### 4. **Content Negotiation**
- Responds with JSON or HTML based on `Accept` header
- Demonstrates `Header_parser.Content_negotiation`

### 5. **Responses**
- Plain text responses
- HTML responses
- JSON responses
- Custom status codes

## Running the Example

### Development Mode (with hot reload)

```bash
# Terminal 1: Run with hot reload
systemfd --no-pid -s http::3000 -- watchexec -r -e ml,mli --ignore _build -- dune exec tapak-showcase

# Terminal 2: Make requests
curl http://localhost:3000/
curl http://localhost:3000/users/42
curl -H "Accept: application/json" http://localhost:3000/api/hello
curl -H "Accept: text/html" http://localhost:3000/api/hello
curl -X POST -d "Hello Tapak!" http://localhost:3000/echo

# Edit examples/showcase/main.ml and save - server will hot reload!
```

### Production Mode

```bash
TAPAK_ENV=production dune exec -- tapak-showcase
```

### Custom Port

```bash
PORT=8080 dune exec -- tapak-showcase
```

## Endpoints

### `GET /`
Homepage with links to other endpoints.

**Example:**
```bash
curl http://localhost:3000/
```

### `GET /users/:id`
User profile page demonstrating named route parameters.

**Example:**
```bash
curl http://localhost:3000/users/123
```

### `GET /api/hello`
JSON API endpoint with content negotiation.

**Examples:**
```bash
# Request JSON
curl -H "Accept: application/json" http://localhost:3000/api/hello

# Request HTML
curl -H "Accept: text/html" http://localhost:3000/api/hello
```

### `GET /files/**`
File browser demonstrating wildcard/splat routing.

**Example:**
```bash
curl http://localhost:3000/files/docs/readme.md
curl http://localhost:3000/files/images/logo.png
```

### `POST /echo` and `PUT /echo`
Echo endpoint that returns the request body. Demonstrates:
- Body decompression (if Content-Encoding header is present)
- Multiple HTTP methods on one route

**Examples:**
```bash
# Simple POST
curl -X POST -d "Hello Tapak!" http://localhost:3000/echo

# POST with gzip compression (requires tapak-compressions)
echo "Hello Tapak!" | gzip | curl -X POST \
  -H "Content-Encoding: gzip" \
  --data-binary @- \
  http://localhost:3000/echo
```

## Code Structure

```ocaml
(* Create handlers *)
let home_handler req = ...
let user_handler req = ...
let not_found_handler req = ...

(* Build router - no exceptions to catch! *)
let routes =
  [ Router.get "/" home_handler
  ; Router.get "/users/:id" user_handler
  ; ...
  ]

let handler = Router.route ~not_found:not_found_handler routes

(* Add middleware *)
let app =
  App.create ~handler ()
  <+> decompression_middleware

(* Run server *)
let () =
  Eio_main.run @@ fun env ->
  let config = Piaf.Server.Config.create address in
  Server.run_dev ~config ~env app  (* or run_with for production *)
```

### Improved Router API

The `Router.route` function provides a cleaner API:

```ocaml
(* Old way - catching exceptions *)
let handler req =
  try Router.router routes req
  with Router.Not_found -> not_found_handler req

(* New way - declarative *)
let handler = Router.route ~not_found:not_found_handler routes
```

## Adding Compression Support

To enable actual compression/decompression (not just the identity encoder), add the `tapak-compressions` package:

```ocaml
(* In dune file *)
(libraries tapak tapak-compressions piaf eio_main)

(* In main.ml *)
let decompression_mw =
  Middleware.Decompression.create Tapak_compressions.decoder

let app =
  App.create ~handler ()
  <+> decompression_mw
```

## Next Steps

- Add more middlewares (logging, authentication, CSRF protection)
- Use `Router.scope` for grouping routes with shared middlewares
- Implement REST resources with `Router.resources`
- Add database integration
- Serve static files
- Add WebSocket support (when available)

## Hot Reload Tips

1. **Install systemfd and watchexec once:**
   ```bash
   cargo install systemfd watchexec-cli
   ```

2. **Create a dev script** (`bin/dev`):
   ```bash
   #!/usr/bin/env bash
   exec systemfd --no-pid -s http::3000 -- \
     watchexec -r -e ml,mli --ignore _build -- dune exec tapak-showcase
   ```

3. **Run it:**
   ```bash
   chmod +x bin/dev
   ./bin/dev
   ```

4. **Edit and save files** - server automatically reloads!

## Troubleshooting

### "Address already in use"
Another process is using port 3000. Either kill it or use a different port:
```bash
PORT=3001 dune exec -- tapak-showcase
```

### Hot reload not working
Make sure you're using `systemfd`, `watchexec -r`, and `Server.run_dev`:
```bash
systemfd --no-pid -s http::3000 -- watchexec -r -e ml,mli --ignore _build -- dune exec tapak-showcase
```

### Module not found errors
Make sure you've built the project:
```bash
dune build
```
