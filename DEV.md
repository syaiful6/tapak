# Development Mode with Hot Reload

Tapak supports zero-downtime hot reloading during development using socket activation.

## Quick Start

### 1. Install systemfd (one-time setup)

```bash
cargo install systemfd
```

### 2. Update your application

Use `Server.run_dev` instead of `Server.run_with` in your main entry point:

```ocaml
(* bin/main.ml *)
let () =
  Eio_main.run @@ fun env ->
  let config = Piaf.Server.Config.create (`Tcp (Eio.Net.Ipaddr.V4.loopback, 3000)) in

  (* Use run_dev for development with hot reload support *)
  Server.run_dev ~config ~env app
```

### 3. Install watchexec (one-time setup)

```bash
cargo install watchexec-cli
```

### 4. Run with hot reload

```bash
systemfd --no-pid -s http::3000 -- watchexec -r -e ml,mli --ignore _build -- dune exec ./bin/main.exe
```

That's it! Now when you save files:

1. `watchexec` detects the file change
2. `dune` rebuilds your project
3. `watchexec` restarts your server with `-r` flag
4. `systemfd` provides the same socket to the new process
5. Active HTTP connections are preserved - zero downtime!

## How It Works

### Socket Activation Protocol

Socket activation is a protocol where:

1. An external process (like `systemfd` or `systemd`) creates and binds the listening socket
2. It passes the socket file descriptor (FD 3) to your application via environment variables
3. Your application accepts connections on the pre-bound socket
4. When rebuilding, the socket stays open, so no connections are dropped

### Without Socket Activation

If you run `Server.run_dev` without `systemfd`, you'll see:

```
WARNING: Development mode without socket activation
  Hot reload will NOT work. Server will restart on each rebuild.
  For hot reload, use: systemfd --no-pid -s http::3000 -- dune exec --watch -- <your-exe>

Starting server on port 3000...
```

The server will still work, but each rebuild will restart it (and drop connections).

### With Socket Activation

When running with `systemfd`, you'll see:

```
Development mode: using socket activation (port 3000)
Hot reload enabled - rebuild with 'dune build --watch'
```

## Production

For production, use `Server.run_with` which doesn't check for socket activation:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  let config = Piaf.Server.Config.create (`Tcp (Eio.Net.Ipaddr.V4.any, 8080)) in
  Server.run_with ~config ~env app
```

## Creating a Development Script

You can create a `bin/dev` script for convenience:

```bash
#!/usr/bin/env bash
# bin/dev
set -euo pipefail

PORT="${PORT:-3000}"

echo "Starting Tapak development server on port $PORT"
echo "Hot reload enabled - save files to trigger rebuild"
echo ""

exec systemfd --no-pid -s "http::$PORT" -- \
  watchexec -r -e ml,mli --ignore _build -- dune exec ./bin/main.exe
```

Make it executable:

```bash
chmod +x bin/dev
./bin/dev
```

## Troubleshooting

### "Address already in use" error

If you see this error, another process is using the port. Either:

- Kill the other process: `lsof -ti:3000 | xargs kill`
- Use a different port: `PORT=3001 ./bin/dev`

### systemfd or watchexec not found

Install them with cargo:

```bash
cargo install systemfd
cargo install watchexec-cli
```

Or use your system's package manager if available.

### Hot reload not working

Make sure:

1. You're using `Server.run_dev` (not `run_with`)
2. You're running via `systemfd` (not directly)
3. You're using `watchexec -r` to restart the process on file changes
4. The `-e ml,mli` flag watches OCaml source files

## Alternative: Use systemd in Production

You can also use systemd socket activation in production for zero-downtime deployments:

```ini
# /etc/systemd/system/myapp.socket
[Unit]
Description=My Tapak App Socket

[Socket]
ListenStream=8080

[Install]
WantedBy=sockets.target
```

```ini
# /etc/systemd/system/myapp.service
[Unit]
Description=My Tapak App
Requires=myapp.socket

[Service]
ExecStart=/path/to/myapp
StandardInput=socket
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
```

Enable and start:

```bash
systemctl enable myapp.socket
systemctl start myapp.socket
```
