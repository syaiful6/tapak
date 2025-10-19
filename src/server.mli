val run_with :
   ?error_handler:Piaf.Server.error_handler
  -> config:Piaf.Server.Config.t
  -> env:Eio_unix.Stdenv.base
  -> Tapak_kernel.App.t
  -> unit
(** [run_with ?error_handler ~config ~env app] starts a production server with
    the given configuration and application.

    @param error_handler Optional custom error handler for server errors.
                         If not provided, Piaf's default error handler is used.

    This function blocks until the server is shut down. *)

val run_dev :
   ?error_handler:Piaf.Server.error_handler
  -> config:Piaf.Server.Config.t
  -> env:Eio_unix.Stdenv.base
  -> Tapak_kernel.App.t
  -> unit
(** [run_dev ?error_handler ~config ~env app] starts a development server with
    hot reload support.

    @param error_handler Optional custom error handler for server errors.

    {b Socket Activation for Hot Reload:}

    This function supports systemd-style socket activation for zero-downtime
    hot reloading during development. When combined with [dune build --watch]
    and a tool like [systemfd], your server can be rebuilt and restarted
    without dropping active connections.

    {b Usage:}

    {v
    # Install systemfd and watchexec (one-time setup)
    cargo install systemfd watchexec-cli

    # Run in development mode with hot reload
    systemfd --no-pid -s http::3000 -- watchexec -r -e ml,mli --ignore _build -- dune exec your_server.exe
    v}

    {b Behavior:}

    - If the [LISTEN_FDS] environment variable is set (socket activation):
      - Uses the provided listening socket (FD 3)
      - Enables zero-downtime hot reload
      - Logs confirmation of socket activation mode

    - If no socket activation is detected:
      - Warns that hot reload is not available
      - Runs normally (server restarts on rebuild)
      - Displays instructions for enabling hot reload

    This function blocks until the server is shut down.

    @see <https://www.freedesktop.org/software/systemd/man/sd_listen_fds.html>
         systemd socket activation protocol
*)
