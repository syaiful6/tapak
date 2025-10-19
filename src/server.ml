open Imports
open Piaf
module Log = (val Logging.setup ~src:"tapak.server" ~doc:"Tapak Server module")

(** Parse systemd socket activation environment variables.
    Returns the Unix file descriptor for FD 3 (first listening socket).
    See: https://www.freedesktop.org/software/systemd/man/sd_listen_fds.html *)
let get_systemd_listen_fd () =
  match Sys.getenv_opt "LISTEN_FDS" with
  | Some n when int_of_string n > 0 ->
    (* systemd passes listening sockets starting at FD 3
       (after stdin=0, stdout=1, stderr=2) *)
    let fd_num = 3 in
    Some (Obj.magic fd_num : Unix.file_descr)
  | _ -> None

let port_of_address = function `Tcp (_, port) -> port | `Unix _ -> 0

let run_with ?error_handler ~config ~env app =
  Eio.Switch.run @@ fun sw ->
  let server =
    Server.create
      ?error_handler
      ~config
      (Tapak_kernel.Server_connection.to_piaf_request_handler app)
  in
  ignore (Server.Command.start ~sw env server : Server.Command.t)

let run_dev ?error_handler ~config ~env app =
  Eio.Switch.run @@ fun sw ->
  let server =
    Server.create
      ?error_handler
      ~config
      (Tapak_kernel.Server_connection.to_piaf_request_handler app)
  in

  match get_systemd_listen_fd () with
  | Some unix_fd ->
    let listening_socket =
      Eio_unix.Net.import_socket_listening ~sw ~close_unix:true unix_fd
    in
    (* Get the actual port from the socket, not from config *)
    let actual_port =
      match Unix.getsockname unix_fd with
      | Unix.ADDR_INET (_, port) -> port
      | Unix.ADDR_UNIX _ -> 0
    in
    Logs.debug (fun log ->
      log "Development mode: using socket activation (port %d)" actual_port);

    let piaf_handler = Server.http_connection_handler server in
    let eio_handler socket addr =
      Eio.Switch.run (fun handler_sw -> piaf_handler ~sw:handler_sw socket addr)
    in
    (* Accept connections in a loop, forked into a fiber *)
    let accept =
      let rec loop () =
        Eio.Net.accept_fork
          listening_socket
          eio_handler
          ~sw
          ~on_error:(fun ex ->
            Logs.err (fun log -> log "Connection error: %a" Fmt.exn ex));
        loop ()
      in
      loop
    in
    Eio.Fiber.fork ~sw accept
  | None ->
    (* No socket activation found - warn but run normally *)
    let port = port_of_address config.address in
    Logs.warn (fun log ->
      log
        "Development mode: no socket activation found, starting server on port \
         %d"
        port);
    Logs.warn (fun log ->
      log
        "Hot reload will NOT work. Server will restart on each rebuild. For \
         hot reload, use: systemfd --no-pid -s http::%d -- watchexec -r -e \
         ml,mli --ignore _build -- dune exec <your-exe>"
        port);
    Logs.info (fun log -> log "Starting server on port %d..." port);
    ignore (Server.Command.start ~sw env server : Server.Command.t)
