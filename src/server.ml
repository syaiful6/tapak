open Imports
open Piaf
module Log = (val Logging.setup ~src:"tapak.server" ~doc:"Tapak Server module")

(** Parse systemd socket activation environment variables.
    Returns the Unix file descriptor for FD 3 (first listening socket).
    See: https://www.freedesktop.org/software/systemd/man/sd_listen_fds.html *)
let get_systemd_listen_fd () =
  match
    Sys.getenv_opt "LISTEN_FDS" |> Fun.flip Option.bind int_of_string_opt
  with
  | Some n when n > 0 ->
    let first_fd =
      Sys.getenv_opt "LISTEN_FDS_FIRST_FD"
      |> Fun.flip Option.bind int_of_string_opt
      |> Option.value ~default:3
    in
    Some (Obj.magic first_fd : Unix.file_descr)
  | _ -> None

let port_of_address = function `Tcp (_, port) -> port | `Unix _ -> 0

let run_with ?error_handler ~config ~env app =
  Eio.Switch.run @@ fun sw ->
  let server =
    Server.create ?error_handler ~config (Tapak_kernel.Handler.to_piaf app)
  in
  Server.Command.start ~sw env server

module Systemd = struct
  type shutdown_resolver = unit -> unit

  type t =
    { sockets :
        Eio_unix.Net.listening_socket_ty Eio_unix.Net.listening_socket list
    ; shutdown_resolvers : shutdown_resolver array
    ; client_sockets :
        ( int
          , Eio_unix.Net.stream_socket_ty Eio_unix.Net.stream_socket )
          Hashtbl.t
          array
    ; clock : float Eio.Time.clock_ty Eio.Resource.t
    ; shutdown_timeout : float
    }

  let shutdown
        { sockets; shutdown_resolvers; client_sockets; clock; shutdown_timeout }
    =
    let length sockets =
      Array.fold_left (fun acc item -> Hashtbl.length item + acc) 0 sockets
    in
    Logs.info (fun m -> m "Starting server teardown...");
    Array.iter (fun resolver -> resolver ()) shutdown_resolvers;
    List.iter Eio.Net.close sockets;
    Eio.Fiber.first
      (fun () ->
         while length client_sockets > 0 do
           Eio.Time.sleep clock 0.1
         done)
      (fun () ->
         Eio.Time.sleep clock shutdown_timeout;
         Array.iter
           (fun client_sockets ->
              Hashtbl.iter
                (fun _ client_socket ->
                   try Eio.Flow.shutdown client_socket `All with
                   | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (ENOTCONN, _, _)), _)
                     ->
                     Logs.debug (fun m -> m "Socket already disconnected"))
                client_sockets)
           client_sockets);
    Logs.info (fun m -> m "Server teardown finished")

  let accept_loop ~sw ~listening_socket ~client_sockets connection_handler =
    let accept =
      let id = ref 0 in
      let rec accept () =
        Eio.Net.accept_fork
          listening_socket
          ~sw
          ~on_error:(fun exn ->
            let bt = Printexc.get_backtrace () in
            Logs.err (fun m ->
              m
                "Error in connection handler: %s@\n%s"
                (Printexc.to_string exn)
                bt))
          (fun socket addr ->
             Eio.Switch.run (fun sw ->
               let connection_id =
                 let cid = !id in
                 incr id;
                 cid
               in
               Hashtbl.replace client_sockets connection_id socket;
               Eio.Switch.on_release sw (fun () ->
                 Hashtbl.remove client_sockets connection_id);
               connection_handler ~sw socket addr));
        accept ()
      in
      accept
    in
    let released_p, released_u = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
      Eio.Fiber.first (fun () -> Eio.Promise.await released_p) accept);
    fun () -> Eio.Promise.resolve released_u ()

  let listen_with_socket
        ~sw
        ~listening_socket
        ~domains
        ~shutdown_timeout
        env
        connection_handler
    =
    let resolvers = Array.make domains (Fun.id, Hashtbl.create 0) in
    let started_domains = Eio.Semaphore.make domains in
    let run_accept_loop =
      let resolver_mutex = Eio.Mutex.create () in
      fun idx ->
        Eio.Switch.run (fun sw ->
          let resolver =
            let client_sockets = Hashtbl.create 256 in
            let resolver =
              accept_loop
                ~sw
                ~client_sockets
                ~listening_socket
                connection_handler
            in
            resolver, client_sockets
          in
          Eio.Mutex.lock resolver_mutex;
          resolvers.(idx) <- resolver;
          Eio.Mutex.unlock resolver_mutex;
          Eio.Semaphore.acquire started_domains)
    in
    for idx = 0 to domains - 1 do
      let run_accept_loop () = run_accept_loop idx in
      if idx = domains - 1
      then Eio.Fiber.fork ~sw run_accept_loop
      else
        Eio.Fiber.fork ~sw (fun () ->
          let domain_mgr = Eio.Stdenv.domain_mgr env in
          Eio.Domain_manager.run domain_mgr run_accept_loop)
    done;
    while Eio.Semaphore.get_value started_domains > 0 do
      Eio.Fiber.yield ()
    done;
    { sockets = [ listening_socket ]
    ; shutdown_resolvers = Array.map fst resolvers
    ; client_sockets = Array.map snd resolvers
    ; clock = Eio.Stdenv.clock env
    ; shutdown_timeout
    }
end

let run_with_systemd_socket ?error_handler ~config ~env app =
  Eio.Switch.run @@ fun sw ->
  let server =
    Server.create ?error_handler ~config (Tapak_kernel.Handler.to_piaf app)
  in

  match get_systemd_listen_fd () with
  | Some unix_fd ->
    let listening_socket =
      Eio_unix.Net.import_socket_listening ~sw ~close_unix:true unix_fd
    in
    let actual_port =
      match Unix.getsockname unix_fd with
      | Unix.ADDR_INET (_, port) -> port
      | Unix.ADDR_UNIX _ -> 0
    in
    Logs.info (fun log ->
      log
        "Using socket activation (port %d, %d domain%s)"
        actual_port
        config.domains
        (if config.domains = 1 then "" else "s"));

    let piaf_handler = Server.http_connection_handler server in
    let connection_handler ~sw socket addr = piaf_handler ~sw socket addr in

    `Systemd
      (Systemd.listen_with_socket
         ~sw
         ~listening_socket
         ~domains:config.domains
         ~shutdown_timeout:config.shutdown_timeout
         env
         connection_handler)
  | None ->
    let port = port_of_address config.address in
    Logs.warn (fun log ->
      log "No socket activation found, starting server normally on port %d" port);
    Logs.warn (fun log ->
      log
        "For hot reload in development, use: systemfd --no-pid -s http::%d -- \
         watchexec -r -e ml,mli --ignore _build -- dune exec <your-exe>"
        port);
    Logs.info (fun log -> log "Starting server on port %d..." port);
    `Piaf (Server.Command.start ~sw env server)
