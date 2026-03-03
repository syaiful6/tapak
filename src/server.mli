val get_systemd_listen_fd : unit -> Unix.file_descr option
(** get_systemd_listen_fd () returns Some fd if the process was started by systemd with a socket activated service,
    and None otherwise.

    You can use this to by importing this file descriptor with Eio_unix.Net.import_socket_listening *)

val make :
   ?conn_closed:(Cohttp_eio.Server.conn -> unit)
  -> Handler.t
  -> Cohttp_eio.Server.t

val run :
   ?max_connections:int
  -> ?additional_domains:_ Eio.Domain_manager.t * int
  -> ?stop:'a Eio.Promise.t
  -> on_error:(exn -> unit)
  -> _ Eio.Net.listening_socket
  -> Handler.t
  -> 'a
