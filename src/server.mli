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
