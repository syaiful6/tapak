val run_with :
   ?error_handler:Piaf.Server.error_handler
  -> config:Piaf.Server.Config.t
  -> env:Eio_unix.Stdenv.base
  -> Handler.t
  -> Piaf.Server.Command.t

module Systemd : sig
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

  val shutdown : t -> unit

  val accept_loop :
     sw:Eio.Std.Switch.t
    -> listening_socket:
         [> ([> `Generic ] as 'a) Eio.Net.listening_socket_ty ] Eio.Std.r
    -> client_sockets:
         (int, 'a Eio.Net.stream_socket_ty Eio.Net.stream_socket) Hashtbl.t
    -> (sw:Eio.Std.Switch.t
        -> 'a Eio.Net.stream_socket_ty Eio.Net.stream_socket
        -> Eio.Net.Sockaddr.stream
        -> unit)
    -> unit
    -> unit

  val listen_with_socket :
     sw:Eio.Std.Switch.t
    -> listening_socket:Eio_unix.Net.listening_socket_ty Eio.Std.r
    -> domains:int
    -> shutdown_timeout:float
    -> < clock : float Eio.Time.clock_ty Eio.Time.clock
       ; domain_mgr : [> Eio.Domain_manager.ty ] Eio.Domain_manager.t
       ; .. >
    -> (sw:Eio.Switch.t
        -> Eio_unix.Net.stream_socket_ty Eio.Net.stream_socket
        -> Eio.Net.Sockaddr.stream
        -> unit)
    -> t
end

val run_with_systemd_socket :
   ?error_handler:Piaf.Server.error_handler
  -> config:Piaf.Server.Config.t
  -> env:Eio_unix.Stdenv.base
  -> Handler.t
  -> [> `Piaf of Piaf.Server.Command.t | `Systemd of Systemd.t ]
