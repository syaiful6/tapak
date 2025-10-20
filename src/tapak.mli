module Context = Context
module Request = Request
module Response = Response
module Headers = Headers
module Body = Body
module Status = Status
module Router = Router
module Middleware = Middleware
module App = App
module Form = Form

module Server : sig
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

    val listen_with_socket :
       sw:Eio.Switch.t
      -> listening_socket:
           Eio_unix.Net.listening_socket_ty Eio_unix.Net.listening_socket
      -> domains:int
      -> shutdown_timeout:float
      -> Eio_unix.Stdenv.base
      -> (sw:Eio.Switch.t
          -> Eio_unix.Net.stream_socket_ty Eio_unix.Net.stream_socket
          -> Eio.Net.Sockaddr.stream
          -> unit)
      -> t
  end

  val run_with :
     ?error_handler:Piaf.Server.error_handler
    -> config:Piaf.Server.Config.t
    -> env:Eio_unix.Stdenv.base
    -> App.t
    -> Piaf.Server.Command.t

  val run_with_systemd_socket :
     ?error_handler:Piaf.Server.error_handler
    -> config:Piaf.Server.Config.t
    -> env:Eio_unix.Stdenv.base
    -> App.t
    -> [> `Piaf of Piaf.Server.Command.t | `Systemd of Systemd.t ]
end

module Header_parser = Header_parser
module Cookies = Cookies
module CSRF = Csrf
module Versions = Versions
