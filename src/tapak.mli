(** Tapak - A composable web framework for OCaml.

    Tapak provides a modular and extensible architecture based on Services and
    Filters (middlewares) built on OCaml 5's effect-based I/O (EIO). *)

(** {1 Core Types} *)

module Service = Service
module Handler = Handler
module Filter = Filter
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
  val run_with :
     ?error_handler:Piaf.Server.error_handler
    -> config:Piaf.Server.Config.t
    -> env:Eio_unix.Stdenv.base
    -> App.t
    -> unit
  (** [run_with ?error_handler ~config ~env app] starts a production server.

      @param error_handler Optional custom error handler for server errors.

      This function blocks until the server is shut down. *)

  val run_dev :
     ?error_handler:Piaf.Server.error_handler
    -> config:Piaf.Server.Config.t
    -> env:Eio_unix.Stdenv.base
    -> App.t
    -> unit
  (** [run_dev ?error_handler ~config ~env app] starts a development server with
      hot reload support via socket activation.

      @param error_handler Optional custom error handler for server errors.

      For hot reload, run with:
      {v systemfd --no-pid -s http::<port> -- watchexec -r -e ml,mli --ignore _build -- dune exec <exe> v}

      See {!DEV.md} for detailed documentation. *)
end

module Header_parser = Header_parser
module Cookies = Cookies
module CSRF = Csrf
module Versions = Versions
