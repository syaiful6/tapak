open Piaf

type t =
  { scheme : Scheme.t
  ; version : Versions.HTTP.t
  ; client_address : Eio.Net.Sockaddr.stream option
  ; sw : Eio.Switch.t option
  ; env : Context.t
  }

let of_piaf (t : Request_info.t) =
  { scheme = t.scheme
  ; version = t.version
  ; client_address = Option.some t.client_address
  ; sw = Option.some t.sw
  ; env = Context.empty
  }

let default =
  { scheme = `HTTP
  ; version = Versions.HTTP.HTTP_1_1
  ; client_address = None
  ; sw = None
  ; env = Context.empty
  }

let with_ ?scheme ?version ?client_address ?sw ?env info =
  { scheme = Option.value scheme ~default:info.scheme
  ; version = Option.value version ~default:info.version
  ; client_address = Option.value client_address ~default:info.client_address
  ; sw = Option.value sw ~default:info.sw
  ; env = Option.value env ~default:info.env
  }
