type t =
  { response : Piaf.Response.t
  ; context : Context.t
  }

let status { response; _ } = Piaf.Response.status response
let headers { response; _ } = Piaf.Response.headers response
let version { response; _ } = Piaf.Response.version response
let body { response; _ } = Piaf.Response.body response
let context { context; _ } = context
let to_piaf { response; _ } = response

let create
      ?(version = Piaf.Versions.HTTP.HTTP_1_1)
      ?(headers = Piaf.Headers.empty)
      ?(body = Piaf.Body.empty)
      ?(context = Context.empty)
      status
  =
  let response = Piaf.Response.create ~version ~headers ~body status in
  { response; context }

let with_ ?status ?headers ?version ?context ?body response =
  let res =
    Piaf.Response.with_ ?status ?headers ?version ?body response.response
  in
  { response = res; context = Option.value context ~default:response.context }

let of_string ?version ?headers ?context ~body status =
  create ?version ?headers ?context ~body:(Body.of_string body) status

let of_bigstring ?version ?headers ?context ~body status =
  create ?version ?headers ?context ~body:(Body.of_bigstring body) status

let of_string_stream ?version ?headers ?context ~body status =
  create ?version ?headers ?context ~body:(Body.of_string_stream body) status

let of_stream ?version ?headers ?context ~body status =
  create ?version ?headers ?context ~body:(Body.of_stream body) status

let sendfile
      ?version
      ?(headers = Piaf.Headers.empty)
      ?(context = Context.empty)
      path
  =
  match Piaf.Response.sendfile ?version ~headers path with
  | Ok response -> Ok { response; context }
  | Error e -> Error e

let copy_file
      ?version
      ?(headers = Piaf.Headers.empty)
      ?(context = Context.empty)
      path
  =
  match Piaf.Response.copy_file ?version ~headers path with
  | Ok response -> Ok { response; context }
  | Error e -> Error e

module Upgrade = struct
  let generic
        ?version
        ?(headers = Piaf.Headers.empty)
        ?(context = Context.empty)
        upgrade_handler
    =
    let response =
      Piaf.Response.Upgrade.generic ?version ~headers upgrade_handler
    in
    { response; context }

  let websocket
        ~f
        ?(headers = Piaf.Headers.empty)
        ?(context = Context.empty)
        request
    =
    match
      Piaf.Response.Upgrade.websocket ~f ~headers (Request.to_piaf request)
    with
    | Ok response -> Ok { response; context }
    | Error e -> Error e
end

let or_internal_error ret =
  let result = Result.map to_piaf ret in
  Piaf.Response.or_internal_error result

let persistent_connection { response; _ } =
  Piaf.Response.persistent_connection response

let pp_hum fmt t = Piaf.Response.pp_hum fmt t.response
