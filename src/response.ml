open Imports

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

(* Extended functions from src *)
let redirect
      ?version
      ?(status : Piaf.Status.redirection = `Found)
      ?(headers = Piaf.Headers.empty)
      ?(context = Context.empty)
      location
  =
  let headers = Headers.add_unless_exists headers "Location" location in
  create ?version ~headers ~context ~body:Body.empty (status :> Piaf.Status.t)

let header key t = Headers.get (headers t) key
let multi_header key t = Headers.get_multi (headers t) key

let add_header (k, v) t =
  let headers = headers t in
  with_ ~headers:(Headers.add headers k v) t

let add_header_or_replace (k, v) t =
  let headers = headers t in
  with_
    ~headers:
      (if Headers.mem headers k
       then Headers.replace headers k v
       else Headers.add headers k v)
    t

let add_header_unless_exists (k, v) t =
  let headers = headers t in
  with_ ~headers:(Headers.add_unless_exists headers k v) t

let add_headers hs t = with_ ~headers:(Headers.add_list (headers t) hs) t

let remove_header key t =
  let headers = headers t in
  with_ ~headers:(Headers.remove headers key) t

let add_to_list_header (k, v) t =
  let headers = Headers.add_to_list_header (headers t) k v in
  with_ ~headers t

let of_string'
      ?(content_type = "text/plain")
      ?version
      ?(status = `OK)
      ?(headers = Headers.empty)
      ?(context = Context.empty)
      body
  =
  let headers = Headers.add_unless_exists headers "Content-Type" content_type in
  create ?version ~headers ~body:(Body.of_string body) ~context status

let of_html
      ?version
      ?status
      ?(headers = Headers.empty)
      ?(context = Context.empty)
      body
  =
  of_string'
    ?version
    ?status
    ~content_type:"text/html; charset=utf-8"
    ~headers
    ~context
    body

let of_json
      ?version
      ?status
      ?(headers = Headers.empty)
      ?(context = Context.empty)
      body
  =
  of_string'
    ?version
    ?status
    ~content_type:"application/json; charset=utf-8"
    ~headers
    ~context
    (body |> Yojson.Safe.to_string)

let negotiate
      ?version
      ?(status = `OK)
      ?(headers = Headers.empty)
      ?(context = Context.empty)
      ?(available_formats =
        Header_parser.Content_negotiation.default_accept_formats)
      request
      render
  =
  let accept_header = Request.header "Accept" request in
  match
    Header_parser.Content_negotiation.negotiate_format
      accept_header
      available_formats
  with
  | Some format ->
    (match render format with
    | Some (content_type, body) ->
      of_string' ?version ~status ~content_type ~headers ~context body
    | None ->
      of_string'
        ?version
        ~status:`Not_acceptable
        ~headers
        ~context
        "Not Acceptable")
  | None ->
    of_string'
      ?version
      ~status:`Not_acceptable
      ~headers
      ~context
      "Not Acceptable"
