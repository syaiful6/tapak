include Tapak_kernel.Response

let redirect
      ?(status : Piaf.Status.redirection = `Found)
      ?version
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

let of_string'
      ?(content_type = "text/plain")
      ?version
      ?(status : Status.t = `OK)
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
