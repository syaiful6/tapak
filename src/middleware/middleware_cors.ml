type origin_policy =
  [ `Allow_all
  | `Allow_list of string list
  | `Allow_predicate of string -> bool
  ]

type t =
  { origins : origin_policy
  ; methods : Piaf.Method.t list
  ; headers : string list
  ; exposed_headers : string list
  ; credentials : bool
  ; max_age : int option
  ; send_preflight : bool
  }

let args
      ?(origins = `Allow_all)
      ?(methods =
        [ `GET; `POST; `PUT; `Other "PATCH"; `DELETE; `HEAD; `OPTIONS ])
      ?(headers =
        [ "Accept"
        ; "Accept-Language"
        ; "Content-Language"
        ; "Content-Type"
        ; "Authorization"
        ])
      ?(exposed_headers = [])
      ?(credentials = false)
      ?max_age
      ?(send_preflight = true)
      ()
  =
  { origins
  ; methods
  ; headers
  ; exposed_headers
  ; credentials
  ; max_age
  ; send_preflight
  }

let permissive () =
  args
    ~origins:`Allow_all
    ~methods:[ `GET; `POST; `PUT; `Other "PATCH"; `DELETE; `HEAD; `OPTIONS ]
    ~headers:[ "*" ]
    ~credentials:false
    ~max_age:86400
    ~send_preflight:true
    ()

let strict ~origins =
  args
    ~origins:(`Allow_list origins)
    ~methods:[ `GET; `POST ]
    ~headers:[ "Content-Type"; "Authorization" ]
    ~credentials:true
    ~send_preflight:true
    ()

let check_origin config origin =
  match config.origins with
  | `Allow_all -> Some "*"
  | `Allow_list origins -> if List.mem origin origins then Some origin else None
  | `Allow_predicate pred -> if pred origin then Some origin else None

let method_to_string = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `DELETE -> "DELETE"
  | `HEAD -> "HEAD"
  | `OPTIONS -> "OPTIONS"
  | `CONNECT -> "CONNECT"
  | `TRACE -> "TRACE"
  | `Other s -> s

let add_cors_headers config origin response =
  let response =
    Response.add_header ("Access-Control-Allow-Origin", origin) response
  in
  let response =
    if config.credentials && origin <> "*"
    then
      Response.add_header ("Access-Control-Allow-Credentials", "true") response
    else response
  in
  let response =
    if config.exposed_headers <> []
    then
      Response.add_header
        ( "Access-Control-Expose-Headers"
        , String.concat ", " config.exposed_headers )
        response
    else response
  in
  response

let is_preflight request =
  Request.meth request = `OPTIONS
  && Request.header "Access-Control-Request-Method" request <> None

let handle_preflight config origin =
  let methods_str =
    String.concat ", " (List.map method_to_string config.methods)
  in
  let headers_str = String.concat ", " config.headers in
  let response = Response.of_string' ~status:`No_content "" in
  let response =
    Response.add_header ("Access-Control-Allow-Origin", origin) response
  in
  let response =
    Response.add_header ("Access-Control-Allow-Methods", methods_str) response
  in
  let response =
    Response.add_header ("Access-Control-Allow-Headers", headers_str) response
  in
  let response =
    if config.credentials && origin <> "*"
    then
      Response.add_header ("Access-Control-Allow-Credentials", "true") response
    else response
  in
  let response =
    match config.max_age with
    | Some age ->
      Response.add_header ("Access-Control-Max-Age", string_of_int age) response
    | None -> response
  in
  Response.add_to_list_header ("Vary", "Origin") response

let call config next request =
  match Request.header "Origin" request with
  | None -> next request
  | Some origin ->
    (match check_origin config origin with
    | None ->
      if is_preflight request && config.send_preflight
      then Response.of_string' ~status:`Forbidden "CORS policy violation"
      else next request
    | Some allowed_origin ->
      if is_preflight request && config.send_preflight
      then handle_preflight config allowed_origin
      else
        let response = next request in
        add_cors_headers config allowed_origin response)
