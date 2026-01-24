open Imports

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

let default_methods =
  [ `GET; `POST; `PUT; `Other "PATCH"; `DELETE; `HEAD; `OPTIONS ]

let default_headers =
  [ "Accept"
  ; "Accept-Language"
  ; "Content-Language"
  ; "Content-Type"
  ; "Authorization"
  ]

let args
      ?(origins = `Allow_all)
      ?(methods = default_methods)
      ?(headers = default_headers)
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

let permissive () = args ~headers:[ "*" ] ~max_age:86400 ()

let strict ~origins =
  args
    ~origins:(`Allow_list origins)
    ~methods:[ `GET; `POST ]
    ~headers:[ "Content-Type"; "Authorization" ]
    ~credentials:true
    ()

let should_vary config =
  match config.origins with
  | `Allow_all -> config.credentials
  | `Allow_list _ | `Allow_predicate _ -> true

let check_origin config origin =
  match config.origins with
  | `Allow_all -> Some (if config.credentials then origin else "*")
  | `Allow_list origins -> Option.some_if (List.mem origin origins) origin
  | `Allow_predicate pred -> Option.some_if (pred origin) origin

let credentials_header ~credentials ~origin =
  if credentials && origin <> "*"
  then Some ("Access-Control-Allow-Credentials", "true")
  else None

let with_vary config response =
  if should_vary config
  then Response.add_to_list_header ("Vary", "Origin") response
  else response

let add_cors_headers config origin response =
  Response.add_headers
    (List.filter_map
       Fun.id
       [ Some ("Access-Control-Allow-Origin", origin)
       ; credentials_header ~credentials:config.credentials ~origin
       ; (if config.exposed_headers <> []
          then
            Some
              ( "Access-Control-Expose-Headers"
              , String.concat ~sep:", " config.exposed_headers )
          else None)
       ])
    response
  |> with_vary config

let is_preflight request =
  Request.meth request = `OPTIONS
  && Option.is_some (Request.header "Access-Control-Request-Method" request)

let handle_preflight config origin request =
  let requested_headers =
    Request.header "Access-Control-Request-Headers" request
  in
  let methods_str =
    config.methods |> List.map Piaf.Method.to_string |> String.concat ~sep:", "
  in
  let headers_str =
    match config.headers, requested_headers with
    | [ "*" ], Some h -> h
    | hs, _ -> String.concat ~sep:", " hs
  in
  Response.of_string' ~status:`No_content ""
  |> Response.add_headers
       (List.filter_map
          Fun.id
          [ Some ("Access-Control-Allow-Origin", origin)
          ; Some ("Access-Control-Allow-Methods", methods_str)
          ; Some ("Access-Control-Allow-Headers", headers_str)
          ; credentials_header ~credentials:config.credentials ~origin
          ; Option.map
              (fun age -> "Access-Control-Max-Age", string_of_int age)
              config.max_age
          ])
  |> with_vary config

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
      then handle_preflight config allowed_origin request
      else
        let response = next request in
        add_cors_headers config allowed_origin response)
