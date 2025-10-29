type error = ..

exception Failed of error

type 'a t = Request.t -> ('a, error) result

type error +=
  | Missing_header of string
  | Missing_query_param of string
  | Invalid_bearer_token

let return x _req = Ok x

let bind guard f req =
  match guard req with Ok v -> f v req | Error e -> Error e

let map f guard req =
  match guard req with Ok v -> Ok (f v) | Error e -> Error e

let map_error f guard req =
  match guard req with Ok v -> Ok v | Error e -> Error (f e)

let and_then f guard req =
  match guard req with Ok v -> f v | Error e -> Error e

let ( &&& ) g1 g2 req =
  match g1 req with
  | Ok v1 -> (match g2 req with Ok v2 -> Ok (v1, v2) | Error e -> Error e)
  | Error e -> Error e

let ( ||| ) g1 g2 req = match g1 req with Ok v -> Ok v | Error _ -> g2 req

let bearer_token req =
  match Piaf.Headers.get (Request.headers req) "authorization" with
  | Some auth ->
    (match String.split_on_char ' ' auth with
    | [ "Bearer"; token ] -> Ok token
    | _ -> Error Invalid_bearer_token)
  | None -> Error Invalid_bearer_token

let header name req =
  match
    Piaf.Headers.get (Request.headers req) (String.lowercase_ascii name)
  with
  | Some value -> Ok value
  | None -> Error (Missing_header name)

let optional_header name req =
  Ok (Piaf.Headers.get (Request.headers req) (String.lowercase_ascii name))

let query_param name req =
  let uri = Request.uri req in
  match Uri.get_query_param uri name with
  | Some value -> Ok value
  | None -> Error (Missing_query_param name)

let optional_query_param name req =
  let uri = Request.uri req in
  Ok (Uri.get_query_param uri name)
