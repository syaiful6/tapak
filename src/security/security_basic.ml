open Imports

module Challenge = struct
  type t = Basic of string
end

module Credential = struct
  type t =
    | Basic of string * string
    | Other of string
end

let string_of_credential = function
  | Credential.Basic (username, password) ->
    Format.sprintf
      "Basic %s"
      (Base64.encode_string (Format.sprintf "%s:%s" username password))
  | Other s -> s

let credential_of_string buf =
  let credential =
    let open Option.Syntax in
    (* The scheme name is case-insensitive, RFC 7235 section 2.1 *)
    let* b64 = String.strip_prefix_ci ~prefix:"Basic " buf in
    let* decoded = Base64.decode b64 |> Result.to_option in
    let+ idx = String.index_opt decoded ':' in
    let username = String.sub decoded ~pos:0 ~len:idx in
    let password =
      String.sub decoded ~pos:(idx + 1) ~len:(String.length decoded - idx - 1)
    in
    Credential.Basic (username, password)
  in
  Option.value ~default:(Credential.Other buf) credential

let string_of_challenge = function
  | Challenge.Basic realm -> Format.sprintf "Basic realm=\"%s\"" realm

let authorization req =
  Option.map credential_of_string (Request.header "Authorization" req)
