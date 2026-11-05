open Imports

module Credential = struct
  type t =
    | Bearer of string
    | Other of string
end

let credential_of_string buf =
  match String.strip_prefix_ci ~prefix:"Bearer " buf with
  | Some token when not (String.equal (String.trim token) "") ->
    Credential.Bearer (String.trim token)
  | _ -> Credential.Other buf

let challenge = "Bearer"

let authorization req =
  Option.map credential_of_string (Request.header "Authorization" req)
