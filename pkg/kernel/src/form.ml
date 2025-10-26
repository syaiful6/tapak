module Multipart = struct
  module PM = Piaf.Form.Multipart

  type part =
    { name : string
    ; filename : string option
    ; content_type : string
    ; body : Body.t
    }

  type t = (string * part) list

  let to_msg_error = function
    | `Msg msg -> `Msg msg
    | `Bad_gateway -> `Msg "Bad gateway"
    | `Bad_request -> `Msg "Bad request"
    | `Connect_error msg -> `Msg ("Connect error: " ^ msg)
    | `Exn exn -> `Msg ("Exception: " ^ Printexc.to_string exn)
    | `Internal_server_error -> `Msg "Internal server error"
    | `Invalid_response_body_length _ -> `Msg "Invalid response body length"
    | `Malformed_response msg -> `Msg ("Malformed response: " ^ msg)
    | `Protocol_error (_, msg) -> `Msg ("Protocol error: " ^ msg)
    | `TLS_error _ -> `Msg "TLS error"
    | `Upgrade_not_supported -> `Msg "Upgrade not supported"

  let parse ?(max_chunk_size = 0x100000 (* 1 MB *)) request =
    let { Request.request = piaf_request; _ } = request in
    match PM.assoc ~max_chunk_size piaf_request with
    | Error e -> Error (to_msg_error e)
    | Ok piaf_fields ->
      let fields =
        List.map
          (fun (_key, piaf_part) ->
             let { PM.name; filename; content_type; body } = piaf_part in
             name, { name; filename; content_type; body })
          piaf_fields
      in
      Ok fields

  let get_part key fields = List.assoc_opt key fields

  let get_all_parts key fields =
    List.filter_map
      (fun (k, part) -> if String.equal k key then Some part else None)
      fields

  let get_field key fields =
    match get_part key fields with
    | None -> None
    | Some { body; _ } ->
      (match Body.to_string body with
      | Ok value -> Some (Ok value)
      | Error e -> Some (Error (to_msg_error e)))

  let get_all_fields key fields =
    let parts = get_all_parts key fields in
    let rec read_all acc = function
      | [] -> Ok (List.rev acc)
      | { body; _ } :: rest ->
        (match Body.to_string body with
        | Ok value -> read_all (value :: acc) rest
        | Error e -> Error (to_msg_error e))
    in
    read_all [] parts

  let drain fields =
    let rec drain_all errors = function
      | [] ->
        (match errors with
        | [] -> Ok ()
        | errs -> Error (`Msg (String.concat "; " (List.rev errs))))
      | (_, { body; _ }) :: rest ->
        (match Body.drain body with
        | Ok () -> drain_all errors rest
        | Error e ->
          let err_msg = match to_msg_error e with `Msg m -> m in
          drain_all (err_msg :: errors) rest)
    in
    drain_all [] fields
end

module Urlencoded = struct
  type t = (string * string list) list

  let of_string s = if s = "" then [] else s |> Uri.query_of_encoded

  let of_body body =
    Body.to_string body
    |> Result.map_error (fun _ -> `Bad_request)
    |> Result.map of_string

  let of_query request =
    let uri = Request.uri request in
    match Uri.query uri with [] -> [] | query -> query

  (* Normalize by grouping duplicate keys into a single entry with all values *)
  let normalize params =
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun (key, values) ->
         match Hashtbl.find_opt tbl key with
         | None -> Hashtbl.add tbl key values
         | Some existing -> Hashtbl.replace tbl key (existing @ values))
      params;
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
    |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)

  (* Get the first value for a key, returns None if key doesn't exist *)
  let get key params =
    match List.assoc_opt key params with
    | None -> None
    | Some [] -> None
    | Some (v :: _) -> Some v

  (* Get all values for a key as a list *)
  let get_list key params =
    List.filter_map
      (fun (k, vs) -> if String.equal k key then Some vs else None)
      params
    |> List.concat
end
