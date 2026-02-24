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

  type node =
    | Object of (string, node) Hashtbl.t
    | Array of node list
    | Part of part

  let to_tree (parts : t) : node =
    (* Group parts by name; multiple parts with the same name form an array. Per
       OpenAPI multipart spec, nested objects/arrays are encoded as a single
       part with Content-Type: application/json rather than bracket notation. *)
    let by_name = Hashtbl.create 16 in
    List.iter
      (fun (name, part) ->
         match Hashtbl.find_opt by_name name with
         | None -> Hashtbl.add by_name name [ part ]
         | Some existing -> Hashtbl.replace by_name name (part :: existing))
      parts;
    let root_tbl = Hashtbl.create (Hashtbl.length by_name) in
    Hashtbl.iter
      (fun name part_list ->
         let ordered = List.rev part_list in
         let node =
           match ordered with
           | [ single ] -> Part single
           | multiple -> Array (List.map (fun p -> Part p) multiple)
         in
         Hashtbl.add root_tbl name node)
      by_name;
    Object root_tbl
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

  type node =
    | Object of (string, node) Hashtbl.t
    | Array of node list ref
    | Value of string list

  let to_json params =
    let params = normalize params in
    let parse_key key =
      match String.split_on_char '[' key with
      | [] -> []
      | base :: rest ->
        let sanitize_part part =
          if String.ends_with ~suffix:"]" part
          then String.sub part 0 (String.length part - 1)
          else part (* This would be a malformed key like `user[name` *)
        in
        base :: List.map sanitize_part rest
    in
    let is_numeric s =
      try
        let _ = int_of_string s in
        true
      with
      | Failure _ -> false
    in
    let root = Object (Hashtbl.create 16) in
    let rec insert node path values =
      match node, path with
      | _, [] -> ()
      | Object h, [ key ] ->
        (* If a value for the same key already exists, this will overwrite. This
           is standard behavior; the last value sent wins. *)
        Hashtbl.replace h key (Value values)
      | Object h, key :: rest ->
        let next_node =
          match Hashtbl.find_opt h key with
          | Some n -> n
          | None ->
            let new_node =
              match rest with
              | "" :: _ -> Array (ref []) (* items[] *)
              | x :: _ when is_numeric x -> Array (ref []) (* items[0] *)
              | _ -> Object (Hashtbl.create 4)
              (* items[foo] *)
            in
            Hashtbl.add h key new_node;
            new_node
        in
        insert next_node rest values
      | Array l, "" :: rest ->
        (match rest with
        | [] ->
          (* Simple array of values: tags[]=foo&tags[]=bar Add each value as a
             separate element to the array. *)
          List.iter (fun v -> l := !l @ [ Value [ v ] ]) values
        | _ ->
          (* Array of objects: items[][id]=1&items[][name]=foo Create a new
             object and recurse into it. *)
          let new_obj = Object (Hashtbl.create 4) in
          l := !l @ [ new_obj ];
          insert new_obj rest values)
      | Array l, key :: rest ->
        (* This handles `items[0][id]`. It treats the array as an object where
           keys are integer strings. *)
        let idx = int_of_string key in
        let rec get_or_create_at_idx i current_list =
          match current_list with
          | [] when i = 0 ->
            let new_node =
              match rest with
              | [] -> Value values
              | "" :: _ -> Array (ref [])
              | _ -> Object (Hashtbl.create 4)
            in
            l := !l @ [ new_node ];
            new_node
          | x :: _ when i = 0 -> x
          | _ :: xs -> get_or_create_at_idx (i - 1) xs
          | [] ->
            (* This case handles sparse arrays, e.g. items[2] when items[0] and
               items[1] don't exist. We pad with `Value []` which will become
               `null`. *)
            let rec pad_and_create n =
              if n < 0
              then failwith "Invalid array index"
              else if n = 0
              then (
                let new_node =
                  match rest with
                  | [] -> Value values
                  | _ -> Object (Hashtbl.create 4)
                in
                l := !l @ [ Value [] (* padding *); new_node ];
                new_node)
              else (
                l := !l @ [ Value [] ];
                pad_and_create (n - 1))
            in
            pad_and_create (idx - List.length !l)
        in
        let node_at_idx = get_or_create_at_idx idx !l in
        insert node_at_idx rest values
      (* Any other combination is a conflict/error. *)
      | Value _, _ :: _ ->
        (* e.g., trying to insert into `user[name]` when `user` is already a value. *)
        (* For now, we silently ignore, though an exception could be raised. *)
        ()
    in
    List.iter (fun (key, values) -> insert root (parse_key key) values) params;
    let rec as_json = function
      | Object h ->
        Jsont.Json.object'
          (Hashtbl.to_seq h
          |> List.of_seq
          |> List.map (fun (k, v) -> Jsont.Json.name k, as_json v))
      | Array l -> Jsont.Json.list (List.map as_json !l)
      | Value [] -> Jsont.Json.null ()
      | Value [ v ] -> Jsont.Json.string v
      | Value vs -> Jsont.Json.list (List.map Jsont.Json.string vs)
    in
    as_json root
end
