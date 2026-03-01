open Imports

module Multipart = struct
  type part =
    { name : string
    ; filename : string option
    ; content_type : string
    ; body : string option Eio.Stream.t
    }

  type t = (string * part) list

  let content_type h = Multipart_form.Header.content_type h
  let content_disposition h = Multipart_form.Header.content_disposition h

  let name_of_header h =
    content_disposition h
    |> Fun.flip Option.bind Multipart_form.Content_disposition.name

  let stream
        ~sw
        ?(bounds = 10)
        ?(buffer_size = 8192)
        ~identify
        flow
        content_type
    =
    let output = Eio.Stream.create max_int in
    let q = Queue.create () in
    let buf = Cstruct.create buffer_size in
    let fresh_id =
      let r = ref 0 in
      fun () ->
        incr r;
        !r
    in
    let emitters header =
      let id = fresh_id () in
      Queue.push (`Id (header, id)) q;
      let push = function
        | None -> Queue.push (`End_of_part id) q
        | Some data -> Queue.push (`Data (id, data)) q
      in
      push, id
    in
    let parser = Multipart_form.parse ~emitters content_type in
    let tbl = Hashtbl.create 0x10 in
    let rec go () =
      match Queue.pop q with
      | `Id (header, id) ->
        let client_id = identify header in
        let stream = Eio.Stream.create bounds in
        Hashtbl.add tbl id (client_id, stream);
        Eio.Stream.add output (Some (client_id, header, stream));
        go ()
      | `Data (id, data) ->
        let _, stream = Hashtbl.find tbl id in
        Eio.Stream.add stream (Some data);
        go ()
      | `End_of_part id ->
        let _, stream = Hashtbl.find tbl id in
        Eio.Stream.add stream None;
        go ()
      | exception Queue.Empty ->
        let data =
          try
            let n = Eio.Flow.single_read flow buf in
            `String (Cstruct.to_string (Cstruct.sub buf 0 n))
          with
          | End_of_file -> `Eof
        in
        (match parser data with
        | `Continue -> go ()
        | `Done t ->
          let client_id_of_id id =
            let client_id, _ = Hashtbl.find tbl id in
            client_id
          in
          Eio.Stream.add output None;
          Result.Ok (Multipart_form.map client_id_of_id t)
        | `Fail _ -> Result.Error (`Msg "Invalid multipart/form"))
    in
    Eio.Fiber.fork_promise ~sw go, output

  let ( <.> ) f g x = f (g x)
  let add_new_line s = s ^ "\r\n"

  let parse request =
    let ctype =
      let default =
        Multipart_form.(
          Content_type.make
            `Text
            (`Iana_token "plain")
            Content_type.Parameters.empty)
      in
      Request.header "content-type" request
      |> Fun.flip
           Option.bind
           (Result.to_option
           <.> (Multipart_form.Content_type.of_string <.> add_new_line))
      |> Option.value ~default
    in
    Eio.Switch.run @@ fun sw ->
    let p, st =
      stream ~sw ~identify:name_of_header (Request.body request) ctype
    in
    let consumed =
      Eio.Fiber.fork_promise ~sw (fun () ->
        let parts = Hashtbl.create 0x10 in
        let rec go () =
          match Eio.Stream.take st with
          | Some (name, header, body) ->
            let key = Option.value name ~default:"" in
            Hashtbl.replace
              parts
              key
              { name = key
              ; filename =
                  content_disposition header
                  |> Fun.flip
                       Option.bind
                       Multipart_form.Content_disposition.filename
              ; content_type =
                  content_type header |> Multipart_form.Content_type.to_string
              ; body
              };
            go ()
          | None -> parts
        in
        go ())
    in
    ignore @@ Eio.Promise.await_exn p;
    Eio.Promise.await_exn consumed |> Hashtbl.to_seq |> List.of_seq

  let body_to_string body =
    let buffer = Buffer.create 1024 in
    let rec go () =
      match Eio.Stream.take body with
      | Some chunk ->
        Buffer.add_string buffer chunk;
        go ()
      | None -> Buffer.contents buffer
    in
    go ()

  let part_to_string part = body_to_string part.body
  let get_part key fields = List.assoc_opt key fields

  let get_all_parts key fields =
    List.filter_map
      (fun (k, part) -> if String.equal k key then Some part else None)
      fields

  let get_field key fields =
    match get_part key fields with
    | None -> None
    | Some { body; _ } -> Some (body_to_string body)

  let get_all_fields key fields =
    let parts = get_all_parts key fields in
    let rec read_all acc = function
      | [] -> List.rev acc
      | { body; _ } :: rest -> read_all (body_to_string body :: acc) rest
    in
    read_all [] parts

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
  let of_body body = Eio.Flow.read_all body |> of_string

  let of_query request =
    let target = Request.target request in
    match String.index_opt target '?' with
    | Some i ->
      of_string
        (String.sub target ~pos:(i + 1) ~len:(String.length target - i - 1))
    | None -> []

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
      match String.split_on_char ~sep:'[' key with
      | [] -> []
      | base :: rest ->
        let sanitize_part part =
          if String.ends_with ~suffix:"]" part
          then String.sub part ~pos:0 ~len:(String.length part - 1)
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
