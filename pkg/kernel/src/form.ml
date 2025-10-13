include Piaf.Form

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
