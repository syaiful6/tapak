include Piaf.Headers

(** [add_to_list_header t name value] adds [value] to the comma-separated list
    header [name]. If the header doesn't exist, it creates it with [value].
    If it exists, it appends [value] to the comma-separated list.

    This is useful for headers like Vary, Accept, Cache-Control, Allow, etc.
    that support comma-separated values according to RFC 7230.

    Example:
    {[
      let headers = empty in
      let headers = add_to_list_header headers "Vary" "Accept-Encoding" in
      let headers = add_to_list_header headers "Vary" "Cookie" in
      (* Results in: Vary: Accept-Encoding, Cookie *)
    ]}

    Note: This function does not deduplicate values. If you need deduplication,
    consider checking if the value exists before adding it. *)
let add_to_list_header headers name value =
  match get headers name with
  | None -> add headers name value
  | Some existing ->
    let values = String.split_on_char ',' existing |> List.map String.trim in
    if List.mem value values
    then headers
    else replace headers name (existing ^ ", " ^ value)

(** [add_to_list_header_multi t name values] adds multiple [values] to the
    comma-separated list header [name]. Efficiently handles multiple values
    at once.

    Example:
    {[
      let headers = empty in
      let headers = add_to_list_header_multi headers "Vary" ["Accept-Encoding"; "Cookie"] in
      (* Results in: Vary: Accept-Encoding, Cookie *)
    ]} *)
let add_to_list_header_multi headers name values =
  List.fold_left
    (fun acc value -> add_to_list_header acc name value)
    headers
    values
