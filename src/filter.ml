type ('req, 'res, 'req2, 'res2) t =
  ('req, 'res) Service.t -> ('req2, 'res2) Service.t

type ('req, 'res) simple = ('req, 'res, 'req, 'res) t

let ( >>> ) f1 f2 s = s |> f1 |> f2

let apply_all filters service =
  List.fold_right (fun f acc -> f acc) filters service

let strip_prefix : prefix:string -> (Request.t, Response.t) simple =
 fun ~prefix next request ->
  let uri = Request.uri request in
  let path = Uri.path uri in
  if String.starts_with ~prefix path
  then
    let prefix_len = String.length prefix in
    let path_len = String.length path in
    let new_path =
      if prefix_len >= path_len
      then "/"
      else
        let stripped = String.sub path prefix_len (path_len - prefix_len) in
        (* Ensure the new path starts with '/' *)
        if String.length stripped > 0 && String.get stripped 0 = '/'
        then stripped
        else "/" ^ stripped
    in
    let new_uri = Uri.with_path uri new_path in
    let new_request = Request.with_ ~target:(Uri.to_string new_uri) request in
    next new_request
  else next request
