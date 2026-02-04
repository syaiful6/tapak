open Imports

(* Kernel types *)
type t =
  { request : Piaf.Request.t
  ; ctx : Request_info.t
  }

let meth { request; _ } = Piaf.Request.meth request
let target { request; _ } = Piaf.Request.target request
let version { request; _ } = Piaf.Request.version request
let headers { request; _ } = Piaf.Request.headers request
let scheme { request; _ } = Piaf.Request.scheme request
let body { request; _ } = Piaf.Request.body request
let uri { request; _ } = Piaf.Request.uri request
let info { ctx; _ } = ctx
let to_piaf { request; _ } = request
let context t = t |> info |> fun ctx -> ctx.env

let create
      ~scheme
      ~version
      ?(headers = Piaf.Headers.empty)
      ~meth
      ~body
      ?(ctx = Request_info.default)
      target
  =
  let request =
    Piaf.Request.create ~scheme ~version ~headers ~meth ~body target
  in
  { request; ctx }

let with_ ?meth ?target ?version ?headers ?body ?ctx request =
  { request =
      Piaf.Request.with_ ?meth ?target ?version ?headers ?body request.request
  ; ctx = Option.value ctx ~default:request.ctx
  }

let with_context env request =
  let ctx = Request_info.with_ ~env request.ctx in
  { request with ctx }

let pp_hum fmt t = Piaf.Request.pp_hum fmt t.request

(* Extended functions from src *)
let header key request = Headers.get (headers request) key

let add_header key value request =
  let headers = Headers.add (headers request) key value in
  with_ ~headers request

let remove_header key request =
  let headers = Headers.remove (headers request) key in
  with_ ~headers request

let replace_header key value request =
  let headers = Headers.replace (headers request) key value in
  with_ ~headers request

let remote_ip request =
  let open Option.Syntax in
  let info = info request in
  let* address = info.client_address in
  match address with
  | `Tcp (ip_v4v6, _) -> Some (Format.asprintf "%a" Eio.Net.Ipaddr.pp ip_v4v6)
  | `Unix _ -> None

let is_trusted_proxy ~trusted_proxies ip =
  match ip with
  | None -> true (* this is unix socket *)
  | Some ip ->
    let ip_addr_result = Ipaddr.of_string ip in
    if Result.is_ok ip_addr_result
    then
      let ip_addr = Result.get_ok ip_addr_result in
      List.exists
        (fun ip_range -> Ipaddr.Prefix.mem ip_addr ip_range)
        trusted_proxies
    else false

let client_ip ~trusted_proxies request =
  let direct_connection_ip = remote_ip request in
  match header "X-Forwarded-For" request with
  | Some xff_value when is_trusted_proxy ~trusted_proxies direct_connection_ip
    ->
    (match String.split_on_char ~sep:',' xff_value with
    | first_ip :: _ -> String.trim first_ip
    | [] -> Option.fold ~none:"" ~some:Fun.id direct_connection_ip)
  | _ -> Option.fold ~none:"" ~some:Fun.id direct_connection_ip

let is_secure ~trusted_proxies request =
  let scheme_is_https =
    match scheme request with `HTTPS -> true | `HTTP -> false
  in
  if scheme_is_https
  then true
  else
    let direct_connection_ip = remote_ip request in
    match header "X-Forwarded-Proto" request with
    | Some proto when is_trusted_proxy ~trusted_proxies direct_connection_ip ->
      String.(equal (lowercase_ascii (trim proto)) "https")
    | _ -> false
