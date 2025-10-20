open Imports

module Log =
  (val Logging.setup ~src:"middleware_logger" ~doc:"Middleware Logger")

type request_info =
  { client_ip : string
  ; request_method : string
  ; request_uri : string
  ; request_protocol : string
  ; response_bytes : int option
  ; referer : string option
  ; user_agent : string option
  ; request_id : string option
  ; duration_ms : float
  }

type formatter = request_info -> string

let apache_common_log_format (info : request_info) : string =
  let bytes =
    match info.response_bytes with Some n -> string_of_int n | None -> "-"
  in
  let referer =
    match info.referer with Some r -> Printf.sprintf "\"%s\"" r | None -> "-"
  in
  let user_agent =
    match info.user_agent with
    | Some ua -> Printf.sprintf "\"%s\"" ua
    | None -> "-"
  in
  Printf.sprintf
    "%s - - \"%s %s %s\" %s %s %s %.0f"
    info.client_ip
    info.request_method
    info.request_uri
    info.request_protocol
    bytes
    referer
    user_agent
    info.duration_ms

type args =
  { now : unit -> float
  ; formatter : formatter
  ; trusted_proxies : string list
  }

type state = args

let args ~now ~trusted_proxies ?(formatter = apache_common_log_format) () : args
  =
  { now; trusted_proxies; formatter }

let get_remote_ip request =
  let open Option.Syntax in
  let request_info = Request.info request in
  let* address = request_info.client_address in
  match address with
  | `Tcp (ip_v4v6, _) -> Some (Fmt.str "%a" Eio.Net.Ipaddr.pp ip_v4v6)
  | `Unix _ -> None

let get_forwarded_ip ~trusted_proxies request =
  let is_ip_in_range ip range =
    (* TODO: Implement proper CIDR range checking. *)
    String.starts_with
      ~prefix:(String.split_on_char ~sep:'/' range |> List.hd)
      ip
  in
  let is_trusted_proxy remote_ip =
    match remote_ip with
    | None -> true (* unix socket, always trusted *)
    | Some ip -> List.exists (is_ip_in_range ip) trusted_proxies
  in
  let direct_connection_ip = get_remote_ip request in
  match Request.header "X-Forwarded-For" request with
  | Some xff_value when is_trusted_proxy direct_connection_ip ->
    (match String.split_on_char ~sep:',' xff_value with
    | first_ip :: _ -> String.trim first_ip
    | [] -> Option.fold ~none:"" ~some:Fun.id direct_connection_ip)
  | _ -> Option.fold ~none:"" ~some:Fun.id direct_connection_ip

let build_request_info ~args ~duration_ms request response =
  let client_ip =
    get_forwarded_ip ~trusted_proxies:args.trusted_proxies request
  in
  let response_bytes =
    Response.header "Content-Length" response |> fun opt ->
    Option.bind opt int_of_string_opt
  in
  let referer = Request.header "Referer" request in
  let user_agent = Request.header "User-Agent" request in
  let request_id = Request.header "X-Request-ID" request in
  { client_ip
  ; request_method = Piaf.Method.to_string (Request.meth request)
  ; request_uri = Piaf.Request.target (Request.to_piaf request)
  ; request_protocol =
      Format.asprintf "%a" Piaf.Versions.HTTP.pp (Request.version request)
  ; response_bytes
  ; referer
  ; user_agent
  ; request_id
  ; duration_ms
  }

external init : args -> state = "%identity"

let call args next request =
  let start_time = args.now () in
  let response = next request in
  let duration_ms = args.now () -. start_time in
  let request_info = build_request_info ~args ~duration_ms request response in
  Log.info (fun m -> m "%s" (args.formatter request_info));
  response
