open Imports

module Log =
  (val Logging.setup ~src:"middleware_logger" ~doc:"Middleware Logger")

type log_info =
  { client_ip : string
  ; request_method : string
  ; request_uri : string
  ; request_protocol : string
  ; status : int
  ; response_bytes : int option
  ; referer : string option
  ; user_agent : string option
  ; request_id : string option
  ; duration_ms : float
  }

type formatter = log_info -> string

let apache_common_log_format (info : log_info) : string =
  let bytes =
    match info.response_bytes with Some n -> string_of_int n | None -> "-"
  in
  let referer =
    match info.referer with Some r -> Format.sprintf "\"%s\"" r | None -> "-"
  in
  let user_agent =
    match info.user_agent with
    | Some ua -> Format.sprintf "\"%s\"" ua
    | None -> "-"
  in
  Format.sprintf
    "%s - - \"%s %s %s\" %d %s %s %s %.0f"
    info.client_ip
    info.request_method
    info.request_uri
    info.request_protocol
    info.status
    bytes
    referer
    user_agent
    info.duration_ms

type t =
  { now : unit -> float
  ; formatter : formatter
  ; trusted_proxies : Ipaddr.Prefix.t list
  }

let args ~now ~trusted_proxies ?(formatter = apache_common_log_format) () : t =
  { now; trusted_proxies; formatter }

let build_log_info ~args ~duration_ms request response =
  let client_ip =
    Request.client_ip ~trusted_proxies:args.trusted_proxies request
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
  ; status = Response.status response |> Piaf.Status.to_code
  ; response_bytes
  ; referer
  ; user_agent
  ; request_id
  ; duration_ms
  }

let call args next request =
  let start_time = args.now () in
  let response = next request in
  let duration_ms = args.now () -. start_time in
  let log_info = build_log_info ~args ~duration_ms request response in
  Log.info (fun m -> m "%s" (args.formatter log_info));
  response
