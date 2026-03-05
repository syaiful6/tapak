open Imports

type t =
  { headers : Http.Header.t
  ; meth : Http.Method.t
  ; target : string
  ; version : Http.Version.t
  ; body : Eio.Flow.source_ty Eio.Resource.t
  ; client_addr : string option
    (* ip addr of the client, if none it connected from unix socket *)
  ; is_secure : bool
    (* Was this request made over an SSL connection? Note that this value will
       not tell you if the client originally made this request over SSL. This
       distinction appear when the application behind reverse proxy, where the
       proxy handle SSL termination, but connection from proxy to application is
       not over SSL *)
  }

include Headers.Make (struct
    type nonrec t = t

    let with_headers f t = { t with headers = f t.headers }
    let headers t = t.headers
  end)

let meth t = t.meth
let target t = t.target
let version t = t.version
let body t = t.body
let empty_body = Cohttp_eio.Body.of_string ""

let is_trusted_proxy ~trusted_proxies ip =
  match ip with
  | None -> true (* this is unix socket, so we trust it *)
  | Some ip ->
    (match Ipaddr.of_string ip with
    | Error _ -> false
    | Ok ip_addr ->
      List.exists
        (fun ip_range -> Ipaddr.Prefix.mem ip_addr ip_range)
        trusted_proxies)

let client_ip ?trusted_proxies t =
  let direct_client_ip = t.client_addr in
  match header "X-Forwarded-For" t with
  | Some xff_value
    when is_trusted_proxy
           ~trusted_proxies:(Option.value trusted_proxies ~default:[])
           direct_client_ip ->
    (match String.split_on_char ~sep:',' xff_value with
    | first_ip :: _ -> String.trim first_ip
    | [] -> Option.value direct_client_ip ~default:"")
  | _ -> Option.value direct_client_ip ~default:""

let is_secure ?trusted_proxies t =
  if t.is_secure
  then true
  else
    match trusted_proxies with
    | None -> false
    | Some trusted_proxies -> is_trusted_proxy ~trusted_proxies t.client_addr

let make
      ?(version = `HTTP_1_1)
      ?(body = empty_body)
      ?client_addr
      ?headers
      ?(meth = `GET)
      ?(is_secure = false)
      target
  =
  { client_addr
  ; headers = Option.value headers ~default:(Http.Header.init ())
  ; meth
  ; target
  ; version
  ; is_secure
  ; body
  }

let with_ ?meth ?target ?version ?headers ?body ?client_addr ?is_secure req =
  { client_addr = Option.value client_addr ~default:req.client_addr
  ; headers = Option.value headers ~default:req.headers
  ; is_secure = Option.value is_secure ~default:req.is_secure
  ; meth = Option.value meth ~default:req.meth
  ; target = Option.value target ~default:req.target
  ; version = Option.value version ~default:req.version
  ; body = Option.value body ~default:req.body
  }

let pp_field field_name pp_v fmt v =
  Format.fprintf fmt "@[<1>%s:@ %a@]" field_name pp_v v

let pp fmt t =
  let open Format in
  pp_open_vbox fmt 0;
  pp_field "meth" Http.Method.pp fmt t.meth;
  pp_print_cut fmt ();
  pp_field "target" pp_print_string fmt t.target;
  pp_print_cut fmt ();
  pp_field "version" Http.Version.pp fmt t.version;
  pp_print_cut fmt ();
  pp_field "headers" Http.Header.pp_hum fmt t.headers;
  pp_print_cut fmt ();
  pp_field
    "client_addr"
    (fun fmt -> function
       | Some addr -> Format.fprintf fmt "%s" addr
       | None -> Format.fprintf fmt "None")
    fmt
    t.client_addr;
  pp_close_box fmt ()

let is_keep_alive t =
  match Http.Header.connection (headers t) with
  | Some `Close -> false
  | Some `Keep_alive -> true
  | Some (`Unknown _) -> false
  | None -> Http.Version.compare (version t) `HTTP_1_1 = 0
