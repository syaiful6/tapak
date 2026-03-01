open Imports
module Log = (val Logging.setup ~src:"tapak.server" ~doc:"Tapak Server module")

(** Parse systemd socket activation environment variables.
    Returns the Unix file descriptor for FD 3 (first listening socket).
    See: https://www.freedesktop.org/software/systemd/man/sd_listen_fds.html *)
let get_systemd_listen_fd () =
  match
    Sys.getenv_opt "LISTEN_FDS" |> Fun.flip Option.bind int_of_string_opt
  with
  | Some n when n > 0 ->
    let first_fd =
      Sys.getenv_opt "LISTEN_FDS_FIRST_FD"
      |> Fun.flip Option.bind int_of_string_opt
      |> Option.value ~default:3
    in
    Some (Obj.magic first_fd : Unix.file_descr)
  | _ -> None

let request_from_cohttp :
   Cohttp_eio.Server.conn
  -> Http.Request.t
  -> Cohttp_eio.Server.body
  -> Request.t
  =
 fun conn req body ->
  let client_addr =
    let sockaddr = conn |> Stdlib.fst |> Stdlib.snd in
    match sockaddr with
    | `Tcp (ip, _) -> Some (Format.asprintf "%a" Eio.Net.Ipaddr.pp ip)
    | `Unix _ -> None
  in
  let headers = Http.Request.headers req in
  let meth = Http.Request.meth req in
  let target = Http.Request.resource req in
  let version = Http.Request.version req in
  Request.make ?client_addr ~version ~headers ~meth ~body target

let response_to_cohttp request (response : Response.t) =
  let body = Response.body response in
  let raw_header = Response.headers response in
  let headers =
    match body.content, Response.status response with
    | `Raw _, _ -> raw_header
    | _, `Switching_protocols -> raw_header
    | _, _ ->
      (match Headers.get_transfer_encoding raw_header, body.length with
        | Unknown, Some content_length ->
          Headers.add_transfer_encoding raw_header (Fixed content_length)
        | Unknown, None -> Headers.add_transfer_encoding raw_header Chunked
        | _ -> raw_header)
      |> fun headers ->
      (match Headers.connection headers with
      | Some _ -> headers
      | None ->
        Headers.add
          headers
          "connection"
          (if Request.is_keep_alive request then "keep-alive" else "close"))
  in
  let status = Response.status response in
  let version = Response.version response in
  Http.Response.make ~version ~status ~headers ()

let send_response_body_writer (response : Http.Response.t) stream _ic oc =
  let need_chunked_encoding =
    match Headers.get_transfer_encoding response.headers with
    | Http.Transfer.Chunked -> true
    | _ -> false
  in
  let writer =
    if need_chunked_encoding
    then
      Bytesrw_util.(
        chunked_writer ~write_footer:true () ~eod:true (writer_of_eio_buf oc))
    else Bytesrw_util.writer_of_eio_buf oc
  in
  stream writer (fun () -> Eio.Buf_write.flush oc);
  Eio.Buf_write.flush oc

let make :
   ?conn_closed:(Cohttp_eio.Server.conn -> unit)
  -> Handler.t
  -> Cohttp_eio.Server.t
  =
 fun ?conn_closed handler ->
  let callback conn req body =
    let request = request_from_cohttp conn req body in
    let response =
      try handler request with
      | exn ->
        Log.err (fun m -> m "Handler raised an exception: %a" Fmt.exn exn);
        Response.plain ~status:`Internal_server_error "Internal Server Error"
    in
    let body = Response.body response in
    let headers = Response.headers response in
    let is_head = match request.meth with `HEAD -> true | _ -> false in
    match body.content with
    | `Empty ->
      `Response
        (Cohttp_eio.Server.respond_string
           ~headers
           ~status:(Response.status response)
           ~body:""
           ())
    | `String body ->
      `Response
        (Cohttp_eio.Server.respond_string
           ~headers
           ~status:(Response.status response)
           ~body:(if is_head then "" else body)
           ())
    | `Raw callback ->
      let response = response_to_cohttp request response in
      `Expert (response, callback)
    | `Stream _ when is_head ->
      let response = response_to_cohttp request response in
      `Response
        (Cohttp_eio.Server.respond_string
           ~headers
           ~status:(Http.Response.status response)
           ~body:""
           ())
    | `Stream stream ->
      let response = response_to_cohttp request response in
      (* cohttp will write the headers for us, so we only need to write the
         body, and flush it at the end of stream *)
      `Expert (response, send_response_body_writer response stream)
  in
  Cohttp_eio.Server.make_response_action ?conn_closed ~callback ()

let run ?max_connections ?additional_domains ?stop ~on_error socket service =
  Cohttp_eio.Server.run
    ?max_connections
    ?additional_domains
    ?stop
    ~on_error
    socket
    (make service)
