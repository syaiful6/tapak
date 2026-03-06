open Imports

type t =
  { version : Http.Version.t
  ; status : Http.Status.t
  ; headers : Http.Header.t
  ; body : Body.t
  }

include Headers.Make (struct
    type nonrec t = t

    let with_headers f t = { t with headers = f t.headers }
    let headers t = t.headers
  end)

let make
      ?(version = `HTTP_1_1)
      ?(headers = Http.Header.init ())
      ?(status = `OK)
      body
  =
  { version; status; headers; body }

let pp_field field_name pp_v fmt v =
  Format.fprintf fmt "@[<1>%s:@ %a@]" field_name pp_v v

let pp fmt t =
  let open Format in
  pp_open_vbox fmt 0;
  pp_field "version" Http.Version.pp fmt t.version;
  pp_print_cut fmt ();
  pp_field "status" Http.Status.pp fmt t.status;
  pp_print_cut fmt ();
  pp_field "headers" Http.Header.pp_hum fmt t.headers;
  pp_print_cut fmt ();
  pp_close_box fmt ()

let status { status; _ } = status
let version { version; _ } = version
let body { body; _ } = body

let with_ ?status ?headers ?version ?body t =
  { version = Option.value version ~default:t.version
  ; status = Option.value status ~default:t.status
  ; headers = Option.value headers ~default:t.headers
  ; body = Option.value body ~default:t.body
  }

let of_string
      ?(version = `HTTP_1_1)
      ?(headers = Http.Header.init ())
      ?(status = `OK)
      body
  =
  { version; headers; body = Body.of_string body; status }

let stream ?version ?headers ?(status = `OK) ?length f =
  { version = Option.value version ~default:`HTTP_1_1
  ; headers = Option.value headers ~default:(Http.Header.init ())
  ; body = Body.stream ?length f
  ; status
  }

let writer ?version ?headers ?(status = `OK) ?length f =
  { version = Option.value version ~default:`HTTP_1_1
  ; headers = Option.value headers ~default:(Http.Header.init ())
  ; body = `Stream (length, f)
  ; status
  }

let plain ?version ?headers ?(status = `OK) body =
  let headers =
    Headers.add_unless_exists
      (Option.value headers ~default:Headers.empty)
      "Content-Type"
      "text/plain"
  in
  of_string ?version ~headers ~status body

let html ?version ?headers ?(status = `OK) body =
  let headers =
    Headers.add_unless_exists
      (Option.value headers ~default:Headers.empty)
      "Content-Type"
      "text/html; charset=utf-8"
  in
  of_string ?version ~headers ~status body

let json ?version ?headers ?(status = `OK) body =
  let headers =
    Headers.add_unless_exists
      (Option.value headers ~default:Headers.empty)
      "Content-Type"
      "application/json; charset=utf-8"
  in
  of_string ?version ~headers ~status body

let redirect
      ?version
      ?headers
      ?(status : Http.Status.redirection = `Found)
      location
  =
  let headers =
    Headers.add_unless_exists
      (Option.value headers ~default:(Headers.init ()))
      "Location"
      location
  in
  of_string ?version ~headers ~status:(status :> Http.Status.t) ""

let negotiate
      ?version
      ?(status = `OK)
      ?(headers = Headers.empty)
      ?(formats = Header_parser.Content_negotiation.default_accept_formats)
      request
      render
  =
  let open Option.Syntax in
  let accept_header = Request.header "Accept" request in
  let response =
    let* format =
      Header_parser.Content_negotiation.negotiate_format accept_header formats
    in
    let* content_type, body = render format in
    let headers =
      Headers.add_unless_exists headers "Content-Type" content_type
    in
    Option.some @@ of_string ?version ~status ~headers body
  in
  Option.value
    response
    ~default:
      (of_string ?version ~headers ~status:`Not_acceptable "Not Acceptable")

let websocket ?size_limit f (request : Request.t) =
  match
    Cows.upgrade_headers
      (Http.Request.make
         ~meth:(Request.meth request)
         ~version:(Request.version request)
         ~headers:(Request.headers request)
         (Request.target request))
  with
  | None -> of_string ~status:`Bad_request "Bad WebSocket handshake"
  | Some headers ->
    let raw =
      Body.raw (fun ic oc ->
        let conn = Cows.make ?size_limit ~role:Server ic oc in
        f conn)
    in
    make ~status:`Switching_protocols ~headers raw

let file ?version ?headers ?(status = `OK) ?(follow = true) path =
  let file_stat =
    try Ok Eio.Path.(stat ~follow path) with
    | Eio.Exn.Io (Eio.Fs.E (Not_found _), _) -> Error `Not_found
    | Eio.Exn.Io (Eio.Fs.E (Permission_denied _), _) -> Error `Forbidden
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Error `Not_found
    | Unix.Unix_error (Unix.EACCES, _, _) -> Error `Forbidden
    | _ -> Error `Internal_server_error
  in
  match file_stat with
  | Error status ->
    of_string
      ?version
      ?headers
      ~status
      (Http.Status.reason_phrase_of_code (Http.Status.to_int status))
  | Ok stat ->
    let content_type =
      Magic_mime.lookup
        ~default:"application/octet-stream"
        (Eio.Path.native_exn path)
    in
    let headers =
      Headers.add_unless_exists
        (Option.value headers ~default:Headers.empty)
        "Content-Type"
        content_type
    in
    let body =
      Body.stream ~length:(Optint.Int63.to_int64 stat.size) (fun write flush ->
        Eio.Path.with_open_in path (fun flow ->
          try
            let cs = Cstruct.create 65536 in
            while true do
              let len = Eio.Flow.single_read flow cs in
              write (Cstruct.to_string ~off:0 ~len cs)
            done
          with
          | End_of_file -> flush ()))
    in
    make ?version ~headers ~status body
