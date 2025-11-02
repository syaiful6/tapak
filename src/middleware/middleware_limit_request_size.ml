type t = { max_bytes : int64 }

let args ~max_bytes = { max_bytes }

let call state service request =
  let request_info = Request.info request in
  match request_info.sw with
  | None ->
    (* No switch available - cannot safely limit request body with streaming.
       Fall back to synchronous limiting with Eio.Switch.run. *)
    (match Request.header "content-length" request with
    | Some len_str ->
      (match Int64.of_string_opt len_str with
      | Some len when len > state.max_bytes ->
        let body = Request.body request in
        (match Body.drain body with
        | Ok () | Error _ ->
          Response.of_string
            ~body:
              (Printf.sprintf
                 "Request too large: %Ld bytes (limit: %Ld bytes)"
                 len
                 state.max_bytes)
            `Payload_too_large)
      | _ ->
        let body = Request.body request in
        Eio.Switch.run (fun sw ->
          match Body.limit ~sw ~max_bytes:state.max_bytes body with
          | Error (`Msg err) ->
            let _ = Body.drain body in
            Response.of_string ~body:err `Payload_too_large
          | Ok limited_body ->
            let request = Request.with_ ~body:limited_body request in
            service request))
    | None ->
      let body = Request.body request in
      Eio.Switch.run (fun sw ->
        match Body.limit ~sw ~max_bytes:state.max_bytes body with
        | Error (`Msg err) ->
          let _ = Body.drain body in
          Response.of_string ~body:err `Payload_too_large
        | Ok limited_body ->
          let request = Request.with_ ~body:limited_body request in
          service request))
  | Some sw ->
    (* Normal case: use the request's switch *)
    (match Request.header "content-length" request with
    | Some len_str ->
      (match Int64.of_string_opt len_str with
      | Some len when len > state.max_bytes ->
        let body = Request.body request in
        (match Body.drain body with
        | Ok () | Error _ ->
          Response.of_string
            ~body:
              (Printf.sprintf
                 "Request too large: %Ld bytes (limit: %Ld bytes)"
                 len
                 state.max_bytes)
            `Payload_too_large)
      | _ ->
        let body = Request.body request in
        (match Body.limit ~sw ~max_bytes:state.max_bytes body with
        | Error (`Msg err) ->
          let _ = Body.drain body in
          Response.of_string ~body:err `Payload_too_large
        | Ok limited_body ->
          let request = Request.with_ ~body:limited_body request in
          service request))
    | None ->
      let body = Request.body request in
      (match Body.limit ~sw ~max_bytes:state.max_bytes body with
      | Error (`Msg err) ->
        let _ = Body.drain body in
        Response.of_string ~body:err `Payload_too_large
      | Ok limited_body ->
        let request = Request.with_ ~body:limited_body request in
        service request))
