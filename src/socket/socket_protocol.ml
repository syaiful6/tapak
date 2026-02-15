open Imports

type t =
  { join_ref : string option
  ; ref_ : string option
  ; topic : string
  ; event : string
  ; payload : Jsont.json
  }

let phx_join = "phx_join"
let phx_leave = "phx_leave"
let phx_reply = "phx_reply"
let phx_error = "phx_error"
let phx_close = "phx_close"
let heartbeat = "heartbeat"
let opt_str = Jsont.option Jsont.string

let jsont =
  Jsont.map
    ~kind:"phoenix_socket_protocol"
    ~enc:(fun { join_ref; ref_; topic; event; payload } ->
      let results =
        Result.traverse_list
          Fun.id
          [ Jsont.Json.encode opt_str join_ref
          ; Jsont.Json.encode opt_str ref_
          ; Jsont.Json.encode Jsont.string topic
          ; Jsont.Json.encode Jsont.string event
          ; Jsont.Json.encode Jsont.json payload
          ]
      in
      match results with
      | Ok xs -> Jsont.Json.list xs
      | Error e ->
        Jsont.Error.msgf
          Jsont.Meta.none
          "Failed to encode Phoenix Socket Protocol: %s"
          e)
    ~dec:(function
      | Jsont.Array ([ join_ref; ref_; topic; event; payload ], meta) ->
        let result =
          let open Result.Syntax in
          let* join_ref = Jsont.Json.decode opt_str join_ref in
          let* ref_ = Jsont.Json.decode opt_str ref_ in
          let* topic = Jsont.Json.decode Jsont.string topic in
          let* event = Jsont.Json.decode Jsont.string event in
          let* payload = Jsont.Json.decode Jsont.json payload in
          Ok { join_ref; ref_; topic; event; payload }
        in
        (match result with
        | Ok v -> v
        | Error e ->
          Jsont.Error.msgf meta "Failed to decode Phoenix Socket Protocol: %s" e)
      | _ -> Jsont.Error.msg Jsont.Meta.none "Expected an array of length 5")
    Jsont.json_array
