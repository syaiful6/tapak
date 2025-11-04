include module type of Tapak_kernel.Body

(** {1 Body Size Limiting}

    Functions for limiting body size during reading. *)

val limit :
   sw:Eio.Switch.t
  -> max_bytes:int64
  -> t
  -> (t, [> `Msg of string ]) result
(** [limit ~sw ~max_bytes body] creates a new body that will fail if more than
    [max_bytes] are read from it.

    This function checks both:
    1. The declared length (from Content-Length or chunk encoding)
    2. The actual bytes read (for defense against lying clients)

    If the declared length exceeds [max_bytes], returns [Error] immediately
    without reading any data.

    If the declared length is unknown or within limits, returns a wrapped body
    that tracks bytes read and fails if the limit is exceeded. The [sw] parameter
    is used to manage the lifecycle of the background fiber that performs the
    limiting.

    {b Example:}
    {[
      let body = Request.body request in
      let request_info = Request.info request in
      match request_info.sw with
      | Some sw ->
        (match Body.limit ~sw ~max_bytes:(10 * 1024 * 1024) body with
        | Error (`Msg err) -> (* Declared size too large *)
        | Ok limited_body ->
            (* Read from limited_body, will fail if actual size exceeds limit *)
            Body.to_string limited_body)
      | None -> (* Handle missing switch *)
    ]} *)
