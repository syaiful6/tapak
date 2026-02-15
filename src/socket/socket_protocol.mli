type t =
  { join_ref : string option
  ; ref_ : string option
  ; topic : string
  ; event : string
  ; payload : Jsont.json
  }

val phx_join : string
val phx_leave : string
val phx_reply : string
val phx_error : string
val phx_close : string
val heartbeat : string
val jsont : t Jsont.t
