type t =
  { cookie_name : string option
  ; expiration : Piaf.Cookies.expiration option
  ; domain : string option
  ; path : string option
  ; secure : bool
  ; http_only : bool
  ; same_site : Cookies.same_site option
  }

val generate_secret : unit -> string

val csrf_input :
   ?cookie_name:string
  -> Tapak_kernel.Request.t
  -> string * string

val verify_csrf_token : ?cookie_name:string -> token:string -> Request.t -> bool
val with_csrf_cookie : ?settings:t -> string -> Response.t -> Response.t
