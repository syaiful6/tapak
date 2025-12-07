(** Cross Site Forgery (CSRF) protection using the double-submit cookie pattern.

    This module provides functions to generate and verify CSRF tokens. It's not
    implemented as middleware, since it requires us to consume the request body.
  *)

type t =
  { cookie_name : string option
    (** Cookie name for storing the CSRF secret. Defaults to ["XSRF-TOKEN"]. *)
  ; expiration : Piaf.Cookies.expiration option
    (** Cookie expiration time. Defaults to 1 year. *)
  ; domain : string option
    (** Cookie domain. Defaults to [None] (current domain). *)
  ; path : string option  (** Cookie path. Defaults to ["/"]. *)
  ; secure : bool
    (** Whether to set the Secure flag (HTTPS only). Defaults to [false]. *)
  ; http_only : bool
    (** Whether to set the HttpOnly flag. Defaults to [false] to allow JavaScript access. *)
  ; same_site : Cookies.same_site option
    (** SameSite cookie attribute. Defaults to [`Lax]. *)
  }
(** Cookie settings for CSRF protection. *)

val generate_secret : unit -> string
(** [generate_secret ()] generates a new random 32-byte secret.

    This is automatically called by {!input} if no existing secret is found
    in the request cookie. *)

val input : ?cookie_name:string -> Tapak_kernel.Request.t -> string * string
(** [input req] generates or retrieves a CSRF token and secret for form rendering.

    Returns [(token, secret)] where:
    - [token] is a masked 64-byte token to embed in your form (safe to send to client)
    - [secret] is the 32-byte secret to store in a cookie (must be kept server-side)

    If a secret already exists in the request cookie, it will be reused. Otherwise,
    a new secret is generated.

    The token is masked on each call to prevent BREACH attacks, so the same secret
    produces different tokens on each request.

    @param cookie_name The name of the cookie to check for existing secret.
                       Defaults to ["XSRF-TOKEN"]. *)

val verify_token : ?cookie_name:string -> token:string -> Request.t -> bool
(** [verify_token ~token req] verifies that a submitted CSRF token matches the
    secret stored in the request cookie.

    Returns [true] if:
    - The secret cookie exists
    - The token is validly formatted (32 or 64 bytes)
    - The unmasked token matches the secret (using constant-time comparison)

    Returns [false] otherwise.

    @param cookie_name The name of the cookie containing the secret.
                       Defaults to ["XSRF-TOKEN"].
    @param token The CSRF token from the form submission (can be masked or unmasked). *)

val with_cookie : ?settings:t -> string -> Response.t -> Response.t
(** [with_cookie secret response] adds a Set-Cookie header to store the CSRF secret.

    This should be called after generating a token with {!input} to ensure the
    secret is stored in the client's cookie.

    @param settings Optional cookie settings. If not provided, uses secure defaults:
                    - Cookie name: ["XSRF-TOKEN"]
                    - Expiration: 1 year
                    - Path: ["/"]
                    - SameSite: [`Lax]
                    - Secure: [false] (set to [true] in production with HTTPS)
                    - HttpOnly: [false] (allows JavaScript to read for AJAX requests)
    @param secret The 32-byte secret returned by {!input}. *)
