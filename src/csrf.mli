(** Cross-Site Request Forgery (CSRF) protection using the double-submit cookie pattern.

    This module provides functions to generate and verify CSRF tokens to protect against
    CSRF attacks. The implementation uses:
    - Token masking to prevent BREACH attacks
    - Constant-time comparison to prevent timing attacks
    - Secure cookie storage with SameSite protection

    {b Basic Usage:}

    {[
      (* In your form GET handler *)
      let form_get_handler req =
        let token, secret = CSRF.csrf_input req in
        let html = Printf.sprintf
          "<form method='POST'>
             <input type='hidden' name='csrf_token' value='%s'>
             <button type='submit'>Submit</button>
           </form>" token
        in
        Response.of_html ~status:`OK html
        |> CSRF.with_csrf_cookie secret

      (* In your form POST handler *)
      let form_post_handler req =
        let form_data = Form.Urlencoded.of_body (Request.body req) in
        match Result.map (Form.Urlencoded.get "csrf_token") form_data with
        | Ok (Some token) when CSRF.verify_csrf_token ~token req ->
          Response.of_html ~status:`OK "Success!"
        | _ ->
          Response.of_html ~status:`Forbidden "Invalid CSRF token"
    ]}
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

    This is automatically called by {!csrf_input} if no existing secret is found
    in the request cookie. *)

val csrf_input :
   ?cookie_name:string
  -> Tapak_kernel.Request.t
  -> string * string
(** [csrf_input req] generates or retrieves a CSRF token and secret for form rendering.

    Returns [(token, secret)] where:
    - [token] is a masked 64-byte token to embed in your form (safe to send to client)
    - [secret] is the 32-byte secret to store in a cookie (must be kept server-side)

    If a secret already exists in the request cookie, it will be reused. Otherwise,
    a new secret is generated.

    The token is masked on each call to prevent BREACH attacks, so the same secret
    produces different tokens on each request.

    @param cookie_name The name of the cookie to check for existing secret.
                       Defaults to ["XSRF-TOKEN"]. *)

val verify_csrf_token : ?cookie_name:string -> token:string -> Request.t -> bool
(** [verify_csrf_token ~token req] verifies that a submitted CSRF token matches the
    secret stored in the request cookie.

    Returns [true] if:
    - The secret cookie exists
    - The token is validly formatted (32 or 64 bytes)
    - The unmasked token matches the secret (using constant-time comparison)

    Returns [false] otherwise.

    @param cookie_name The name of the cookie containing the secret.
                       Defaults to ["XSRF-TOKEN"].
    @param token The CSRF token from the form submission (can be masked or unmasked). *)

val with_csrf_cookie : ?settings:t -> string -> Response.t -> Response.t
(** [with_csrf_cookie secret response] adds a Set-Cookie header to store the CSRF secret.

    This should be called after generating a token with {!csrf_input} to ensure the
    secret is stored in the client's cookie.

    @param settings Optional cookie settings. If not provided, uses secure defaults:
                    - Cookie name: ["XSRF-TOKEN"]
                    - Expiration: 1 year
                    - Path: ["/"]
                    - SameSite: [`Lax]
                    - Secure: [false] (set to [true] in production with HTTPS)
                    - HttpOnly: [false] (allows JavaScript to read for AJAX requests)
    @param secret The 32-byte secret returned by {!csrf_input}. *)
