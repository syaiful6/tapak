(** CSRF (Cross-Site Request Forgery) Protection

    This module implements CSRF protection using the double-submit cookie pattern,
    similar to Django's approach. The implementation provides:

    - Cryptographically secure token generation
    - Token masking to prevent BREACH attacks
    - Constant-time comparison to prevent timing attacks
    - Cookie-based secret storage (readable by JavaScript for AJAX/SPA frameworks)

    {1 Security Model}

    The double-submit cookie pattern works as follows:

    1. A random secret is generated and stored in a cookie
    2. The secret is masked with a one-time pad and embedded in forms/requests
    3. On submission, the masked token is unmasked and compared with the cookie secret
    4. Attackers can't forge requests because they can't read cookies from other domains
       (thanks to SameSite policy)

    {1 Usage Example}

    {2 Rendering a Form}

    {[
      let render_form_handler request =
        (* Get CSRF token and secret for the form *)
        let token, secret = csrf_input request in

        (* Render your form with the token *)
        let html = render_template ~csrf_token:token in
        let response = Response.of_html html in

        (* Set the CSRF cookie with the secret *)
        with_csrf_cookie secret response
    ]}

    {2 Validating a Form Submission}

    {[
      let handle_post_handler request =
        (* Extract token from form field, header, or JSON body *)
        let token =
          match get_form_field request "csrf_token" with
          | Some t -> t
          | None ->
            (* Or check X-CSRF-Token header for AJAX requests *)
            get_header request "X-CSRF-Token" |> Option.value ~default:""
        in

        (* Verify the token matches the cookie *)
        if verify_csrf_token ~token request then
          (* Process the request *)
          process_form request
        else
          (* Reject with 403 Forbidden *)
          Response.create `Forbidden
    ]}

    {2 Production Configuration}

    {[
      (* In production, use secure cookies *)
      let production_settings = {
        cookie_name = Some "XSRF-TOKEN";
        expiration = Some (`Max_age (365L * 24L * 60L * 60L)); (* 1 year *)
        domain = Some ".example.com";
        path = Some "/";
        secure = true;  (* HTTPS only *)
        http_only = false;  (* Allow JS access *)
        same_site = Some `Strict;  (* Strict for maximum protection *)
      }

      with_csrf_cookie ~settings:production_settings secret response
    ]}

    {1 Key Rotation}

    To rotate CSRF secrets (e.g., on user login/logout):

    {[
      let rotate_csrf_secret request response =
        let new_secret = generate_secret () in
        with_csrf_cookie new_secret response
    ]}
*)

(** {1 Types} *)

type t =
  { cookie_name : string option  (** Cookie name. Defaults to ["XSRF-TOKEN"] *)
  ; expiration : Piaf.Cookies.expiration option
    (** Cookie expiration. Defaults to 1 year *)
  ; domain : string option
    (** Cookie domain. Defaults to [None] (current domain) *)
  ; path : string option  (** Cookie path. Defaults to ["/"] *)
  ; secure : bool
    (** Require HTTPS. Defaults to [false] (set [true] in production!) *)
  ; http_only : bool
    (** Prevent JavaScript access. Defaults to [false] (allows JS to read for AJAX) *)
  ; same_site : Piaf.Cookies.same_site option
    (** SameSite policy. Defaults to [`Lax] (critical for CSRF protection) *)
  }
(** Cookie settings for CSRF protection *)

(** {1 Secret Generation} *)

val generate_secret : unit -> string
(** [generate_secret ()] generates a new cryptographically secure CSRF secret.

    Use this for:
    - Key rotation on login/logout
    - Periodic key rotation for security
    - Manual secret management

    The secret is a 32-byte random string encoded in base62 (alphanumeric).

    Example:
    {[
      let new_secret = generate_secret () in
      with_csrf_cookie new_secret response
    ]} *)

(** {1 Token Generation} *)

val csrf_input :
   ?cookie_name:string
  -> Tapak_kernel.Request.t
  -> string * string
(** [csrf_input ?cookie_name request] generates a CSRF token for embedding in forms.

    Returns a pair [(token, secret)] where:
    - [token] is a masked token to include in your form (safe to expose in HTML)
    - [secret] is the secret that must be stored in a cookie via {!with_csrf_cookie}

    The function handles two cases:
    - If the request already has a CSRF cookie, it uses that secret
    - If no cookie exists, it generates a new secret

    In both cases, the token is freshly masked with a one-time pad to prevent
    BREACH attacks.

    @param cookie_name Cookie name to check. Defaults to ["XSRF-TOKEN"]

    Example:
    {[
      let token, secret = csrf_input request in
      let html = {|<input type="hidden" name="csrf_token" value="|} ^ token ^ {|">|} in
      let response = Response.of_html html in
      with_csrf_cookie secret response
    ]} *)

(** {1 Token Validation} *)

val verify_csrf_token :
   ?cookie_name:string
  -> token:string
  -> Tapak_kernel.Request.t
  -> bool
(** [verify_csrf_token ?cookie_name ~token request] verifies a CSRF token.

    Returns [true] if:
    - The request has a CSRF cookie with the given [cookie_name]
    - The [token] matches the cookie secret

    Returns [false] if:
    - No CSRF cookie is present
    - The token format is invalid
    - The token doesn't match the secret

    The comparison is done in constant time to prevent timing attacks.

    @param cookie_name Cookie name to check. Defaults to ["XSRF-TOKEN"]
    @param token The token to verify, extract it from headers, form fields, or JSON body

    {b Note:} This function does NOT extract the token from the request.
    You must extract it yourself from:
    - Form fields (e.g., [csrfmiddlewaretoken])
    - Headers (e.g., [X-CSRF-Token], [X-CSRFToken])
    - JSON body

    Example:
    {[
      (* Extract token from header or form *)
      let token =
        match Request.header request "X-CSRF-Token" with
        | Some t -> t
        | None -> get_form_field request "csrf_token" |> Option.value ~default:""
      in

      if verify_csrf_token ~token request then
        (* Proceed *)
      else
        Response.create `Forbidden
    ]} *)

(** {1 Response Helpers} *)

val with_csrf_cookie :
   ?settings:t
  -> string
  -> Piaf.Response.t
  -> Piaf.Response.t
(** [with_csrf_cookie ?settings secret response] adds a CSRF cookie to the response.

    This function:
    - Creates a Set-Cookie header with the given [secret]
    - Preserves any existing Set-Cookie headers
    - Uses the provided [settings] or sensible defaults

    @param settings Cookie configuration. Defaults:
    - [cookie_name]: ["XSRF-TOKEN"]
    - [expiration]: 1 year
    - [path]: ["/"]
    - [secure]: [false] (set [true] in production!)
    - [http_only]: [false] (allows JS access for AJAX)
    - [same_site]: [`Lax]

    @param secret The CSRF secret to store (from {!csrf_input} or {!generate_secret})

    Example:
    {[
      let token, secret = csrf_input request in
      let response = Response.of_html (render_form ~token) in
      with_csrf_cookie secret response
    ]}

    Production example:
    {[
      let prod_settings = {
        cookie_name = Some "XSRF-TOKEN";
        expiration = Some (`Max_age 31536000L); (* 1 year *)
        domain = None;
        path = Some "/";
        secure = true;  (* HTTPS only *)
        http_only = false;
        same_site = Some `Strict;
      } in
      with_csrf_cookie ~settings:prod_settings secret response
    ]} *)
