let secret_length = 32
let token_length = 2 * secret_length
let generate_secret () = Random.string secret_length
let cookie_name = "XSRF-TOKEN"

let mask_chiper_secret secret =
  let mask = generate_secret () in
  let sb =
    Seq.map (String.index Random.ascii_alpha_digits) (String.to_seq secret)
  in
  let sm =
    Seq.map (String.index Random.ascii_alpha_digits) (String.to_seq mask)
  in
  let pairs = Seq.zip sb sm in
  let length = String.length Random.ascii_alpha_digits in
  let chiper =
    pairs
    |> Seq.map (fun (x, y) ->
      String.get Random.ascii_alpha_digits ((x + y) mod length))
  in
  mask ^ String.of_seq chiper

let unmask_chiper_token token =
  let mask = String.sub token 0 secret_length in
  let chiper = String.sub token secret_length secret_length in
  let mb =
    Seq.map (String.index Random.ascii_alpha_digits) (String.to_seq mask)
  in
  let cb =
    Seq.map (String.index Random.ascii_alpha_digits) (String.to_seq chiper)
  in
  let pairs = Seq.zip cb mb in
  let length = String.length Random.ascii_alpha_digits in
  pairs
  |> Seq.map (fun (x, y) ->
    String.get Random.ascii_alpha_digits ((x - y + length) mod length))
  |> String.of_seq

let check_token_format token =
  let len = String.length token in
  len = token_length || len = secret_length
(* We accept both masked and unmasked tokens for compatibility *)

let constant_time_equal s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  let result = ref (len1 lxor len2) in
  let min_len = min len1 len2 in
  for i = 0 to min_len - 1 do
    result := !result lor (Char.code s1.[i] lxor Char.code s2.[i])
  done;
  !result = 0

(** [is_token_match ~token ~secret] returns true if the provided token matches the secret.
    The token can be either masked or unmasked. If the token format is invalid, it returns false. *)
let is_token_match ~token ~secret =
  if not (check_token_format token)
  then false
  else
    let unmasked_token =
      if String.length token = token_length
      then unmask_chiper_token token
      else token
    in
    constant_time_equal unmasked_token secret

let get_secret_from_request ~cookie_name request =
  let open Tapak_kernel in
  let headers = Request.headers request in
  let cookies = Piaf.Cookies.Cookie.parse headers in
  List.assoc_opt cookie_name cookies

type t =
  { cookie_name : string option
  ; expiration : Piaf.Cookies.expiration option
  ; domain : string option
  ; path : string option
  ; secure : bool
  ; http_only : bool
  ; same_site : Piaf.Cookies.same_site option
  }

let verify_csrf_token ?(cookie_name = cookie_name) ~token request =
  match get_secret_from_request ~cookie_name request with
  | Some secret -> is_token_match ~token ~secret
  | None -> false

let csrf_input ?(cookie_name = cookie_name) request =
  let secret =
    match get_secret_from_request ~cookie_name request with
    | Some secret -> secret
    | None -> generate_secret ()
  in
  let token = mask_chiper_secret secret in
  token, secret

let with_csrf_cookie ?settings secret response =
  let open Piaf.Cookies in
  let settings =
    match settings with
    | Some settings -> settings
    | None ->
      { cookie_name = Some cookie_name
      ; expiration = Some (`Max_age (365 * 24 * 60 * 60 |> Int64.of_int))
      ; domain = None
      ; path = Some "/"
      ; secure = false
      ; http_only = false
      ; same_site = Some `Lax
      }
  in
  let cookie_name = Option.value ~default:cookie_name settings.cookie_name in
  let header_name, header_value =
    Set_cookie.make
      ?expiration:settings.expiration
      ?domain:settings.domain
      ?path:settings.path
      ?same_site:settings.same_site
      ~secure:settings.secure
      ~http_only:settings.http_only
      (cookie_name, secret)
    |> Set_cookie.serialize
  in
  let headers =
    Piaf.Headers.add (Piaf.Response.headers response) header_name header_value
  in
  Piaf.Response.with_ ~headers response
