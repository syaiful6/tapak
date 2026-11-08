type expiration = Cohttp.Cookie.expiration
type cookie = Cohttp.Cookie.cookie

type same_site =
  [ `None
  | `Lax
  | `Strict
  ]

let same_site_to_string = function
  | `None -> "None"
  | `Lax -> "Lax"
  | `Strict -> "Strict"

module Set_cookie = struct
  type t =
    { cookie : cookie
    ; expiration : expiration option
    ; domain : string option
    ; path : string option
    ; secure : bool
    ; http_only : bool
    ; same_site : same_site option
    }

  let make
        ?expiration
        ?domain
        ?path
        ?same_site
        ~secure
        ~http_only
        (cookie_name, cookie_value)
    =
    { cookie = cookie_name, cookie_value
    ; expiration
    ; domain
    ; path
    ; secure
    ; http_only
    ; same_site
    }

  let serialize t =
    let parts =
      List.filter_map
        Fun.id
        [ (if t.secure || t.same_site = Some `None then Some "Secure" else None)
        ; (if t.http_only then Some "HttpOnly" else None)
        ; t.path |> Option.map (fun p -> Printf.sprintf "Path=%s" p)
        ; t.domain |> Option.map (fun d -> Printf.sprintf "Domain=%s" d)
        ; t.expiration
          |> Fun.flip Option.bind (function
            | `Session -> None
            | `Max_age age -> Option.some @@ Printf.sprintf "Max-Age=%Ld" age)
        ; t.same_site
          |> Option.map (fun c ->
            Printf.sprintf "SameSite=%s" (same_site_to_string c))
        ]
    in
    let n, c = t.cookie in
    let parts = (n ^ "=" ^ c) :: parts in
    "Set-Cookie", String.concat "; " parts
end

module Cookie = struct
  include Cohttp.Cookie.Cookie_hdr

  let is_space = function ' ' | '\t' -> true | _ -> false

  let rec skip_spaces s stop i =
    if i < stop && is_space (String.unsafe_get s i)
    then skip_spaces s stop (i + 1)
    else i

  let rec find_from s stop c i =
    if i >= stop
    then stop
    else if String.unsafe_get s i = c
    then i
    else find_from s stop c (i + 1)

  (* [$]-prefixed attributes such as [$Path] are not cookies *)
  let[@tail_mod_cons] rec parse_pairs s pos =
    let total = String.length s in
    if pos > total
    then []
    else
      let stop = find_from s total ';' pos in
      let start = skip_spaces s stop pos in
      if start = stop || String.unsafe_get s start = '$'
      then parse_pairs s (stop + 1)
      else
        let eq = find_from s stop '=' start in
        let name = String.sub s start (eq - start) in
        let value =
          if eq = stop then "" else String.sub s (eq + 1) (stop - eq - 1)
        in
        (name, value) :: parse_pairs s (stop + 1)

  (* Behaves as [extract] does, including later headers coming first *)
  let parse hdr =
    List.fold_left
      (fun acc header -> parse_pairs header 0 @ acc)
      []
      (Http.Header.get_multi hdr "cookie")
end
