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

  let parse = extract
end
