type t =
  [ `Email
  | `Idn_email
  | `Hostname
  | `Uri
  | `Uuid
  | `Date
  | `Date_time
  | `Time
  | `Duration
  | `Ipv4
  | `Ipv6
  | `Custom of string
  ]

let email_re = [%re {|/^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$/|}]

let idn_email_re =
  [%re {|/^(?:[a-zA-Z0-9._%+-]+)@(?:[a-zA-Z0-9.-]+|[^@\\s]+)\\.[a-zA-Z]{2,}$/|}]
(* Simplified version for IDN emails *)

let date_re = [%re {|/^[0-9]{4}-[0-9]{2}-[0-9]{2}$/|}]
(* Format: YYYY-MM-DD *)

let time_re =
  [%re
    "/^([0-9]{2}):([0-9]{2}):([0-9]{2})(\\.[0-9]+)?(?:([Zz])|([+-])([0-9]{2}):([0-9]{2}))?$/"]
(* Format: HH:MM:SS[.ffffff][Z|(+|-)HH:MM] *)

let datetime_re =
  [%re
    "/^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?(Z|[+-][0-9]{2}:[0-9]{2})?$/"]
(* Format: YYYY-MM-DDTHH:MM:SS[.ffffff][Z|(+|-)HH:MM] *)

let extended_iso_8601_duration_re =
  [%re
    {|/^P((\d+Y(\d+M(\d+D)?)?|\d+M(\d+D)?|\d+D)(T(\d+H(\d+M(\d+S)?)?|\d+M(\d+S)?|\d+S))?|T(\d+H(\d+M(\d+S)?)?|\d+M(\d+S)?|\d+S)|\d+W)$/|}]
(* Format: PnYnMnDTnHnMnS *)

let uuid_re =
  [%re
    "/^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$/"]
(* Format: 8-4-4-4-12 hexadecimal characters with hyphens *)

let validate_regex pattern =
  try
    Js.Re.fromString pattern |> ignore;
    Ok ()
  with
  | e ->
    (match Js.Exn.asJsExn e with
    | Some e ->
      Error [ Option.value ~default:"Invalid pattern regex" (Js.Exn.message e) ]
    | None -> Error [ "Invalid pattern regex" ])

let pattern pat str =
  match validate_regex pat with
  | Error e -> Error e
  | Ok () ->
    (try
       let re = Js.Re.fromString pat in
       if Js.Re.test ~str re
       then Ok str
       else Error [ "String doesn't match pattern: " ^ pat ]
     with
    | e ->
      Error
        [ Option.value
            ~default:"Invalid pattern regex"
            (Js.Exn.asJsExn e |> Fun.flip Option.bind Js.Exn.message)
        ])

(** this is probably not the best, but it work for now *)
let ipv4_re =
  [%re
    "/^(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)\\.){3}(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)$/"]

let validate_ipv4 str =
  if Js.Re.test ~str ipv4_re then Ok str else Error [ "Invalid IPv4 address" ]

let ipv6_re =
  [%re
    "/^(?:(?:(?:[0-9a-f]{1,4}:){6}|::(?:[0-9a-f]{1,4}:){5}|(?:[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){4}|(?:(?:[0-9a-f]{1,4}:)?[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){3}|(?:(?:[0-9a-f]{1,4}:){0,2}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){2}|(?:(?:[0-9a-f]{1,4}:){0,3}[0-9a-f]{1,4})?::[0-9a-f]{1,4}:|(?:(?:[0-9a-f]{1,4}:){0,4}[0-9a-f]{1,4})?::)(?:[0-9a-f]{1,4}:[0-9a-f]{1,4}|(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)\\.){3}(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d))|(?:(?:[0-9a-f]{1,4}:){0,5}[0-9a-f]{1,4})?::[0-9a-f]{1,4}|(?:(?:[0-9a-f]{1,4}:){0,6}[0-9a-f]{1,4})?::)$/i"]

let validate_ipv6 str =
  if Js.Re.test ~str ipv6_re then Ok str else Error [ "Invalid IPv6 address" ]

(** TODO: implement hostname validation *)
let validate_hostname str = Ok str

let validate_email str =
  if Js.Re.test ~str email_re then Ok str else Error [ "Invalid email address" ]

let validate_idn_email str =
  if Js.Re.test ~str idn_email_re
  then Ok str
  else Error [ "Invalid IDN email address" ]

let is_leap_year year =
  (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0

let days_in_month year month =
  match month with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if is_leap_year year then 29 else 28
  | _ -> 0

external int_of_float : float -> int = "%identity"

let validate_date str =
  if Js.Re.test ~str date_re
  then
    try
      let year =
        Js.Float.fromString (Js.String.slice ~start:0 ~end_:4 str)
        |> int_of_float
      in
      let month =
        Js.Float.fromString (Js.String.slice ~start:5 ~end_:7 str)
        |> int_of_float
      in
      let day =
        Js.Float.fromString (Js.String.slice ~start:8 ~end_:10 str)
        |> int_of_float
      in
      if year < 1 || year > 9999
      then Error [ "Year must be between 1 and 9999" ]
      else if month < 1 || month > 12
      then Error [ "Invalid month: " ^ Js.Int.toString month ]
      else if day < 1 || day > days_in_month year month
      then
        Error
          [ "Invalid day: "
            ^ Js.Int.toString day
            ^ " for year "
            ^ Js.Int.toString year
            ^ " month "
            ^ Js.Int.toString month
          ]
      else Ok str
    with
    | e -> Error [ Printexc.to_string e ]
  else Error [ "Invalid date format (expected YYYY-MM-DD)" ]

let match_at ~index captures =
  Js.Array.at ~index captures |> fun opt -> Option.bind opt Js.Nullable.toOption

let validate_time str =
  match Js.Re.exec ~str time_re with
  | None ->
    Error [ "Invalid time format (expected HH:MM:SS[.ffffff][Z|(+|-)HH:MM])" ]
  | Some matches ->
    let captures = Js.Re.captures matches in
    let hour =
      match_at ~index:1 captures
      |> Option.map Js.Float.fromString
      |> Option.value ~default:0.
      |> int_of_float
    in
    let minute =
      match_at ~index:2 captures
      |> Option.map Js.Float.fromString
      |> Option.value ~default:0.
      |> int_of_float
    in
    let second =
      match_at ~index:3 captures
      |> Option.map Js.Float.fromString
      |> Option.value ~default:0.
      |> int_of_float
    in
    if hour > 23 || minute > 59 || second > 60
    then
      Error
        [ "Invalid time: hour must be 0-23, minute must be 0-59, second must \
           be 0-60"
        ]
    else
      let tz_hour =
        match_at ~index:7 captures
        |> Option.map Js.Float.fromString
        |> Option.value ~default:0.
        |> int_of_float
      in
      let tz_minute =
        match_at ~index:8 captures
        |> Option.map Js.Float.fromString
        |> Option.value ~default:0.
        |> int_of_float
      in
      (match match_at ~index:6 captures with
      | Some _ when tz_hour > 23 || tz_minute > 59 ->
        Error
          [ "Invalid timezone offset: hour must be 0-23, minute must be 0-59" ]
      | _ when second < 60 -> Ok str
      | _ ->
        (* leap second check *)
        let tz_sign =
          if Option.value ~default:"" (match_at ~index:6 captures) = "-"
          then -1
          else 1
        in
        let tz_hour =
          match_at ~index:7 captures
          |> Option.map Js.Float.fromString
          |> Option.value ~default:0.
          |> int_of_float
        in
        let tz_minute =
          match_at ~index:8 captures
          |> Option.map Js.Float.fromString
          |> Option.value ~default:0.
          |> int_of_float
        in
        let total_utc_min =
          (hour * 60) + minute - (tz_sign * ((tz_hour * 60) + tz_minute))
        in
        if ((total_utc_min mod 1440) + 1440) mod 1440 = 1439
        then Ok str
        else
          Error
            [ "Invalid time: second must be 0-59, or 60 only if the time is \
               23:59:60 UTC"
            ])

let validate_datetime str =
  let time_re = [%re "/T/i"] in
  let date_time = Js.String.splitByRe ~regexp:time_re str in
  if Js.Array.length date_time <> 2
  then
    Error
      [ "Invalid date-time format (expected \
         YYYY-MM-DDTHH:MM:SS[.ffffff][Z|(+|-)HH:MM])"
      ]
  else
    let date_part =
      Js.Array.at ~index:0 date_time |> Option.join |> Option.value ~default:""
    in
    let time_part =
      Js.Array.at ~index:1 date_time |> Option.join |> Option.value ~default:""
    in
    match validate_date date_part with
    | Error e -> Error e
    | Ok _ -> validate_time time_part

let validate_duration str =
  if Js.Re.test ~str extended_iso_8601_duration_re
  then Ok str
  else Error [ "Invalid duration format (expected PnYnMnDTnHnMnS)" ]

let uri_re =
  [%re
    {|/^[a-z][a-z0-9+\-.]*:(?:\/\/(?:(?:[-a-z0-9._~!$&'()*+,;=:]|%[0-9a-f]{2})*@)?(?:\[(?:(?:(?:[\da-f]{1,4}:){6}(?:[\da-f]{1,4}:[\da-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)\.){3}(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d))|::(?:[\da-f]{1,4}:){5}(?:[\da-f]{1,4}:[\da-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)\.){3}(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d))|(?:[\da-f]{1,4})?::(?:[\da-f]{1,4}:){4}(?:[\da-f]{1,4}:[\da-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)\.){3}(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d))|(?:(?:[\da-f]{1,4}:){0,1}[\da-f]{1,4})?::(?:[\da-f]{1,4}:){3}(?:[\da-f]{1,4}:[\da-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)\.){3}(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d))|(?:(?:[\da-f]{1,4}:){0,2}[\da-f]{1,4})?::(?:[\da-f]{1,4}:){2}(?:[\da-f]{1,4}:[\da-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)\.){3}(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d))|(?:(?:[\da-f]{1,4}:){0,3}[\da-f]{1,4})?::[\da-f]{1,4}:(?:[\da-f]{1,4}:[\da-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)\.){3}(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d))|(?:(?:[\da-f]{1,4}:){0,4}[\da-f]{1,4})?::(?:[\da-f]{1,4}:[\da-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)\.){3}(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d))|(?:(?:[\da-f]{1,4}:){0,5}[\da-f]{1,4})?::[\da-f]{1,4}|(?:(?:[\da-f]{1,4}:){0,6}[\da-f]{1,4})?::)|v[0-9a-f]+\.[-a-z0-9._~!$&'()*+,;=:]+)\]|(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)\.){3}(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)|(?:[-a-z0-9._~!$&'()*+,;=]|%[0-9a-f]{2})*)(?::\d*)?(?:\/(?:[-a-z0-9._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*|\/(?:(?:[-a-z0-9._~!$&'()*+,;=:@]|%[0-9a-f]{2})+(?:\/(?:[-a-z0-9._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*)?|(?:[-a-z0-9._~!$&'()*+,;=:@]|%[0-9a-f]{2})+(?:\/(?:[-a-z0-9._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*)?(?:\?(?:[-a-z0-9._~!$&'()*+,;=:@/?]|%[0-9a-f]{2})*)?(?:#(?:[-a-z0-9._~!$&'()*+,;=:@/?]|%[0-9a-f]{2})*)?$/i|}]

let validate_uri str =
  if Js.Re.test ~str uri_re then Ok str else Error [ "Invalid URI" ]

let validate_uuid str =
  if Js.Re.test ~str uuid_re
  then Ok str
  else Error [ "Invalid UUID format (expected 8-4-4-4-12 hex format)" ]

let validate : t -> string -> (string, string list) result =
 fun format str ->
  match format with
  | `Email -> validate_email str
  | `Idn_email -> validate_idn_email str
  | `Hostname -> validate_hostname str
  | `Uri -> validate_uri str
  | `Uuid -> validate_uuid str
  | `Date -> validate_date str
  | `Date_time -> validate_datetime str
  | `Duration -> validate_duration str
  | `Time -> validate_time str
  | `Ipv4 -> validate_ipv4 str
  | `Ipv6 -> validate_ipv6 str
  | `Custom pt -> pattern pt str

let to_string : t -> string = function
  | `Email -> "email"
  | `Idn_email -> "idn-email"
  | `Hostname -> "hostname"
  | `Uri -> "uri"
  | `Uuid -> "uuid"
  | `Date -> "date"
  | `Date_time -> "date-time"
  | `Duration -> "duration"
  | `Time -> "time"
  | `Ipv4 -> "ipv4"
  | `Ipv6 -> "ipv6"
  | `Custom s -> s
