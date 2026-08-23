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

let email_re =
  Re.compile (Re.Perl.re "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")
(* Simplified version, in the future we will implement email validator with
   angstrom *)

let idn_email_re =
  Re.compile
    (Re.Perl.re
       "^(?:[a-zA-Z0-9._%+-]+)@(?:[a-zA-Z0-9.-]+|[^@\\s]+)\\.[a-zA-Z]{2,}$")
(* Simplified version for IDN emails *)

let date_re = Re.(compile (Perl.re "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"))
(* Format: YYYY-MM-DD *)

let time_re =
  Re.(
    compile
      (Perl.re
         "^([0-9]{2}):([0-9]{2}):([0-9]{2})(\\.[0-9]+)?(?:(Z|z)|([+-])([0-9]{2}):([0-9]{2}))?$"))
(* Format: HH:MM:SS[.ffffff][Z|(+|-)HH:MM] Capture groups: 1=hour 2=minute
   3=second 4=frac 5=Z 6=sign 7=tz_hour 8=tz_minute *)

let datetime_re =
  Re.(
    compile
      (Perl.re
         "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?(Z|[+-][0-9]{2}:[0-9]{2})?$"))
(* Format: YYYY-MM-DDTHH:MM:SS[.ffffff][Z|(+|-)HH:MM] *)

let extended_iso_8601_duration_re =
  Re.(
    compile
      (Perl.re
         "^P(?:(?:[0-9]+Y)?(?:[0-9]+M)?(?:[0-9]+D)?)(?:T(?:[0-9]+H)?(?:[0-9]+M)?(?:[0-9]+S)?)?$"))
(* Format: PnYnMnDTnHnMnS *)

let uuid_re =
  Re.compile
    (Re.Perl.re
       "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$")
(* Format: 8-4-4-4-12 hexadecimal characters with hyphens *)

let validate_regex pattern =
  try
    ignore (Re.Perl.re pattern |> Re.compile);
    Ok ()
  with
  | e -> Error [ Printexc.to_string e ]

let pattern pattern str =
  match validate_regex pattern with
  | Error e -> Error e
  | Ok () ->
    (try
       let re = Re.compile (Re.Perl.re pattern) in
       if Re.execp re str
       then Ok str
       else Error [ Printf.sprintf "String does not match pattern: %s" pattern ]
     with
    | e -> Error [ Printexc.to_string e ])

let validate_ipv4 str =
  match Ipaddr.V4.of_string str with
  | Ok _ -> Ok str
  | Error (`Msg msg) -> Error [ msg ]

let validate_ipv6 str =
  match Ipaddr.V6.of_string str with
  | Ok _ -> Ok str
  | Error (`Msg msg) -> Error [ msg ]

let validate_hostname str =
  let len = String.length str in
  if len = 0
  then Error [ "Hostname cannot be empty" ]
  else if len > 253
  then Error [ "Hostname too long (max 253 characters)" ]
  else
    let labels = String.split_on_char '.' str in
    let validate_label label =
      let label_len = String.length label in
      if label_len = 0
      then Error "Label cannot be empty"
      else if label_len > 63
      then Error "Label too long (max 63 characters)"
      else if label.[0] = '-' || label.[label_len - 1] = '-'
      then Error "Label cannot start or end with hyphen"
      else
        let valid_char c =
          (c >= 'a' && c <= 'z')
          || (c >= 'A' && c <= 'Z')
          || (c >= '0' && c <= '9')
          || c = '-'
        in
        if String.for_all valid_char label
        then Ok ()
        else Error "Label contains invalid characters"
    in
    match
      List.find_map
        (fun l ->
           match validate_label l with Error e -> Some e | Ok () -> None)
        labels
    with
    | Some err -> Error [ err ]
    | None -> Ok str

let validate_email str =
  if Re.execp email_re str then Ok str else Error [ "Invalid email format" ]

let validate_idn_email str =
  if Re.execp idn_email_re str
  then Ok str
  else Error [ "Invalid IDN email format" ]

let is_leap_year year =
  (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0

let days_in_month year month =
  match month with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if is_leap_year year then 29 else 28
  | _ -> 0

let validate_date str =
  if Re.execp date_re str
  then
    try
      let year = int_of_string (String.sub str 0 4) in
      let month = int_of_string (String.sub str 5 2) in
      let day = int_of_string (String.sub str 8 2) in
      if year < 1 || year > 9999
      then Error [ "Year must be between 1 and 9999" ]
      else if month < 1 || month > 12
      then Error [ Printf.sprintf "Invalid month: %d" month ]
      else if day < 1 || day > days_in_month year month
      then
        Error
          [ Printf.sprintf "Invalid day: %d for year %d month %d" day year month
          ]
      else Ok str
    with
    | e -> Error [ Printexc.to_string e ]
  else Error [ "Invalid date format (expected YYYY-MM-DD)" ]

let validate_time str =
  match Re.exec_opt time_re str with
  | None ->
    Error [ "Invalid time format (expected HH:MM:SS[.ffffff][Z|(+|-)HH:MM])" ]
  | Some g ->
    (try
       let group_int n =
         match Re.Group.get_opt g n with Some s -> int_of_string s | None -> 0
       in
       let hour = group_int 1 in
       let minute = group_int 2 in
       let second = group_int 3 in
       if hour > 23 || minute > 59 || second > 60
       then
         Error
           [ "Invalid time: hour must be 0-23, minute must be 0-59, second \
              must be 0-60"
           ]
       else begin
         let tz_hour = group_int 7 in
         let tz_minute = group_int 8 in
         match Re.Group.get_opt g 6 with
         | Some _ when tz_hour > 23 || tz_minute > 59 ->
           Error
             [ "Invalid timezone offset: hour must be 0-23, minute must be 0-59"
             ]
         | _ when second < 60 -> Ok str
         | _ ->
           (* leap second: only valid when the UTC-equivalent time is
              23:59:60 *)
           let tz_sign = if Re.Group.get_opt g 6 = Some "-" then -1 else 1 in
           let total_utc_min =
             (hour * 60) + minute - (tz_sign * ((tz_hour * 60) + tz_minute))
           in
           if ((total_utc_min mod 1440) + 1440) mod 1440 = 1439
           then Ok str
           else
             Error
               [ "Invalid time: second must be 0-59, or 60 only if the time is \
                  23:59:60 UTC"
               ]
       end
     with
    | e -> Error [ Printexc.to_string e ])

let validate_datetime str =
  if Re.execp datetime_re str
  then
    try
      (* Extract date part (YYYY-MM-DD) *)
      let date_part = String.sub str 0 10 in
      (* Extract time part (everything after T) *)
      let time_part = String.sub str 11 (String.length str - 11) in
      match validate_date date_part, validate_time time_part with
      | Ok _, Ok _ -> Ok str
      | Error e, _ | _, Error e -> Error e
    with
    | e -> Error [ Printexc.to_string e ]
  else
    Error
      [ "Invalid datetime format (expected \
         YYYY-MM-DDTHH:MM:SS[.ffffff][Z|(+|-)HH:MM])"
      ]

let validate_duration str =
  if Re.execp extended_iso_8601_duration_re str
  then Ok str
  else
    Error
      [ "Invalid duration format (expected extended ISO 8601 duration format \
         PnYnMnDTnHnMnS)"
      ]

let validate_uri str =
  try
    let uri = Uri.of_string str in
    match Uri.scheme uri with
    | None -> Error [ "URI must have a scheme (e.g., http://, https://)" ]
    | Some _ -> Ok str
  with
  | e -> Error [ Printexc.to_string e ]

let validate_uuid str =
  if Re.execp uuid_re str
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
