module Fmt = struct
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
           "^[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?(Z|[+-][0-9]{2}:[0-9]{2})?$"))
  (* Format: HH:MM:SS[.ffffff][Z|(+|-)HH:MM] *)

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
         else
           Error [ Printf.sprintf "String does not match pattern: %s" pattern ]
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
            [ Printf.sprintf
                "Invalid day: %d for year %d month %d"
                day
                year
                month
            ]
        else Ok str
      with
      | e -> Error [ Printexc.to_string e ]
    else Error [ "Invalid date format (expected YYYY-MM-DD)" ]

  let validate_time str =
    if Re.execp time_re str
    then
      try
        let hour = int_of_string (String.sub str 0 2) in
        let minute = int_of_string (String.sub str 3 2) in
        let second = int_of_string (String.sub str 6 2) in
        if hour < 0 || hour > 23
        then Error [ Printf.sprintf "Invalid hour: %d" hour ]
        else if minute < 0 || minute > 59
        then Error [ Printf.sprintf "Invalid minute: %d" minute ]
        else if second < 0 || second > 59
        then Error [ Printf.sprintf "Invalid second: %d" second ]
        else Ok str
      with
      | e -> Error [ Printexc.to_string e ]
    else
      Error [ "Invalid time format (expected HH:MM:SS[.ffffff][Z|(+|-)HH:MM])" ]

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
end

type _ num =
  | Int_ty : int num
  | Int32_ty : int32 num
  | Int64_ty : int64 num
  | Float_ty : float num

type _ num_constraint =
  | Min : 'a -> 'a num_constraint
  | Max : 'a -> 'a num_constraint
  | Exclusive_min : 'a -> 'a num_constraint
  | Exclusive_max : 'a -> 'a num_constraint
  | Multiple_of : 'a -> 'a num_constraint

type _ t =
  | Min_length : int -> string t
  | Max_length : int -> string t
  | Pattern : string -> string t
  | Format : Fmt.t -> string t
  | Numeric : ('a num * 'a num_constraint list) -> 'a t
  | Min_items : int -> 'a list t
  | Max_items : int -> 'a list t
  | Unique_items : 'a list t
  | Any_of : 'a t list -> 'a t
  | All_of : 'a t list -> 'a t
  | One_of : 'a t list -> 'a t
  | Not : 'a t -> 'a t

let int_min n = Numeric (Int_ty, [ Min n ])
let int_max n = Numeric (Int_ty, [ Max n ])
let int_range min max = Numeric (Int_ty, [ Min min; Max max ])
let int_multiple_of n = Numeric (Int_ty, [ Multiple_of n ])
let int32_min n = Numeric (Int32_ty, [ Min n ])
let int32_max n = Numeric (Int32_ty, [ Max n ])
let int32_range min max = Numeric (Int32_ty, [ Min min; Max max ])
let int64_min n = Numeric (Int64_ty, [ Min n ])
let int64_max n = Numeric (Int64_ty, [ Max n ])
let int64_range min max = Numeric (Int64_ty, [ Min min; Max max ])
let float_min n = Numeric (Float_ty, [ Min n ])
let float_max n = Numeric (Float_ty, [ Max n ])
let float_range min max = Numeric (Float_ty, [ Min min; Max max ])
let min_length n = Min_length n
let max_length n = Max_length n
let length_range min max = All_of [ Min_length min; Max_length max ]
let pattern p = Pattern p
let format f = Format f
let min_items n = Min_items n
let max_items n = Max_items n
let float_exclusive_max n = Numeric (Float_ty, [ Exclusive_max n ])
let float_exclusive_min n = Numeric (Float_ty, [ Exclusive_min n ])
let int_exclusive_max n = Numeric (Int_ty, [ Exclusive_max n ])
let int_exclusive_min n = Numeric (Int_ty, [ Exclusive_min n ])
let int32_exclusive_max n = Numeric (Int32_ty, [ Exclusive_max n ])
let int32_exclusive_min n = Numeric (Int32_ty, [ Exclusive_min n ])
let int64_exclusive_max n = Numeric (Int64_ty, [ Exclusive_max n ])
let int64_exclusive_min n = Numeric (Int64_ty, [ Exclusive_min n ])
let unique_items = Unique_items
let any_of constraints = Any_of constraints
let all_of constraints = All_of constraints
let one_of constraints = One_of constraints
let not constraint_ = Not constraint_

let eval_num : type a. a num -> a num_constraint -> a -> (a, string list) result
  =
 fun ty num_constraint value ->
  match ty, num_constraint with
  | Int_ty, Min min ->
    if value < min
    then Error [ Printf.sprintf "Integer %d is less than minimum %d" value min ]
    else Ok value
  | Int_ty, Max max ->
    if value > max
    then Error [ Printf.sprintf "Integer %d exceeds maximum %d" value max ]
    else Ok value
  | Int_ty, Exclusive_min min ->
    if value <= min
    then
      Error
        [ Printf.sprintf
            "Integer %d is not greater than exclusive minimum %d"
            value
            min
        ]
    else Ok value
  | Int_ty, Exclusive_max max ->
    if value >= max
    then
      Error
        [ Printf.sprintf
            "Integer %d is not less than exclusive maximum %d"
            value
            max
        ]
    else Ok value
  | Int_ty, Multiple_of n ->
    if value mod n <> 0
    then Error [ Printf.sprintf "Integer %d is not a multiple of %d" value n ]
    else Ok value
  | Int32_ty, Min min ->
    if Int32.compare value min < 0
    then Error [ Printf.sprintf "Int32 %ld is less than minimum %ld" value min ]
    else Ok value
  | Int32_ty, Max max ->
    if Int32.compare value max > 0
    then Error [ Printf.sprintf "Int32 %ld exceeds maximum %ld" value max ]
    else Ok value
  | Int32_ty, Exclusive_min min ->
    if Int32.compare value min <= 0
    then
      Error
        [ Printf.sprintf
            "Int32 %ld is not greater than exclusive minimum %ld"
            value
            min
        ]
    else Ok value
  | Int32_ty, Exclusive_max max ->
    if Int32.compare value max >= 0
    then
      Error
        [ Printf.sprintf
            "Int32 %ld is not less than exclusive maximum %ld"
            value
            max
        ]
    else Ok value
  | Int32_ty, Multiple_of n ->
    if Int32.rem value n <> Int32.zero
    then Error [ Printf.sprintf "Int32 %ld is not a multiple of %ld" value n ]
    else Ok value
  | Int64_ty, Min min ->
    if Int64.compare value min < 0
    then Error [ Printf.sprintf "Int64 %Ld is less than minimum %Ld" value min ]
    else Ok value
  | Int64_ty, Max max ->
    if Int64.compare value max > 0
    then Error [ Printf.sprintf "Int64 %Ld exceeds maximum %Ld" value max ]
    else Ok value
  | Int64_ty, Exclusive_min min ->
    if Int64.compare value min <= 0
    then
      Error
        [ Printf.sprintf
            "Int64 %Ld is not greater than exclusive minimum %Ld"
            value
            min
        ]
    else Ok value
  | Int64_ty, Exclusive_max max ->
    if Int64.compare value max >= 0
    then
      Error
        [ Printf.sprintf
            "Int64 %Ld is not less than exclusive maximum %Ld"
            value
            max
        ]
    else Ok value
  | Int64_ty, Multiple_of n ->
    if Int64.rem value n <> Int64.zero
    then Error [ Printf.sprintf "Int64 %Ld is not a multiple of %Ld" value n ]
    else Ok value
  | Float_ty, Min min ->
    if Float.compare value min < 0
    then Error [ Printf.sprintf "Float %f is less than minimum %f" value min ]
    else Ok value
  | Float_ty, Max max ->
    if Float.compare value max > 0
    then Error [ Printf.sprintf "Float %f exceeds maximum %f" value max ]
    else Ok value
  | Float_ty, Exclusive_min min ->
    if Float.compare value min <= 0
    then
      Error
        [ Printf.sprintf
            "Float %f is not greater than exclusive minimum %f"
            value
            min
        ]
    else Ok value
  | Float_ty, Exclusive_max max ->
    if Float.compare value max >= 0
    then
      Error
        [ Printf.sprintf
            "Float %f is not less than exclusive maximum %f"
            value
            max
        ]
    else Ok value
  | Float_ty, Multiple_of n ->
    let remainder = Float.rem value n in
    (* Use a small epsilon for floating point comparison *)
    if Float.abs remainder < epsilon_float *. Float.abs n
    then Ok value
    else Error [ Printf.sprintf "Float %f is not a multiple of %f" value n ]

let rec eval : type a. a t -> a -> (a, string list) result =
 fun constraint_ value ->
  match constraint_ with
  | Min_length min_len ->
    let len = String.length value in
    if len < min_len
    then
      Error
        [ Printf.sprintf "String length %d is less than minimum %d" len min_len
        ]
    else Ok value
  | Max_length max_len ->
    let len = String.length value in
    if len > max_len
    then
      Error [ Printf.sprintf "String length %d exceeds maximum %d" len max_len ]
    else Ok value
  | Pattern pattern -> Fmt.pattern pattern value
  | Format format -> Fmt.validate format value
  | Numeric (ty, constraints) ->
    let results =
      List.map
        (fun num_constraint -> eval_num ty num_constraint value)
        constraints
    in
    let errors =
      List.filter_map (function Error e -> Some e | Ok _ -> None) results
    in
    if List.length errors > 0 then Error (List.concat errors) else Ok value
  | Min_items len ->
    if List.length value < len
    then
      Error
        [ Printf.sprintf
            "List length %d is less than minimum %d"
            (List.length value)
            len
        ]
    else Ok value
  | Max_items _n ->
    if List.length value > _n
    then
      Error
        [ Printf.sprintf
            "List length %d exceeds maximum %d"
            (List.length value)
            _n
        ]
    else Ok value
  | Unique_items ->
    let tbl = Hashtbl.create (List.length value) in
    let rec check_unique = function
      | [] -> Ok value
      | x :: xs ->
        if Hashtbl.mem tbl x
        then Error [ "List contains duplicate items" ]
        else (
          Hashtbl.add tbl x ();
          check_unique xs)
    in
    check_unique value
  | Any_of constraints ->
    (* At least one constraint must pass *)
    let results = List.map (fun c -> eval c value) constraints in
    let has_success =
      List.exists (function Ok _ -> true | Error _ -> false) results
    in
    if has_success
    then Ok value
    else
      let errors =
        List.filter_map (function Error e -> Some e | Ok _ -> None) results
      in
      Error ([ "None of the constraints matched:" ] @ List.concat errors)
  | All_of constraints ->
    (* All constraints must pass *)
    let results = List.map (fun c -> eval c value) constraints in
    let errors =
      List.filter_map (function Error e -> Some e | Ok _ -> None) results
    in
    if List.length errors > 0 then Error (List.concat errors) else Ok value
  | One_of constraints ->
    (* Exactly one constraint must pass *)
    let results = List.map (fun c -> eval c value) constraints in
    let successes =
      List.filter (function Ok _ -> true | Error _ -> false) results
    in
    let success_count = List.length successes in
    if success_count = 1
    then Ok value
    else if success_count = 0
    then Error [ "None of the constraints matched (expected exactly one)" ]
    else
      Error
        [ Printf.sprintf
            "Multiple constraints matched (expected exactly one, got %d)"
            success_count
        ]
  | Not constraint_ ->
    (* Constraint must NOT pass *)
    (match eval constraint_ value with
    | Ok _ -> Error [ "Constraint should not have matched" ]
    | Error _ -> Ok value)

let apply_all : type a. a t list -> a -> (a, string list) result =
 fun constraints value ->
  let results = List.map (fun c -> eval c value) constraints in
  let errors =
    List.filter_map (function Error e -> Some e | Ok _ -> None) results
  in
  if List.length errors > 0 then Error (List.concat errors) else Ok value

let apply : type a. a t option -> a -> (a, string list) result =
 fun constraint_ value ->
  match constraint_ with None -> Ok value | Some c -> eval c value
