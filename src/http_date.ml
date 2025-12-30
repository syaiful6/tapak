open Angstrom

type month =
  [ `Jan
  | `Feb
  | `Mar
  | `Apr
  | `May
  | `Jun
  | `Jul
  | `Aug
  | `Sep
  | `Oct
  | `Nov
  | `Dec
  ]

type year = int
type day = int

type weekday =
  [ `Mon
  | `Tue
  | `Wed
  | `Thu
  | `Fri
  | `Sat
  | `Sun
  ]

type t =
  { year : year
  ; month : month
  ; day : day
  ; hour : int
  ; minute : int
  ; second : int
  ; weekday : weekday
  }

type date =
  { year : year
  ; month : month
  ; day : day
  }

let month_to_string = function
  | `Jan -> "Jan"
  | `Feb -> "Feb"
  | `Mar -> "Mar"
  | `Apr -> "Apr"
  | `May -> "May"
  | `Jun -> "Jun"
  | `Jul -> "Jul"
  | `Aug -> "Aug"
  | `Sep -> "Sep"
  | `Oct -> "Oct"
  | `Nov -> "Nov"
  | `Dec -> "Dec"

let weekday_to_string = function
  | `Mon -> "Mon"
  | `Tue -> "Tue"
  | `Wed -> "Wed"
  | `Thu -> "Thu"
  | `Fri -> "Fri"
  | `Sat -> "Sat"
  | `Sun -> "Sun"

let pp formatter { year; month; day; hour; minute; second; weekday } =
  Format.fprintf
    formatter
    "%s, %02d %s %04d %02d:%02d:%02d GMT"
    (weekday_to_string weekday)
    day
    (month_to_string month)
    year
    hour
    minute
    second

let ( <$ ) a p = p >>| fun _ -> a
(* map to constant value *)

let parse_week_day =
  `Mon
  <$ string "Mon"
  <|> (`Tue <$ string "Tue")
  <|> (`Wed <$ string "Wed")
  <|> (`Thu <$ string "Thu")
  <|> (`Fri <$ string "Fri")
  <|> (`Sat <$ string "Sat")
  <|> (`Sun <$ string "Sun")

let parse_month =
  `Jan
  <$ string "Jan"
  <|> (`Feb <$ string "Feb")
  <|> (`Mar <$ string "Mar")
  <|> (`Apr <$ string "Apr")
  <|> (`May <$ string "May")
  <|> (`Jun <$ string "Jun")
  <|> (`Jul <$ string "Jul")
  <|> (`Aug <$ string "Aug")
  <|> (`Sep <$ string "Sep")
  <|> (`Oct <$ string "Oct")
  <|> (`Nov <$ string "Nov")
  <|> (`Dec <$ string "Dec")

let sp = () <$ char ' '
let any_digit = satisfy (function '0' .. '9' -> true | _ -> false)
let char_to_int s = Char.code s - Char.code '0'

let digit2 =
  let format2 x y = (x * 10) + y in
  format2 <$> (char_to_int <$> any_digit) <*> (char_to_int <$> any_digit)

let digit4 =
  let format4 w x y z = (w * 1000) + (x * 100) + (y * 10) + z in
  format4
  <$> (char_to_int <$> any_digit)
  <*> (char_to_int <$> any_digit)
  <*> (char_to_int <$> any_digit)
  <*> (char_to_int <$> any_digit)

let fail_if_nothing msg parser =
  parser >>= function Some v -> return v | None -> fail msg

let valid_days d = if d >= 1 && d <= 31 then Some d else None

let parse_date =
  lift3
    (fun day month year -> { day; month; year })
    (fail_if_nothing "Invalid day" (digit2 <* sp >>| valid_days))
    (parse_month <* sp)
    (fail_if_nothing "Invalid year" (digit4 <* sp >>| fun year -> Some year))

let parse_time =
  let valid_hour d = if d >= 0 && d <= 23 then Some d else None in
  let valid_minute_seconds d = if d >= 0 && d <= 59 then Some d else None in
  lift3
    (fun hour minute second -> hour, minute, second)
    (fail_if_nothing "Invalid hour" (digit2 <* char ':' >>| valid_hour))
    (fail_if_nothing
       "Invalid minute"
       (digit2 <* char ':' >>| valid_minute_seconds))
    (fail_if_nothing "Invalid second" (digit2 >>| valid_minute_seconds))

let rfc1123_parser =
  lift4
    (fun weekday date (hour, minute, second) () ->
       { year = date.year
       ; month = date.month
       ; day = date.day
       ; hour
       ; minute
       ; second
       ; weekday
       })
    (parse_week_day <* string ", ")
    parse_date
    parse_time
    (* RFC 2616 defines GMT only but there are actually ill-formed ones such as
       "+0000" and "UTC" in the wild. *)
    (sp <* (string "GMT" <|> string "+0000" <|> string "UTC") <* end_of_input)

let parse date_str = parse_string ~consume:All rfc1123_parser date_str

let month_to_int = function
  | `Jan -> 1
  | `Feb -> 2
  | `Mar -> 3
  | `Apr -> 4
  | `May -> 5
  | `Jun -> 6
  | `Jul -> 7
  | `Aug -> 8
  | `Sep -> 9
  | `Oct -> 10
  | `Nov -> 11
  | `Dec -> 12

let int_to_month = function
  | 1 -> Some `Jan
  | 2 -> Some `Feb
  | 3 -> Some `Mar
  | 4 -> Some `Apr
  | 5 -> Some `May
  | 6 -> Some `Jun
  | 7 -> Some `Jul
  | 8 -> Some `Aug
  | 9 -> Some `Sep
  | 10 -> Some `Oct
  | 11 -> Some `Nov
  | 12 -> Some `Dec
  | _ -> None

let weekday_of_ptime_weekday (wd : Ptime.weekday) : weekday =
  match wd with
  | `Mon -> `Mon
  | `Tue -> `Tue
  | `Wed -> `Wed
  | `Thu -> `Thu
  | `Fri -> `Fri
  | `Sat -> `Sat
  | `Sun -> `Sun

let to_ptime { year; month; day; hour; minute; second; weekday = _ } =
  match
    Ptime.of_date_time
      ((year, month_to_int month, day), ((hour, minute, second), 0))
  with
  | Some ptime -> Ok ptime
  | None -> Error "Invalid date/time values"

let of_ptime ptime =
  (* Truncate to second precision to match HTTP date format. This ensures
     correct If-Modified-Since comparisons by dropping subsecond precision that
     filesystems may provide. *)
  let ptime_truncated = Ptime.truncate ~frac_s:0 ptime in
  let (year, month_int, day), ((hour, minute, second), _tz_offset_s) =
    Ptime.to_date_time ptime_truncated
  in
  let weekday = Ptime.weekday ptime_truncated |> weekday_of_ptime_weekday in
  match int_to_month month_int with
  | Some month -> Ok { year; month; day; hour; minute; second; weekday }
  | None -> Error "Invalid month"

let equal : t -> t -> bool =
 fun d1 d2 ->
  d1.year = d2.year
  && d1.month = d2.month
  && d1.day = d2.day
  && d1.hour = d2.hour
  && d1.minute = d2.minute
  && d1.second = d2.second
