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
  { year : int
  ; month : month
  ; day : int
  ; hour : int
  ; minute : int
  ; second : int
  ; weekday : weekday
  }

val parse : string -> (t, string) result
(** Parse RFC 1123 date string.

      @param date_str Date string in RFC 1123 format
      @return Ok with parsed date, or Error with parse error message

      Example: [parse "Mon, 27 Jul 2009 12:28:53 GMT"] *)

val pp : Format.formatter -> t -> unit
(** Format date as RFC 1123 string.

      Example output: ["Mon, 27 Jul 2009 12:28:53 GMT"] *)

val of_ptime : Ptime.t -> (t, string) result
(** Convert a Ptime.t to HTTP date.

      @param ptime The Ptime value to convert
      @return Ok with converted date, or Error if conversion fails

      Note: HTTP dates are always in GMT/UTC timezone.
      The weekday is automatically calculated from the date.
      Subsecond precision is truncated to match HTTP date format (second precision only).
      This ensures correct If-Modified-Since comparisons. *)

val to_ptime : t -> (Ptime.t, string) result
(** Convert HTTP date to Ptime.t.

      @param date The HTTP date to convert
      @return Ok with Ptime value, or Error if date values are invalid

      Note: The resulting Ptime is in UTC. The weekday field is ignored
      during conversion as Ptime calculates it from the date. *)

val equal : t -> t -> bool
(** Check if two HTTP dates are equal. *)
