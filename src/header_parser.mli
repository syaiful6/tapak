(** HTTP header parsing utilities for content negotiation, encoding, ranges, and dates. *)

(** Accept-Encoding header parsing *)
module Accept : sig
  type encoding =
    [ `Gzip
    | `Deflate
    | `Br
    | `Zstd
    | `Identity
    | `Star
    | `Other of string
    ]

  type q = int
  (** Quality value (0-1000, where 1000 = 1.0) *)

  type 'a qlist = (q * 'a) list
  (** List of items with quality values *)

  val encodings : string option -> encoding qlist
  (** Parse Accept-Encoding header value into a quality-sorted list of encodings.

      @param header_value Optional Accept-Encoding header string
      @return List of (quality, encoding) pairs sorted by quality (highest first)

      Example: [encodings (Some "gzip, deflate;q=0.8")] returns
      [(1000, `Gzip); (800, `Deflate)] *)
end

(** Accept (media type) header parsing *)
module Media_type : sig
  type media_range =
    | Any
    | Type_wildcard of string
    | Concrete of string * string
    | Extension of string

  type media_type =
    { range : media_range
    ; params : (string * string) list
      (** Media type parameters (excluding q) *)
    }
  (** Media type with parameters *)

  type accept_item =
    { media : media_type
    ; q : int  (** Quality value (0-1000) *)
    }
  (** Accept header item with quality value *)

  val parse_accept : string option -> accept_item list
  (** Parse Accept header value into a list of media type preferences.

      @param accept_header Optional Accept header string
      @return List of accept items with quality values

      Example: [parse_accept (Some "text/html, application/json;q=0.9")]
      returns items for HTML (q=1000) and JSON (q=900) *)

  val matches_range : string * string -> media_range -> bool
  (** Check if a content type matches a media range.

      @param content_type Tuple of (type, subtype)
      @param range Media range to match against
      @return true if the content type matches the range *)

  val find_best_match : string -> accept_item list -> accept_item option
  (** Find the best matching accept item for a given content type.

      @param content_type Content type string (e.g., "application/json")
      @param accept_items List of accept preferences
      @return The best matching item, or None if no match *)

  val is_acceptable : string -> accept_item list -> bool
  (** Check if a content type is acceptable according to Accept header.

      @param content_type Content type to check
      @param accept_items List of accept preferences
      @return true if the content type is acceptable (q > 0) *)

  (** Common media type constants *)

  val json : string
  (** "application/json" *)

  val html : string
  (** "text/html" *)

  val xml : string
  (** "application/xml" *)

  val text : string
  (** "text/plain" *)

  val form : string
  (** "application/x-www-form-urlencoded" *)

  val multipart : string
  (** "multipart/form-data" *)
end

(** HTTP Range header parsing (RFC 7233) *)
module Range : sig
  type t =
    | From of int64
    | From_to of int64 * int64
    | Suffix of int64

  val parse : string option -> (t list, string) result
  (** Parse a Range header value.

      @param header_value Optional Range header string
      @return Ok with list of ranges, or Error with parse error message

      Example: [parse (Some "bytes=0-499, -500")] returns
      [Ok [From_to (0L, 499L); Suffix 500L]] *)

  val render : t list -> string
  (** Render ranges back to header value format.

      @param ranges List of range specifications
      @return Range header value string (e.g., "bytes=0-499,1000-")

      Example: [render [From_to (0L, 499L); From 1000L]] returns
      ["bytes=0-499,1000-"] *)
end

(** Content negotiation utilities *)
module Content_negotiation : sig
  type format =
    [ `Json
    | `Html
    | `Xml
    | `Text
    | `Other of string
    ]
  (** Common content format types *)

  val format_to_media_type : format -> string
  (** Convert format to media type string.

      Example: [format_to_media_type `Json] returns ["application/json"] *)

  val media_type_to_format : string -> format
  (** Convert media type string to format.

      Example: [media_type_to_format "application/json"] returns [`Json] *)

  val negotiate_format : string option -> format list -> format option
  (** Negotiate the best format based on Accept header and available formats.

      @param accept_header Optional Accept header value
      @param available_formats List of formats the server can provide
      @return The best matching format, or None if no acceptable format

      Example: [negotiate_format (Some "text/html, application/json;q=0.9") [`Json; `Html]]
      returns [Some `Html] *)

  val preferred_format :
     ?default:format
    -> string option
    -> format list
    -> format
  (** Get preferred format with a default fallback.

      @param default Default format if no match (default: [`Html])
      @param accept_header Optional Accept header value
      @param available_formats List of available formats
      @return Best matching format, or default if none match *)

  val accepts : format -> string option -> bool
  (** Check if a specific format is acceptable.

      @param format Format to check
      @param accept_header Optional Accept header value
      @return true if the format is acceptable (q > 0) *)

  val negotiate_encoding :
     string option
    -> Accept.encoding list
    -> Accept.encoding option
  (** Negotiate the best encoding based on Accept-Encoding header.

      @param accept_encoding_header Optional Accept-Encoding header value
      @param available_encodings List of encodings the server supports
      @return The best matching encoding, or None if no acceptable encoding

      Example: [negotiate_encoding (Some "gzip, deflate;q=0.8") [`Gzip; `Deflate]]
      returns [Some `Gzip] *)

  val preferred_encoding :
     ?default:Accept.encoding
    -> string option
    -> Accept.encoding list
    -> Accept.encoding
  (** Get preferred encoding with a default fallback.

      @param default Default encoding if no match (default: [`Identity])
      @param accept_encoding_header Optional Accept-Encoding header value
      @param available_encodings List of available encodings
      @return Best matching encoding, or default if none match *)
end

(** HTTP Date header parsing (RFC 1123 format) *)
module Date : sig
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
end
