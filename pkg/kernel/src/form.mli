module Multipart = Piaf.Form.Multipart

module Urlencoded : sig
  type t = (string * string list) list

  val of_string : string -> (string * string list) list

  val of_body :
     Body.t
    -> ((string * string list) list, [> `Bad_request ]) result

  val of_query : Request.t -> (string * string list) list

  val normalize : t -> t
  (** [normalize params] groups duplicate keys into single entries with all values combined.
      For example: [("color", ["red"]); ("color", ["blue"])] becomes [("color", ["red"; "blue"])].
      The resulting list is sorted by key name. *)

  val get : string -> t -> string option
  (** [get key params] returns the first value associated with [key], or [None] if not found. *)

  val get_list : string -> t -> string list
  (** [get_list key params] returns all values associated with [key] across all occurrences. *)
end
