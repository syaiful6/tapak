module Multipart : sig
  (** Multipart form field representation.

      {b Important}: Do not rely on [filename] presence to distinguish between
      text fields and file uploads. Some clients (like curl) can send files
      without filenames, while browsers may send empty filenames for files.
      Use the field name and your form's structure to determine the field type. *)

  type part =
    { name : string
    ; filename : string option
    ; content_type : string
    ; body : string option Eio.Stream.t
    }
  (** A multipart form part. Both text fields and file uploads use this representation. *)

  type t = (string * part) list
  (** Association list of field names to their parts. *)

  val part_to_string : part -> string
  (** [body_to_string stream] reads the entire body of a multipart part into a string.
      Use this for small text fields. For file uploads or large fields, read the
      body stream directly to avoid memory issues. *)

  val parse : Request.t -> t
  (** [parse request] parses a multipart/form-data request. *)

  val get_part : string -> t -> part option
  (** [get_part key fields] returns the first part with [key], or [None]. *)

  val get_all_parts : string -> t -> part list
  (** [get_all_parts key fields] returns all parts for [key]. *)

  val get_field : string -> t -> string option
  (** [get_field key fields] is a convenience function that:
      1. Finds the first part with [key]
      2. Reads its body to a string

      Use this for text form fields where you expect small values. *)

  val get_all_fields : string -> t -> string list
  (** [get_all_fields key fields] reads all parts with [key] as strings. *)

  type node =
    | Object of (string, node) Hashtbl.t
    | Array of node list
    | Part of part
    (** Tree node representing multipart form structure per OpenAPI spec.

        Per the OpenAPI multipart spec, complex fields (objects, arrays) are
        encoded as a single part with [Content-Type: application/json]. Repeated
        parts with the same field name produce an [Array] node. Simple scalar
        fields are [Part] nodes whose body is read as a plain string.

        Decoders (e.g. in [Sch_ext]) inspect [part.content_type] to decide how
        to interpret each [Part]:
        - ["application/json"] → parse body as JSON, decode against schema
        - binary / with filename → treat as file upload ([Sch.File])
        - otherwise → read body as a string scalar *)

  val to_tree : t -> node
  (** [to_tree parts] organises multipart parts into a lookup tree.

      Parts are grouped by field name. A field that appears only once becomes a
      [Part] leaf; a field that appears multiple times (repeated fields, i.e. an
      array of values) becomes an [Array] of [Part] leaves. *)
end

module Urlencoded : sig
  type t = (string * string list) list

  val of_string : string -> t
  val of_body : _ Eio.Flow.source -> t
  val of_query : Request.t -> t

  val normalize : t -> t
  (** [normalize params] groups duplicate keys into single entries with all values combined.
      For example: [("color", ["red"]); ("color", ["blue"])] becomes [("color", ["red"; "blue"])].
      The resulting list is sorted by key name. *)

  val get : string -> t -> string option
  (** [get key params] returns the first value associated with [key], or [None] if not found. *)

  val get_list : string -> t -> string list
  (** [get_list key params] returns all values associated with [key] across all occurrences. *)

  val to_json : t -> Jsont.json
  (** [to_json params] converts the URL-encoded parameters into a nested
      Jsont.json structure, interpreting bracket notation (e.g., [user[name]])
      as nested objects and arrays. This allows deep schema validation for forms. *)
end
