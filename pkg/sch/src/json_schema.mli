module Json_type : sig
  type t
  (** json type set *)

  (** json value type *)
  type value =
    | Null
    | Boolean
    | Number
    | Integer
    | String
    | Array
    | Object

  val null : t
  val boolean : t
  val number : t
  val integer : t
  val string : t
  val array : t
  val object_ : t
  val of_value : value -> t
  val union : t -> t -> t
  val contains : t -> value -> bool
  val is_empty : t -> bool
  val type_of : Jsont.json -> value
  val contains_json : t -> Jsont.json -> bool
  val of_list : value list -> t
  val iter : (value -> unit) -> t -> unit
  val type_name : value -> string
  val type_of_string : string -> value option
  val to_list_of_name : t -> string list
  val jsont : t Jsont.t
end

module Draft : sig
  type t =
    | Draft4
    | Draft6
    | Draft7
    | Draft2019_09
    | Draft2020_12

  val of_url : string -> t option
  val to_string : t -> string
  val jsont : t Jsont.t
end

module Or_bool : sig
  type 'a t =
    | Schema of 'a
    | Bool of bool

  val jsont : 'a Jsont.t -> 'a t Jsont.t
end

module Or_ref : sig
  type 'a t =
    | Value of 'a
    | Ref of string

  val find_member : string -> Jsont.mem list -> Jsont.json option
  val jsont : 'a Jsont.t -> 'a t Jsont.t
end

val list_string_map_jsont : 'a Jsont.t -> (string * 'a) list Jsont.t

type t =
  { schema : Draft.t option
  ; id : string option
  ; id_legacy : string option
  ; anchor : string option
  ; ref_ : string option
  ; dynamic_ref : string option
  ; dynamic_anchor : string option
  ; vocabulary : (string * bool) list option
  ; comment : string option
  ; defs : (string * schema) list option
  ; type_ : Json_type.t
  ; additional_items : schema option
  ; unevaluated_items : schema option
  ; prefix_items : schema list option
  ; items : schema option
  ; contains : schema option
  ; additional_properties : schema option
  ; unevaluated_properties : schema option
  ; properties : (string * schema) list option
  ; pattern_properties : (string * schema) list option
  ; dependent_schemas : (string * schema) list option
  ; property_names : schema option
  ; if_ : schema option
  ; then_ : schema option
  ; else_ : schema option
  ; all_of : schema list option
  ; any_of : schema list option
  ; one_of : schema list option
  ; not_ : schema option
  ; multiple_of : float option
  ; maximum : float option
  ; exclusive_maximum : float option
  ; minimum : float option
  ; exclusive_minimum : float option
  ; max_length : int option
  ; min_length : int option
  ; pattern : string option
  ; max_items : int option
  ; min_items : int option
  ; unique_items : bool option
  ; max_contains : int option
  ; min_contains : int option
  ; max_properties : int option
  ; min_properties : int option
  ; required : string list option
  ; dependent_required : (string * string list) list option
  ; enum : Jsont.json list option
  ; const : Jsont.json option
  ; title : string option
  ; description : string option
  ; default : Jsont.json option
  ; deprecated : bool option
  ; read_only : bool option
  ; write_only : bool option
  ; nullable : bool option
  ; examples : Jsont.json list option
  ; format : string option
  ; content_media_type : string option
  ; content_encoding : string option
  ; content_schema : schema option
  }

and schema = t Or_ref.t Or_bool.t

type schema_obj = t

val empty : t
val merge : t -> t -> t
val jsont : t Jsont.t
val schema_jsont : schema Jsont.t
val of_string : string -> (t, Jsont.Error.t) result
val to_string : t -> (string, Jsont.Error.t) result

module Constraint : sig
  module Fmt : sig
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

    val validate : t -> string -> (string, string list) result
    (** [validate fmt str] validates [str] against the format [fmt].
        Returns [Ok str] if valid, or [Error errors] if invalid. *)

    val pattern : string -> string -> (string, string list) result
    (** [pattern pat str] validates [str] against the regex pattern [pat].
        Returns [Ok str] if valid, or [Error errors] if invalid. *)
  end

  type 'a t

  val string_format : string t -> string option
  val int_min : int -> int t
  val int_max : int -> int t
  val int_range : int -> int -> int t
  val int_multiple_of : int -> int t
  val int32_min : int32 -> int32 t
  val int32_max : int32 -> int32 t
  val int32_range : int32 -> int32 -> int32 t
  val int64_min : int64 -> int64 t
  val int64_max : int64 -> int64 t
  val int64_range : int64 -> int64 -> int64 t
  val float_min : float -> float t
  val float_max : float -> float t
  val float_range : float -> float -> float t
  val min_length : int -> string t
  val max_length : int -> string t
  val length_range : int -> int -> string t
  val pattern : string -> string t
  val format : Fmt.t -> string t
  val min_items : int -> 'a list t
  val max_items : int -> 'a list t
  val float_exclusive_max : float -> float t
  val float_exclusive_min : float -> float t
  val int_exclusive_max : int -> int t
  val int_exclusive_min : int -> int t
  val int32_exclusive_max : int32 -> int32 t
  val int32_exclusive_min : int32 -> int32 t
  val int64_exclusive_max : int64 -> int64 t
  val int64_exclusive_min : int64 -> int64 t
  val unique_items : 'a list t
  val any_of : 'a t list -> 'a t
  val all_of : 'a t list -> 'a t
  val one_of : 'a t list -> 'a t
  val not : 'a t -> 'a t
  val apply_all : 'a t list -> 'a -> ('a, string list) result
  val apply : 'a t option -> 'a -> ('a, string list) result
  val to_json_schema_obj : 'a t -> schema_obj
  val to_json_schema : 'a t -> schema
end
