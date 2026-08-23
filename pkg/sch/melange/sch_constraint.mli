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
