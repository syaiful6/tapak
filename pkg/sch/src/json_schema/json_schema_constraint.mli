module Fmt : sig
  type t =
    [ `Custom of string
    | `Date
    | `Date_time
    | `Duration
    | `Email
    | `Hostname
    | `Idn_email
    | `Ipv4
    | `Ipv6
    | `Time
    | `Uri
    | `Uuid
    ]

  val pattern : string -> string -> (string, string list) result
  val validate : t -> string -> (string, string list) result
  val to_string : t -> string
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
