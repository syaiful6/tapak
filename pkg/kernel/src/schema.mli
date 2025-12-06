module Constraint : sig
  type _ num_t =
    | Int_ty : int num_t
    | Int32_ty : int32 num_t
    | Int64_ty : int64 num_t
    | Float_ty : float num_t

  type _ num_constraint =
    | Min : 'a -> 'a num_constraint
    | Max : 'a -> 'a num_constraint
    | Exclusive_min : 'a -> 'a num_constraint
    | Exclusive_max : 'a -> 'a num_constraint
    | Multiple_of : 'a -> 'a num_constraint

  type string_format =
    [ `Email
    | `Uri
    | `Uuid
    | `Date
    | `Date_time
    | `Ipv4
    | `Ipv6
    | `Custom of string
    ]

  type _ t =
    | Min_length : int -> string t
    | Max_length : int -> string t
    | Pattern : string -> string t
    | Format : string_format -> string t
    | Numeric : ('a num_t * 'a num_constraint list) -> 'a t
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
  val format : string_format -> string t
  val min_items : int -> 'a list t
  val max_items : int -> 'a list t
  val unique_items : 'a list t
  val any_of : 'a t list -> 'a t
  val all_of : 'a t list -> 'a t
  val one_of : 'a t list -> 'a t
  val not : 'a t -> 'a t

  val eval : 'a t -> 'a -> ('a, string list) result
  (** Evaluate a single constraint against a value. Returns Ok value if the constraint
      is satisfied, or Error with a list of validation error messages. *)

  val apply_constraints : 'a t list -> 'a -> ('a, string list) result
  (** Apply multiple constraints to a value, collecting all errors.
      This is the recommended way to validate values with multiple constraints.
      Custom field interpreters should use this function to apply field constraints. *)

  val apply_constraint : 'a t option -> 'a -> ('a, string list) result
  (** Apply an optional constraint to a value. Returns Ok value if the constraint is None
      or if the constraint is satisfied. Returns Error if the constraint fails. *)

  val to_json_schema : 'a t -> (string * Yojson.Safe.t) list
  (** Convert a constraint to JSON Schema properties. Returns a list of key-value pairs
      that can be merged into a JSON Schema object. *)
end

type _ input =
  | Json : Yojson.Safe.t input
  | Urlencoded : Form.Urlencoded.t input
  | Multipart : Form.Multipart.t input

type _ field =
  | Str :
      { default : string option
      ; constraint_ : string Constraint.t option
      }
      -> string field
  | Int :
      { default : int option
      ; constraint_ : int Constraint.t option
      }
      -> int field
  | Int32 :
      { default : int32 option
      ; constraint_ : int32 Constraint.t option
      }
      -> int32 field
  | Int64 :
      { default : int64 option
      ; constraint_ : int64 Constraint.t option
      }
      -> int64 field
  | Bool : { default : bool option } -> bool field
  | Float :
      { default : float option
      ; constraint_ : float Constraint.t option
      }
      -> float field
  | Option : 'a field -> 'a option field
  | List :
      { default : 'a list option
      ; item : 'a field
      ; constraint_ : 'a list Constraint.t option
      }
      -> 'a list field
  | File : Form.Multipart.part field
  | Choice :
      { choices : (string * 'a) list
      ; default : 'a option
      }
      -> ('a * int) list field
  | Object : 'a t -> 'a field

and _ t =
  | Pure : 'a -> 'a t
  | Field :
      { field : 'a field
      ; name : string
      }
      -> 'a t
  | App : (('b -> 'a) t * 'b t) -> 'a t
  | Map :
      { transform : 'b -> ('a, string list) result
      ; tree : 'b t
      }
      -> 'a t

type 'input interpreter =
  { eval : 'a. 'a t -> 'input -> ('a, string list) result }

module type FIELD_INTERPRETER = sig
  type input

  val get_input : string -> input -> (input, string list) result
  (** Extract input value by field name. If the field is missing, return a default
      empty/null value for this input type. Return Error only if the input isn't
      a valid object/container. *)

  val eval : input interpreter -> 'a field -> input -> ('a, string list) result
  (** Evaluate a field against an input. Takes a tree evaluator (interpreter) that
      can evaluate nested Object fields. *)
end

module Yojson_interpreter : FIELD_INTERPRETER with type input = Yojson.Safe.t

module Multipart_interpreter :
  FIELD_INTERPRETER with type input = Form.Multipart.node

module Field : sig
  type 'a t = 'a field

  val str :
     ?default:string
    -> ?constraint_:string Constraint.t
    -> unit
    -> string field

  val int : ?default:int -> ?constraint_:int Constraint.t -> unit -> int field

  val int32 :
     ?default:int32
    -> ?constraint_:int32 Constraint.t
    -> unit
    -> int32 field

  val int64 :
     ?default:int64
    -> ?constraint_:int64 Constraint.t
    -> unit
    -> int64 field

  val bool : ?default:bool -> unit -> bool field

  val float :
     ?default:float
    -> ?constraint_:float Constraint.t
    -> unit
    -> float field

  val list :
     ?default:'a list
    -> ?constraint_:'a list Constraint.t
    -> 'a field
    -> 'a list field

  val option : 'a field -> 'a option field
  val choice : ?default:'a -> (string * 'a) list -> ('a * int) list field
  val file : unit -> Form.Multipart.part field
end

val validate : ('a -> ('b, string list) result) -> 'a t -> 'b t
val map : ('a -> 'b) -> 'a t -> 'b t
val return : 'a -> 'a t
val field : string -> 'a field -> 'a t
val int : ?default:int -> ?constraint_:int Constraint.t -> string -> int t

val str :
   ?default:string
  -> ?constraint_:string Constraint.t
  -> string
  -> string t

val int32 :
   ?default:int32
  -> ?constraint_:int32 Constraint.t
  -> string
  -> int32 t

val int64 :
   ?default:int64
  -> ?constraint_:int64 Constraint.t
  -> string
  -> int64 t

val bool : ?default:bool -> string -> bool t

val float :
   ?default:float
  -> ?constraint_:float Constraint.t
  -> string
  -> float t

val option : string -> 'a field -> 'a option t

val list :
   ?default:'a list
  -> ?constraint_:'a list Constraint.t
  -> string
  -> 'a field
  -> 'a list t

val choice : ?default:'a -> string -> (string * 'a) list -> ('a * int) list t
val obj : string -> 'a t -> 'a t
val file : string -> Form.Multipart.part t

val evaluate :
   (module FIELD_INTERPRETER with type input = 'b)
  -> 'a t
  -> 'b
  -> ('a, (string * string) list) result
(** Evaluate a schema against any input type by providing your own interpreter.

    Returns [Ok value] when validation succeeds, or [Error errors] where each error
    is a [(field_name, error_message)] pair so you know exactly what went wrong. *)

val eval : 'b input -> 'a t -> 'b -> ('a, (string * string) list) result
(** Evaluate a schema against JSON, URL-encoded, or multipart form data.

    This is a convenience wrapper around {!evaluate} that picks the right interpreter
     based on the input type.

    Use this when working with the built-in input types. If you need to validate
    custom data formats, use {!evaluate} with your own FIELD_INTERPRETER instead. *)

module Syntax : sig
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end
