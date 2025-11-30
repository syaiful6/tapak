type _ input =
  | Json : Yojson.Safe.t input
  | Urlencoded : Form.Urlencoded.t input
  | Multipart : Form.Multipart.t input

type _ field =
  | Str : { default : string option } -> string field
  | Int : { default : int option } -> int field
  | Int32 : { default : int32 option } -> int32 field
  | Int64 : { default : int64 option } -> int64 field
  | Bool : { default : bool option } -> bool field
  | Float : { default : float option } -> float field
  | Option : 'a field -> 'a option field
  | List :
      { default : 'a list option
      ; item : 'a field
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

  val str : ?default:string -> unit -> string field
  val int : ?default:int -> unit -> int field
  val int32 : ?default:int32 -> unit -> int32 field
  val int64 : ?default:int64 -> unit -> int64 field
  val bool : ?default:bool -> unit -> bool field
  val float : ?default:float -> unit -> float field
  val list : ?default:'a list -> 'a field -> 'a list field
  val option : 'a field -> 'a option field
  val choice : ?default:'a -> (string * 'a) list -> ('a * int) list field
  val file : unit -> Form.Multipart.part field
end

module Validator : sig
  type ('a, 'b) t = 'a -> ('b, string list) result

  val nes : (string, string) t
  val str : ?min_len:int -> ?max_len:int -> (string, string) t
  val int : ?min:int -> ?max:int -> (int, int) t
  val int32 : ?min:int32 -> ?max:int32 -> (int32, int32) t
  val int64 : ?min:int64 -> ?max:int64 -> (int64, int64) t
end

val validate : ('a -> ('b, string list) result) -> 'a t -> 'b t
val map : ('a -> 'b) -> 'a t -> 'b t
val return : 'a -> 'a t
val field : string -> 'a field -> 'a t
val int : ?default:int -> string -> int t
val str : ?default:string -> string -> string t
val int32 : ?default:int32 -> string -> int32 t
val int64 : ?default:int64 -> string -> int64 t
val bool : ?default:bool -> string -> bool t
val float : ?default:float -> string -> float t
val option : string -> 'a field -> 'a option t
val list : ?default:'a list -> string -> 'a field -> 'a list t
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
