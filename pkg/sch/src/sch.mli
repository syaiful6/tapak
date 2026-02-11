module Json_schema = Json_schema
module Constraint = Json_schema.Constraint
module Free = Free
module Sig = Sig

type unknown_handling =
  | Skip
  | Error_on_unknown

type 'o fieldk

type ('o, 'a) field =
  { name : string
  ; doc : string
  ; codec : 'a t
  ; default : 'a option
  ; get : 'o -> 'a
  ; omit : 'a -> bool
  }

and 'a base_map =
  { doc : string
  ; constraint_ : 'a Constraint.t option
  }

and _ t =
  | Str : string base_map -> string t
  | Password : string base_map -> string t
  | Int : int base_map -> int t
  | Int32 : int32 base_map -> int32 t
  | Int64 : int64 base_map -> int64 t
  | Bool : { doc : string } -> bool t
  | Float : float base_map -> float t
  | Double : float base_map -> float t
  | File : File.t t
  | Option : 'a t -> 'a option t
  | List :
      { doc : string
      ; item : 'a t
      ; constraint_ : 'a list Constraint.t option
      }
      -> 'a list t
  | Object :
      { kind : string
      ; doc : string
      ; unknown : unknown_handling
      ; members : ('o fieldk, 'o) Free.t
      }
      -> 'o t
  | Rec : 'a t Lazy.t -> 'a t
  | Iso :
      { fwd : 'b -> ('a, string list) result
      ; bwd : 'a -> 'b
      ; repr : 'b t
      }
      -> 'a t

type decode_error =
  { path : string list
  ; message : string
  }

val error : string -> decode_error
val errors : string list -> decode_error list
val in_field : string -> decode_error list -> decode_error list
val error_to_pair : decode_error -> string * string
val type_name : 'a t -> string
val format_name : 'a t -> string option
val doc : 'a t -> string
val with_doc_basemap : ?doc:string -> ('a, 'b) field -> ('a, 'b) field

val with_constraint_basemap :
   ?constraint_:'a Constraint.t
  -> 'a base_map
  -> 'a base_map

val with_constraint : ?constraint_:'a Constraint.t -> 'a t -> 'a t
val string : string t
val password : string t
val bool : bool t
val int : int t
val int32 : int32 t
val int64 : int64 t
val float : float t
val double : float t
val file : File.t t
val option : 'a t -> 'a option t
val rec' : 'a t Lazy.t -> 'a t

val custom :
   enc:('a -> 'b)
  -> dec:('b -> ('a, string list) result)
  -> 'b t
  -> 'a t

val list : ?doc:string -> ?constraint_:'a list Constraint.t -> 'a t -> 'a list t

module Object : sig
  include module type of Free.Syntax

  external inj : ('o, 'a) field -> ('a, 'o fieldk) Sig.app = "%identity"
  external prj : ('a, 'o fieldk) Sig.app -> ('o, 'a) field = "%identity"

  val mem :
     ?doc:string
    -> ?default:'a
    -> ?omit:('a -> bool)
    -> ?enc:('b -> 'a)
    -> string
    -> 'a t
    -> ('b fieldk, 'a) Free.t

  val mem_opt :
     ?doc:string
    -> ?enc:('a -> 'b option)
    -> 'b t
    -> string
    -> ('a fieldk, 'b option) Free.t

  val define :
     ?kind:string
    -> ?doc:string
    -> ?unknown:unknown_handling
    -> ('a fieldk, 'a) Free.t
    -> 'a t
end

module Validation : sig
  type 'a t =
    | Success of 'a
    | Error of decode_error list

  val pure : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val to_result : 'a t -> ('a, (string * string) list) result
end

type mem_lookup = string -> Jsont.object' -> Jsont.json option

val mem_exact : mem_lookup
val mem_ci : mem_lookup
val mem_is_known_exact : (string, 'a) Hashtbl.t -> Jsont.name -> bool
val mem_is_known_ci : (string, 'a) Hashtbl.t -> Jsont.name -> bool

module Json_decoder : sig
  val coerce_string : 'a t -> string -> 'a Validation.t
  val decode : ?lookup:mem_lookup -> Jsont.json -> 'a t -> 'a Validation.t
  val decode_string : ?lookup:mem_lookup -> 'a t -> string -> 'a Validation.t

  val decode_reader :
     ?lookup:mem_lookup
    -> 'a t
    -> Bytesrw.Bytes.Reader.t
    -> 'a Validation.t
end

module Json_encoder : sig
  type format =
    | Minify
    | Indent of int

  val encode :
     ?buf:Bytesrw.Bytes.t
    -> ?eod:bool
    -> ?format:format
    -> Bytesrw.Bytes.Writer.t
    -> 'a t
    -> 'a
    -> unit

  val encode_string : ?format:format -> 'a t -> 'a -> string
end

val to_json_schema : ?draft:Json_schema.Draft.t -> 'a t -> Json_schema.t
