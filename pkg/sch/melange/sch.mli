module Constraint = Sch_constraint
module Free = Sch_free
module Sig = Sch_sig
module File = Sch_file

type unknown_handling = Sch_dsl.unknown_handling =
  | Skip
  | Error_on_unknown

type 'o fieldk = 'o Sch_dsl.fieldk

type ('o, 'a) field = ('o, 'a) Sch_dsl.field =
  { name : string
  ; doc : string
  ; codec : 'a t
  ; default : 'a option
  ; get : 'o -> 'a
  ; omit : 'a -> bool
  }

and 'a base_map = 'a Sch_dsl.base_map =
  { doc : string
  ; constraint_ : 'a Constraint.t option
  }

and 'a union_case = 'a Sch_dsl.union_case =
  | Case :
      { tag : string
      ; doc : string
      ; codec : 'b t
      ; inject : 'b -> 'a
      ; project : 'a -> 'b option
      }
      -> 'a union_case

and 'a t = 'a Sch_dsl.t =
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
  | Map :
      { doc : string
      ; item : 'a t
      ; constraint_ : (string * 'a) list Constraint.t option
      }
      -> (string * 'a) list t
  | Object :
      { kind : string
      ; doc : string
      ; unknown : unknown_handling
      ; members : ('o fieldk, 'o) Free.t
      }
      -> 'o t
  | Union :
      { doc : string
      ; discriminator : string
      ; cases : 'a union_case list
      }
      -> 'a t
  | Rec : 'a t Lazy.t -> 'a t
  | Iso :
      { fwd : 'b -> ('a, string list) result
      ; bwd : 'a -> 'b
      ; repr : 'b t
      }
      -> 'a t

val case_tags : 'a union_case list -> string list
val find_case_by_tag : String.t -> 'a union_case list -> 'a union_case option

type 'a projected_case = 'a Sch_dsl.projected_case =
  | Projected :
      { tag : string
      ; codec : 'b t
      ; payload : 'b
      }
      -> 'a projected_case

val find_case_for_value : 'a -> 'a union_case list -> 'b projected_case option

type decode_error = Sch_dsl.decode_error =
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
val is_object_codec : 'a t -> bool

val with_basemap :
   ?constraint_:'a Constraint.t
  -> ?doc:string
  -> 'a base_map
  -> 'a base_map

val with_ :
   ?constraint_:'a Constraint.t
  -> ?doc:string
  -> ?discriminator:string
  -> 'a t
  -> 'a t

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

val map :
   ?doc:string
  -> ?constraint_:(string * 'a) list Constraint.t
  -> 'a t
  -> (string * 'a) list t

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
    -> string
    -> 'b t
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
  val traverse : ('a -> 'b t) -> 'a list -> 'b list t
end

module Union : sig
  type 'a case = 'a union_case

  val case :
     ?doc:string
    -> tag:string
    -> inj:('a -> 'b)
    -> proj:('b -> 'a option)
    -> 'a t
    -> 'b union_case

  val ensure_unique_tags : 'a union_case list -> unit

  val define :
     ?doc:string
    -> ?discriminator:String.t
    -> 'a union_case list
    -> 'a t
end

module Json : sig
  type format =
    | Minify
    | Indent of int

  val decode : 'a t -> Js.Json.t -> 'a Validation.t
  val decode_string : 'a t -> string -> 'a Validation.t
  val coerce_string : 'a t -> string -> 'a Validation.t
  val encode : 'a t -> 'a -> Js.Json.t
  val encode_string : ?format:format -> 'a t -> 'a -> string
end
