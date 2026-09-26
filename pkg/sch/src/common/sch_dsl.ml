module Constraint = Sch_constraint
module Free = Sch_free
module Sig = Sch_sig
module File = Sch_file

let ( <|> ) ma mb = match ma with None -> mb | Some _ -> ma

type unknown_handling =
  | Skip
  | Error_on_unknown

type 'o fieldk
(** field kind, used to lifted to LKHT, it the same
    as field below *)

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

and 'a union_case =
  | Case :
      { tag : string
      ; doc : string
      ; codec : 'b t
      ; inject : 'b -> 'a
      ; project : 'a -> 'b option
      }
      -> 'a union_case
  (** a prism like structure for union cases, the inject and project functions are
   used to convert between the case payload and the union type *)

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
  | Tagless_union :
      { doc : string
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

let case_tags (cases : _ union_case list) =
  List.map (fun (Case c) -> c.tag) cases

let find_case_by_tag tag cases =
  List.find_opt (fun (Case c) -> String.equal c.tag tag) cases

type 'a projected_case =
  | Projected :
      { tag : string
      ; codec : 'b t
      ; payload : 'b
      }
      -> 'a projected_case

let rec find_case_for_value value (cases : 'a union_case list) =
  match cases with
  | [] -> None
  | Case c :: rest ->
    (match c.project value with
    | Some payload -> Some (Projected { tag = c.tag; codec = c.codec; payload })
    | None -> find_case_for_value value rest)

let find_all_cases_for_value value (cases : 'a union_case list) =
  List.filter_map
    (fun (Case c) ->
       match c.project value with
       | Some payload ->
         Some (Projected { tag = c.tag; codec = c.codec; payload })
       | None -> None)
    cases

let ensure_nonempty_cases msg (cases : 'a union_case list) =
  match cases with [] -> invalid_arg msg | _ -> ()

let ensure_unique_tags msg (cases : 'a union_case list) =
  let rec loop = function
    | [] -> ()
    | tag :: rest -> if List.mem tag rest then invalid_arg msg else loop rest
  in
  loop (case_tags cases)

type decode_error =
  { path : string list  (** Field path, e.g., ["user"; "address"; "city"] *)
  ; message : string
  }
(** Structured decode error with field path for precise error location *)

(** Create an error at the current location (empty path) *)
let error message = { path = []; message }

(** Create multiple errors at the current location *)
let errors messages = List.map error messages

(** Add a field name to the path of all errors *)
let in_field name errs =
  List.map (fun e -> { e with path = name :: e.path }) errs

(** Convert a decode_error to a (path_string, message) pair *)
let error_to_pair e =
  let path_str = if e.path = [] then "" else String.concat "." e.path in
  path_str, e.message

(** typename according to openapi *)
let rec type_name : type a. a t -> string = function
  | Str _ -> "string"
  | Password _ -> "string"
  | Int _ -> "integer"
  | Int32 _ -> "integer"
  | Int64 _ -> "integer"
  | Bool _ -> "boolean"
  | Float _ -> "number"
  | Double _ -> "number"
  | File -> "string"
  | Option ta -> type_name ta
  | List _ -> "array"
  | Map _ -> "object"
  | Object _ -> "object"
  | Union _ -> "object"
  | Tagless_union _ -> "union"
  | Rec t -> type_name (Lazy.force t)
  | Iso { repr; _ } -> type_name repr

(** format according to openapi *)
let rec format_name : type a. a t -> string option = function
  | Str { constraint_; _ } -> Option.bind constraint_ Constraint.string_format
  | Password _ -> Some "password"
  | Int _ -> Some "int32"
  | Int32 _ -> Some "int32"
  | Int64 _ -> Some "int64"
  | Bool _ -> None
  | Float _ -> Some "float"
  | Double _ -> Some "double"
  | File -> None
  | Option ta -> format_name ta
  | List _ -> None
  | Map _ -> None
  | Object _ -> None
  | Union _ -> None
  | Tagless_union _ -> None
  | Rec t -> format_name (Lazy.force t)
  | Iso { repr; _ } -> format_name repr

let rec doc : type a. a t -> string = function
  | Str { doc; _ } -> doc
  | Password { doc; _ } -> doc
  | Int { doc; _ } -> doc
  | Int32 { doc; _ } -> doc
  | Int64 { doc; _ } -> doc
  | Bool { doc; _ } -> doc
  | Float { doc; _ } -> doc
  | Double { doc; _ } -> doc
  | File -> "A file upload"
  | Option t -> doc t
  | List { doc; _ } -> doc
  | Map { doc; _ } -> doc
  | Object { doc; _ } -> doc
  | Union { doc; _ } -> doc
  | Tagless_union { doc; _ } -> doc
  | Rec t -> doc (Lazy.force t)
  | Iso { repr; _ } -> doc repr

let rec is_object_codec : type a. a t -> bool = function
  | Object _ -> true
  | Union _ -> true
  | Tagless_union _ -> false
  | Rec t -> is_object_codec (Lazy.force t)
  | Iso { repr; _ } -> is_object_codec repr
  | _ -> false

let with_basemap ?constraint_:ct ?doc:d (c : 'a base_map) =
  { constraint_ = ct <|> c.constraint_; doc = Option.value d ~default:c.doc }

let rec with_ : type a.
  ?constraint_:a Constraint.t
  -> ?doc:string
  -> ?discriminator:string
  -> a t
  -> a t
  =
 fun ?constraint_ ?doc ?discriminator codec ->
  match codec with
  | Str c -> Str (with_basemap ?constraint_ ?doc c)
  | Password c -> Password (with_basemap ?constraint_ ?doc c)
  | Int c -> Int (with_basemap ?constraint_ ?doc c)
  | Int32 c -> Int32 (with_basemap ?constraint_ ?doc c)
  | Int64 c -> Int64 (with_basemap ?constraint_ ?doc c)
  | Bool _ -> codec
  | Float c -> Float (with_basemap ?constraint_ ?doc c)
  | Double c -> Double (with_basemap ?constraint_ ?doc c)
  | File -> codec
  | Option _ -> codec
  | List c ->
    List
      { c with
        constraint_ = constraint_ <|> c.constraint_
      ; doc = Option.value doc ~default:c.doc
      }
  | Map c ->
    Map
      { c with
        constraint_ = constraint_ <|> c.constraint_
      ; doc = Option.value doc ~default:c.doc
      }
  | Object _ -> codec
  | Union u ->
    Union
      { u with
        doc = Option.value doc ~default:u.doc
      ; discriminator = Option.value discriminator ~default:u.discriminator
      }
  | Tagless_union u ->
    Tagless_union { u with doc = Option.value doc ~default:u.doc }
  | Rec t -> Rec (lazy (with_ ?constraint_ ?doc ?discriminator (Lazy.force t)))
  | Iso _ -> codec

let string = Str { doc = ""; constraint_ = None }
let password = Password { doc = ""; constraint_ = None }
let bool = Bool { doc = "" }
let int = Int { doc = ""; constraint_ = None }
let int32 = Int32 { doc = ""; constraint_ = None }
let int64 = Int64 { doc = ""; constraint_ = None }
let float = Float { doc = ""; constraint_ = None }
let double = Double { doc = ""; constraint_ = None }
let file = File
let option t = Option t
let rec' t = Rec t
let custom ~enc ~dec repr = Iso { fwd = dec; bwd = enc; repr }

let list ?doc:c ?constraint_:ct t =
  List { doc = Option.value c ~default:"A list"; item = t; constraint_ = ct }

let map ?doc:c ?constraint_:ct t =
  Map { doc = Option.value c ~default:"A map"; item = t; constraint_ = ct }

module Object = struct
  include Free.Syntax

  external inj : ('o, 'a) field -> ('a, 'o fieldk) Sig.app = "%identity"
  external prj : ('a, 'o fieldk) Sig.app -> ('o, 'a) field = "%identity"

  let no_encode name _v =
    raise (Invalid_argument (Printf.sprintf "No encoder for member %s" name))

  let mem ?(doc = "") ?default ?(omit = Fun.const false) ?enc name codec =
    let get = Option.value enc ~default:(no_encode name) in
    let field = { name; doc; codec; default; get; omit } in
    Free.lift (inj field)

  let mem_opt ?doc ?enc name codec =
    mem name ?doc ~default:None ?enc ~omit:Option.is_none (option codec)

  let define ?(kind = "") ?(doc = "") ?(unknown = Skip) members =
    Object { kind; doc; unknown; members }
end

module Validation = struct
  type 'a t =
    | Success of 'a
    | Error of decode_error list

  let pure x = Success x
  let map f = function Success x -> Success (f x) | Error errs -> Error errs

  let apply tf tx =
    match tf, tx with
    | Success f, Success x -> Success (f x)
    | Error errs_f, Error errs_x -> Error (errs_f @ errs_x)
    | Error errs, _ | _, Error errs -> Error errs

  let lifta2 f xa xb = apply (map f xa) xb

  let traverse f xs =
    List.fold_right
      (fun x acc -> lifta2 (fun a b -> a :: b) (f x) acc)
      xs
      (pure [])

  let to_result = function
    | Success x -> Ok x
    | Error errs -> Error (List.map error_to_pair errs)
end
