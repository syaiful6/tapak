(** JSON Schema specification types with jsont codecs

    This module defines types that mirror the JSON Schema specification
    with bidirectional JSON codecs *)

(** JSON Schema data model *)
module Json_type = struct
  type t = int

  type value =
    | Null
    | Boolean
    | Number
    | Integer
    | String
    | Array
    | Object

  let null = 1 lsl 0
  let boolean = 1 lsl 1
  let number = 1 lsl 2
  let string = 1 lsl 3
  let array = 1 lsl 4
  let object_ = 1 lsl 5
  let integer = 1 lsl 6

  let of_value = function
    | Null -> null
    | Boolean -> boolean
    | Number -> number
    | Integer -> integer
    | String -> string
    | Array -> array
    | Object -> object_

  let union a b = a lor b
  let contains set value = set land of_value value <> 0
  let is_empty set = set = 0

  let type_of = function
    | Jsont.Null _ -> Null
    | Jsont.Bool _ -> Boolean
    | Jsont.Number _ -> Number
    | Jsont.String _ -> String
    | Jsont.Array _ -> Array
    | Jsont.Object _ -> Object

  let contains_json set json = contains set (type_of json)

  let of_list values =
    List.fold_left (fun acc v -> union acc (of_value v)) 0 values

  let iter f set =
    let apply value = if contains set value then f value in
    apply Null;
    apply Boolean;
    apply Number;
    apply Integer;
    apply String;
    apply Array;
    apply Object

  let type_name = function
    | Null -> "null"
    | Boolean -> "boolean"
    | Number -> "number"
    | Integer -> "integer"
    | String -> "string"
    | Array -> "array"
    | Object -> "object"

  let type_of_string = function
    | "null" -> Some Null
    | "boolean" -> Some Boolean
    | "number" -> Some Number
    | "integer" -> Some Integer
    | "string" -> Some String
    | "array" -> Some Array
    | "object" -> Some Object
    | _ -> None

  let to_list_of_name set =
    let types = ref [] in
    iter (fun t -> types := type_name t :: !types) set;
    !types

  let jsont : t Jsont.t =
    Jsont.map
      Jsont.json
      ~kind:"type"
      ~dec:(fun json ->
        match json with
        | Jsont.String (s, _) ->
          (match type_of_string s with
          | Some k -> of_value k
          | None -> Jsont.Error.msg Jsont.Meta.none ("unknown type: " ^ s))
        | Jsont.Array (arr, _) ->
          let rec aux acc = function
            | [] -> acc
            | Jsont.String (s, _) :: rest ->
              (match type_of_string s with
              | Some k -> aux (union acc (of_value k)) rest
              | None -> Jsont.Error.msg Jsont.Meta.none ("unknown type: " ^ s))
            | _ ->
              Jsont.Error.msg Jsont.Meta.none "type array must contain strings"
          in
          aux 0 arr
        | _ -> Jsont.Error.msg Jsont.Meta.none "type must be string or array")
      ~enc:(fun bitset ->
        let types = to_list_of_name bitset in
        match types with
        | [] -> Jsont.Null ((), Jsont.Meta.none)
        | [ single ] -> Jsont.String (single, Jsont.Meta.none)
        | _ ->
          Jsont.Array
            ( List.map (fun s -> Jsont.String (s, Jsont.Meta.none)) types
            , Jsont.Meta.none ))
end

module Draft = struct
  type t =
    | Draft4
    | Draft6
    | Draft7
    | Draft2019_09
    | Draft2020_12

  let of_url url =
    let url =
      match String.index_opt url '#' with
      | Some idx -> String.sub url 0 idx
      | None -> url
    in
    let url =
      if String.starts_with ~prefix:"http://" url
      then String.sub url 7 (String.length url - 7)
      else if String.starts_with ~prefix:"https://" url
      then String.sub url 8 (String.length url - 8)
      else url
    in
    match url with
    | "json-schema.org/schema" -> Some Draft2020_12
    | "json-schema.org/draft/2020-12/schema" -> Some Draft2020_12
    | "json-schema.org/draft/2019-09/schema" -> Some Draft2019_09
    | "json-schema.org/draft-07/schema" -> Some Draft7
    | "json-schema.org/draft-06/schema" -> Some Draft6
    | "json-schema.org/draft-04/schema" -> Some Draft4
    | _ -> None

  let to_string = function
    | Draft4 -> "http://json-schema.org/draft-04/schema"
    | Draft6 -> "http://json-schema.org/draft-06/schema"
    | Draft7 -> "http://json-schema.org/draft-07/schema"
    | Draft2019_09 -> "https://json-schema.org/draft/2019-09/schema"
    | Draft2020_12 -> "https://json-schema.org/draft/2020-12/schema"

  let jsont : t Jsont.t =
    Jsont.map
      Jsont.string
      ~kind:"draft"
      ~dec:(fun s ->
        match of_url s with
        | Some draft -> draft
        | None -> Jsont.Error.msg Jsont.Meta.none ("unknown draft: " ^ s))
      ~enc:to_string
end

module Or_bool = struct
  type 'a t =
    | Schema of 'a (* a schema object *)
    | Bool of bool (* a boolean schema *)

  let jsont schema =
    Jsont.map
      Jsont.json
      ~kind:"schema_or_bool"
      ~dec:(fun json ->
        match json with
        | Jsont.Bool (b, _) -> Bool b
        | Jsont.Object _ ->
          (match
             Jsont_bytesrw.decode_string
               schema
               (Result.get_ok (Jsont_bytesrw.encode_string Jsont.json json))
           with
          | Ok schema -> Schema schema
          | Error e -> Jsont.Error.msg Jsont.Meta.none e)
        | _ -> Jsont.Error.msg Jsont.Meta.none "Expected bool or object")
      ~enc:(function
        | Bool b -> Jsont.Bool (b, Jsont.Meta.none)
        | Schema v ->
          (match Jsont_bytesrw.encode_string schema v with
          | Ok s ->
            (match Jsont_bytesrw.decode_string Jsont.json s with
            | Ok json -> json
            | Error _ ->
              Jsont.Error.msg
                Jsont.Meta.none
                "Failed to decode schema object during encoding")
          | _ ->
            Jsont.Error.msg
              Jsont.Meta.none
              "Failed to encode schema object during encoding"))
end

module Or_ref = struct
  type 'a t =
    | Value of 'a (* a schema object *)
    | Ref of string (* a $ref *)

  let find_member name (mems : Jsont.mem list) : Jsont.json option =
    mems
    |> List.find_map (fun ((n, _meta), v) -> if n = name then Some v else None)

  let jsont (value_jsont : 'a Jsont.t) : 'a t Jsont.t =
    Jsont.map
      Jsont.json
      ~kind:"or_ref"
      ~dec:(fun json ->
        match json with
        | Jsont.Object (mems, _meta) ->
          (match find_member "$ref" mems with
          | Some (Jsont.String (ref_str, _)) -> Ref ref_str
          | _ ->
            (match
               Jsont_bytesrw.decode_string
                 value_jsont
                 (Result.get_ok (Jsont_bytesrw.encode_string Jsont.json json))
             with
            | Ok v -> Value v
            | Error e -> Jsont.Error.msg Jsont.Meta.none e))
        | _ ->
          (match
             Jsont_bytesrw.decode_string
               value_jsont
               (Result.get_ok (Jsont_bytesrw.encode_string Jsont.json json))
           with
          | Ok v -> Value v
          | Error e -> Jsont.Error.msg Jsont.Meta.none e))
      ~enc:(function
        | Ref r ->
          Jsont.Object
            ( [ ("$ref", Jsont.Meta.none), Jsont.String (r, Jsont.Meta.none) ]
            , Jsont.Meta.none )
        | Value v ->
          (match Jsont_bytesrw.encode_string value_jsont v with
          | Ok s ->
            (match Jsont_bytesrw.decode_string Jsont.json s with
            | Ok json -> json
            | Error _ -> Jsont.Null ((), Jsont.Meta.none))
          | Error _ -> Jsont.Null ((), Jsont.Meta.none)))
end

module String_map = Map.Make (String)

let list_string_map_jsont value_jsont : (string * 'a) list Jsont.t =
  Jsont.map
    (Jsont.Object.as_string_map value_jsont)
    ~kind:"string_map"
    ~dec:String_map.bindings
    ~enc:(fun bindings -> bindings |> List.to_seq |> String_map.of_seq)

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

let empty =
  { schema = None
  ; id = None
  ; id_legacy = None
  ; anchor = None
  ; ref_ = None
  ; dynamic_ref = None
  ; dynamic_anchor = None
  ; vocabulary = None
  ; comment = None
  ; defs = None
  ; type_ = 0
  ; additional_items = None
  ; unevaluated_items = None
  ; prefix_items = None
  ; items = None
  ; contains = None
  ; additional_properties = None
  ; unevaluated_properties = None
  ; properties = None
  ; pattern_properties = None
  ; dependent_schemas = None
  ; property_names = None
  ; if_ = None
  ; then_ = None
  ; else_ = None
  ; all_of = None
  ; any_of = None
  ; one_of = None
  ; not_ = None
  ; multiple_of = None
  ; maximum = None
  ; exclusive_maximum = None
  ; minimum = None
  ; exclusive_minimum = None
  ; max_length = None
  ; min_length = None
  ; pattern = None
  ; max_items = None
  ; min_items = None
  ; unique_items = None
  ; max_contains = None
  ; min_contains = None
  ; max_properties = None
  ; min_properties = None
  ; required = None
  ; dependent_required = None
  ; enum = None
  ; const = None
  ; title = None
  ; description = None
  ; default = None
  ; deprecated = None
  ; read_only = None
  ; write_only = None
  ; nullable = None
  ; examples = None
  ; format = None
  ; content_media_type = None
  ; content_encoding = None
  ; content_schema = None
  }

(* [merge a b] merge a schema and b into one, such that fields not defined in a
   will be filled by b *)
let merge : t -> t -> t =
 fun a b ->
  let ( <|> ) c d =
    match c, d with
    | Some x, _ -> Some x
    | None, Some y -> Some y
    | None, None -> None
  in
  { schema = a.schema <|> b.schema
  ; id = a.id <|> b.id
  ; id_legacy = a.id_legacy <|> b.id_legacy
  ; anchor = a.anchor <|> b.anchor
  ; ref_ = a.ref_ <|> b.ref_
  ; dynamic_ref = a.dynamic_ref <|> b.dynamic_ref
  ; dynamic_anchor = a.dynamic_anchor <|> b.dynamic_anchor
  ; vocabulary = a.vocabulary <|> b.vocabulary
  ; comment = a.comment <|> b.comment
  ; defs = a.defs <|> b.defs
  ; type_ = (if Json_type.is_empty a.type_ then b.type_ else a.type_)
  ; additional_items = a.additional_items <|> b.additional_items
  ; unevaluated_items = a.unevaluated_items <|> b.unevaluated_items
  ; prefix_items = a.prefix_items <|> b.prefix_items
  ; items = a.items <|> b.items
  ; contains = a.contains <|> b.contains
  ; additional_properties = a.additional_properties <|> b.additional_properties
  ; unevaluated_properties =
      a.unevaluated_properties <|> b.unevaluated_properties
  ; properties = a.properties <|> b.properties
  ; pattern_properties = a.pattern_properties <|> b.pattern_properties
  ; dependent_schemas = a.dependent_schemas <|> b.dependent_schemas
  ; property_names = a.property_names <|> b.property_names
  ; if_ = a.if_ <|> b.if_
  ; then_ = a.then_ <|> b.then_
  ; else_ = a.else_ <|> b.else_
  ; all_of = a.all_of <|> b.all_of
  ; any_of = a.any_of <|> b.any_of
  ; one_of = a.one_of <|> b.one_of
  ; not_ = a.not_ <|> b.not_
  ; multiple_of = a.multiple_of <|> b.multiple_of
  ; maximum = a.maximum <|> b.maximum
  ; exclusive_maximum = a.exclusive_maximum <|> b.exclusive_maximum
  ; minimum = a.minimum <|> b.minimum
  ; exclusive_minimum = a.exclusive_minimum <|> b.exclusive_minimum
  ; max_length = a.max_length <|> b.max_length
  ; min_length = a.min_length <|> b.min_length
  ; pattern = a.pattern <|> b.pattern
  ; max_items = a.max_items <|> b.max_items
  ; min_items = a.min_items <|> b.min_items
  ; unique_items = a.unique_items <|> b.unique_items
  ; max_contains = a.max_contains <|> b.max_contains
  ; min_contains = a.min_contains <|> b.min_contains
  ; max_properties = a.max_properties <|> b.max_properties
  ; min_properties = a.min_properties <|> b.min_properties
  ; required = a.required <|> b.required
  ; dependent_required = a.dependent_required <|> b.dependent_required
  ; enum = a.enum <|> b.enum
  ; const = a.const <|> b.const
  ; title = a.title <|> b.title
  ; description = a.description <|> b.description
  ; default = a.default <|> b.default
  ; deprecated = a.deprecated <|> b.deprecated
  ; read_only = a.read_only <|> b.read_only
  ; write_only = a.write_only <|> b.write_only
  ; nullable = a.nullable <|> b.nullable
  ; examples = a.examples <|> b.examples
  ; format = a.format <|> b.format
  ; content_media_type = a.content_media_type <|> b.content_media_type
  ; content_encoding = a.content_encoding <|> b.content_encoding
  ; content_schema = a.content_schema <|> b.content_schema
  }

let rec schema_jsont =
  lazy begin
    Or_bool.jsont (Or_ref.jsont (Jsont.rec' jsont))
  end

and jsont =
  lazy begin
    let make
          schema
          id
          id_legacy
          anchor
          ref_
          dynamic_ref
          dynamic_anchor
          vocabulary
          comment
          defs
          type_opt
          additional_items
          unevaluated_items
          prefix_items
          items
          contains
          additional_properties
          unevaluated_properties
          properties
          pattern_properties
          dependent_schemas
          property_names
          if_
          then_
          else_
          all_of
          any_of
          one_of
          not_
          multiple_of
          maximum
          exclusive_maximum
          minimum
          exclusive_minimum
          max_length
          min_length
          pattern
          max_items
          min_items
          unique_items
          max_contains
          min_contains
          max_properties
          min_properties
          required
          dependent_required
          enum
          const
          title
          description
          default
          deprecated
          read_only
          write_only
          nullable
          examples
          format
          content_media_type
          content_encoding
          content_schema
      =
      { schema
      ; id
      ; id_legacy
      ; anchor
      ; ref_
      ; dynamic_ref
      ; dynamic_anchor
      ; vocabulary
      ; comment
      ; defs
      ; type_ = Option.value ~default:0 type_opt
      ; additional_items
      ; unevaluated_items
      ; prefix_items
      ; items
      ; contains
      ; additional_properties
      ; unevaluated_properties
      ; properties
      ; pattern_properties
      ; dependent_schemas
      ; property_names
      ; if_
      ; then_
      ; else_
      ; all_of
      ; any_of
      ; one_of
      ; not_
      ; multiple_of
      ; maximum
      ; exclusive_maximum
      ; minimum
      ; exclusive_minimum
      ; max_length
      ; min_length
      ; pattern
      ; max_items
      ; min_items
      ; unique_items
      ; max_contains
      ; min_contains
      ; max_properties
      ; min_properties
      ; required
      ; dependent_required
      ; enum
      ; const
      ; title
      ; description
      ; default
      ; deprecated
      ; read_only
      ; write_only
      ; nullable
      ; examples
      ; format
      ; content_media_type
      ; content_encoding
      ; content_schema
      }
    in
    Jsont.Object.map ~kind:"schema_obj" make
    |> Jsont.Object.opt_mem "$schema" Draft.jsont ~enc:(fun t -> t.schema)
    |> Jsont.Object.opt_mem "$id" Jsont.string ~enc:(fun t -> t.id)
    |> Jsont.Object.opt_mem "id" Jsont.string ~enc:(fun t -> t.id_legacy)
    |> Jsont.Object.opt_mem "$anchor" Jsont.string ~enc:(fun t -> t.anchor)
    |> Jsont.Object.opt_mem "$ref" Jsont.string ~enc:(fun t -> t.ref_)
    |> Jsont.Object.opt_mem "$dynamicRef" Jsont.string ~enc:(fun t ->
      t.dynamic_ref)
    |> Jsont.Object.opt_mem "$dynamicAnchor" Jsont.string ~enc:(fun t ->
      t.dynamic_anchor)
    |> Jsont.Object.opt_mem
         "$vocabulary"
         (list_string_map_jsont Jsont.bool)
         ~enc:(fun t -> t.vocabulary)
    |> Jsont.Object.opt_mem "$comment" Jsont.string ~enc:(fun t -> t.comment)
    |> Jsont.Object.opt_mem
         "$defs"
         (list_string_map_jsont (Jsont.rec' schema_jsont))
         ~enc:(fun t -> t.defs)
    |> Jsont.Object.opt_mem "type" Json_type.jsont ~enc:(fun t ->
      if Json_type.is_empty t.type_ then None else Some t.type_)
    |> Jsont.Object.opt_mem
         "additionalItems"
         (Jsont.rec' schema_jsont)
         ~enc:(fun t -> t.additional_items)
    |> Jsont.Object.opt_mem
         "unevaluatedItems"
         (Jsont.rec' schema_jsont)
         ~enc:(fun t -> t.unevaluated_items)
    |> Jsont.Object.opt_mem
         "prefixItems"
         (Jsont.list (Jsont.rec' schema_jsont))
         ~enc:(fun t -> t.prefix_items)
    |> Jsont.Object.opt_mem "items" (Jsont.rec' schema_jsont) ~enc:(fun t ->
      t.items)
    |> Jsont.Object.opt_mem "contains" (Jsont.rec' schema_jsont) ~enc:(fun t ->
      t.contains)
    |> Jsont.Object.opt_mem
         "additionalProperties"
         (Jsont.rec' schema_jsont)
         ~enc:(fun t -> t.additional_properties)
    |> Jsont.Object.opt_mem
         "unevaluatedProperties"
         (Jsont.rec' schema_jsont)
         ~enc:(fun t -> t.unevaluated_properties)
    |> Jsont.Object.opt_mem
         "properties"
         (list_string_map_jsont (Jsont.rec' schema_jsont))
         ~enc:(fun t -> t.properties)
    |> Jsont.Object.opt_mem
         "patternProperties"
         (list_string_map_jsont (Jsont.rec' schema_jsont))
         ~enc:(fun t -> t.pattern_properties)
    |> Jsont.Object.opt_mem
         "dependentSchemas"
         (list_string_map_jsont (Jsont.rec' schema_jsont))
         ~enc:(fun t -> t.dependent_schemas)
    |> Jsont.Object.opt_mem
         "propertyNames"
         (Jsont.rec' schema_jsont)
         ~enc:(fun t -> t.property_names)
    |> Jsont.Object.opt_mem "if" (Jsont.rec' schema_jsont) ~enc:(fun t -> t.if_)
    |> Jsont.Object.opt_mem "then" (Jsont.rec' schema_jsont) ~enc:(fun t ->
      t.then_)
    |> Jsont.Object.opt_mem "else" (Jsont.rec' schema_jsont) ~enc:(fun t ->
      t.else_)
    |> Jsont.Object.opt_mem
         "allOf"
         (Jsont.list (Jsont.rec' schema_jsont))
         ~enc:(fun t -> t.all_of)
    |> Jsont.Object.opt_mem
         "anyOf"
         (Jsont.list (Jsont.rec' schema_jsont))
         ~enc:(fun t -> t.any_of)
    |> Jsont.Object.opt_mem
         "oneOf"
         (Jsont.list (Jsont.rec' schema_jsont))
         ~enc:(fun t -> t.one_of)
    |> Jsont.Object.opt_mem "not" (Jsont.rec' schema_jsont) ~enc:(fun t ->
      t.not_)
    |> Jsont.Object.opt_mem "multipleOf" Jsont.number ~enc:(fun t ->
      t.multiple_of)
    |> Jsont.Object.opt_mem "maximum" Jsont.number ~enc:(fun t -> t.maximum)
    |> Jsont.Object.opt_mem "exclusiveMaximum" Jsont.number ~enc:(fun t ->
      t.exclusive_maximum)
    |> Jsont.Object.opt_mem "minimum" Jsont.number ~enc:(fun t -> t.minimum)
    |> Jsont.Object.opt_mem "exclusiveMinimum" Jsont.number ~enc:(fun t ->
      t.exclusive_minimum)
    |> Jsont.Object.opt_mem "maxLength" Jsont.int ~enc:(fun t -> t.max_length)
    |> Jsont.Object.opt_mem "minLength" Jsont.int ~enc:(fun t -> t.min_length)
    |> Jsont.Object.opt_mem "pattern" Jsont.string ~enc:(fun t -> t.pattern)
    |> Jsont.Object.opt_mem "maxItems" Jsont.int ~enc:(fun t -> t.max_items)
    |> Jsont.Object.opt_mem "minItems" Jsont.int ~enc:(fun t -> t.min_items)
    |> Jsont.Object.opt_mem "uniqueItems" Jsont.bool ~enc:(fun t ->
      t.unique_items)
    |> Jsont.Object.opt_mem "maxContains" Jsont.int ~enc:(fun t ->
      t.max_contains)
    |> Jsont.Object.opt_mem "minContains" Jsont.int ~enc:(fun t ->
      t.min_contains)
    |> Jsont.Object.opt_mem "maxProperties" Jsont.int ~enc:(fun t ->
      t.max_properties)
    |> Jsont.Object.opt_mem "minProperties" Jsont.int ~enc:(fun t ->
      t.min_properties)
    |> Jsont.Object.opt_mem "required" (Jsont.list Jsont.string) ~enc:(fun t ->
      t.required)
    |> Jsont.Object.opt_mem
         "dependentRequired"
         (list_string_map_jsont (Jsont.list Jsont.string))
         ~enc:(fun t -> t.dependent_required)
    |> Jsont.Object.opt_mem "enum" (Jsont.list Jsont.json) ~enc:(fun t ->
      t.enum)
    |> Jsont.Object.opt_mem "const" Jsont.json ~enc:(fun t -> t.const)
    |> Jsont.Object.opt_mem "title" Jsont.string ~enc:(fun t -> t.title)
    |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun t ->
      t.description)
    |> Jsont.Object.opt_mem "default" Jsont.json ~enc:(fun t -> t.default)
    |> Jsont.Object.opt_mem "deprecated" Jsont.bool ~enc:(fun t -> t.deprecated)
    |> Jsont.Object.opt_mem "readOnly" Jsont.bool ~enc:(fun t -> t.read_only)
    |> Jsont.Object.opt_mem "writeOnly" Jsont.bool ~enc:(fun t -> t.write_only)
    |> Jsont.Object.opt_mem "nullable" Jsont.bool ~enc:(fun t -> t.nullable)
    |> Jsont.Object.opt_mem "examples" (Jsont.list Jsont.json) ~enc:(fun t ->
      t.examples)
    |> Jsont.Object.opt_mem "format" Jsont.string ~enc:(fun t -> t.format)
    |> Jsont.Object.opt_mem "contentMediaType" Jsont.string ~enc:(fun t ->
      t.content_media_type)
    |> Jsont.Object.opt_mem "contentEncoding" Jsont.string ~enc:(fun t ->
      t.content_encoding)
    |> Jsont.Object.opt_mem
         "contentSchema"
         (Jsont.rec' schema_jsont)
         ~enc:(fun t -> t.content_schema)
    |> Jsont.Object.skip_unknown
    |> Jsont.Object.finish
  end

let jsont = Lazy.force jsont
let schema_jsont = Lazy.force schema_jsont
let of_string s = Jsont_bytesrw.decode_string' jsont s

let to_string schema =
  Jsont_bytesrw.encode_string' ~format:Jsont.Indent jsont schema

module Constraint = struct
  include Json_schema_constraint

  let string_format = function Format f -> Some (Fmt.to_string f) | _ -> None

  let rec to_json_schema_obj : type a. a Json_schema_constraint.t -> schema_obj =
   fun constraint_ ->
    match constraint_ with
    | Min_length n -> { empty with min_length = Some n }
    | Max_length n -> { empty with max_length = Some n }
    | Pattern p -> { empty with pattern = Some p }
    | Format f -> { empty with format = Some (Fmt.to_string f) }
    | Numeric (num_ty, constraints) ->
      (match num_ty with
      | Int_ty ->
        let constraints = (constraints : int num_constraint list) in
        List.fold_left
          (fun sch constraint_ ->
             match constraint_ with
             | Min n -> { sch with minimum = Some (float_of_int n) }
             | Max n -> { sch with maximum = Some (float_of_int n) }
             | Exclusive_min n ->
               { sch with exclusive_minimum = Some (float_of_int n) }
             | Exclusive_max n ->
               { sch with exclusive_maximum = Some (float_of_int n) }
             | Multiple_of n -> { sch with multiple_of = Some (float_of_int n) })
          empty
          constraints
      | Int32_ty ->
        let constraints = (constraints : int32 num_constraint list) in
        List.fold_left
          (fun sch constraint_ ->
             match constraint_ with
             | Min n -> { sch with minimum = Some (Int32.to_float n) }
             | Max n -> { sch with maximum = Some (Int32.to_float n) }
             | Exclusive_min n ->
               { sch with exclusive_minimum = Some (Int32.to_float n) }
             | Exclusive_max n ->
               { sch with exclusive_maximum = Some (Int32.to_float n) }
             | Multiple_of n ->
               { sch with multiple_of = Some (Int32.to_float n) })
          empty
          constraints
      | Int64_ty ->
        let constraints = (constraints : int64 num_constraint list) in
        List.fold_left
          (fun sch constraint_ ->
             match constraint_ with
             | Min n -> { sch with minimum = Some (Int64.to_float n) }
             | Max n -> { sch with maximum = Some (Int64.to_float n) }
             | Exclusive_min n ->
               { sch with exclusive_minimum = Some (Int64.to_float n) }
             | Exclusive_max n ->
               { sch with exclusive_maximum = Some (Int64.to_float n) }
             | Multiple_of n ->
               { sch with multiple_of = Some (Int64.to_float n) })
          empty
          constraints
      | Float_ty ->
        let constraints = (constraints : float num_constraint list) in
        List.fold_left
          (fun sch constraint_ ->
             match constraint_ with
             | Min n -> { sch with minimum = Some n }
             | Max n -> { sch with maximum = Some n }
             | Exclusive_min n -> { sch with exclusive_minimum = Some n }
             | Exclusive_max n -> { sch with exclusive_maximum = Some n }
             | Multiple_of n -> { sch with multiple_of = Some n })
          empty
          constraints)
    | Min_items n -> { empty with min_items = Some n }
    | Max_items n -> { empty with max_items = Some n }
    | Unique_items -> { empty with unique_items = Some true }
    | Any_of ts ->
      let schemas = List.map to_json_schema ts in
      { empty with any_of = Some schemas }
    | All_of ts ->
      let has_complex =
        ts
        |> List.exists (fun c ->
          match c with Any_of _ | One_of _ | Not _ -> true | _ -> false)
      in
      if has_complex
      then { empty with all_of = Some (List.map to_json_schema ts) }
      else ts |> List.map to_json_schema_obj |> List.fold_left merge empty
    | One_of ts ->
      let schemas = List.map to_json_schema ts in
      { empty with one_of = Some schemas }
    | Not t ->
      let schema = to_json_schema t in
      { empty with not_ = Some schema }

  and to_json_schema : type a. a t -> schema =
   fun constraint_ ->
    Or_bool.Schema (Or_ref.Value (to_json_schema_obj constraint_))
end
