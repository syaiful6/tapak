open Tapak_kernel

type parameter =
  { name : string
  ; in_ : [ `Path | `Query | `Header ]
  ; description : string option
  ; required : bool
  ; schema : Yojson.Safe.t
  ; style : string option
  ; explode : bool option
  }

type request_body =
  { description : string option
  ; required : bool
  ; content : (string * Yojson.Safe.t) list
  }

type operation =
  { operation_id : string option
  ; summary : string option
  ; description : string option
  ; tags : string list
  ; parameters : parameter list
  ; request_body : request_body option
  ; responses : Yojson.Safe.t
  }

type path_item =
  { get : operation option
  ; post : operation option
  ; put : operation option
  ; patch : operation option
  ; delete : operation option
  ; head : operation option
  }

let rec path_to_string : type a b. (a, b) Router.path -> string * parameter list
  =
 fun path ->
  match path with
  | Nil -> "", []
  | Literal (lit, rest) ->
    let rest_path, params = path_to_string rest in
    (if lit = "" then rest_path else "/" ^ lit ^ rest_path), params
  | Capture { type_name; format_name; rest; _ } ->
    let rest_path, params = path_to_string rest in
    let param_name = "param" ^ string_of_int (List.length params) in
    let schema =
      match format_name with
      | Some format ->
        `Assoc [ "type", `String type_name; "format", `String format ]
      | None -> `Assoc [ "type", `String type_name ]
    in
    let param =
      { name = param_name
      ; in_ = `Path
      ; description = None
      ; required = true
      ; schema
      ; style = None
      ; explode = None
      }
    in
    Format.sprintf "/{%s}%s" param_name rest_path, param :: params
  | Enum { type_name; format_name; values; rest; format; _ } ->
    let rest_path, params = path_to_string rest in
    let param_name = "param" ^ string_of_int (List.length params) in
    let enum_values = List.map (fun v -> `String (format v)) values in
    let schema =
      match format_name with
      | Some format ->
        `Assoc
          [ "type", `String type_name
          ; "format", `String format
          ; "enum", `List enum_values
          ]
      | None -> `Assoc [ "type", `String type_name; "enum", `List enum_values ]
    in
    let param =
      { name = param_name
      ; in_ = `Path
      ; description = None
      ; required = true
      ; schema
      ; style = None
      ; explode = None
      }
    in
    Format.sprintf "/{%s}%s" param_name rest_path, param :: params
  | Annotated { segment; name; description } ->
    let seg_path, params = path_to_string segment in
    (match params with
    | param :: rest ->
      let updated_param = { param with name; description } in
      let old_pattern = "{" ^ param.name ^ "}" in
      let new_pattern = "{" ^ name ^ "}" in
      let regexp = Str.regexp_string old_pattern in
      let updated_path = Str.global_replace regexp new_pattern seg_path in
      updated_path, updated_param :: rest
    | [] -> seg_path, params)
  | Splat rest ->
    let rest_path, params = path_to_string rest in
    let param_name = "splat" in
    let param =
      { name = param_name
      ; in_ = `Path
      ; description = Some "Catch-all path segments"
      ; required = true
      ; schema = `Assoc [ "type", `String "string" ]
      ; style = None
      ; explode = None
      }
    in
    Format.sprintf "/{%s}%s" param_name rest_path, param :: params

let rec extract_metadata : type a b. (a, b) Router.schema -> Router.metadata =
 fun schema ->
  match schema with
  | Method _ ->
    { operation_id = None
    ; summary = None
    ; description = None
    ; tags = []
    ; body_description = None
    ; include_in_schema = true
    }
  | Query { rest; _ } -> extract_metadata rest
  | Body { rest; _ } -> extract_metadata rest
  | Guard { rest; _ } -> extract_metadata rest
  | Response_model { rest; _ } -> extract_metadata rest
  | Meta { meta; rest } ->
    let base_meta = extract_metadata rest in
    { operation_id =
        (match meta.operation_id with
        | Some _ as id -> id
        | None -> base_meta.operation_id)
    ; summary =
        (match meta.summary with Some _ as s -> s | None -> base_meta.summary)
    ; description =
        (match meta.description with
        | Some _ as d -> d
        | None -> base_meta.description)
    ; tags = meta.tags @ base_meta.tags
    ; body_description =
        (match meta.body_description with
        | Some _ as bd -> bd
        | None -> base_meta.body_description)
    ; include_in_schema = meta.include_in_schema && base_meta.include_in_schema
    }

let rec field_to_openapi_schema : type a. a Schema.field -> Yojson.Safe.t =
 fun field ->
  match field with
  | Str { default; constraint_ } ->
    let base = [ "type", `String "string" ] in
    let with_default =
      match default with
      | Some d -> ("default", `String d) :: base
      | None -> base
    in
    let with_constraints =
      match constraint_ with
      | Some c -> Schema.Constraint.to_json_schema c @ with_default
      | None -> with_default
    in
    `Assoc with_constraints
  | Int { default; constraint_ } ->
    let base = [ "type", `String "integer"; "format", `String "int32" ] in
    let with_default =
      match default with Some d -> ("default", `Int d) :: base | None -> base
    in
    let with_constraints =
      match constraint_ with
      | Some c -> Schema.Constraint.to_json_schema c @ with_default
      | None -> with_default
    in
    `Assoc with_constraints
  | Int32 { default; constraint_ } ->
    let base = [ "type", `String "integer"; "format", `String "int32" ] in
    let with_default =
      match default with
      | Some d -> ("default", `Int (Int32.to_int d)) :: base
      | None -> base
    in
    let with_constraints =
      match constraint_ with
      | Some c -> Schema.Constraint.to_json_schema c @ with_default
      | None -> with_default
    in
    `Assoc with_constraints
  | Int64 { default; constraint_ } ->
    let base = [ "type", `String "integer"; "format", `String "int64" ] in
    let with_default =
      match default with
      | Some d -> ("default", `Int (Int64.to_int d)) :: base
      | None -> base
    in
    let with_constraints =
      match constraint_ with
      | Some c -> Schema.Constraint.to_json_schema c @ with_default
      | None -> with_default
    in
    `Assoc with_constraints
  | Bool { default } ->
    let base = [ "type", `String "boolean" ] in
    let with_default =
      match default with Some d -> ("default", `Bool d) :: base | None -> base
    in
    `Assoc with_default
  | Float { default; constraint_ } ->
    let base = [ "type", `String "number"; "format", `String "float" ] in
    let with_default =
      match default with
      | Some d -> ("default", `Float d) :: base
      | None -> base
    in
    let with_constraints =
      match constraint_ with
      | Some c -> Schema.Constraint.to_json_schema c @ with_default
      | None -> with_default
    in
    `Assoc with_constraints
  | Option inner ->
    let inner_schema = field_to_openapi_schema inner in
    (match inner_schema with
    | `Assoc fields -> `Assoc (("nullable", `Bool true) :: fields)
    | _ -> inner_schema)
  | List { item; default; constraint_ } ->
    let item_schema = field_to_openapi_schema item in
    let base = [ "type", `String "array"; "items", item_schema ] in
    let with_default =
      match default with
      | Some [] -> ("default", `List []) :: base
      | Some _ -> base
      | None -> base
    in
    let with_constraints =
      match constraint_ with
      | Some c -> Schema.Constraint.to_json_schema c @ with_default
      | None -> with_default
    in
    `Assoc with_constraints
  | File -> `Assoc [ "type", `String "string"; "format", `String "binary" ]
  | Choice { choices; default; _ } ->
    let enum_values = List.map (fun (key, _) -> `String key) choices in
    let base = [ "type", `String "string"; "enum", `List enum_values ] in
    let with_default = match default with Some _ -> base | None -> base in
    `Assoc with_default
  | Object schema -> schema_to_openapi_schema schema

and schema_to_openapi_schema : type a. a Schema.t -> Yojson.Safe.t =
 fun schema ->
  match schema with
  | Pure _ -> `Assoc [ "type", `String "object" ]
  | Field { field; name } ->
    let field_schema = field_to_openapi_schema field in
    `Assoc
      [ "type", `String "object"; "properties", `Assoc [ name, field_schema ] ]
  | App (left, right) ->
    let left_schema = schema_to_openapi_schema left in
    let right_schema = schema_to_openapi_schema right in
    (match left_schema, right_schema with
    | `Assoc left_fields, `Assoc right_fields ->
      let left_props =
        List.assoc_opt "properties" left_fields
        |> Option.value ~default:(`Assoc [])
      in
      let right_props =
        List.assoc_opt "properties" right_fields
        |> Option.value ~default:(`Assoc [])
      in
      (match left_props, right_props with
      | `Assoc lp, `Assoc rp ->
        `Assoc [ "type", `String "object"; "properties", `Assoc (lp @ rp) ]
      | _ -> left_schema)
    | _ -> left_schema)
  | Map { tree; _ } -> schema_to_openapi_schema tree

let rec schema_to_query_parameters : type a. a Schema.t -> parameter list =
 fun schema ->
  match schema with
  | Pure _ -> []
  | Field { field; name } ->
    let is_required, param_schema =
      match field with
      | Option inner -> false, field_to_openapi_schema inner
      | List { default; _ } ->
        ( (match default with Some _ -> false | None -> true)
        , field_to_openapi_schema field )
      | _ ->
        let has_default =
          match field with
          | Str { default = Some _; _ }
          | Int { default = Some _; _ }
          | Int32 { default = Some _; _ }
          | Int64 { default = Some _; _ }
          | Bool { default = Some _ }
          | Float { default = Some _; _ }
          | Choice { default = Some _; _ } ->
            true
          | _ -> false
        in
        not has_default, field_to_openapi_schema field
    in
    let style, explode =
      match field with List _ -> Some "form", Some true | _ -> None, None
    in
    [ { name
      ; in_ = `Query
      ; description = None
      ; required = is_required
      ; schema = param_schema
      ; style
      ; explode
      }
    ]
  | App (left, right) ->
    schema_to_query_parameters left @ schema_to_query_parameters right
  | Map { tree; _ } -> schema_to_query_parameters tree

let rec extract_query_parameters : type a b.
  (a, b) Router.schema -> parameter list
  =
 fun schema ->
  match schema with
  | Method _ -> []
  | Query { schema = query_schema; rest } ->
    schema_to_query_parameters query_schema @ extract_query_parameters rest
  | Body { rest; _ } -> extract_query_parameters rest
  | Guard { rest; _ } -> extract_query_parameters rest
  | Response_model { rest; _ } -> extract_query_parameters rest
  | Meta { rest; _ } -> extract_query_parameters rest

let rec extract_request_body : type a b.
  (a, b) Router.schema -> request_body option
  =
 fun schema ->
  match schema with
  | Method _ -> None
  | Query { rest; _ } -> extract_request_body rest
  | Body { input_type; schema; _ } ->
    let content_type =
      match input_type with
      | Json -> "application/json"
      | Urlencoded -> "application/x-www-form-urlencoded"
      | Multipart -> "multipart/form-data"
    in
    let body_schema = schema_to_openapi_schema schema in
    Some
      { description = None
      ; required = true
      ; content = [ content_type, body_schema ]
      }
  | Guard { rest; _ } -> extract_request_body rest
  | Response_model { rest; _ } -> extract_request_body rest
  | Meta { rest; _ } -> extract_request_body rest

let schema_to_operation : type a b. (a, b) Router.schema -> operation =
 fun schema ->
  let metadata = extract_metadata schema in
  let rec get_path : type a b. (a, b) Router.schema -> string * parameter list =
   fun s ->
    match s with
    | Method (_, path) -> path_to_string path
    | Query { rest; _ } -> get_path rest
    | Body { rest; _ } -> get_path rest
    | Guard { rest; _ } -> get_path rest
    | Response_model { rest; _ } -> get_path rest
    | Meta { rest; _ } -> get_path rest
  in
  let _, path_parameters = get_path schema in
  let query_parameters = extract_query_parameters schema in
  let all_parameters = path_parameters @ query_parameters in
  let request_body =
    match extract_request_body schema with
    | Some body -> Some { body with description = metadata.body_description }
    | None -> None
  in
  { operation_id = metadata.operation_id
  ; summary = metadata.summary
  ; description = metadata.description
  ; tags = metadata.tags
  ; parameters = all_parameters
  ; request_body
  ; responses =
      `Assoc
        [ ( "200"
          , `Assoc
              [ "description", `String "Successful response"
              ; ( "content"
                , `Assoc
                    [ ( "application/json"
                      , `Assoc [ "schema", `Assoc [ "type", `String "object" ] ]
                      )
                    ] )
              ] )
        ]
  }

let parameter_to_json param =
  let in_str =
    match param.in_ with
    | `Path -> "path"
    | `Query -> "query"
    | `Header -> "header"
  in
  let base =
    [ "name", `String param.name
    ; "in", `String in_str
    ; "required", `Bool param.required
    ; "schema", param.schema
    ]
  in
  let with_description =
    match param.description with
    | Some desc -> ("description", `String desc) :: base
    | None -> base
  in
  let with_style =
    match param.style with
    | Some style -> ("style", `String style) :: with_description
    | None -> with_description
  in
  let with_explode =
    match param.explode with
    | Some explode -> ("explode", `Bool explode) :: with_style
    | None -> with_style
  in
  `Assoc with_explode

let request_body_to_json body =
  let content =
    `Assoc
      (List.map
         (fun (ct, schema) -> ct, `Assoc [ "schema", schema ])
         body.content)
  in
  let base = [ "required", `Bool body.required; "content", content ] in
  let with_description =
    match body.description with
    | Some desc -> ("description", `String desc) :: base
    | None -> base
  in
  `Assoc with_description

let operation_to_json op =
  let base = [ "responses", op.responses ] in
  let with_operation_id =
    match op.operation_id with
    | Some id -> ("operationId", `String id) :: base
    | None -> base
  in
  let with_summary =
    match op.summary with
    | Some s -> ("summary", `String s) :: with_operation_id
    | None -> with_operation_id
  in
  let with_description =
    match op.description with
    | Some d -> ("description", `String d) :: with_summary
    | None -> with_summary
  in
  let with_tags =
    if op.tags = []
    then with_description
    else
      ("tags", `List (List.map (fun t -> `String t) op.tags))
      :: with_description
  in
  let with_parameters =
    if op.parameters = []
    then with_tags
    else
      ("parameters", `List (List.map parameter_to_json op.parameters))
      :: with_tags
  in
  let with_request_body =
    match op.request_body with
    | Some body -> ("requestBody", request_body_to_json body) :: with_parameters
    | None -> with_parameters
  in
  `Assoc with_request_body

let path_item_to_json (item : path_item) : Yojson.Safe.t option =
  let fields = [] in
  let with_get =
    match item.get with
    | Some op -> ("get", operation_to_json op) :: fields
    | None -> fields
  in
  let with_post =
    match item.post with
    | Some op -> ("post", operation_to_json op) :: with_get
    | None -> with_get
  in
  let with_put =
    match item.put with
    | Some op -> ("put", operation_to_json op) :: with_post
    | None -> with_post
  in
  let with_patch =
    match item.patch with
    | Some op -> ("patch", operation_to_json op) :: with_put
    | None -> with_put
  in
  let with_delete =
    match item.delete with
    | Some op -> ("delete", operation_to_json op) :: with_patch
    | None -> with_patch
  in
  let with_head =
    match item.head with
    | Some op -> ("head", operation_to_json op) :: with_delete
    | None -> with_delete
  in
  if with_head = [] then None else Some (`Assoc with_head)

let rec extract_paths ?(prefix = "") (routes : Router.route list) :
  (string * path_item) list
  =
  let merge_operations path_map path (method_key : Piaf.Method.t) operation =
    let existing =
      List.assoc_opt path path_map
      |> Option.value
           ~default:
             { get = None
             ; post = None
             ; put = None
             ; patch = None
             ; delete = None
             ; head = None
             }
    in
    let updated =
      match method_key with
      | `GET -> { existing with get = Some operation }
      | `POST -> { existing with post = Some operation }
      | `PUT -> { existing with put = Some operation }
      | `DELETE -> { existing with delete = Some operation }
      | `HEAD -> { existing with head = Some operation }
      | `Other "PATCH" -> { existing with patch = Some operation }
      | _ -> existing
    in
    (path, updated) :: List.filter (fun (p, _) -> p <> path) path_map
  in
  let process_route acc = function
    | Router.Route { schema; _ } ->
      let metadata = extract_metadata schema in
      if not metadata.include_in_schema
      then acc
      else
        let operation = schema_to_operation schema in
        let rec get_path_and_method : type a b.
          (a, b) Router.schema -> string * Piaf.Method.t
          =
         fun s ->
          match s with
          | Method (meth, path) ->
            let path_str, _ = path_to_string path in
            prefix ^ path_str, meth
          | Query { rest; _ } -> get_path_and_method rest
          | Body { rest; _ } -> get_path_and_method rest
          | Guard { rest; _ } -> get_path_and_method rest
          | Response_model { rest; _ } -> get_path_and_method rest
          | Meta { rest; _ } -> get_path_and_method rest
        in
        let path, method_ = get_path_and_method schema in
        merge_operations acc path method_ operation
    | Router.Scope { prefix = scope_prefix; routes; _ } ->
      let scope_path_str, _ = path_to_string scope_prefix in
      let new_prefix = prefix ^ scope_path_str in
      let nested_paths = extract_paths ~prefix:new_prefix routes in
      List.fold_left
        (fun acc (path, item) ->
           match List.assoc_opt path acc with
           | None -> (path, item) :: acc
           | Some existing ->
             let merged =
               { get =
                   (match item.get with
                   | Some _ as g -> g
                   | None -> existing.get)
               ; post =
                   (match item.post with
                   | Some _ as p -> p
                   | None -> existing.post)
               ; put =
                   (match item.put with
                   | Some _ as p -> p
                   | None -> existing.put)
               ; patch =
                   (match item.patch with
                   | Some _ as p -> p
                   | None -> existing.patch)
               ; delete =
                   (match item.delete with
                   | Some _ as d -> d
                   | None -> existing.delete)
               ; head =
                   (match item.head with
                   | Some _ as h -> h
                   | None -> existing.head)
               }
             in
             (path, merged) :: List.filter (fun (p, _) -> p <> path) acc)
        acc
        nested_paths
  in
  List.fold_left process_route [] routes

let generate
      ?(title = "API Documentation")
      ?(version = "1.0.0")
      ?(description = "")
      ?(base_path = "")
      (routes : Router.route list) : Yojson.Safe.t
  =
  let paths = extract_paths ~prefix:base_path routes in
  let paths_json =
    List.filter_map
      (fun (path, item) ->
         match path_item_to_json item with
         | Some json -> Some (path, json)
         | None -> None)
      paths
  in
  let info =
    `Assoc
      [ "title", `String title
      ; "version", `String version
      ; "description", `String description
      ]
  in
  `Assoc
    [ "openapi", `String "3.0.0"; "info", info; "paths", `Assoc paths_json ]

let to_string ?title ?version ?description ?base_path routes =
  generate ?title ?version ?description ?base_path routes
  |> Yojson.Safe.pretty_to_string
