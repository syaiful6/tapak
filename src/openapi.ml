open Imports
module Spec = Openapi_spec
module Json_schema = Sch.Json_schema

let wrap_schema t = Json_schema.(Or_bool.Schema (Or_ref.Value t))

module Diflist = struct
  type 'a t = 'a list -> 'a list

  let[@inline] single : 'a -> 'a t = fun x -> fun xs -> x :: xs
  let empty : 'a t = fun xs -> xs
  let[@inline] concat : 'a t -> 'a t -> 'a t = fun a b -> fun xs -> a (b xs)
  let[@inline] to_list : 'a t -> 'a list = fun dl -> dl []
end

let rec path_to_string : type a b.
  (a, b) Router.path -> string * Spec.Parameter.t list
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
      Spec.Json_schema.of_base
        { Json_schema.empty with
          type_ =
            Option.value
              (Json_schema.Json_type.type_of_string type_name)
              ~default:Json_schema.Json_type.String
            |> Json_schema.Json_type.of_value
        ; format = format_name
        }
    in
    let param =
      Spec.Parameter.
        { name = param_name
        ; in_ = Spec.Location.Path
        ; description = None
        ; required = true
        ; schema = Some (wrap_schema schema)
        ; style = None
        ; explode = None
        ; allow_reserved = false
        ; allow_empty_value = false
        ; deprecated = false
        ; example = None
        ; content = []
        }
    in
    Printf.sprintf "/{%s}%s" param_name rest_path, param :: params
  | Enum { type_name; format_name; values; rest; format; _ } ->
    let rest_path, params = path_to_string rest in
    let param_name = "param" ^ string_of_int (List.length params) in
    let enum_values = List.map (fun v -> Jsont.Json.string (format v)) values in
    let schema =
      Spec.Json_schema.of_base
        { Json_schema.empty with
          type_ =
            Option.value
              (Json_schema.Json_type.type_of_string type_name)
              ~default:Json_schema.Json_type.String
            |> Json_schema.Json_type.of_value
        ; format = format_name
        ; enum = Some enum_values
        }
    in
    let param =
      Spec.Parameter.
        { name = param_name
        ; in_ = Spec.Location.Path
        ; description = None
        ; required = true
        ; schema = Some (wrap_schema schema)
        ; style = None
        ; explode = None
        ; allow_reserved = false
        ; allow_empty_value = false
        ; deprecated = false
        ; example = None
        ; content = []
        }
    in
    Printf.sprintf "/{%s}%s" param_name rest_path, param :: params
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
    let schema =
      Spec.Json_schema.of_base
        { Json_schema.empty with type_ = Json_schema.Json_type.string }
    in
    let param =
      Spec.Parameter.
        { name = param_name
        ; in_ = Spec.Location.Path
        ; description = Some "Catch-all path segments"
        ; required = true
        ; schema = Some (wrap_schema schema)
        ; style = None
        ; explode = None
        ; allow_reserved = false
        ; allow_empty_value = false
        ; deprecated = false
        ; example = None
        ; content = []
        }
    in
    Format.sprintf "/{%s}%s" param_name rest_path, param :: params

let rec extract_metadata : type a b. (a, b) Router.schema -> Router.metadata =
 fun schema ->
  let open Option.Syntax in
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
  | Header { rest; _ } -> extract_metadata rest
  | Cookie { rest; _ } -> extract_metadata rest
  | Body { rest; _ } -> extract_metadata rest
  | Extract { rest; _ } -> extract_metadata rest
  | Response_model { rest; _ } -> extract_metadata rest
  | Meta { meta; rest } ->
    let base_meta = extract_metadata rest in
    { operation_id = meta.operation_id <|> base_meta.operation_id
    ; summary = meta.summary <|> base_meta.summary
    ; description = meta.description <|> base_meta.description
    ; tags = meta.tags @ base_meta.tags
    ; body_description = meta.body_description <|> base_meta.body_description
    ; include_in_schema = meta.include_in_schema && base_meta.include_in_schema
    }

let schema_to_openapi_schema : type a. a Sch.t -> Spec.Json_schema.t =
 fun schema ->
  let t = Sch.to_json_schema schema in
  Spec.Json_schema.of_base t

module To_parameter = struct
  type fieldc = Spec.Parameter.t Diflist.t

  module Fc = Sch.Sig.Make (struct
      type 'a t = fieldc
    end)

  let fc_applicative : Fc.t Sch.Sig.applicative =
    { pure = (fun _ -> Fc.inj Diflist.empty)
    ; map = (fun _f va -> Fc.inj (Fc.prj va))
    ; apply = (fun vf va -> Fc.inj (Diflist.concat (Fc.prj va) (Fc.prj vf)))
    }

  let rec convert : type a. Spec.Location.t -> a Sch.t -> Spec.Parameter.t list =
   fun loc codec ->
    match codec with
    | Sch.Object { members; _ } ->
      let result =
        Fc.prj @@ Sch.Free.run fc_applicative (field_nat loc) members
      in
      Diflist.to_list result
    | Rec t -> convert loc (Lazy.force t)
    | Iso { repr; _ } -> convert loc repr
    | Union { discriminator; cases; _ } ->
      let enums =
        List.map (fun (Sch.Case c) -> Jsont.Json.string c.tag) cases
      in
      let dp =
        Spec.Parameter.
          { name = discriminator
          ; in_ = loc
          ; schema =
              Some
                (wrap_schema
                   (Spec.Json_schema.of_base
                      { Json_schema.empty with
                        type_ = Json_schema.Json_type.string
                      ; enum = Some enums
                      }))
          ; description = None
          ; required = true
          ; deprecated = false
          ; allow_empty_value = false
          ; allow_reserved = false
          ; explode = None
          ; style = None
          ; example = None
          ; content = []
          }
      in
      let case_params =
        List.concat_map
          (fun (Sch.Case c) ->
             let params = convert loc c.codec in
             List.map
               (fun p -> { p with Spec.Parameter.required = false })
               params)
          cases
      in
      let seen = Hashtbl.create 8 in
      let unique_case_params =
        List.filter
          (fun (p : Spec.Parameter.t) ->
             if Hashtbl.mem seen p.name
             then false
             else (
               Hashtbl.add seen p.name ();
               true))
          case_params
      in
      dp :: unique_case_params
    | _ -> []

  and field_nat : type o. Spec.Location.t -> (o Sch.fieldk, Fc.t) Sch.Sig.nat =
   fun location ->
    { Sch.Sig.run =
        (fun (type b) (fa : (b, o Sch.fieldk) Sch.Sig.app) ->
          let field = Sch.Object.prj fa in
          let schema = schema_to_openapi_schema field.codec in
          let required = Option.is_none field.default in
          let style, explode =
            match field.codec with
            | Sch.List _ -> Some Spec.Style.Form, Some true
            | _ -> None, None
          in
          let parameter =
            Spec.Parameter.
              { name = field.name
              ; in_ = location
              ; description =
                  (if String.equal field.doc "" then None else Some field.doc)
              ; schema = Some (wrap_schema schema)
              ; required
              ; deprecated = false
              ; allow_empty_value = false
              ; allow_reserved = false
              ; explode
              ; style
              ; example = None
              ; content = []
              }
          in
          Fc.inj (Diflist.single parameter))
    }
end

type any_schema = Schema : ('a, Response.t) Router.schema -> any_schema

let rec get_method_and_path : type a b.
  (a, b) Router.schema -> Http.Method.t * string * Spec.Parameter.t list
  =
 fun schema ->
  match schema with
  | Router.Method (meth, path) ->
    let path_str, path_params = path_to_string path in
    meth, path_str, path_params
  | Router.Query { rest; _ } -> get_method_and_path rest
  | Router.Header { rest; _ } -> get_method_and_path rest
  | Router.Cookie { rest; _ } -> get_method_and_path rest
  | Router.Body { rest; _ } -> get_method_and_path rest
  | Router.Extract { rest; _ } -> get_method_and_path rest
  | Router.Meta { rest; _ } -> get_method_and_path rest
  | Router.Response_model { rest; _ } -> get_method_and_path rest

type operation_parts =
  { parameters : Spec.Parameter.t list
  ; body : (Router.media_type * Spec.Json_schema.t) option
  ; body_required : bool
  ; response : (Http.Status.t * Spec.Json_schema.t) option
  }

let rec extract_parts : type a b. (a, b) Router.schema -> operation_parts =
 fun schema ->
  match schema with
  | Router.Method _ ->
    { parameters = []; body = None; body_required = true; response = None }
  | Router.Query { schema; rest } ->
    let parts = extract_parts rest in
    let params = To_parameter.convert Spec.Location.Query schema in
    { parts with parameters = parts.parameters @ params }
  | Router.Header { schema; rest } ->
    let parts = extract_parts rest in
    let params = To_parameter.convert Spec.Location.Header schema in
    { parts with parameters = parts.parameters @ params }
  | Router.Cookie { schema; rest } ->
    let parts = extract_parts rest in
    let params = To_parameter.convert Spec.Location.Cookie schema in
    { parts with parameters = parts.parameters @ params }
  | Router.Body { input_type; schema; rest } ->
    let parts = extract_parts rest in
    let json_schema = schema_to_openapi_schema schema in
    { parts with body = Some (input_type, json_schema) }
  | Router.Response_model { schema; status; rest } ->
    let parts = extract_parts rest in
    let json_schema = schema_to_openapi_schema schema in
    { parts with response = Some (status, json_schema) }
  | Router.Extract { rest; _ } -> extract_parts rest
  | Router.Meta { rest; _ } -> extract_parts rest

let content_type_of_media_type = function
  | Router.Json -> "application/json"
  | Router.Urlencoded -> "application/x-www-form-urlencoded"
  | Router.Multipart -> "multipart/form-data"

let make_media_type json_schema =
  Spec.Media_type.
    { schema = Some (wrap_schema json_schema)
    ; example = None
    ; examples = []
    ; encoding = []
    }

let make_request_body description input_type json_schema required =
  Spec.Request_body.
    { description
    ; content =
        [ content_type_of_media_type input_type, make_media_type json_schema ]
    ; required = Some required
    }

let make_responses status json_schema =
  let status_str = string_of_int (Http.Status.to_int status) in
  let response =
    Spec.Response.
      { description = ""
      ; headers = []
      ; content = [ "application/json", make_media_type json_schema ]
      ; links = []
      }
  in
  Spec.Responses.
    { default = None
    ; responses = [ status_str, Spec.Json_schema.Or_ref.Value response ]
    }

let default_responses =
  Spec.Responses.
    { default = None
    ; responses =
        [ ( "200"
          , Spec.Json_schema.Or_ref.Value
              Spec.Response.
                { description = "OK"; headers = []; content = []; links = [] } )
        ]
    }

let build_operation all_param_refs parts metadata =
  let request_body =
    Option.map
      (fun (input_type, json_schema) ->
         Spec.Json_schema.Or_ref.Value
           (make_request_body
              metadata.Router.body_description
              input_type
              json_schema
              parts.body_required))
      parts.body
  in
  let responses =
    match parts.response with
    | Some (status, json_schema) -> make_responses status json_schema
    | None -> default_responses
  in
  Spec.Operation.
    { tags = (if metadata.Router.tags = [] then None else Some metadata.tags)
    ; summary = metadata.summary
    ; description = metadata.description
    ; external_docs = None
    ; operation_id = metadata.operation_id
    ; parameters = all_param_refs
    ; request_body
    ; responses
    ; callbacks = []
    ; deprecated = false
    ; security = None
    ; servers = []
    }

let empty_path_item =
  Spec.Path_item.
    { ref = None
    ; summary = None
    ; description = None
    ; get = None
    ; put = None
    ; post = None
    ; delete = None
    ; options = None
    ; head = None
    ; patch = None
    ; trace = None
    ; servers = []
    ; parameters = []
    }

let add_operation meth operation item =
  let open Spec.Path_item in
  match meth with
  | `GET -> { item with get = Some operation }
  | `POST -> { item with post = Some operation }
  | `PUT -> { item with put = Some operation }
  | `DELETE -> { item with delete = Some operation }
  | `HEAD -> { item with head = Some operation }
  | `OPTIONS -> { item with options = Some operation }
  | `TRACE -> { item with trace = Some operation }
  | `PATCH -> { item with patch = Some operation }
  | `Other "*" ->
    { item with
      get = Some operation
    ; post = Some operation
    ; put = Some operation
    ; delete = Some operation
    ; patch = Some operation
    }
  | `CONNECT | `Other _ -> item

let rec flatten prefix prefix_params routes =
  List.concat_map
    (fun route ->
       match route with
       | Router.Route { schema; _ } -> [ prefix, prefix_params, Schema schema ]
       | Router.Scope { prefix = scope_prefix; routes = children; _ } ->
         let scope_path, scope_params = path_to_string scope_prefix in
         flatten (prefix ^ scope_path) (prefix_params @ scope_params) children)
    routes

let assemble ~title ~version ~description operations =
  let path_map : (string, Spec.Path_item.t) Hashtbl.t = Hashtbl.create 16 in
  List.iter
    (fun (full_path, meth, operation) ->
       let full_path = if String.equal full_path "" then "/" else full_path in
       let item =
         Option.value
           ~default:empty_path_item
           (Hashtbl.find_opt path_map full_path)
       in
       Hashtbl.replace path_map full_path (add_operation meth operation item))
    operations;
  let paths =
    Hashtbl.fold
      (fun path item acc -> (path, Spec.Json_schema.Or_ref.Value item) :: acc)
      path_map
      []
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  { Spec.openapi = "3.1.0"
  ; info =
      Spec.Info.
        { title
        ; description
        ; terms_of_service = None
        ; contact = None
        ; license = None
        ; version
        }
  ; servers = []
  ; paths
  ; webhooks = []
  ; components = None
  ; security = []
  ; tags = []
  ; external_docs = None
  }

let generate
      ?(title = "API")
      ?(version = "0.0.0")
      ?description
      ?(base_path = "")
      routes
  =
  flatten base_path [] routes
  |> List.filter_map (fun (prefix, prefix_params, Schema schema) ->
    let metadata = extract_metadata schema in
    if metadata.include_in_schema
    then begin
      let meth, path_str, path_params = get_method_and_path schema in
      let parts = extract_parts schema in
      let all_param_refs =
        List.map
          (fun p -> Spec.Json_schema.Or_ref.Value p)
          (prefix_params @ path_params @ parts.parameters)
      in
      Some
        (prefix ^ path_str, meth, build_operation all_param_refs parts metadata)
    end
    else None)
  |> assemble ~title ~version ~description

let generate_string ?title ?version ?description ?base_path routes =
  let spec = generate ?title ?version ?description ?base_path routes in
  Spec.to_string spec |> Result.get_ok

module Of_router2 = struct
  type parts =
    { parameters : Spec.Parameter.t Diflist.t
    ; body : (Router.media_type * Spec.Json_schema.t) option
    ; body_optional : bool
    ; never : bool
    }

  module Pc = Sch.Sig.Make (struct
      type 'a t = parts
    end)

  let empty =
    { parameters = Diflist.empty
    ; body = None
    ; body_optional = false
    ; never = false
    }

  (* A body can only be consumed once, so the first one declared is the one that
     gets the data. *)
  let combine first second =
    let with_body = match first.body with Some _ -> first | None -> second in
    { parameters = Diflist.concat first.parameters second.parameters
    ; body = with_body.body
    ; body_optional = with_body.body_optional
    ; never = first.never || second.never
    }

  let pc_applicative : Pc.t Sch.Sig.applicative =
    { pure = (fun _ -> Pc.inj empty)
    ; map = (fun _f va -> Pc.inj (Pc.prj va))
    ; apply = (fun vf va -> Pc.inj (combine (Pc.prj va) (Pc.prj vf)))
    }

  let of_parameters params =
    { empty with
      parameters =
        List.fold_right
          (fun p acc -> Diflist.concat (Diflist.single p) acc)
          params
          Diflist.empty
    }

  (* Gates and the block can read the same parameter, but OpenAPI forbids
     repeating a name and location in one operation. The request fails if any
     reader requires the parameter, so required wins. *)
  let same_parameter (a : Spec.Parameter.t) (b : Spec.Parameter.t) =
    a.in_ = b.in_
    &&
    match a.in_ with
    | Spec.Location.Header -> Sch_ext.CI.equal a.name b.name
    | _ -> String.equal a.name b.name

  let merge_duplicates params =
    List.fold_left
      (fun acc (p : Spec.Parameter.t) ->
         if List.exists (same_parameter p) acc
         then
           List.map
             (fun (q : Spec.Parameter.t) ->
                if same_parameter p q
                then { q with required = q.required || p.required }
                else q)
             acc
         else acc @ [ p ])
      []
      params

  (* A parameter is only required by an alternative if every branch requires it.
     A branch that can't succeed tells callers nothing, so it is ignored. [<|>]
     allows a body in one branch only, so the other branch succeeds without
     it. *)
  let join left right =
    if left.never
    then right
    else if right.never
    then left
    else
      let lefts = merge_duplicates (Diflist.to_list left.parameters) in
      let rights = merge_duplicates (Diflist.to_list right.parameters) in
      let weaken others (p : Spec.Parameter.t) =
        let required_by_others =
          List.exists
            (fun (q : Spec.Parameter.t) -> same_parameter p q && q.required)
            others
        in
        { p with required = p.required && required_by_others }
      in
      let only_right =
        List.filter (fun q -> not (List.exists (same_parameter q) lefts)) rights
      in
      { (of_parameters
           (List.map (weaken rights) lefts @ List.map (weaken lefts) only_right))
        with
        body = (match left.body with Some _ -> left.body | None -> right.body)
      ; body_optional = true
      }

  (* Nothing here touches a request: [Extract] contributes nothing, and [Refine]
     only contributes what its source declares. *)
  let rec collect : type a. a Router2.schemaf -> parts =
   fun schema -> Pc.prj (Sch.Free.run pc_applicative (effect_nat ()) schema)

  and effect_nat : unit -> (Router2.Schema.schemak, Pc.t) Sch.Sig.nat =
   fun () ->
    { Sch.Sig.run =
        (fun (type b) (fa : (b, Router2.Schema.schemak) Sch.Sig.app) ->
          let effect_ : b Router2.Schema.t = Router2.Schema.prj fa in
          let parts : parts = collect_effect effect_ in
          (Pc.inj parts : (b, Pc.t) Sch.Sig.app))
    }

  and collect_effect : type a. a Router2.Schema.t -> parts = function
    | Query schema ->
      of_parameters (To_parameter.convert Spec.Location.Query schema)
    | Header schema ->
      of_parameters (To_parameter.convert Spec.Location.Header schema)
    | Cookie schema ->
      of_parameters (To_parameter.convert Spec.Location.Cookie schema)
    | Body { media_type; schema } ->
      { empty with body = Some (media_type, schema_to_openapi_schema schema) }
    | Extract _ -> empty
    | Refine { source; _ } -> collect source
    | Memo { source; _ } -> collect source
    | Fail _ -> { empty with never = true }
    | Alt { left; right } -> join (collect left) (collect right)

  let rec flatten prefix prefix_params gates routes =
    List.concat_map
      (fun route ->
         match route with
         | Router2.Route { endpoint = Endpoint e; schema; _ } ->
           if e.meta.include_in_schema
           then begin
             let path_str, path_params = path_to_string e.path in
             let parts =
               collect (Router2.with_gates (gates @ e.gates) schema)
             in
             let parameters =
               merge_duplicates
                 (prefix_params @ path_params @ Diflist.to_list parts.parameters)
             in
             let response =
               Option.map
                 (fun (Router2.Response_doc { status; schema }) ->
                    status, schema_to_openapi_schema schema)
                 e.response
             in
             [ ( prefix ^ path_str
               , e.method_
               , build_operation
                   (List.map
                      (fun p -> Spec.Json_schema.Or_ref.Value p)
                      parameters)
                   { parameters
                   ; body = parts.body
                   ; body_required = not parts.body_optional
                   ; response
                   }
                   e.meta )
             ]
           end
           else []
         | Router2.Scope
             { prefix = scope_prefix
             ; routes = children
             ; gates = scope_gates
             ; _
             } ->
           let scope_path, scope_params = path_to_string scope_prefix in
           flatten
             (prefix ^ scope_path)
             (prefix_params @ scope_params)
             (gates @ scope_gates)
             children)
      routes
end

let generate_router2
      ?(title = "API")
      ?(version = "0.0.0")
      ?description
      ?(base_path = "")
      routes
  =
  Of_router2.flatten base_path [] [] routes
  |> assemble ~title ~version ~description

let generate_router2_string ?title ?version ?description ?base_path routes =
  let spec = generate_router2 ?title ?version ?description ?base_path routes in
  Spec.to_string spec |> Result.get_ok
