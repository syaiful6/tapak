module String_map = struct
  include Map.Make (String)

  let jsont : type a. a Jsont.t -> (string * a) list Jsont.t =
   fun codec ->
    let map_jsont = Jsont.Object.as_string_map codec in
    Jsont.map
      ~kind:"string_map"
      ~dec:bindings
      ~enc:(List.fold_left (fun m (k, v) -> add k v m) empty)
      map_jsont
end

module Contact = struct
  type t =
    { name : string option
    ; url : string option
    ; email : string option
    }

  let jsont =
    Jsont.Object.map ~kind:"Contact" (fun name url email ->
      { name; url; email })
    |> Jsont.Object.opt_mem ~enc:(fun c -> c.name) "name" Jsont.string
    |> Jsont.Object.opt_mem ~enc:(fun c -> c.url) "url" Jsont.string
    |> Jsont.Object.opt_mem ~enc:(fun c -> c.email) "email" Jsont.string
    |> Jsont.Object.finish
end

module License = struct
  type t =
    { name : string
    ; url : string option
    }

  let jsont =
    Jsont.Object.map ~kind:"License" (fun name url -> { name; url })
    |> Jsont.Object.mem ~enc:(fun l -> l.name) "name" Jsont.string
    |> Jsont.Object.opt_mem ~enc:(fun l -> l.url) "url" Jsont.string
    |> Jsont.Object.finish
end

module Info = struct
  type t =
    { title : string
    ; description : string option
    ; terms_of_service : string option
    ; contact : Contact.t option
    ; license : License.t option
    ; version : string
    }

  let jsont =
    Jsont.Object.map
      ~kind:"Info"
      (fun title description terms_of_service contact license version ->
         { title; description; terms_of_service; contact; license; version })
    |> Jsont.Object.mem ~enc:(fun i -> i.title) "title" Jsont.string
    |> Jsont.Object.opt_mem
         ~enc:(fun i -> i.description)
         "description"
         Jsont.string
    |> Jsont.Object.opt_mem
         ~enc:(fun i -> i.terms_of_service)
         "termsOfService"
         Jsont.string
    |> Jsont.Object.opt_mem ~enc:(fun i -> i.contact) "contact" Contact.jsont
    |> Jsont.Object.opt_mem ~enc:(fun i -> i.license) "license" License.jsont
    |> Jsont.Object.mem ~enc:(fun i -> i.version) "version" Jsont.string
    |> Jsont.Object.finish
end

module Server = struct
  module Variable = struct
    type t =
      { enum : string list option
      ; default : string
      ; description : string option
      }

    let jsont =
      Jsont.Object.map ~kind:"Server.Variable" (fun enum default description ->
        { enum; default; description })
      |> Jsont.Object.opt_mem
           ~enc:(fun v -> v.enum)
           "enum"
           (Jsont.list Jsont.string)
      |> Jsont.Object.mem ~enc:(fun v -> v.default) "default" Jsont.string
      |> Jsont.Object.opt_mem
           ~enc:(fun v -> v.description)
           "description"
           Jsont.string
      |> Jsont.Object.finish
  end

  type t =
    { url : string
    ; description : string option
    ; variables : (string * Variable.t) list
    }

  let jsont =
    Jsont.Object.map ~kind:"Server" (fun url description variables ->
      { url; description; variables })
    |> Jsont.Object.mem ~enc:(fun s -> s.url) "url" Jsont.string
    |> Jsont.Object.opt_mem
         ~enc:(fun s -> s.description)
         "description"
         Jsont.string
    |> Jsont.Object.mem
         ~enc:(fun s -> s.variables)
         ~dec_absent:[]
         "variables"
         (String_map.jsont Variable.jsont)
    |> Jsont.Object.finish
end

module External_docs = struct
  type t =
    { description : string option
    ; url : string
    }

  let jsont =
    Jsont.Object.map ~kind:"ExternalDocs" (fun description url ->
      { description; url })
    |> Jsont.Object.opt_mem
         ~enc:(fun d -> d.description)
         "description"
         Jsont.string
    |> Jsont.Object.mem ~enc:(fun d -> d.url) "url" Jsont.string
    |> Jsont.Object.finish
end

module Tag = struct
  type t =
    { name : string
    ; description : string option
    ; external_docs : External_docs.t option
    }

  let jsont =
    Jsont.Object.map ~kind:"Tag" (fun name description external_docs ->
      { name; description; external_docs })
    |> Jsont.Object.mem ~enc:(fun t -> t.name) "name" Jsont.string
    |> Jsont.Object.opt_mem
         ~enc:(fun t -> t.description)
         "description"
         Jsont.string
    |> Jsont.Object.opt_mem
         ~enc:(fun t -> t.external_docs)
         "externalDocs"
         External_docs.jsont
    |> Jsont.Object.finish
end

module Discriminator = struct
  type t =
    { property_name : string
    ; mapping : (string * string) list
    }

  let jsont =
    Jsont.Object.map ~kind:"Discriminator" (fun property_name mapping ->
      { property_name; mapping })
    |> Jsont.Object.mem
         ~enc:(fun d -> d.property_name)
         "propertyName"
         Jsont.string
    |> Jsont.Object.mem
         ~enc:(fun d -> d.mapping)
         "mapping"
         (String_map.jsont Jsont.string)
    |> Jsont.Object.finish
end

module Json_schema = struct
  module Base = Sch.Json_schema
  module Json_type = Base.Json_type
  module Draft = Base.Draft
  module Or_bool = Base.Or_bool
  module Or_ref = Base.Or_ref

  module Xml = struct
    type t =
      { name : string option
      ; namespace : string option
      ; prefix : string option
      ; attribute : bool option
      ; wrapped : bool option
      }

    let jsont : t Jsont.t =
      Jsont.Object.map
        ~kind:"OpenAPI.Xml"
        (fun name namespace prefix attribute wrapped ->
           { name; namespace; prefix; attribute; wrapped })
      |> Jsont.Object.opt_mem ~enc:(fun x -> x.name) "name" Jsont.string
      |> Jsont.Object.opt_mem
           ~enc:(fun x -> x.namespace)
           "namespace"
           Jsont.string
      |> Jsont.Object.opt_mem ~enc:(fun x -> x.prefix) "prefix" Jsont.string
      |> Jsont.Object.opt_mem ~enc:(fun x -> x.attribute) "attribute" Jsont.bool
      |> Jsont.Object.opt_mem ~enc:(fun x -> x.wrapped) "wrapped" Jsont.bool
      |> Jsont.Object.finish
  end

  type t =
    { core : Base.t
    ; discriminator : Discriminator.t option
    ; xml : Xml.t option
    ; external_docs : External_docs.t option
    ; example : Jsont.json option
    ; examples : (string * Jsont.json) list option
    ; extensions : (string * Jsont.json) list
    }

  and schema = t Or_ref.t Or_bool.t

  let empty =
    { core = Base.empty
    ; discriminator = None
    ; xml = None
    ; external_docs = None
    ; example = None
    ; examples = None
    ; extensions = []
    }

  let of_base core = { empty with core }
  let to_base t = t.core

  let is_extension_key name =
    let len = String.length name in
    len >= 2 && Char.lowercase_ascii name.[0] = 'x' && name.[1] = '-'

  let split_extensions (members : Jsont.mem list) =
    List.fold_right
      (fun (((name, _) as nm), value) (exts, keep) ->
         if is_extension_key name
         then (name, value) :: exts, keep
         else exts, (nm, value) :: keep)
      members
      ([], [])

  let decode_with codec json =
    match Jsont_bytesrw.encode_string Jsont.json json with
    | Ok payload ->
      (match Jsont_bytesrw.decode_string codec payload with
      | Ok value -> value
      | Error err -> Jsont.Error.msg Jsont.Meta.none err)
    | Error err -> Jsont.Error.msg Jsont.Meta.none err

  let encode_with codec value =
    match Jsont_bytesrw.encode_string codec value with
    | Ok payload ->
      (match Jsont_bytesrw.decode_string Jsont.json payload with
      | Ok json -> json
      | Error err -> Jsont.Error.msg Jsont.Meta.none err)
    | Error err -> Jsont.Error.msg Jsont.Meta.none err

  let find_member name members =
    members
    |> List.find_map (fun ((member_name, _meta), value) ->
      if String.equal member_name name then Some value else None)

  let remove_member name members =
    List.filter
      (fun ((member_name, _), _) -> not (String.equal member_name name))
      members

  let add_member name value members =
    match value with
    | None -> members
    | Some v -> members @ [ (name, Jsont.Meta.none), v ]

  let jsont : t Jsont.t =
    Jsont.map
      Jsont.json
      ~kind:"OpenAPI.Schema"
      ~dec:(fun json ->
        match json with
        | Jsont.Object (members, meta) ->
          let extensions, base_members = split_extensions members in
          let core_json = Jsont.Object (base_members, meta) in
          let core = decode_with Base.jsont core_json in
          let discriminator =
            find_member "discriminator" base_members
            |> Option.map (decode_with Discriminator.jsont)
          in
          let xml =
            find_member "xml" base_members |> Option.map (decode_with Xml.jsont)
          in
          let external_docs =
            find_member "externalDocs" base_members
            |> Option.map (decode_with External_docs.jsont)
          in
          let example = find_member "example" base_members in
          let examples =
            find_member "examples" base_members
            |> Option.map (decode_with (String_map.jsont Jsont.json))
          in
          { core
          ; discriminator
          ; xml
          ; external_docs
          ; example
          ; examples
          ; extensions
          }
        | _ -> Jsont.Error.msg Jsont.Meta.none "Schema must be an object")
      ~enc:(fun schema ->
        match encode_with Base.jsont schema.core with
        | Jsont.Object (members, meta) ->
          let members =
            members
            |> remove_member "discriminator"
            |> remove_member "xml"
            |> remove_member "externalDocs"
            |> remove_member "example"
            |> remove_member "examples"
            |> remove_member "$schema"
            |> add_member
                 "discriminator"
                 (Option.map
                    (encode_with Discriminator.jsont)
                    schema.discriminator)
            |> add_member "xml" (Option.map (encode_with Xml.jsont) schema.xml)
            |> add_member
                 "externalDocs"
                 (Option.map
                    (encode_with External_docs.jsont)
                    schema.external_docs)
            |> add_member "example" schema.example
            |> add_member
                 "examples"
                 (Option.map
                    (fun ex ->
                       Jsont.Object
                         ( List.map (fun (k, v) -> (k, Jsont.Meta.none), v) ex
                         , Jsont.Meta.none ))
                    schema.examples)
          in
          let members =
            List.fold_left
              (fun acc (name, value) -> add_member name (Some value) acc)
              members
              schema.extensions
          in
          Jsont.Object (members, meta)
        | _ ->
          Jsont.Error.msg Jsont.Meta.none "Schema encoding must yield an object")

  let schema_jsont : schema Jsont.t = Or_bool.jsont (Or_ref.jsont jsont)
end

module Location = struct
  type t =
    | Query
    | Header
    | Path
    | Cookie

  let jsont =
    Jsont.map
      ~kind:"Parameter_location"
      ~dec:(function
        | "query" -> Query
        | "header" -> Header
        | "path" -> Path
        | "cookie" -> Cookie
        | s ->
          Jsont.Error.msgf Jsont.Meta.none "Unknown parameter location: %s" s)
      ~enc:(function
        | Query -> "query"
        | Header -> "header"
        | Path -> "path"
        | Cookie -> "cookie")
      Jsont.string
end

module Style = struct
  type t =
    | Matrix
    | Label
    | Form
    | Simple
    | Space_delimited
    | Pipe_delimited
    | Deep_object

  let jsont =
    Jsont.map
      ~kind:"Parameter_style"
      ~dec:(function
        | "matrix" -> Matrix
        | "label" -> Label
        | "form" -> Form
        | "simple" -> Simple
        | "spaceDelimited" -> Space_delimited
        | "pipeDelimited" -> Pipe_delimited
        | "deepObject" -> Deep_object
        | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown parameter style: %s" s)
      ~enc:(function
        | Matrix -> "matrix"
        | Label -> "label"
        | Form -> "form"
        | Simple -> "simple"
        | Space_delimited -> "spaceDelimited"
        | Pipe_delimited -> "pipeDelimited"
        | Deep_object -> "deepObject")
      Jsont.string
end

module Example = struct
  type t =
    { summary : string option
    ; description : string option
    ; value : Jsont.json option
    ; external_value : string option
    }

  let jsont =
    Jsont.Object.map
      ~kind:"Example"
      (fun summary description value external_value ->
         { summary; description; value; external_value })
    |> Jsont.Object.opt_mem ~enc:(fun e -> e.summary) "summary" Jsont.string
    |> Jsont.Object.opt_mem
         ~enc:(fun e -> e.description)
         "description"
         Jsont.string
    |> Jsont.Object.opt_mem ~enc:(fun e -> e.value) "value" Jsont.json
    |> Jsont.Object.opt_mem
         ~enc:(fun e -> e.external_value)
         "externalValue"
         Jsont.string
    |> Jsont.Object.finish

  let example_or_ref = Json_schema.Or_ref.jsont jsont
end

module Header = struct
  type t =
    { description : string option
    ; required : bool option
    ; deprecated : bool option
    ; style : Style.t option
    ; explode : bool option
    ; schema : Json_schema.schema option
    }

  let jsont =
    Jsont.Object.map
      ~kind:"Header"
      (fun description required deprecated style explode schema ->
         { description; required; deprecated; style; explode; schema })
    |> Jsont.Object.opt_mem
         ~enc:(fun h -> h.description)
         "description"
         Jsont.string
    |> Jsont.Object.opt_mem ~enc:(fun h -> h.required) "required" Jsont.bool
    |> Jsont.Object.opt_mem ~enc:(fun h -> h.deprecated) "deprecated" Jsont.bool
    |> Jsont.Object.opt_mem ~enc:(fun h -> h.style) "style" Style.jsont
    |> Jsont.Object.opt_mem ~enc:(fun h -> h.explode) "explode" Jsont.bool
    |> Jsont.Object.opt_mem
         ~enc:(fun h -> h.schema)
         "schema"
         Json_schema.schema_jsont
    |> Jsont.Object.finish

  let header_or_ref = Json_schema.Or_ref.jsont jsont
end

module Encoding = struct
  type t =
    { content_type : string
    ; headers : (string * Header.t Json_schema.Or_ref.t) list
    ; style : Style.t option
    ; explode : bool option
    }

  let jsont =
    Jsont.Object.map ~kind:"Encoding" (fun content_type headers style explode ->
      { content_type; headers; style; explode })
    |> Jsont.Object.mem
         ~enc:(fun e -> e.content_type)
         "contentType"
         Jsont.string
    |> Jsont.Object.mem
         ~enc:(fun e -> e.headers)
         "headers"
         (String_map.jsont Header.header_or_ref)
    |> Jsont.Object.opt_mem ~enc:(fun e -> e.style) "style" Style.jsont
    |> Jsont.Object.opt_mem ~enc:(fun e -> e.explode) "explode" Jsont.bool
    |> Jsont.Object.finish

  let encoding_map = String_map.jsont jsont
end

module Media_type = struct
  type t =
    { schema : Json_schema.schema option
    ; example : Jsont.json option
    ; examples : (string * Example.t Json_schema.Or_ref.t) list
    ; encoding : (string * Encoding.t) list
    }

  let jsont =
    Jsont.Object.map ~kind:"MediaType" (fun schema example examples encoding ->
      { schema; example; examples; encoding })
    |> Jsont.Object.opt_mem
         ~enc:(fun m -> m.schema)
         "schema"
         Json_schema.schema_jsont
    |> Jsont.Object.opt_mem ~enc:(fun m -> m.example) "example" Jsont.json
    |> Jsont.Object.mem
         ~enc:(fun m -> m.examples)
         ~dec_absent:[]
         "examples"
         (String_map.jsont Example.example_or_ref)
    |> Jsont.Object.mem
         ~enc:(fun m -> m.encoding)
         ~dec_absent:[]
         "encoding"
         (String_map.jsont Encoding.jsont)
    |> Jsont.Object.finish

  let media_type_map = String_map.jsont jsont
end

module Parameter = struct
  type t =
    { name : string
    ; in_ : Location.t
    ; description : string option
    ; required : bool
    ; deprecated : bool
    ; allow_empty_value : bool
    ; style : Style.t option
    ; explode : bool option
    ; allow_reserved : bool
    ; schema : Json_schema.schema option
    ; example : Jsont.json option
    ; content : (string * Media_type.t) list
    }

  let jsont =
    Jsont.Object.map
      ~kind:"Parameter"
      (fun
          name
           in_
           description
           required
           deprecated
           allow_empty_value
           style
           explode
           allow_reserved
           schema
           example
           content
         ->
         { name
         ; in_
         ; description
         ; required
         ; deprecated
         ; allow_empty_value
         ; style
         ; explode
         ; allow_reserved
         ; schema
         ; example
         ; content
         })
    |> Jsont.Object.mem ~enc:(fun p -> p.name) "name" Jsont.string
    |> Jsont.Object.mem ~enc:(fun p -> p.in_) "in" Location.jsont
    |> Jsont.Object.opt_mem
         ~enc:(fun p -> p.description)
         "description"
         Jsont.string
    |> Jsont.Object.mem ~enc:(fun p -> p.required) "required" Jsont.bool
    |> Jsont.Object.mem ~enc:(fun p -> p.deprecated) "deprecated" Jsont.bool
    |> Jsont.Object.mem
         ~enc:(fun p -> p.allow_empty_value)
         "allowEmptyValue"
         Jsont.bool
    |> Jsont.Object.opt_mem ~enc:(fun p -> p.style) "style" Style.jsont
    |> Jsont.Object.opt_mem ~enc:(fun p -> p.explode) "explode" Jsont.bool
    |> Jsont.Object.mem
         ~enc:(fun p -> p.allow_reserved)
         "allowReserved"
         Jsont.bool
    |> Jsont.Object.opt_mem
         ~enc:(fun p -> p.schema)
         "schema"
         Json_schema.schema_jsont
    |> Jsont.Object.opt_mem ~enc:(fun p -> p.example) "example" Jsont.json
    |> Jsont.Object.mem
         ~enc:(fun p -> p.content)
         ~enc_omit:(fun s -> List.is_empty s)
         ~dec_absent:[]
         "content"
         (String_map.jsont Media_type.jsont)
    |> Jsont.Object.finish

  let parameter_or_ref = Json_schema.Or_ref.jsont jsont
end

module Request_body = struct
  type t =
    { description : string option
    ; content : (string * Media_type.t) list
    ; required : bool option
    }

  let jsont =
    Jsont.Object.map ~kind:"RequestBody" (fun description content required ->
      { description; content; required })
    |> Jsont.Object.opt_mem
         ~enc:(fun r -> r.description)
         "description"
         Jsont.string
    |> Jsont.Object.mem
         ~enc:(fun r -> r.content)
         ~dec_absent:[]
         "content"
         (String_map.jsont Media_type.jsont)
    |> Jsont.Object.opt_mem ~enc:(fun r -> r.required) "required" Jsont.bool
    |> Jsont.Object.finish

  let request_body_or_ref = Json_schema.Or_ref.jsont jsont
end

module Link = struct
  type t =
    { operation_ref : string option
    ; operation_id : string option
    ; parameters : (string * Json_schema.schema) list
    ; request_body : Json_schema.schema option
    ; description : string option
    ; server : Server.t option
    }

  let jsont =
    Jsont.Object.map
      ~kind:"Link"
      (fun
          operation_ref
           operation_id
           parameters
           request_body
           description
           server
         ->
         { operation_ref
         ; operation_id
         ; parameters
         ; request_body
         ; description
         ; server
         })
    |> Jsont.Object.opt_mem
         ~enc:(fun l -> l.operation_ref)
         "operationRef"
         Jsont.string
    |> Jsont.Object.opt_mem
         ~enc:(fun l -> l.operation_id)
         "operationId"
         Jsont.string
    |> Jsont.Object.mem
         ~enc:(fun l -> l.parameters)
         ~dec_absent:[]
         "parameters"
         (String_map.jsont Json_schema.schema_jsont)
    |> Jsont.Object.opt_mem
         ~enc:(fun l -> l.request_body)
         "requestBody"
         Json_schema.schema_jsont
    |> Jsont.Object.opt_mem
         ~enc:(fun l -> l.description)
         "description"
         Jsont.string
    |> Jsont.Object.opt_mem ~enc:(fun l -> l.server) "server" Server.jsont
    |> Jsont.Object.finish

  let link_or_ref = Json_schema.Or_ref.jsont jsont
end

module Response = struct
  type t =
    { description : string
    ; headers : (string * Header.t Json_schema.Or_ref.t) list
    ; content : (string * Media_type.t) list
    ; links : (string * Link.t Json_schema.Or_ref.t) list
    }

  let jsont =
    Jsont.Object.map ~kind:"Response" (fun description headers content links ->
      { description; headers; content; links })
    |> Jsont.Object.mem ~enc:(fun r -> r.description) "description" Jsont.string
    |> Jsont.Object.mem
         ~enc:(fun r -> r.headers)
         "headers"
         (String_map.jsont Header.header_or_ref)
    |> Jsont.Object.mem
         ~enc:(fun r -> r.content)
         ~dec_absent:[]
         "content"
         (String_map.jsont Media_type.jsont)
    |> Jsont.Object.mem
         ~enc:(fun r -> r.links)
         ~dec_absent:[]
         "links"
         (String_map.jsont Link.link_or_ref)
    |> Jsont.Object.finish

  let response_or_ref = Json_schema.Or_ref.jsont jsont
end

module Responses = struct
  type t =
    { default : Response.t Json_schema.Or_ref.t option
    ; responses : (string * Response.t Json_schema.Or_ref.t) list
    }

  let jsont =
    let enc r =
      let members =
        List.map
          (fun (code, resp) ->
             ( (code, Jsont.Meta.none)
             , Json_schema.encode_with Response.response_or_ref resp ))
          r.responses
      in
      let members =
        match r.default with
        | None -> members
        | Some d ->
          ( ("default", Jsont.Meta.none)
          , Json_schema.encode_with Response.response_or_ref d )
          :: members
      in
      Jsont.Object (members, Jsont.Meta.none)
    in
    let dec json =
      match json with
      | Jsont.Object (members, _) ->
        let default =
          List.find_map
            (fun ((name, _), value) ->
               if String.equal name "default"
               then
                 Some (Json_schema.decode_with Response.response_or_ref value)
               else None)
            members
        in
        let responses =
          List.filter_map
            (fun ((name, _), value) ->
               if String.equal name "default"
               then None
               else
                 Some
                   (name, Json_schema.decode_with Response.response_or_ref value))
            members
        in
        { default; responses }
      | _ -> Jsont.Error.msg Jsont.Meta.none "Responses must be an object"
    in
    Jsont.map ~kind:"Responses" ~dec ~enc Jsont.json
end

module Security_requirement = struct
  type t = (string * string list) list

  let jsont : t Jsont.t = String_map.jsont (Jsont.list Jsont.string)
end

module Callback = struct
  type t = Jsont.json

  let jsont : t Jsont.t = Jsont.json
  let callback_or_ref = Json_schema.Or_ref.jsont jsont
end

module Operation = struct
  type t =
    { tags : string list option
    ; summary : string option
    ; description : string option
    ; external_docs : External_docs.t option
    ; operation_id : string option
    ; parameters : Parameter.t Json_schema.Or_ref.t list
    ; request_body : Request_body.t Json_schema.Or_ref.t option
    ; responses : Responses.t
    ; callbacks : (string * Callback.t Json_schema.Or_ref.t) list
    ; deprecated : bool
    ; security : Security_requirement.t list option
    ; servers : Server.t list
    }

  let jsont =
    Jsont.Object.map
      ~kind:"Operation"
      (fun
          tags
           summary
           description
           external_docs
           operation_id
           parameters
           request_body
           responses
           callbacks
           deprecated
           security
           servers
         ->
         { tags
         ; summary
         ; description
         ; external_docs
         ; operation_id
         ; parameters
         ; request_body
         ; responses
         ; callbacks
         ; deprecated
         ; security
         ; servers
         })
    |> Jsont.Object.opt_mem
         ~enc:(fun o -> o.tags)
         "tags"
         (Jsont.list Jsont.string)
    |> Jsont.Object.opt_mem ~enc:(fun o -> o.summary) "summary" Jsont.string
    |> Jsont.Object.opt_mem
         ~enc:(fun o -> o.description)
         "description"
         Jsont.string
    |> Jsont.Object.opt_mem
         ~enc:(fun o -> o.external_docs)
         "externalDocs"
         External_docs.jsont
    |> Jsont.Object.opt_mem
         ~enc:(fun o -> o.operation_id)
         "operationId"
         Jsont.string
    |> Jsont.Object.mem
         ~enc:(fun o -> o.parameters)
         "parameters"
         (Jsont.list Parameter.parameter_or_ref)
    |> Jsont.Object.opt_mem
         ~enc:(fun o -> o.request_body)
         "requestBody"
         Request_body.request_body_or_ref
    |> Jsont.Object.mem ~enc:(fun o -> o.responses) "responses" Responses.jsont
    |> Jsont.Object.mem
         ~enc:(fun o -> o.callbacks)
         ~enc_omit:List.is_empty
         "callbacks"
         (String_map.jsont (Json_schema.Or_ref.jsont Callback.jsont))
    |> Jsont.Object.mem ~enc:(fun o -> o.deprecated) "deprecated" Jsont.bool
    |> Jsont.Object.opt_mem
         ~enc:(fun o -> o.security)
         "security"
         (Jsont.list Security_requirement.jsont)
    |> Jsont.Object.mem
         ~enc:(fun o -> o.servers)
         ~enc_omit:List.is_empty
         "servers"
         (Jsont.list Server.jsont)
    |> Jsont.Object.finish
end

module Path_item = struct
  type t =
    { ref : string option
    ; summary : string option
    ; description : string option
    ; get : Operation.t option
    ; put : Operation.t option
    ; post : Operation.t option
    ; delete : Operation.t option
    ; options : Operation.t option
    ; head : Operation.t option
    ; patch : Operation.t option
    ; trace : Operation.t option
    ; servers : Server.t list
    ; parameters : Parameter.t Json_schema.Or_ref.t list
    }

  let jsont =
    Jsont.Object.map
      ~kind:"PathItem"
      (fun
          ref
           summary
           description
           get
           put
           post
           delete
           options
           head
           patch
           trace
           servers
           parameters
         ->
         { ref
         ; summary
         ; description
         ; get
         ; put
         ; post
         ; delete
         ; options
         ; head
         ; patch
         ; trace
         ; servers
         ; parameters
         })
    |> Jsont.Object.opt_mem ~enc:(fun p -> p.ref) "$ref" Jsont.string
    |> Jsont.Object.opt_mem ~enc:(fun p -> p.summary) "summary" Jsont.string
    |> Jsont.Object.opt_mem
         ~enc:(fun p -> p.description)
         "description"
         Jsont.string
    |> Jsont.Object.opt_mem ~enc:(fun p -> p.get) "get" Operation.jsont
    |> Jsont.Object.opt_mem ~enc:(fun p -> p.put) "put" Operation.jsont
    |> Jsont.Object.opt_mem ~enc:(fun p -> p.post) "post" Operation.jsont
    |> Jsont.Object.opt_mem ~enc:(fun p -> p.delete) "delete" Operation.jsont
    |> Jsont.Object.opt_mem ~enc:(fun p -> p.options) "options" Operation.jsont
    |> Jsont.Object.opt_mem ~enc:(fun p -> p.head) "head" Operation.jsont
    |> Jsont.Object.opt_mem ~enc:(fun p -> p.patch) "patch" Operation.jsont
    |> Jsont.Object.opt_mem ~enc:(fun p -> p.trace) "trace" Operation.jsont
    |> Jsont.Object.mem
         ~enc:(fun p -> p.servers)
         ~dec_absent:[]
         "servers"
         (Jsont.list Server.jsont)
    |> Jsont.Object.mem
         ~enc:(fun p -> p.parameters)
         "parameters"
         (Jsont.list Parameter.parameter_or_ref)
    |> Jsont.Object.finish

  let path_item_or_ref = Json_schema.Or_ref.jsont jsont
end

module Security_scheme_type = struct
  type t =
    | Api_key
    | Http
    | OAuth2
    | Openid_connect

  let jsont =
    Jsont.map
      ~kind:"Security_schema_type"
      ~dec:(function
        | "apiKey" -> Api_key
        | "http" -> Http
        | "oauth2" -> OAuth2
        | "openIdConnect" -> Openid_connect
        | s ->
          Jsont.Error.msgf Jsont.Meta.none "Unknown security schema type: %s" s)
      ~enc:(function
        | Api_key -> "apiKey"
        | Http -> "http"
        | OAuth2 -> "oauth2"
        | Openid_connect -> "openIdConnect")
      Jsont.string
end

module Oauth2_flow = struct
  type t =
    { authorization_url : string option
    ; token_url : string option
    ; refresh_url : string option
    ; scopes : (string * string) list
    }

  let jsont =
    Jsont.Object.map
      ~kind:"OAuth2Flows"
      (fun authorization_url token_url refresh_url scopes ->
         { authorization_url; token_url; refresh_url; scopes })
    |> Jsont.Object.opt_mem
         ~enc:(fun f -> f.authorization_url)
         "authorizationUrl"
         Jsont.string
    |> Jsont.Object.opt_mem ~enc:(fun f -> f.token_url) "tokenUrl" Jsont.string
    |> Jsont.Object.opt_mem
         ~enc:(fun f -> f.refresh_url)
         "refreshUrl"
         Jsont.string
    |> Jsont.Object.mem
         ~enc:(fun f -> f.scopes)
         "scopes"
         (String_map.jsont Jsont.string)
    |> Jsont.Object.finish
end

module Oauth2_flows = struct
  type t =
    { authorization_code : Oauth2_flow.t option
    ; implicit : Oauth2_flow.t option
    ; password : Oauth2_flow.t option
    ; client_credentials : Oauth2_flow.t option
    }

  let jsont =
    Jsont.Object.map
      ~kind:"OAuth2Flows"
      (fun authorization_code implicit password client_credentials ->
         { authorization_code; implicit; password; client_credentials })
    |> Jsont.Object.opt_mem
         ~enc:(fun f -> f.authorization_code)
         "authorizationCode"
         Oauth2_flow.jsont
    |> Jsont.Object.opt_mem
         ~enc:(fun f -> f.implicit)
         "implicit"
         Oauth2_flow.jsont
    |> Jsont.Object.opt_mem
         ~enc:(fun f -> f.password)
         "password"
         Oauth2_flow.jsont
    |> Jsont.Object.opt_mem
         ~enc:(fun f -> f.client_credentials)
         "clientCredentials"
         Oauth2_flow.jsont
    |> Jsont.Object.finish
end

module Security_scheme = struct
  type t =
    { type_ : Security_scheme_type.t
    ; description : string option
    ; name : string option
    ; in_ : Location.t option
    ; scheme : string option
    ; bearer_format : string option
    ; flows : Oauth2_flows.t option
    ; open_id_connect_url : string option
    }

  let jsont =
    Jsont.Object.map
      ~kind:"SecurityScheme"
      (fun
          type_
           description
           name
           in_
           scheme
           bearer_format
           flows
           open_id_connect_url
         ->
         { type_
         ; description
         ; name
         ; in_
         ; scheme
         ; bearer_format
         ; flows
         ; open_id_connect_url
         })
    |> Jsont.Object.mem
         ~enc:(fun s -> s.type_)
         "type"
         Security_scheme_type.jsont
    |> Jsont.Object.opt_mem
         ~enc:(fun s -> s.description)
         "description"
         Jsont.string
    |> Jsont.Object.opt_mem ~enc:(fun s -> s.name) "name" Jsont.string
    |> Jsont.Object.opt_mem ~enc:(fun s -> s.in_) "in" Location.jsont
    |> Jsont.Object.opt_mem ~enc:(fun s -> s.scheme) "scheme" Jsont.string
    |> Jsont.Object.opt_mem
         ~enc:(fun s -> s.bearer_format)
         "bearerFormat"
         Jsont.string
    |> Jsont.Object.opt_mem ~enc:(fun s -> s.flows) "flows" Oauth2_flows.jsont
    |> Jsont.Object.opt_mem
         ~enc:(fun s -> s.open_id_connect_url)
         "openIdConnectUrl"
         Jsont.string
    |> Jsont.Object.finish

  let security_scheme_or_ref = Json_schema.Or_ref.jsont jsont
end

module Component = struct
  type t =
    { schemas : (string * Json_schema.schema) list
    ; responses : (string * Response.t Json_schema.Or_ref.t) list
    ; parameters : (string * Parameter.t Json_schema.Or_ref.t) list
    ; examples : (string * Example.t Json_schema.Or_ref.t) list
    ; request_bodies : (string * Request_body.t Json_schema.Or_ref.t) list
    ; headers : (string * Header.t Json_schema.Or_ref.t) list
    ; security_schemes : (string * Security_scheme.t Json_schema.Or_ref.t) list
    ; links : (string * Link.t Json_schema.Or_ref.t) list
    ; callbacks : (string * Callback.t Json_schema.Or_ref.t) list
    ; path_items : (string * Path_item.t Json_schema.Or_ref.t) list
    }

  let jsont =
    Jsont.Object.map
      ~kind:"Components"
      (fun
          schemas
           responses
           parameters
           examples
           request_bodies
           headers
           security_schemes
           links
           callbacks
           path_items
         ->
         { schemas
         ; responses
         ; parameters
         ; examples
         ; request_bodies
         ; headers
         ; security_schemes
         ; links
         ; callbacks
         ; path_items
         })
    |> Jsont.Object.mem
         ~enc:(fun c -> c.schemas)
         "schemas"
         (String_map.jsont Json_schema.schema_jsont)
    |> Jsont.Object.mem
         ~enc:(fun c -> c.responses)
         "responses"
         (String_map.jsont Response.response_or_ref)
    |> Jsont.Object.mem
         ~enc:(fun c -> c.parameters)
         "parameters"
         (String_map.jsont Parameter.parameter_or_ref)
    |> Jsont.Object.mem
         ~enc:(fun c -> c.examples)
         "examples"
         (String_map.jsont Example.example_or_ref)
    |> Jsont.Object.mem
         ~enc:(fun c -> c.request_bodies)
         "requestBodies"
         (String_map.jsont Request_body.request_body_or_ref)
    |> Jsont.Object.mem
         ~enc:(fun c -> c.headers)
         "headers"
         (String_map.jsont Header.header_or_ref)
    |> Jsont.Object.mem
         ~enc:(fun c -> c.security_schemes)
         "securitySchemes"
         (String_map.jsont Security_scheme.security_scheme_or_ref)
    |> Jsont.Object.mem
         ~enc:(fun c -> c.links)
         "links"
         (String_map.jsont Link.link_or_ref)
    |> Jsont.Object.mem
         ~enc:(fun c -> c.callbacks)
         "callbacks"
         (String_map.jsont (Json_schema.Or_ref.jsont Callback.jsont))
    |> Jsont.Object.mem
         ~enc:(fun c -> c.path_items)
         "pathItems"
         (String_map.jsont Path_item.path_item_or_ref)
    |> Jsont.Object.finish
end

type t =
  { openapi : string
  ; info : Info.t
  ; servers : Server.t list
  ; paths : (string * Path_item.t Json_schema.Or_ref.t) list
  ; webhooks : (string * Path_item.t Json_schema.Or_ref.t) list
  ; components : Component.t option
  ; security : Security_requirement.t list
  ; tags : Tag.t list
  ; external_docs : External_docs.t option
  }

let jsont =
  Jsont.Object.map
    ~kind:"OpenAPI"
    (fun
        openapi
         info
         servers
         paths
         webhooks
         components
         security
         tags
         external_docs
       ->
       { openapi
       ; info
       ; servers
       ; paths
       ; webhooks
       ; components
       ; security
       ; tags
       ; external_docs
       })
  |> Jsont.Object.mem ~enc:(fun o -> o.openapi) "openapi" Jsont.string
  |> Jsont.Object.mem ~enc:(fun o -> o.info) "info" Info.jsont
  |> Jsont.Object.mem
       ~enc:(fun o -> o.servers)
       "servers"
       (Jsont.list Server.jsont)
  |> Jsont.Object.mem
       ~enc:(fun o -> o.paths)
       "paths"
       (String_map.jsont Path_item.path_item_or_ref)
  |> Jsont.Object.mem
       ~enc:(fun o -> o.webhooks)
       ~enc_omit:List.is_empty
       "webhooks"
       (String_map.jsont Path_item.path_item_or_ref)
  |> Jsont.Object.opt_mem
       ~enc:(fun o -> o.components)
       "components"
       Component.jsont
  |> Jsont.Object.mem
       ~enc:(fun o -> o.security)
       ~enc_omit:List.is_empty
       "security"
       (Jsont.list Security_requirement.jsont)
  |> Jsont.Object.mem
       ~enc:(fun o -> o.tags)
       ~enc_omit:List.is_empty
       "tags"
       (Jsont.list Tag.jsont)
  |> Jsont.Object.opt_mem
       ~enc:(fun o -> o.external_docs)
       "externalDocs"
       External_docs.jsont
  |> Jsont.Object.finish

let of_string s = Jsont_bytesrw.decode_string jsont s
let of_string' s = Jsont_bytesrw.decode_string' jsont s
let to_string ?format v = Jsont_bytesrw.encode_string ?format jsont v
let to_string' ?format v = Jsont_bytesrw.encode_string' ?format jsont v

let resolve_schema_ref ref_str spec =
  if not (String.length ref_str > 0 && ref_str.[0] = '#')
  then None
  else
    let parts = String.split_on_char '/' ref_str in
    match parts with
    | [ "#"; "components"; "schemas"; name ] ->
      (match spec.components with
      | None -> None
      | Some c ->
        (match List.assoc_opt name c.schemas with
        | Some (Json_schema.Or_bool.Schema (Json_schema.Or_ref.Value s)) ->
          Some s
        | Some (Json_schema.Or_bool.Schema (Json_schema.Or_ref.Ref _)) ->
          None (* nested refs not supported yet *)
        | Some (Json_schema.Or_bool.Bool _) ->
          None (* boolean schemas don't resolve to a schema object *)
        | None -> None))
    | _ -> None
