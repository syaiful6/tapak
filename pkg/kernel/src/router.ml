exception Not_found
exception Bad_request of string
exception Validation_failed of (string * string) list

type (_, _) path =
  | Nil : ('a, 'a) path
  | Literal : string * ('a, 'b) path -> ('a, 'b) path
  | Capture :
      { parse : string -> 'param option
      ; format : 'param -> string
      ; type_name : string
      ; format_name : string option
      ; rest : ('a, 'b) path
      }
      -> ('param -> 'a, 'b) path
  | Enum :
      { parse : string -> 'param option
      ; format : 'param -> string
      ; type_name : string
      ; format_name : string option
      ; values : 'param list
      ; rest : ('a, 'b) path
      }
      -> ('param -> 'a, 'b) path
  | Annotated :
      { segment : ('a, 'b) path
      ; name : string
      ; description : string option
      }
      -> ('a, 'b) path
  | Splat : ('a, 'b) path -> (string list -> 'a, 'b) path

type metadata =
  { operation_id : string option
  ; summary : string option
  ; description : string option
  ; tags : string list
  ; body_description : string option
  }

type (_, _) schema =
  | Method : Piaf.Method.t * ('a, 'b) path -> ('a, 'b) schema
  | Response_model :
      { encoder : 'resp -> Response.t
      ; rest : ('a, Request.t -> 'resp) schema
      }
      -> ('a, Request.t -> Response.t) schema
  | Body :
      { input_type : 'input Schema.input
      ; schema : 'validated Schema.t
      ; rest : ('a, 'b) schema
      }
      -> ('validated -> 'a, 'b) schema
  | Guard :
      { guard : 'g Request_guard.t
      ; rest : ('a, 'b) schema
      }
      -> ('g -> 'a, 'b) schema
  | Meta :
      { meta : metadata
      ; rest : ('a, 'b) schema
      }
      -> ('a, 'b) schema

let parse_int s = try Some (int_of_string s) with Failure _ -> None
let parse_int32 s = try Some (Int32.of_string s) with Failure _ -> None
let parse_int64 s = try Some (Int64.of_string s) with Failure _ -> None

let parse_bool = function
  | "true" -> Some true
  | "false" -> Some false
  | _ -> None

let int : (int -> 'a, 'a) path =
  Capture
    { parse = parse_int
    ; format = string_of_int
    ; type_name = "integer"
    ; format_name = Some "int32"
    ; rest = Nil
    }

let int32 : (int32 -> 'a, 'a) path =
  Capture
    { parse = parse_int32
    ; format = Int32.to_string
    ; type_name = "integer"
    ; format_name = Some "int32"
    ; rest = Nil
    }

let int64 : (int64 -> 'a, 'a) path =
  Capture
    { parse = parse_int64
    ; format = Int64.to_string
    ; type_name = "integer"
    ; format_name = Some "int64"
    ; rest = Nil
    }

let str : (string -> 'a, 'a) path =
  Capture
    { parse = Option.some
    ; format = Fun.id
    ; type_name = "string"
    ; format_name = None
    ; rest = Nil
    }

let bool : (bool -> 'a, 'a) path =
  Capture
    { parse = parse_bool
    ; format = string_of_bool
    ; type_name = "boolean"
    ; format_name = None
    ; rest = Nil
    }

let splat : (string list -> 'a, 'a) path = Splat Nil
let s literal : ('a, 'a) path = Literal (literal, Nil)

let custom : type a.
  parse:(string -> 'param option)
  -> format:('param -> string)
  -> type_name:string
  -> ?format_name:string
  -> unit
  -> ('param -> a, a) path
  =
 fun ~parse ~format ~type_name ?format_name () ->
  Capture { parse; format; type_name; format_name; rest = Nil }

let enum : type a.
  parse:(string -> 'param option)
  -> format:('param -> string)
  -> type_name:string
  -> ?format_name:string
  -> values:'param list
  -> unit
  -> ('param -> a, a) path
  =
 fun ~parse ~format ~type_name ?format_name ~values () ->
  Enum { parse; format; type_name; format_name; values; rest = Nil }

type route =
  | Route :
      { schema : ('a, Request.t -> Response.t) schema
      ; handler : 'a
      }
      -> route
  | Scope :
      { prefix : ('a, 'a) path
      ; routes : route list
      ; middlewares : Middleware.t list
      }
      -> route

let is_slug s =
  let len = String.length s in
  if len = 0
  then false
  else
    let rec check i =
      if i >= len
      then true
      else
        let c = s.[i] in
        if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '-'
        then check (i + 1)
        else false
    in
    check 0

let slug : (string -> 'a, 'a) path =
  Capture
    { parse = (fun s -> if is_slug s then Some s else None)
    ; format = Fun.id
    ; type_name = "string"
    ; format_name = Some "slug"
    ; rest = Nil
    }

let rec ( / ) : type a b c. (a, c) path -> (c, b) path -> (a, b) path =
 fun left right ->
  match left with
  | Nil -> right
  | Literal (lit, rest) -> Literal (lit, rest / right)
  | Splat rest -> Splat (rest / right)
  | Capture { parse; format; type_name; format_name; rest } ->
    Capture { parse; format; type_name; format_name; rest = rest / right }
  | Enum { parse; format; type_name; format_name; values; rest } ->
    Enum { parse; format; type_name; format_name; values; rest = rest / right }
  | Annotated { segment; name; description } ->
    Annotated { segment = segment / right; name; description }

let get : ('a, 'b) path -> ('a, 'b) schema =
 fun pattern -> Method (`GET, pattern)

let head : ('a, 'b) path -> ('a, 'b) schema =
 fun pattern -> Method (`HEAD, pattern)

let post : ('a, 'b) path -> ('a, 'b) schema =
 fun pattern -> Method (`POST, pattern)

let put : ('a, 'b) path -> ('a, 'b) schema =
 fun pattern -> Method (`PUT, pattern)

let patch : ('a, 'b) path -> ('a, 'b) schema =
 fun pattern -> Method (`Other "PATCH", pattern)

let delete : ('a, 'b) path -> ('a, 'b) schema =
 fun pattern -> Method (`DELETE, pattern)

let any : ('a, 'b) path -> ('a, 'b) schema =
 fun pattern -> Method (`Other "*", pattern)

let body : type a b c input.
  input Schema.input -> a Schema.t -> (b, c) schema -> (a -> b, c) schema
  =
 fun input_type schema rest -> Body { input_type; schema; rest }

let guard : type a b g. g Request_guard.t -> (a, b) schema -> (g -> a, b) schema
  =
 fun guard schema -> Guard { guard; rest = schema }

let response_model : type a resp.
  (resp -> Response.t)
  -> (a, Request.t -> resp) schema
  -> (a, Request.t -> Response.t) schema
  =
 fun encoder rest -> Response_model { encoder; rest }

let empty_metadata =
  { operation_id = None
  ; summary = None
  ; description = None
  ; tags = []
  ; body_description = None
  }

let operation_id : type a b. string -> (a, b) schema -> (a, b) schema =
 fun id rest ->
  Meta { meta = { empty_metadata with operation_id = Some id }; rest }

let summary : type a b. string -> (a, b) schema -> (a, b) schema =
 fun s rest -> Meta { meta = { empty_metadata with summary = Some s }; rest }

let description : type a b. string -> (a, b) schema -> (a, b) schema =
 fun d rest ->
  Meta { meta = { empty_metadata with description = Some d }; rest }

let tags : type a b. string list -> (a, b) schema -> (a, b) schema =
 fun ts rest -> Meta { meta = { empty_metadata with tags = ts }; rest }

let tag : type a b. string -> (a, b) schema -> (a, b) schema =
 fun t rest -> Meta { meta = { empty_metadata with tags = [ t ] }; rest }

let p : type a b. string -> (a, b) path -> (a, b) path =
 fun name segment -> Annotated { segment; name; description = None }

let ann : type a b. string * string -> (a, b) path -> (a, b) path =
 fun (name, desc) segment ->
  Annotated { segment; name; description = Some desc }

let split_path path =
  let only_path =
    match String.index_opt path '?' with
    | Some idx -> String.sub path 0 idx
    | None -> path
  in
  let segments = String.split_on_char '/' only_path in
  List.filter (fun s -> s <> "") segments

let rec can_match_path : type a b. (a, b) path -> string list -> bool =
 fun pattern segments ->
  match pattern, segments with
  | Nil, [] -> true
  | Nil, _ :: _ -> false
  | Literal ("", rest), [] -> can_match_path rest []
  | Literal (expected, rest), seg :: segs when String.equal expected seg ->
    can_match_path rest segs
  | Splat rest, _ -> can_match_path rest []
  | Capture { parse; rest; _ }, seg :: segs ->
    (match parse seg with Some _ -> can_match_path rest segs | None -> false)
  | Enum { parse; rest; _ }, seg :: segs ->
    (match parse seg with Some _ -> can_match_path rest segs | None -> false)
  | Annotated { segment; _ }, segs -> can_match_path segment segs
  | _ -> false

let expected_content_type : type input. input Schema.input -> string = function
  | Schema.Json -> "application/json"
  | Schema.Urlencoded -> "application/x-www-form-urlencoded"
  | Schema.Multipart -> "multipart/form-data"

let validate_content_type : type input.
  input Schema.input -> Request.t -> (unit, string) result
  =
 fun input_type request ->
  let expected = expected_content_type input_type in
  let actual = Piaf.Headers.get (Request.headers request) "content-type" in
  match actual with
  | None ->
    Error (Printf.sprintf "Missing Content-Type header, expected: %s" expected)
  | Some content_type ->
    (* Extract media type, ignoring charset and other parameters *)
    let media_type =
      match String.split_on_char ';' content_type with
      | media :: _ -> String.trim media
      | [] -> content_type
    in
    if
      String.equal
        (String.lowercase_ascii media_type)
        (String.lowercase_ascii expected)
    then Ok ()
    else
      Error
        (Printf.sprintf
           "Unsupported Content-Type: %s, expected: %s"
           media_type
           expected)

let rec match_pattern : type a b.
  (a, b) path -> string list -> Request.t -> a -> b option
  =
 fun pattern segments request k ->
  match pattern, segments with
  | Nil, [] -> Some k
  | Nil, _ :: _ -> None (* Reject trailing segments *)
  | Literal ("", rest), [] -> match_pattern rest [] request k
  | Literal (expected, rest), seg :: segs when String.equal expected seg ->
    match_pattern rest segs request k
  | Splat rest, segs -> match_pattern rest [] request (k segs)
  | Capture { parse; rest; _ }, seg :: segs ->
    (match parse seg with
    | Some v -> match_pattern rest segs request (k v)
    | None -> None)
  | Enum { parse; rest; _ }, seg :: segs ->
    (match parse seg with
    | Some v -> match_pattern rest segs request (k v)
    | None -> None)
  | Annotated { segment; _ }, segs -> match_pattern segment segs request k
  | _ -> None

let rec get_method : type a b. (a, b) schema -> Piaf.Method.t = function
  | Method (m, _) -> m
  | Body { rest; _ } -> get_method rest
  | Guard { rest; _ } -> get_method rest
  | Meta { rest; _ } -> get_method rest
  | Response_model { rest; _ } -> get_method rest

let into : type a. a -> (a, Request.t -> Response.t) schema -> route =
 fun handler schema -> Route { schema; handler }

let rec match_prefix : type a b.
  (a, b) path -> string list -> string list option
  =
 fun pattern segments ->
  match pattern, segments with
  | Nil, segs -> Some segs
  | Literal ("", rest), segs -> match_prefix rest segs
  | Literal (expected, rest), seg :: segs when String.equal expected seg ->
    match_prefix rest segs
  | Annotated { segment; _ }, segs -> match_prefix segment segs
  | _ -> None

let rec can_match_schema : type a b.
  (a, b) schema -> string list -> Request.t -> bool
  =
 fun schema segments request ->
  match schema with
  | Method (meth, pattern) ->
    let method_matches =
      match meth with
      | `Other "*" -> true
      | _ ->
        Piaf.Method.to_string meth
        = Piaf.Method.to_string (Request.meth request)
    in
    method_matches && can_match_path pattern segments
  | Body { rest; _ } -> can_match_schema rest segments request
  | Guard { rest; _ } -> can_match_schema rest segments request
  | Response_model { rest; _ } -> can_match_schema rest segments request
  | Meta { rest; _ } -> can_match_schema rest segments request

let evaluate_body_schema : type a b.
  a Schema.input
  -> b Schema.t
  -> Request.t
  -> (b, (string * string) list) result
  =
 fun input schema request ->
  match input with
  | Schema.Json ->
    (match Body.to_string (Request.body request) with
    | Ok body_str ->
      (match
         try Ok (Yojson.Safe.from_string body_str) with
         | _ -> Error "Invalid JSON body"
       with
      | Ok json -> Schema.eval Schema.Json schema json
      | Error e -> Error [ "body", e ])
    | Error _ -> raise (Bad_request "Failed to read request body"))
  | Schema.Urlencoded ->
    (match Form.Urlencoded.of_body (Request.body request) with
    | Ok form -> Schema.eval Schema.Urlencoded schema form
    | Error _ -> raise (Bad_request "Invalid URL-encoded body"))
  | Schema.Multipart ->
    (match Form.Multipart.parse request with
    | Ok form -> Schema.eval Schema.Multipart schema form
    | Error _ -> raise (Bad_request "Invalid multipart body"))

let rec evaluate_schema : type a b.
  (a, b) schema -> string list -> Request.t -> a -> b option
  =
 fun schema segments request k ->
  match schema with
  | Method (_, pattern) -> match_pattern pattern segments request k
  | Body { input_type; schema; rest } ->
    (match validate_content_type input_type request with
    | Error msg -> raise (Bad_request msg)
    | Ok () ->
      (match evaluate_body_schema input_type schema request with
      | Ok validated_data ->
        evaluate_schema rest segments request (k validated_data)
      | Error errors -> raise (Validation_failed errors)))
  | Response_model { encoder; rest } ->
    (match evaluate_schema rest segments request k with
    | Some data_fn -> Some (fun request -> data_fn request |> encoder)
    | None -> None)
  | Guard { guard; rest } ->
    (match guard request with
    | Ok g -> evaluate_schema rest segments request (k g)
    | Error e -> raise (Request_guard.Failed e))
  | Meta { rest; _ } -> evaluate_schema rest segments request k

let rec match_route ?(middlewares = []) route segments request =
  match route with
  | Route route ->
    if can_match_schema route.schema segments request
    then
      match evaluate_schema route.schema segments request route.handler with
      | Some handler ->
        let service = Filter.apply_all middlewares handler in
        Some (service request)
      | None -> None
    else None
  | Scope scope ->
    (match match_prefix scope.prefix segments with
    | None -> None
    | Some remaining_segments ->
      let accumulated_middlewares = middlewares @ scope.middlewares in
      List.find_map
        (fun route ->
           match_route
             ~middlewares:accumulated_middlewares
             route
             remaining_segments
             request)
        scope.routes)

let match' routes request =
  let segments = split_path (Request.target request) in
  List.find_map (fun route -> match_route route segments request) routes

let router routes request =
  match match' routes request with
  | Some response -> response
  | None -> raise Not_found

let rec sprintf' : type a. (a, string) path -> string -> a =
 fun pattern acc ->
  match pattern with
  | Nil -> acc
  | Literal ("", rest) -> sprintf' rest (if acc = "" then "/" else acc)
  | Literal (s, rest) -> sprintf' rest (acc ^ "/" ^ s)
  | Splat rest ->
    fun segments ->
      let splat_path = String.concat "/" segments in
      sprintf' rest (if splat_path = "" then acc else acc ^ "/" ^ splat_path)
  | Capture { format; rest; _ } -> fun v -> sprintf' rest (acc ^ "/" ^ format v)
  | Enum { format; rest; _ } -> fun v -> sprintf' rest (acc ^ "/" ^ format v)
  | Annotated { segment; _ } -> sprintf' segment acc

let sprintf pattern = sprintf' pattern ""

let scope ?(middlewares = []) prefix routes =
  Scope { prefix; routes; middlewares }

module type Resource = sig
  type id

  val id_path : unit -> (id -> 'a, 'a) path
  val index : Handler.t
  val new_ : Handler.t
  val create : Handler.t
  val get : id -> Handler.t
  val edit : id -> Handler.t
  val update : id -> Handler.t
  val delete : id -> Handler.t
end

let resource ?(middlewares = []) prefix_builder (module R : Resource) =
  scope
    ~middlewares
    prefix_builder
    [ get (s "") |> into R.index
    ; get (s "new") |> into R.new_
    ; post (s "") |> into R.create
    ; get (R.id_path ()) |> into R.get
    ; get (R.id_path () / s "edit") |> into R.edit
    ; put (R.id_path ()) |> into R.update
    ; delete (R.id_path ()) |> into R.delete
    ]
