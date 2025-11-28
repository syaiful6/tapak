exception Not_found
exception Bad_request of string

type _ content_type =
  | Json : Yojson.Safe.t content_type
  | Urlencoded : Form.Urlencoded.t content_type
  | Multipart : Form.Multipart.t content_type

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
  | Request_body :
      { content_type : 'body content_type
      ; rest : ('a, 'b) schema
      }
      -> ('body -> 'a, 'b) schema
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

let req_body : type a b body_type.
  body_type content_type -> (a, b) schema -> (body_type -> a, b) schema
  =
 fun content_type schema -> Request_body { content_type; rest = schema }

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

let parse_body : type body.
  body content_type -> Request.t -> (body, string) result
  =
 fun content_type request ->
  let body = Request.body request in
  match content_type with
  | Json ->
    (match Body.to_string body with
    | Ok body_str ->
      (try Ok (Yojson.Safe.from_string body_str) with _ -> Error body_str)
    | Error _ -> Error "Can't read request body")
  | Urlencoded ->
    (match Form.Urlencoded.of_body body with
    | Ok form -> Ok form
    | Error _ -> Error "Invalid urlencoded body")
  | Multipart ->
    (match Form.Multipart.parse request with
    | Ok form -> Ok form
    | Error _ -> Error "Invalid multipart body")

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
  | Request_body { rest; _ } -> get_method rest
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
  | Request_body { rest; _ } -> can_match_schema rest segments request
  | Guard { rest; _ } -> can_match_schema rest segments request
  | Response_model { rest; _ } -> can_match_schema rest segments request
  | Meta { rest; _ } -> can_match_schema rest segments request

let rec match_schema : type a b.
  (a, b) schema -> string list -> Request.t -> a -> b option
  =
 fun schema segments request k ->
  match schema with
  | Method (meth, pattern) ->
    let method_matches =
      match meth with
      | `Other "*" -> true
      | _ ->
        Piaf.Method.to_string meth
        = Piaf.Method.to_string (Request.meth request)
    in
    if not method_matches
    then None
    else match_pattern pattern segments request k
  | Request_body { content_type; rest } ->
    if not (can_match_schema rest segments request)
    then None
    else (
      match parse_body content_type request with
      | Ok body -> match_schema rest segments request (k body)
      | Error msg -> raise (Bad_request msg))
  | Response_model { encoder; rest } ->
    (match match_schema rest segments request k with
    | Some data_fn -> Some (fun request -> data_fn request |> encoder)
    | None -> None)
  | Guard { guard; rest } ->
    if not (can_match_schema rest segments request)
    then None
    else (
      match guard request with
      | Ok g -> match_schema rest segments request (k g)
      | Error e -> raise (Request_guard.Failed e))
  | Meta { rest; _ } -> match_schema rest segments request k

let rec match_route ?(middlewares = []) route segments request =
  match route with
  | Route route ->
    (match match_schema route.schema segments request route.handler with
    | Some handler ->
      let service = Middleware.apply_all middlewares handler in
      Some (service request)
    | None -> None)
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
