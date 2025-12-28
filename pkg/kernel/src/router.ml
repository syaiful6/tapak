exception Not_found
exception Bad_request of string
exception Validation_failed of (string * string) list

type extractor_error = ..

exception Extraction_failed of extractor_error

type 'a extractor = Request.t -> ('a, extractor_error) result

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
  ; include_in_schema : bool
  }

type (_, _) schema =
  | Method : Piaf.Method.t * ('a, 'b) path -> ('a, 'b) schema
  | Query :
      { schema : 'query Schema.t
      ; rest : ('a, 'b) schema
      }
      -> ('query -> 'a, 'b) schema
  | Header :
      { schema : 'header Schema.t
      ; rest : ('a, 'b) schema
      }
      -> ('header -> 'a, 'b) schema
  | Cookie :
      { schema : 'cookie Schema.t
      ; rest : ('a, 'b) schema
      }
      -> ('cookie -> 'a, 'b) schema
  | Response_model :
      { encoder : 'resp -> Yojson.Safe.t
      ; schema : 'resp Schema.t
      ; status : Piaf.Status.t
      ; rest : ('a, 'resp) schema
      }
      -> ('a, Response.t) schema
  | Body :
      { input_type : 'input Schema.input
      ; schema : 'validated Schema.t
      ; rest : ('a, 'b) schema
      }
      -> ('validated -> 'a, 'b) schema
  | Extract :
      { extractor : 'g extractor
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
    ; format_name = None
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
      { schema : ('a, Response.t) schema
      ; handler : 'a
      ; middlewares : Middleware.t list
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

let query : type a b c. a Schema.t -> (b, c) schema -> (a -> b, c) schema =
 fun schema rest -> Query { schema; rest }

let header : type a b c. a Schema.t -> (b, c) schema -> (a -> b, c) schema =
 fun schema rest -> Header { schema; rest }

let cookie : type a b c. a Schema.t -> (b, c) schema -> (a -> b, c) schema =
 fun schema rest -> Cookie { schema; rest }

let extract : type a b g. g extractor -> (a, b) schema -> (g -> a, b) schema =
 fun extractor schema -> Extract { extractor; rest = schema }

let response_model : type a resp.
  status:Piaf.Status.t
  -> schema:resp Schema.t
  -> encoder:(resp -> Yojson.Safe.t)
  -> (a, resp) schema
  -> (a, Response.t) schema
  =
 fun ~status ~schema ~encoder rest ->
  Response_model { encoder; schema; status; rest }

let request : type a b. (a, b) schema -> (Request.t -> a, b) schema =
 fun schema -> Extract { extractor = Result.ok; rest = schema }

let unit : type a b. (a, b) schema -> (unit -> a, b) schema =
 fun schema -> Extract { extractor = (fun _ -> Result.ok ()); rest = schema }

let empty_metadata =
  { operation_id = None
  ; summary = None
  ; description = None
  ; tags = []
  ; body_description = None
  ; include_in_schema = true
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

let include_in_schema : type a b. bool -> (a, b) schema -> (a, b) schema =
 fun value rest ->
  Meta { meta = { empty_metadata with include_in_schema = value }; rest }

let p : type a b. string -> (a, b) path -> (a, b) path =
 fun name segment -> Annotated { segment; name; description = None }

let ann : type a b. string * string -> (a, b) path -> (a, b) path =
 fun (name, desc) segment ->
  Annotated { segment; name; description = Some desc }

module Path_cursor = struct
  type t =
    { path : string
    ; pos : int
    ; len : int
    }

  let create path =
    let total_len = String.length path in
    let len =
      match String.index_opt path '?' with Some idx -> idx | None -> total_len
    in
    let pos = if len > 0 && path.[0] = '/' then 1 else 0 in
    { path; pos; len }

  let[@inline] rec next t =
    if t.pos >= t.len
    then None
    else
      let rec find_end i =
        if i >= t.len
        then i
        else if t.path.[i] = '/'
        then i
        else find_end (i + 1)
      in
      let end_pos = find_end t.pos in
      if end_pos = t.pos
      then next { t with pos = end_pos + 1 }
      else
        let segment = String.sub t.path t.pos (end_pos - t.pos) in
        let segment = Uri.pct_decode segment in
        let next_pos = if end_pos < t.len then end_pos + 1 else end_pos in
        Some (segment, { t with pos = next_pos })

  let rec to_list t =
    match next t with None -> [] | Some (seg, t') -> seg :: to_list t'

  let rec skip n cursor =
    if n <= 0
    then cursor
    else
      match next cursor with
      | None -> cursor
      | Some (_, rest) -> skip (n - 1) rest
end

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
  (a, b) path -> Path_cursor.t -> Request.t -> a -> b option
  =
 fun pattern cursor request k ->
  match pattern with
  | Nil ->
    (match Path_cursor.next cursor with
    | None -> Some k
    | Some _ -> None (* Reject trailing segments *))
  | Literal ("", rest) ->
    (* Empty literal doesn't consume anything, just continue *)
    match_pattern rest cursor request k
  | Literal (expected, rest) ->
    (match Path_cursor.next cursor with
    | Some (seg, rest_cursor) when String.equal expected seg ->
      match_pattern rest rest_cursor request k
    | _ -> None)
  | Splat rest ->
    let remaining = Path_cursor.to_list cursor in
    match_pattern rest (Path_cursor.create "") request (k remaining)
  | Capture { parse; rest; _ } ->
    (match Path_cursor.next cursor with
    | Some (seg, rest_cursor) ->
      (match parse seg with
      | Some v -> match_pattern rest rest_cursor request (k v)
      | None -> None)
    | None -> None)
  | Enum { parse; rest; _ } ->
    (match Path_cursor.next cursor with
    | Some (seg, rest_cursor) ->
      (match parse seg with
      | Some v -> match_pattern rest rest_cursor request (k v)
      | None -> None)
    | None -> None)
  | Annotated { segment; _ } -> match_pattern segment cursor request k

let rec get_method : type a b. (a, b) schema -> Piaf.Method.t = function
  | Method (m, _) -> m
  | Query { rest; _ } -> get_method rest
  | Header { rest; _ } -> get_method rest
  | Cookie { rest; _ } -> get_method rest
  | Body { rest; _ } -> get_method rest
  | Extract { rest; _ } -> get_method rest
  | Meta { rest; _ } -> get_method rest
  | Response_model { rest; _ } -> get_method rest

let into : type a. a -> (a, Response.t) schema -> route =
 fun handler schema -> Route { schema; handler; middlewares = [] }

let recover_middleware handler next a =
  match next a with
  | b -> b
  | exception e ->
    (match handler a e with Some resp -> resp | None -> raise e)

let recover : (Request.t -> exn -> Response.t option) -> route -> route =
 fun handler route ->
  match route with
  | Route { schema; handler = h; middlewares } ->
    Route
      { schema
      ; handler = h
      ; middlewares = middlewares @ [ recover_middleware handler ]
      }
  | Scope { prefix; routes; middlewares } ->
    Scope
      { prefix
      ; routes
      ; middlewares = middlewares @ [ recover_middleware handler ]
      }

module Cookie_parser = struct
  let parse request =
    let header = Request.headers request in
    let cookies = Piaf.Cookies.Cookie.parse header in
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun (key, value) ->
         let existing_key =
           Hashtbl.fold
             (fun k _ acc -> if String.equal k key then Some k else acc)
             tbl
             None
         in
         match existing_key with
         | Some k ->
           let values = Hashtbl.find tbl k in
           Hashtbl.replace tbl k (value :: values)
         | None -> Hashtbl.add tbl key [ value ])
      cookies;
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []

  let to_yojson request =
    let forms = parse request in
    let fields =
      List.map
        (fun (key, values) ->
           match values with
           | [] -> key, `Null
           | [ single ] -> key, `String single
           | multiple ->
             key, `List (List.map (fun v -> `String v) (List.rev multiple)))
        forms
    in
    `Assoc fields
end

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
  (a, b) schema -> Path_cursor.t -> a -> Request.t -> b option
  =
 fun schema cursor k request ->
  match schema with
  | Method (_, pattern) -> match_pattern pattern cursor request k
  | Query { schema; rest } ->
    (match
       Schema.eval Schema.Urlencoded schema (Form.Urlencoded.of_query request)
     with
    | Ok query_data -> evaluate_schema rest cursor (k query_data) request
    | Error errors -> raise (Validation_failed errors))
  | Header { schema; rest } ->
    (match
       Schema.evaluate
         (module Schema.Header_interpreter)
         schema
         (Schema_headers.to_yojson (Request.headers request))
     with
    | Ok header_data -> evaluate_schema rest cursor (k header_data) request
    | Error errors -> raise (Validation_failed errors))
  | Cookie { schema; rest } ->
    (match Schema.eval Schema.Json schema (Cookie_parser.to_yojson request) with
    | Ok cookie_data -> evaluate_schema rest cursor (k cookie_data) request
    | Error errors -> raise (Validation_failed errors))
  | Body { input_type; schema; rest } ->
    (match validate_content_type input_type request with
    | Error msg -> raise (Bad_request msg)
    | Ok () ->
      (match evaluate_body_schema input_type schema request with
      | Ok validated_data ->
        evaluate_schema rest cursor (k validated_data) request
      | Error errors -> raise (Validation_failed errors)))
  | Response_model { encoder; status; rest; _ } ->
    (match evaluate_schema rest cursor k request with
    | Some data ->
      let json = encoder data in
      let body = Yojson.Safe.to_string json in
      let headers =
        Piaf.Headers.of_list [ "content-type", "application/json" ]
      in
      Some (Response.of_string ~headers ~body status)
    | None -> None)
  | Extract { extractor; rest } ->
    (match extractor request with
    | Ok g -> evaluate_schema rest cursor (k g) request
    | Error e -> raise (Extraction_failed e))
  | Meta { rest; _ } -> evaluate_schema rest cursor k request

module Trie = struct
  type segment =
    | Lit of string
    | Capt
    | Splat_key

  type flat_route =
    | Flat_route :
        { schema : ('a, Response.t) schema
        ; handler : 'a
        ; method_str : string
        ; segments : segment list
        ; middlewares : Middleware.t list
        ; prefix_len : int
        }
        -> flat_route

  let flat_route_method_str (Flat_route { method_str; _ }) = method_str
  let flat_route_segments (Flat_route { segments; _ }) = segments

  module String_map = Map.Make (String)

  type t =
    { literals : t String_map.t
    ; capture : t option
    ; splat : t option
    ; handlers : flat_route String_map.t
    }

  let rec extract_path_segments : type a b. (a, b) path -> segment list =
    function
    | Nil -> []
    | Literal ("", rest) -> extract_path_segments rest
    | Literal (s, rest) -> Lit s :: extract_path_segments rest
    | Capture { rest; _ } -> Capt :: extract_path_segments rest
    | Enum { rest; _ } -> Capt :: extract_path_segments rest
    | Splat rest -> Splat_key :: extract_path_segments rest
    | Annotated { segment; _ } -> extract_path_segments segment

  let rec extract_path_segments_and_method : type a b.
    (a, b) schema -> Piaf.Method.t * segment list
    = function
    | Method (m, pattern) -> m, extract_path_segments pattern
    | Query { rest; _ } -> extract_path_segments_and_method rest
    | Header { rest; _ } -> extract_path_segments_and_method rest
    | Cookie { rest; _ } -> extract_path_segments_and_method rest
    | Body { rest; _ } -> extract_path_segments_and_method rest
    | Response_model { rest; _ } -> extract_path_segments_and_method rest
    | Extract { rest; _ } -> extract_path_segments_and_method rest
    | Meta { rest; _ } -> extract_path_segments_and_method rest

  let rec flatten_route prefix_len prefix middlewares route =
    match route with
    | Route { schema; handler; middlewares = route_mws } ->
      let method_, segments = extract_path_segments_and_method schema in
      [ Flat_route
          { schema
          ; handler
          ; method_str = Piaf.Method.to_string method_ (* Cache method string *)
          ; segments = prefix @ segments
          ; middlewares = middlewares @ route_mws
          ; prefix_len
          }
      ]
    | Scope { prefix = scope_prefix; routes; middlewares = scope_mws } ->
      let scope_segments = extract_path_segments scope_prefix in
      let new_prefix = prefix @ scope_segments in
      let new_prefix_len = prefix_len + List.length scope_segments in
      routes
      |> List.concat_map
           (flatten_route new_prefix_len new_prefix (middlewares @ scope_mws))

  let flatten_routes routes = routes |> List.concat_map (flatten_route 0 [] [])

  let empty =
    { literals = String_map.empty
    ; capture = None
    ; splat = None
    ; handlers = String_map.empty
    }

  let rec insert_route : segment list -> flat_route -> t -> t =
   fun segments route node ->
    match segments with
    | [] ->
      { node with
        handlers =
          String_map.add (flat_route_method_str route) route node.handlers
      }
    | Lit s :: segs ->
      let child =
        match String_map.find_opt s node.literals with
        | Some n -> n
        | None -> empty
      in
      let updated_child = insert_route segs route child in
      { node with literals = String_map.add s updated_child node.literals }
    | Capt :: segs ->
      let child = match node.capture with Some n -> n | None -> empty in
      let updated_child = insert_route segs route child in
      { node with capture = Some updated_child }
    | Splat_key :: segs ->
      let child = match node.splat with Some n -> n | None -> empty in
      let updated_child = insert_route segs route child in
      { node with splat = Some updated_child }

  let compile routes =
    let routes' = flatten_routes routes in
    List.fold_left
      (fun trie route -> insert_route (flat_route_segments route) route trie)
      empty
      routes'

  let rec match_trie cursor method_str trie =
    let try_splat () =
      match trie.splat with
      | Some splat_child ->
        match_trie (Path_cursor.create "") method_str splat_child
      | None -> None
    in
    match Path_cursor.next cursor with
    | None ->
      (match String_map.find_opt method_str trie.handlers with
      | Some route -> Some route
      | None ->
        (match String_map.find_opt "*" trie.handlers with
        | Some route -> Some route
        | None -> try_splat ()))
    | Some (seg, rest_cursor) ->
      let try_capture () =
        match trie.capture with
        | Some capt_child ->
          (match match_trie rest_cursor method_str capt_child with
          | Some r -> Some r
          | None -> try_splat ())
        | None -> try_splat ()
      in
      (match String_map.find_opt seg trie.literals with
      | Some child ->
        (match match_trie rest_cursor method_str child with
        | Some r -> Some r
        | None -> try_capture ())
      | None -> try_capture ())

  let route_filter next request =
    match next request with Some resp -> resp | None -> raise Not_found

  let router' trie request =
    let path = Request.target request in
    let cursor = Path_cursor.create path in
    let method_str = Piaf.Method.to_string (Request.meth request) in
    match match_trie cursor method_str trie with
    | Some (Flat_route { schema; handler; middlewares; prefix_len; _ }) ->
      let cursor = Path_cursor.create path |> Path_cursor.skip prefix_len in
      let service =
        Filter.apply_all
          middlewares
          (route_filter (evaluate_schema schema cursor handler))
      in
      service request
    | None -> raise Not_found

  let router routes =
    let trie = compile routes in
    router' trie
end

let router = Trie.router

let match' routes =
  let trie = Trie.compile routes in
  fun request -> try Some (Trie.router' trie request) with Not_found -> None

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
  val get : Request.t -> id -> Response.t
  val edit : Request.t -> id -> Response.t
  val update : Request.t -> id -> Response.t
  val delete : Request.t -> id -> Response.t
end

let resource ?(middlewares = []) prefix_builder (module R : Resource) =
  scope
    ~middlewares
    prefix_builder
    [ get (s "") |> request |> into R.index
    ; get (s "new") |> request |> into R.new_
    ; post (s "") |> request |> into R.create
    ; get (R.id_path ()) |> request |> into R.get
    ; get (R.id_path () / s "edit") |> request |> into R.edit
    ; put (R.id_path ()) |> request |> into R.update
    ; delete (R.id_path ()) |> request |> into R.delete
    ]
