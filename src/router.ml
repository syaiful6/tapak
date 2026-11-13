exception Not_found
exception Bad_request of string
exception Validation_failed of (string * string) list

type extractor_error = ..

exception Extraction_failed of extractor_error

type 'a extractor = Request.t -> ('a, extractor_error) result

type media_type =
  | Json
  | Urlencoded
  | Multipart

type (_, _) path =
  | Nil : ('a, 'a) path
  | Literal : string * ('a, 'b) path -> ('a, 'b) path
  | Capture :
      { parse : off:int -> len:int -> string -> 'param option
      ; format : 'param -> string
      ; type_name : string
      ; format_name : string option
      ; rest : ('a, 'b) path
      }
      -> ('param -> 'a, 'b) path
  | Enum :
      { parse : off:int -> len:int -> string -> 'param option
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

let parse_int ~off ~len s = Span.parse_int s (Span.make ~off ~len)
let parse_int32 ~off ~len s = Span.parse_int32 s (Span.make ~off ~len)
let parse_int64 ~off ~len s = Span.parse_int64 s (Span.make ~off ~len)
let parse_bool ~off ~len s = Span.parse_bool s (Span.make ~off ~len)

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
    { parse = (fun ~off ~len s -> String.sub s off len |> Option.some)
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
  parse:(off:int -> len:int -> string -> 'param option)
  -> format:('param -> string)
  -> type_name:string
  -> ?format_name:string
  -> unit
  -> ('param -> a, a) path
  =
 fun ~parse ~format ~type_name ?format_name () ->
  Capture { parse; format; type_name; format_name; rest = Nil }

let enum : type a.
  parse:(off:int -> len:int -> string -> 'param option)
  -> format:('param -> string)
  -> type_name:string
  -> ?format_name:string
  -> values:'param list
  -> unit
  -> ('param -> a, a) path
  =
 fun ~parse ~format ~type_name ?format_name ~values () ->
  Enum { parse; format; type_name; format_name; values; rest = Nil }

let is_slug ~off ~len s =
  if len = 0
  then false
  else
    let rec check i =
      if i >= len
      then true
      else
        let c = s.[off + i] in
        if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '-'
        then check (i + 1)
        else false
    in
    check 0

let slug : (string -> 'a, 'a) path =
  Capture
    { parse =
        (fun ~off ~len s ->
          if is_slug ~off ~len s then Some (String.sub s off len) else None)
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

let p : type a b. string -> (a, b) path -> (a, b) path =
 fun name segment -> Annotated { segment; name; description = None }

let ann : type a b. string * string -> (a, b) path -> (a, b) path =
 fun (name, desc) segment ->
  Annotated { segment; name; description = Some desc }

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

let validation_error_to_json errors =
  Jsont.Json.(object' (errors |> List.map (fun (k, v) -> name k, string v)))

module Security = struct
  type scheme =
    | Basic of { realm : string }
    | Bearer of { format : string option }
    | Api_key of
        { name : string
        ; in_ : Security_api_key.location
        }

  type reason =
    | Missing
    | Malformed
    | Invalid

  type extractor_error += Unauthorized of scheme * reason

  (* Keep parsing and OpenAPI documentation in the same value. *)
  type 'a t =
    { scheme : scheme
    ; extractor : 'a extractor
    }

  let scheme t = t.scheme

  let verified scheme = function
    | Some v -> Ok v
    | None -> Error (Unauthorized (scheme, Invalid))

  (* Verification identifies the caller. [None] produces a 401 response; use
     [refine] with a separate error for authorization. *)
  let basic ?(realm = "Restricted") verify =
    let scheme = Basic { realm } in
    { scheme
    ; extractor =
        (fun req ->
          match Security_basic.authorization req with
          | None -> Error (Unauthorized (scheme, Missing))
          | Some (Basic (username, password)) ->
            verified scheme (verify ~username ~password)
          | Some (Other _) -> Error (Unauthorized (scheme, Malformed)))
    }

  let bearer ?format verify =
    let scheme = Bearer { format } in
    { scheme
    ; extractor =
        (fun req ->
          match Security_bearer.authorization req with
          | None -> Error (Unauthorized (scheme, Missing))
          | Some (Bearer token) -> verified scheme (verify token)
          | Some (Other _) -> Error (Unauthorized (scheme, Malformed)))
    }

  let api_key ~in_ name verify =
    let scheme = Api_key { name; in_ } in
    { scheme
    ; extractor =
        (fun req ->
          match Security_api_key.find ~in_ name req with
          | None -> Error (Unauthorized (scheme, Missing))
          | Some key -> verified scheme (verify key))
    }

  (* The WWW-Authenticate value for a 401 response, if the scheme has one *)
  let challenge = function
    | Basic { realm } ->
      Some Security_basic.(string_of_challenge (Challenge.Basic realm))
    | Bearer _ -> Some Security_bearer.challenge
    | Api_key _ -> None

  let default_unauthorized _req _scheme _reason =
    Response.of_string ~status:`Unauthorized "Unauthorized"

  (* A handler that sets its own WWW-Authenticate keeps it *)
  let with_challenge scheme response =
    match challenge scheme with
    | Some value ->
      Response.add_header_unless_exists "WWW-Authenticate" value response
    | None -> response

  let default_name = function
    | Basic _ -> "basicAuth"
    | Bearer _ -> "bearerAuth"
    | Api_key { name; _ } -> name
end

let basic = Security.basic
let bearer = Security.bearer
let api_key = Security.api_key

module Schema = struct
  type schemak
  type 'a schemaf = (schemak, 'a) Sch.Free.t

  type _ t =
    | Query : 'query Sch.t -> 'query t
    | Header : 'header Sch.t -> 'header t
    | Cookie : 'cookie Sch.t -> 'cookie t
    | Body :
        { media_types : media_type list
        ; schema : 'v Sch.t
        }
        -> 'v t
    | Extract : 'a extractor -> 'a t
    | Secured :
        { name : string
        ; scheme : Security.scheme
        ; extractor : 'a extractor
        }
        -> 'a t
    | Refine :
        { source : 'a schemaf
        ; check : 'a -> ('b, extractor_error) result
        }
        -> 'b t
    | Alt :
        { left : 'a schemaf
        ; right : 'a schemaf
        }
        -> 'a t
    | Fail : extractor_error -> 'a t
    | Memo :
        { key : 'a Context.key
        ; source : 'a schemaf
        }
        -> 'a t

  external inj : 'a t -> ('a, schemak) Sch.Sig.app = "%identity"
  external prj : ('a, schemak) Sch.Sig.app -> 'a t = "%identity"
end

type 'a schemaf = 'a Schema.schemaf

include Sch.Free.Syntax

let pure = Sch.Free.pure
let lift t = Sch.Free.lift (Schema.inj t)
let query schema = lift (Schema.Query schema)
let header schema = lift (Schema.Header schema)
let cookie schema = lift (Schema.Cookie schema)

(* The request's content type picks the decoder; [schema] is the same for all *)
let body_of media_types schema =
  if media_types = []
  then invalid_arg "Router.body_of: expected at least one media type";
  lift
    (Schema.Body { media_types = List.sort_uniq compare media_types; schema })

let body media_type schema = body_of [ media_type ] schema
let extract extractor = lift (Schema.Extract extractor)
let request = extract Result.ok

let secured ?name { Security.scheme; extractor } =
  let name =
    match name with Some name -> name | None -> Security.default_name scheme
  in
  lift (Schema.Secured { name; scheme; extractor })

let refine check source = lift (Schema.Refine { source; check })

(* Each call creates its own cache entry, so sharing means reusing the value *)
let memo source =
  let key = Context.Key.create { Context.name = None; show = None } in
  lift (Schema.Memo { key; source })

let map = Sch.Free.map
let fail e = lift (Schema.Fail e)

module Body_scan = struct
  type k

  external inj : bool -> ('a, k) Sch.Sig.app = "%identity"
  external prj : ('a, k) Sch.Sig.app -> bool = "%identity"

  let applicative : k Sch.Sig.applicative =
    { pure = (fun _ -> inj false)
    ; map = (fun _ a -> inj (prj a))
    ; apply = (fun f a -> inj (prj f || prj a))
    }
end

let rec has_body : type a. a schemaf -> bool =
 fun schema ->
  Body_scan.prj
    (Sch.Free.run
       Body_scan.applicative
       { run =
           (fun effect_ -> Body_scan.inj (effect_has_body (Schema.prj effect_)))
       }
       schema)

and effect_has_body : type a. a Schema.t -> bool = function
  | Body _ -> true
  | Query _ | Header _ | Cookie _ | Extract _ | Secured _ | Fail _ -> false
  | Refine { source; _ } -> has_body source
  | Memo { source; _ } -> has_body source
  | Alt { left; right } -> has_body left || has_body right

(* [left <|> right] runs [right] if [left] fails validation or extraction. Other
   exceptions propagate. If both fail, [right]'s error is raised. Effects from
   [left] are not rolled back.

   A body stream cannot be read twice, so only one branch may read it. To accept
   several body shapes, use a union in one [Sch.t]. *)
let ( <|> ) left right =
  if has_body left && has_body right
  then invalid_arg "Router.( <|> ): only one branch may read the body";
  lift (Schema.Alt { left; right })

let void source = Sch.Free.map ignore source

type metadata =
  { operation_id : string option
  ; summary : string option
  ; description : string option
  ; tags : string list
  ; body_description : string option
  ; include_in_schema : bool
  }

let empty_metadata =
  { operation_id = None
  ; summary = None
  ; description = None
  ; tags = []
  ; body_description = None
  ; include_in_schema = true
  }

type response_doc =
  | Response_doc :
      { status : Http.Status.t
      ; schema : 'resp Sch.t
      }
      -> response_doc

type ('f, 'res) endpoint =
  | Endpoint :
      { method_ : Http.Method.t
      ; path : ('f, 'r) path
      ; finish : 'r -> 'res
      ; gates : unit schemaf list
      ; response : response_doc option
      ; meta : metadata
      }
      -> ('f, 'res) endpoint

let endpoint method_ path =
  Endpoint
    { method_
    ; path
    ; finish = Fun.id
    ; gates = []
    ; response = None
    ; meta = empty_metadata
    }

let get path = endpoint `GET path
let head path = endpoint `HEAD path
let post path = endpoint `POST path
let put path = endpoint `PUT path
let patch path = endpoint `PATCH path
let delete path = endpoint `DELETE path
let any path = endpoint (`Other "*") path

let gate source (Endpoint e) =
  Endpoint { e with gates = e.gates @ [ void source ] }

let response_model ~status ~schema (Endpoint e) =
  Endpoint
    { e with
      finish =
        (fun r ->
          Response.json ~status (Sch.Json.encode_string schema (e.finish r)))
    ; response = Some (Response_doc { status; schema })
    }

let map_meta f (Endpoint e) = Endpoint { e with meta = f e.meta }
let operation_id id = map_meta (fun m -> { m with operation_id = Some id })
let summary s = map_meta (fun m -> { m with summary = Some s })
let description d = map_meta (fun m -> { m with description = Some d })
let tags ts = map_meta (fun m -> { m with tags = ts })
let tag t = map_meta (fun m -> { m with tags = m.tags @ [ t ] })
let include_in_schema v = map_meta (fun m -> { m with include_in_schema = v })

type route =
  | Route :
      { endpoint : ('f, Response.t) endpoint
      ; schema : 'f schemaf
      ; middlewares : Middleware.t list
      }
      -> route
  | Scope :
      { prefix : ('a, 'a) path
      ; routes : route list
      ; gates : unit schemaf list
      ; middlewares : Middleware.t list
      }
      -> route

let handle schema endpoint = Route { endpoint; schema; middlewares = [] }
let ( >> ) endpoint schema = handle schema endpoint

let handle_request handler endpoint =
  handle (Sch.Free.map handler request) endpoint

let scope ?(middlewares = []) ?(gates = []) prefix routes =
  Scope { prefix; routes; gates; middlewares }

let recover_middleware handler next a =
  match next a with
  | b -> b
  | exception e ->
    (match handler a e with Some resp -> resp | None -> raise e)

let recover handler route =
  match route with
  | Route r ->
    Route
      { r with middlewares = r.middlewares @ [ recover_middleware handler ] }
  | Scope r ->
    Scope
      { r with middlewares = r.middlewares @ [ recover_middleware handler ] }

let[@inline] str_has_pct ~off ~len str =
  let end_pos = off + len in
  let rec loop i =
    if i >= end_pos
    then false
    else if String.unsafe_get str i = '%'
    then true
    else loop (i + 1)
  in
  loop off

let[@inline] span_equal_lit path span lit =
  if str_has_pct ~off:(Span.off span) ~len:(Span.len span) path
  then String.equal lit (Uri.pct_decode (String.sub path span.off span.len))
  else Span.equal path span lit

module Path_cursor = struct
  type t =
    { path : string
    ; pos : int
    ; len : int
    }

  let create path =
    let total_len = String.length path in
    let len = String.index_opt path '?' |> Option.value ~default:total_len in
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
        let span = Span.make ~off:t.pos ~len:(end_pos - t.pos) in
        let next_pos = if end_pos < t.len then end_pos + 1 else end_pos in
        Some (span, { t with pos = next_pos })

  let rec to_list t =
    match next t with
    | None -> []
    | Some (span, t') ->
      let segment =
        if str_has_pct ~off:(Span.off span) ~len:(Span.len span) t.path
        then Uri.pct_decode (String.sub t.path span.off span.len)
        else String.sub t.path span.off span.len
      in
      segment :: to_list t'

  let rec skip n cursor =
    if n <= 0
    then cursor
    else
      match next cursor with
      | None -> cursor
      | Some (_, rest) -> skip (n - 1) rest
end

(* Captures are parsed up front and handed back as a function, so a request with
   a malformed capture is rejected before any gate runs. *)
let rec match_path : type a b. (a, b) path -> Path_cursor.t -> (a -> b) option =
 fun pattern cursor ->
  match pattern with
  | Nil ->
    (match Path_cursor.next cursor with
    | None -> Some Fun.id
    | Some _ -> None (* Reject trailing segments *))
  | Literal ("", rest) ->
    (* Empty literal doesn't consume anything, just continue *)
    match_path rest cursor
  | Literal (expected, rest) ->
    (match Path_cursor.next cursor with
    | Some (span, rest_cursor)
      when span_equal_lit rest_cursor.path span expected ->
      match_path rest rest_cursor
    | _ -> None)
  | Splat rest ->
    let remaining = Path_cursor.to_list cursor in
    Option.map
      (fun k f -> k (f remaining))
      (match_path rest (Path_cursor.create ""))
  | Capture { parse; rest; _ } -> match_capture parse rest cursor
  | Enum { parse; rest; _ } -> match_capture parse rest cursor
  | Annotated { segment; _ } -> match_path segment cursor

and match_capture : type p a b.
  (off:int -> len:int -> string -> p option)
  -> (a, b) path
  -> Path_cursor.t
  -> ((p -> a) -> b) option
  =
 fun parse rest cursor ->
  match Path_cursor.next cursor with
  | Some (span, rest_cursor) ->
    let off = Span.off span in
    let len = Span.len span in
    let result =
      if str_has_pct ~off ~len rest_cursor.path
      then
        let decoded = Uri.pct_decode (String.sub rest_cursor.path off len) in
        parse ~off:0 ~len:(String.length decoded) decoded
      else parse ~off ~len rest_cursor.path
    in
    (match result with
    | Some v -> Option.map (fun k f -> k (f v)) (match_path rest rest_cursor)
    | None -> None)
  | None -> None

let expected_content_type : media_type -> string = function
  | Json -> "application/json"
  | Urlencoded -> "application/x-www-form-urlencoded"
  | Multipart -> "multipart/form-data"

let select_media_type :
  media_type list -> Request.t -> (media_type, string) result
  =
 fun media_types request ->
  let expected =
    String.concat ", " (List.map expected_content_type media_types)
  in
  let actual = Request.header "content-type" request in
  match actual with
  | None ->
    Error (Printf.sprintf "Missing Content-Type header, expected: %s" expected)
  | Some content_type ->
    (* Extract media type, ignoring charset and other parameters *)
    let actual_media_type =
      match String.split_on_char ';' content_type with
      | media :: _ -> String.trim media
      | [] -> content_type
    in
    (match
       List.find_opt
         (fun candidate ->
            Sch_ext.CI.equal actual_media_type (expected_content_type candidate))
         media_types
     with
    | Some media_type -> Ok media_type
    | None ->
      Error
        (Printf.sprintf
           "Unsupported Content-Type: %s, expected: %s"
           actual_media_type
           expected))

let cookies_to_json request =
  Cookies.Cookie.parse (Request.headers request)
  |> Sch_ext.Forms.group ~equal:String.equal
  |> Sch_ext.Forms.to_json

let evaluate_body_schema : type a.
  media_type -> a Sch.t -> Request.t -> (a, (string * string) list) result
  =
 fun input schema request ->
  match input with
  | Json ->
    Sch.Json.decode_reader schema (Request.body request)
    |> Sch.Validation.to_result
  | Urlencoded ->
    Sch.Json.decode
      schema
      (Request.body request
      |> Form.Urlencoded.of_body
      |> Form.Urlencoded.to_json)
    |> Sch.Validation.to_result
  | Multipart ->
    Sch_ext.Multipart_decoder.decode_request schema request
    |> Sch.Validation.to_result

module Reader = struct
  type k

  (* [query] and [cookies] hold what the effects decode from, so several effects
     reading the same source parse it once. *)
  type env =
    { request : Request.t
    ; mutable memo : Context.t
    ; mutable query : Jsont.json option
    ; mutable cookies : Jsont.json option
    }

  type 'a t = env -> 'a

  external inj : 'a t -> ('a, k) Sch.Sig.app = "%identity"
  external prj : ('a, k) Sch.Sig.app -> 'a t = "%identity"

  (* For [apply f a], [Sch.Free.run] puts the earlier effects in [f]. Evaluating
     [f] first is what runs effects in the order they were declared. *)
  let applicative : k Sch.Sig.applicative =
    { pure = (fun a -> inj (fun _ -> a))
    ; map = (fun f a -> inj (fun env -> f (prj a env)))
    ; apply =
        (fun f a ->
          inj (fun env ->
            let f = prj f env in
            f (prj a env)))
    }

  let query_json env =
    match env.query with
    | Some json -> json
    | None ->
      let json =
        Form.Urlencoded.of_query env.request |> Form.Urlencoded.to_json
      in
      env.query <- Some json;
      json

  let cookies_json env =
    match env.cookies with
    | Some json -> json
    | None ->
      let json = cookies_to_json env.request in
      env.cookies <- Some json;
      json
end

let decoded = function
  | Sch.Validation.Success v -> v
  | Sch.Validation.Error errors ->
    raise (Validation_failed (List.map Sch.error_to_pair errors))

let rec compile : type a. a schemaf -> Reader.env -> a =
 fun schema ->
  Reader.prj
    (Sch.Free.run
       Reader.applicative
       { run = (fun effect_ -> Reader.inj (compile_effect (Schema.prj effect_)))
       }
       schema)

and compile_effect : type a. a Schema.t -> Reader.env -> a = function
  | Query schema ->
    fun env -> decoded (Sch.Json.decode schema (Reader.query_json env))
  | Header schema ->
    (match Sch_ext.Declared.names schema with
    | Some names ->
      fun { Reader.request; _ } ->
        decoded
          (Sch.Json.decode
             schema
             (Sch_ext.Header.to_json_of_names names (Request.headers request)))
    | None ->
      fun { Reader.request; _ } ->
        decoded
          (Sch.Json.decode
             ~lookup:Sch.mem_ci
             schema
             (Sch_ext.Header.to_json (Request.headers request))))
  | Cookie schema ->
    fun env -> decoded (Sch.Json.decode schema (Reader.cookies_json env))
  | Body { media_types; schema } ->
    fun { Reader.request; _ } ->
      (match select_media_type media_types request with
      | Error msg -> raise (Bad_request msg)
      | Ok media_type ->
        (match evaluate_body_schema media_type schema request with
        | Ok validated_data -> validated_data
        | Error errors -> raise (Validation_failed errors)))
  | Extract extractor ->
    fun { Reader.request; _ } ->
      (match extractor request with
      | Ok v -> v
      | Error e -> raise (Extraction_failed e))
  | Secured { extractor; _ } ->
    fun { Reader.request; _ } ->
      (match extractor request with
      | Ok v -> v
      | Error e -> raise (Extraction_failed e))
  | Refine { source; check } ->
    let run = compile source in
    fun env ->
      (match check (run env) with
      | Ok v -> v
      | Error e -> raise (Extraction_failed e))
  | Fail e -> fun _ -> raise (Extraction_failed e)
  | Alt { left; right } ->
    let run_left = compile left in
    let run_right = compile right in
    fun env ->
      (try run_left env with
      | Extraction_failed _ | Validation_failed _ | Bad_request _ ->
        run_right env)
  | Memo { key; source } ->
    let run = compile source in
    fun env ->
      (match Context.find key env.memo with
      | Some v -> v
      | None ->
        let v = run env in
        env.memo <- Context.add key v env.memo;
        v)

let compile_route schema =
  let run = compile schema in
  fun request ->
    run { Reader.request; memo = Context.empty; query = None; cookies = None }

let with_gates gates schema =
  List.fold_right
    (fun gate rest -> Sch.Free.apply (Sch.Free.map (fun () x -> x) gate) rest)
    gates
    schema

module Trie = struct
  type segment =
    | Lit of string
    | Capt of
        { parser : Obj.t (* Only compared physically, to tell parsers apart *)
        ; label : string
        }
    | Splat_key

  type flat_route =
    | Flat_route :
        { method_ : Http.Method.t
        ; path : ('f, 'r) path
        ; finish : 'r -> Response.t
        ; run : Request.t -> 'f
        ; segments : segment list
        ; middlewares : Middleware.t list
        ; prefix_len : int
        }
        -> flat_route

  let flat_route_method (Flat_route { method_; _ }) = method_
  let flat_route_segments (Flat_route { segments; _ }) = segments

  module String_map = Map.Make (String)

  module Method_map = Map.Make (struct
      type t = Http.Method.t

      let method_to_int = function
        | `GET -> 0
        | `POST -> 1
        | `HEAD -> 2
        | `DELETE -> 3
        | `PATCH -> 4
        | `PUT -> 5
        | `OPTIONS -> 6
        | `TRACE -> 7
        | `CONNECT -> 8
        | `Other _ -> 9

      let compare a b =
        match a, b with
        | `Other s1, `Other s2 -> String.compare s1 s2
        | _ -> Int.compare (method_to_int a) (method_to_int b)
    end)

  let find_by_span ~path ~off ~len map =
    let segment =
      if str_has_pct ~off ~len path
      then Uri.pct_decode (String.sub path off len)
      else String.sub path off len
    in
    String_map.find_opt segment map

  type t =
    { literals : t String_map.t
    ; capture : t option
    ; splat : t option
    ; handlers : flat_route Method_map.t
    }

  let capture_segment parse type_name format_name =
    let label =
      match format_name with
      | Some format -> type_name ^ ":" ^ format
      | None -> type_name
    in
    Capt { parser = Obj.repr parse; label }

  let rec extract_path_segments : type a b. (a, b) path -> segment list =
    function
    | Nil -> []
    | Literal ("", rest) -> extract_path_segments rest
    | Literal (s, rest) -> Lit s :: extract_path_segments rest
    | Capture { parse; type_name; format_name; rest; _ } ->
      capture_segment parse type_name format_name :: extract_path_segments rest
    | Enum { parse; type_name; format_name; rest; _ } ->
      capture_segment parse type_name format_name :: extract_path_segments rest
    | Splat rest -> Splat_key :: extract_path_segments rest
    | Annotated { segment; _ } -> extract_path_segments segment

  let rec flatten_route prefix_len prefix middlewares gates route =
    match route with
    | Route { endpoint = Endpoint e; schema; middlewares = route_mws } ->
      let segments = extract_path_segments e.path in
      [ Flat_route
          { method_ = e.method_
          ; path = e.path
          ; finish = e.finish
          ; run = compile_route (with_gates (gates @ e.gates) schema)
          ; segments = prefix @ segments
          ; middlewares = middlewares @ route_mws
          ; prefix_len
          }
      ]
    | Scope
        { prefix = scope_prefix
        ; routes
        ; gates = scope_gates
        ; middlewares = scope_mws
        } ->
      let scope_segments = extract_path_segments scope_prefix in
      let new_prefix = prefix @ scope_segments in
      let new_prefix_len = prefix_len + List.length scope_segments in
      routes
      |> List.concat_map
           (flatten_route
              new_prefix_len
              new_prefix
              (middlewares @ scope_mws)
              (gates @ scope_gates))

  let flatten_routes routes =
    routes |> List.concat_map (flatten_route 0 [] [] [])

  let empty =
    { literals = String_map.empty
    ; capture = None
    ; splat = None
    ; handlers = Method_map.empty
    }

  let any_method : Http.Method.t = `Other "*"

  let describe_route (Flat_route { method_; segments; _ }) =
    let method_name =
      if method_ = any_method then "ANY" else Http.Method.to_string method_
    in
    let segment_name = function
      | Lit s -> s
      | Capt { label; _ } -> "<" ^ label ^ ">"
      | Splat_key -> "*"
    in
    method_name ^ " /" ^ String.concat "/" (List.map segment_name segments)

  let reject_conflict route other reason =
    invalid_arg
      (Printf.sprintf
         "Router: %s conflicts with %s: %s"
         (describe_route route)
         (describe_route other)
         reason)

  let methods_overlap route other =
    let a = flat_route_method route in
    let b = flat_route_method other in
    a = b || a = any_method || b = any_method

  let same_segment a b =
    match a, b with
    | Lit a, Lit b -> String.equal a b
    | Capt a, Capt b -> a.parser == b.parser
    | Splat_key, Splat_key -> true
    | (Lit _ | Capt _ | Splat_key), _ -> false

  (* A failed capture does not try another route. Reject overlaps that would
     leave a route unreachable or make its documentation ambiguous. A specific
     method may coexist with [any] when both parse the path the same way. *)
  let check_same_path route handlers =
    Method_map.iter
      (fun _ other ->
         if flat_route_method route = flat_route_method other
         then reject_conflict route other "both match the same requests"
         else if
           methods_overlap route other
           && not
                (List.equal
                   same_segment
                   (flat_route_segments route)
                   (flat_route_segments other))
         then
           reject_conflict
             route
             other
             "both match the same requests with different capture parsers, and \
              a failed capture doesn't try the next route")
      handlers

  let rec insert_route : segment list -> flat_route -> t -> t =
   fun segments route node ->
    match segments with
    | [] ->
      check_same_path route node.handlers;
      { node with
        handlers = Method_map.add (flat_route_method route) route node.handlers
      }
    | Lit s :: segs ->
      let child =
        String_map.find_opt s node.literals |> Option.value ~default:empty
      in
      let updated_child = insert_route segs route child in
      { node with literals = String_map.add s updated_child node.literals }
    | Capt _ :: segs ->
      let child = node.capture |> Option.value ~default:empty in
      let updated_child = insert_route segs route child in
      { node with capture = Some updated_child }
    | Splat_key :: segs ->
      let child = node.splat |> Option.value ~default:empty in
      let updated_child = insert_route segs route child in
      { node with splat = Some updated_child }

  let rec routes_under node =
    List.map snd (Method_map.bindings node.handlers)
    @ List.concat_map
        (fun (_, child) -> routes_under child)
        (String_map.bindings node.literals)
    @ List.concat_map
        routes_under
        (List.filter_map Fun.id [ node.capture; node.splat ])

  let rec check_captures_beside_splats node =
    (match node.capture, node.splat with
    | Some capture, Some splat ->
      let splat_routes = routes_under splat in
      List.iter
        (fun route ->
           List.iter
             (fun other ->
                if methods_overlap route other
                then
                  reject_conflict
                    route
                    other
                    "a capture that fails to parse doesn't fall back to the \
                     splat")
             splat_routes)
        (routes_under capture)
    | _ -> ());
    String_map.iter
      (fun _ child -> check_captures_beside_splats child)
      node.literals;
    Option.iter check_captures_beside_splats node.capture;
    Option.iter check_captures_beside_splats node.splat

  let rec has_segment_after_splat = function
    | [] | [ Splat_key ] -> false
    | Splat_key :: _ -> true
    | _ :: segments -> has_segment_after_splat segments

  let check_splat_is_last route =
    if has_segment_after_splat (flat_route_segments route)
    then
      invalid_arg
        (Printf.sprintf
           "Router: %s can never match: a splat takes the rest of the path"
           (describe_route route))

  let compile routes =
    let routes' = flatten_routes routes in
    List.iter check_splat_is_last routes';
    let trie =
      List.fold_left
        (fun trie route -> insert_route (flat_route_segments route) route trie)
        empty
        routes'
    in
    check_captures_beside_splats trie;
    trie

  let select_route method_ handlers =
    match Method_map.find_opt method_ handlers with
    | Some _ as route -> route
    | None ->
      let get =
        if method_ = `HEAD then Method_map.find_opt `GET handlers else None
      in
      (match get with
      | Some _ -> get
      | None -> Method_map.find_opt any_method handlers)

  let rec match_trie cursor method_ trie =
    let first_some option fallback =
      match option with Some _ -> option | None -> fallback ()
    in
    let try_splat () =
      match trie.splat with
      | Some splat_child ->
        match_trie (Path_cursor.create "") method_ splat_child
      | None -> None
    in
    match Path_cursor.next cursor with
    | None -> first_some (select_route method_ trie.handlers) try_splat
    | Some (span, rest_cursor) ->
      let try_capture () =
        match trie.capture with
        | Some capt_child ->
          first_some (match_trie rest_cursor method_ capt_child) try_splat
        | None -> try_splat ()
      in
      let off = Span.off span in
      let len = Span.len span in
      (match find_by_span ~path:rest_cursor.path ~off ~len trie.literals with
      | Some child ->
        first_some (match_trie rest_cursor method_ child) try_capture
      | None -> try_capture ())

  let rec collect_methods cursor trie acc =
    let from_splat acc =
      match trie.splat with
      | Some splat_child ->
        collect_methods (Path_cursor.create "") splat_child acc
      | None -> acc
    in
    match Path_cursor.next cursor with
    | None ->
      Method_map.fold (fun m _ acc -> Method_map.add m () acc) trie.handlers acc
      |> from_splat
    | Some (span, rest_cursor) ->
      let off = Span.off span in
      let len = Span.len span in
      let acc =
        match find_by_span ~path:rest_cursor.path ~off ~len trie.literals with
        | Some child -> collect_methods rest_cursor child acc
        | None -> acc
      in
      let acc =
        match trie.capture with
        | Some capt_child -> collect_methods rest_cursor capt_child acc
        | None -> acc
      in
      from_splat acc

  let allow_header methods =
    let methods = Method_map.remove any_method methods in
    let methods =
      if Method_map.mem `GET methods
      then Method_map.add `HEAD () methods
      else methods
    in
    Method_map.add `OPTIONS () methods
    |> Method_map.bindings
    |> List.map (fun (m, ()) -> Http.Method.to_string m)
    |> String.concat ", "

  let no_route target method_ trie =
    let methods =
      collect_methods (Path_cursor.create target) trie Method_map.empty
    in
    if Method_map.is_empty methods then raise Not_found;
    let allow = allow_header methods in
    let response =
      if method_ = `OPTIONS
      then Response.make ~status:`No_content Body.empty
      else Response.of_string ~status:`Method_not_allowed "Method Not Allowed"
    in
    Response.add_header_unless_exists "Allow" allow response

  let router' ~unauthorized trie request =
    let target = Request.target request in
    let method_ = Request.meth request in
    match match_trie (Path_cursor.create target) method_ trie with
    | Some
        (Flat_route
           { method_ = route_method
           ; path
           ; finish
           ; run
           ; middlewares
           ; prefix_len
           ; _
           }) ->
      let cursor = Path_cursor.create target |> Path_cursor.skip prefix_len in
      let service request =
        match match_path path cursor with
        | Some apply_captures -> finish (apply_captures (run request))
        | None -> raise Not_found
      in
      (* Outside the route's middlewares, so [recover] gets the first chance *)
      let response =
        try Middleware.apply_all middlewares service request with
        | Extraction_failed (Security.Unauthorized (scheme, reason)) ->
          Security.with_challenge scheme (unauthorized request scheme reason)
      in
      if method_ = `HEAD && route_method <> `HEAD
      then Response.with_ ~body:Body.empty response
      else response
    | None -> no_route target method_ trie

  let router ~unauthorized routes =
    let trie = compile routes in
    router' ~unauthorized trie
end

let match' ?(unauthorized = Security.default_unauthorized) routes =
  let trie = Trie.compile routes in
  fun request ->
    try Some (Trie.router' ~unauthorized trie request) with Not_found -> None

let of_list
      ?(not_found = Handler.not_found)
      ?(unauthorized = Security.default_unauthorized)
      route_list
  =
  let matcher = Trie.router ~unauthorized route_list in
  fun req -> try matcher req with Not_found -> not_found req
