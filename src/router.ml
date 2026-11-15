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

  (* Store the extractor and its OpenAPI scheme together. *)
  type 'a t =
    { scheme : scheme
    ; extractor : 'a extractor
    }

  let scheme t = t.scheme

  let verified scheme = function
    | Some v -> Ok v
    | None -> Error (Unauthorized (scheme, Invalid))

  (* Verification identifies the caller. [None] returns 401. Use [refine] for
     authorization checks. *)
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

  (* Return the scheme's WWW-Authenticate value, when defined. *)
  let challenge = function
    | Basic { realm } ->
      Some Security_basic.(string_of_challenge (Challenge.Basic realm))
    | Bearer _ -> Some Security_bearer.challenge
    | Api_key _ -> None

  let default_unauthorized _req _scheme _reason =
    Response.of_string ~status:`Unauthorized "Unauthorized"

  (* Preserve a WWW-Authenticate header set by the handler. *)
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

(* Select the decoder from Content-Type and use [schema] for every format. *)
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

(* Each call creates a cache entry. Reuse the returned value to share it. *)
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

(* Split a request target into path segments once per request. The int array [|
   off0; len0; off1; len1; ... |] indexes the target. Drop the query and empty
   segments, so [//a/] contains only [a]. Negate a segment length when it
   contains ['%']; later stages can then decode it without another scan. *)
module Tokens = struct
  let path_end target =
    let n = String.length target in
    let rec go i =
      if i >= n || String.unsafe_get target i = '?' then i else go (i + 1)
    in
    go 0

  let rec count_segments target stop i in_segment n =
    if i >= stop
    then if in_segment then n + 1 else n
    else if String.unsafe_get target i = '/'
    then
      count_segments target stop (i + 1) false (if in_segment then n + 1 else n)
    else count_segments target stop (i + 1) true n

  let of_target target =
    let stop = path_end target in
    let toks = Array.make (2 * count_segments target stop 0 false 0) 0 in
    let i = ref 0 in
    let k = ref 0 in
    while !i < stop do
      if String.unsafe_get target !i = '/'
      then incr i
      else begin
        let start = !i in
        let pct = ref false in
        while !i < stop && String.unsafe_get target !i <> '/' do
          if String.unsafe_get target !i = '%' then pct := true;
          incr i
        done;
        let len = !i - start in
        Array.unsafe_set toks !k start;
        Array.unsafe_set toks (!k + 1) (if !pct then -len else len);
        k := !k + 2
      end
    done;
    toks

  let[@inline] count toks = Array.length toks lsr 1
  let[@inline] off toks i = Array.unsafe_get toks (2 * i)
  let[@inline] raw_len toks i = Array.unsafe_get toks ((2 * i) + 1)

  let decoded target toks i =
    Uri.pct_decode (String.sub target (off toks i) (-raw_len toks i))

  let segment target toks i =
    let len = raw_len toks i in
    if len < 0
    then decoded target toks i
    else String.sub target (off toks i) len

  let rec segments_from target toks i =
    if i >= count toks
    then []
    else segment target toks i :: segments_from target toks (i + 1)

  let find_literal target toks i map =
    let len = raw_len toks i in
    if len < 0
    then Path_map.find (decoded target toks i) map
    else Path_map.find_span target ~off:(off toks i) ~len map

  let parse parse target toks i =
    let len = raw_len toks i in
    if len < 0
    then
      let d = decoded target toks i in
      parse ~off:0 ~len:(String.length d) d
    else parse ~off:(off toks i) ~len target
end

(* Build a capture reader once per route. After matching, each capture has a
   fixed segment index, so the reader need not walk the path again. [i] is the
   first segment after any scope prefix.

   For each request, the reader parses the captures and applies them to the
   handler. A parse failure raises [Not_found] before gates or middleware
   run. *)
let rec stage : type a b. int -> (a, b) path -> string -> int array -> a -> b =
 fun i -> function
  | Nil -> fun _ _ -> Fun.id
  | Literal ("", rest) -> stage i rest
  | Literal (_, rest) -> stage (i + 1) rest
  | Annotated { segment; _ } -> stage i segment
  | Capture { parse; rest; _ } -> stage_capture i parse rest
  | Enum { parse; rest; _ } -> stage_capture i parse rest
  | Splat rest ->
    (* [check_splat_is_last] rejects segments after a splat *)
    let k = stage i rest in
    fun target toks ->
      let segments = Tokens.segments_from target toks i in
      let apply_rest = k target toks in
      fun f -> apply_rest (f segments)

and stage_capture : type p a b.
  int
  -> (off:int -> len:int -> string -> p option)
  -> (a, b) path
  -> string
  -> int array
  -> (p -> a)
  -> b
  =
 fun i parse rest ->
  let k = stage (i + 1) rest in
  fun target toks ->
    match Tokens.parse parse target toks i with
    | None -> raise_notrace Not_found
    | Some v ->
      let apply_rest = k target toks in
      fun f -> apply_rest (f v)

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
    (* Ignore charset and other Content-Type parameters. *)
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

  (* Cache parsed query parameters and cookies across effects. *)
  type env =
    { request : Request.t
    ; mutable memo : Context.t
    ; mutable query : Jsont.json option
    ; mutable cookies : Jsont.json option
    }

  type 'a t = env -> 'a

  external inj : 'a t -> ('a, k) Sch.Sig.app = "%identity"
  external prj : ('a, k) Sch.Sig.app -> 'a t = "%identity"

  (* In [apply f a], [Sch.Free.run] places earlier effects in [f]. Evaluate [f]
     first to preserve declaration order. *)
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
        { parser : Obj.t (* Physical equality distinguishes parsers. *)
        ; label : string
        }
    | Splat_key

  type flat_route =
    | Flat_route :
        { method_ : Http.Method.t
        ; extract : string -> int array -> 'f -> 'r
        ; finish : 'r -> Response.t
        ; run : Request.t -> 'f
        ; segments : segment list
        ; middlewares : Middleware.t list
        }
        -> flat_route

  let flat_route_method (Flat_route { method_; _ }) = method_
  let flat_route_segments (Flat_route { segments; _ }) = segments

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

  type t =
    { literals : t Path_map.t
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
          ; extract = stage prefix_len e.path
          ; finish = e.finish
          ; run = compile_route (with_gates (gates @ e.gates) schema)
          ; segments = prefix @ segments
          ; middlewares = middlewares @ route_mws
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
    { literals = Path_map.empty
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

  (* Capture failures do not try another route. Reject overlaps that make a
     route unreachable or its documentation ambiguous. A specific method may
     coexist with [any] when both parse the path identically. *)
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
        Path_map.find s node.literals |> Option.value ~default:empty
      in
      let updated_child = insert_route segs route child in
      { node with literals = Path_map.insert s updated_child node.literals }
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
        (Path_map.bindings node.literals)
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
    Path_map.iter
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

  (* A splat consumes the remaining segments, including none. *)
  let match_splat method_ trie =
    match trie.splat with
    | Some child -> select_route method_ child.handlers
    | None -> None

  (* Try literals, then the capture, then the splat. Recursion handles
     backtracking; parse captures only after selecting a route. *)
  let rec match_trie target toks i method_ trie =
    if i >= Tokens.count toks
    then
      match select_route method_ trie.handlers with
      | Some _ as route -> route
      | None -> match_splat method_ trie
    else
      let literal =
        match Tokens.find_literal target toks i trie.literals with
        | Some child -> match_trie target toks (i + 1) method_ child
        | None -> None
      in
      match literal with
      | Some _ -> literal
      | None ->
        let capture =
          match trie.capture with
          | Some child -> match_trie target toks (i + 1) method_ child
          | None -> None
        in
        (match capture with
        | Some _ -> capture
        | None -> match_splat method_ trie)

  let add_methods handlers acc =
    Method_map.fold (fun m _ acc -> Method_map.add m () acc) handlers acc

  let rec collect_methods target toks i trie acc =
    let acc =
      match trie.splat with
      | Some child -> add_methods child.handlers acc
      | None -> acc
    in
    if i >= Tokens.count toks
    then add_methods trie.handlers acc
    else
      let acc =
        match Tokens.find_literal target toks i trie.literals with
        | Some child -> collect_methods target toks (i + 1) child acc
        | None -> acc
      in
      match trie.capture with
      | Some child -> collect_methods target toks (i + 1) child acc
      | None -> acc

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

  let no_route target toks method_ trie =
    let methods = collect_methods target toks 0 trie Method_map.empty in
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
    let toks = Tokens.of_target target in
    match match_trie target toks 0 method_ trie with
    | Some
        (Flat_route
           { method_ = route_method; extract; finish; run; middlewares; _ }) ->
      (* Treat a malformed capture as 404 before running gates or middleware. *)
      let apply_captures = extract target toks in
      let service request = finish (apply_captures (run request)) in
      (* Catch outside route middleware so [recover] handles the error first. *)
      let response =
        try Middleware.apply_all middlewares service request with
        | Extraction_failed (Security.Unauthorized (scheme, reason)) ->
          Security.with_challenge scheme (unauthorized request scheme reason)
      in
      if method_ = `HEAD && route_method <> `HEAD
      then Response.with_ ~body:Body.empty response
      else response
    | None -> no_route target toks method_ trie

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
