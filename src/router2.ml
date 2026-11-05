exception Not_found = Router.Not_found
exception Bad_request = Router.Bad_request
exception Validation_failed = Router.Validation_failed
exception Extraction_failed = Router.Extraction_failed

type extractor_error = Router.extractor_error = ..
type 'a extractor = 'a Router.extractor

type media_type = Router.media_type =
  | Json
  | Urlencoded
  | Multipart

type ('a, 'b) path = ('a, 'b) Router.path

let int = Router.int
let int32 = Router.int32
let int64 = Router.int64
let str = Router.str
let bool = Router.bool
let splat = Router.splat
let slug = Router.slug
let custom = Router.custom
let enum = Router.enum
let s = Router.s
let ( / ) = Router.( / )
let p = Router.p
let ann = Router.ann
let sprintf = Router.sprintf

module Schema = struct
  type schemak
  type 'a schemaf = (schemak, 'a) Sch.Free.t

  type _ t =
    | Query : 'query Sch.t -> 'query t
    | Header : 'header Sch.t -> 'header t
    | Cookie : 'cookie Sch.t -> 'cookie t
    | Body :
        { media_type : media_type
        ; schema : 'v Sch.t
        }
        -> 'v t
    | Extract : 'a extractor -> 'a t
    | Refine :
        { source : 'a schemaf
        ; check : 'a -> ('b, extractor_error) result
        }
        -> 'b t
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
let body media_type schema = lift (Schema.Body { media_type; schema })
let extract extractor = lift (Schema.Extract extractor)
let request = extract Result.ok
let refine check source = lift (Schema.Refine { source; check })

(* Each call creates its own cache entry, so sharing means reusing the value *)
let memo source =
  let key = Context.Key.create { Context.name = None; show = None } in
  lift (Schema.Memo { key; source })

let map = Sch.Free.map
let void source = Sch.Free.map ignore source

type metadata = Router.metadata =
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

let ( >-> ) endpoint schema = Route { endpoint; schema; middlewares = [] }

(* The handler takes the request so it runs per request: a bare ['f] would be
   evaluated once, when the route is built, for a route with no captures. *)
let ( >> ) endpoint handler = endpoint >-> Sch.Free.map handler request

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

let validate_content_type : media_type -> Request.t -> (unit, string) result =
 fun input_type request ->
  let expected = expected_content_type input_type in
  let actual = Request.header "content-type" request in
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
    if Sch_ext.CI.equal media_type expected
    then Ok ()
    else
      Error
        (Printf.sprintf
           "Unsupported Content-Type: %s, expected: %s"
           media_type
           expected)

module Cookie_parser = struct
  let parse request =
    let header = Request.headers request in
    let cookies = Cookies.Cookie.parse header in
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

  let to_json request =
    let forms = parse request in
    let fields =
      List.map
        (fun (key, values) ->
           match values with
           | [] -> Jsont.Json.name key, Jsont.Json.null ()
           | [ single ] -> Jsont.Json.name key, Jsont.Json.string single
           | multiple ->
             ( Jsont.Json.name key
             , Jsont.Json.list (List.map Jsont.Json.string (List.rev multiple))
             ))
        forms
    in
    Jsont.Json.object' fields
end

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

  type env =
    { request : Request.t
    ; mutable memo : Context.t
    }

  type 'a t = env -> 'a

  external inj : 'a t -> ('a, k) Sch.Sig.app = "%identity"
  external prj : ('a, k) Sch.Sig.app -> 'a t = "%identity"

  (* [Sch.Free.run] folds the tail of the effect sequence into the function side
     of [apply] and the head into the argument side. Evaluating the argument
     first is what runs effects in the order they were declared. *)
  let applicative : k Sch.Sig.applicative =
    { pure = (fun a -> inj (fun _ -> a))
    ; map = (fun f a -> inj (fun env -> f (prj a env)))
    ; apply =
        (fun f a ->
          inj (fun env ->
            let x = prj a env in
            (prj f env) x))
    }
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
    fun { Reader.request; _ } ->
      decoded
        (Sch.Json.decode
           schema
           (Form.Urlencoded.of_query request |> Form.Urlencoded.to_json))
  | Header schema ->
    fun { Reader.request; _ } ->
      decoded
        (Sch.Json.decode
           ~lookup:Sch.mem_ci
           schema
           (Sch_ext.Header.to_json (Request.headers request)))
  | Cookie schema ->
    fun { Reader.request; _ } ->
      decoded (Sch.Json.decode schema (Cookie_parser.to_json request))
  | Body { media_type; schema } ->
    fun { Reader.request; _ } ->
      (match validate_content_type media_type request with
      | Error msg -> raise (Bad_request msg)
      | Ok () ->
        (match evaluate_body_schema media_type schema request with
        | Ok validated_data -> validated_data
        | Error errors -> raise (Validation_failed errors)))
  | Extract extractor ->
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
  fun request -> run { Reader.request; memo = Context.empty }

let with_gates gates schema =
  List.fold_right
    (fun gate rest -> Sch.Free.apply (Sch.Free.map (fun () x -> x) gate) rest)
    gates
    schema

module Trie = struct
  type segment =
    | Lit of string
    | Capt
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

  let rec extract_path_segments : type a b. (a, b) path -> segment list =
    function
    | Nil -> []
    | Literal ("", rest) -> extract_path_segments rest
    | Literal (s, rest) -> Lit s :: extract_path_segments rest
    | Capture { rest; _ } -> Capt :: extract_path_segments rest
    | Enum { rest; _ } -> Capt :: extract_path_segments rest
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

  let rec insert_route : segment list -> flat_route -> t -> t =
   fun segments route node ->
    match segments with
    | [] ->
      { node with
        handlers = Method_map.add (flat_route_method route) route node.handlers
      }
    | Lit s :: segs ->
      let child =
        String_map.find_opt s node.literals |> Option.value ~default:empty
      in
      let updated_child = insert_route segs route child in
      { node with literals = String_map.add s updated_child node.literals }
    | Capt :: segs ->
      let child = node.capture |> Option.value ~default:empty in
      let updated_child = insert_route segs route child in
      { node with capture = Some updated_child }
    | Splat_key :: segs ->
      let child = node.splat |> Option.value ~default:empty in
      let updated_child = insert_route segs route child in
      { node with splat = Some updated_child }

  let compile routes =
    let routes' = flatten_routes routes in
    List.fold_left
      (fun trie route -> insert_route (flat_route_segments route) route trie)
      empty
      routes'

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
    | None ->
      first_some (Method_map.find_opt method_ trie.handlers) (fun () ->
        (* Fall back to "any" method handler *)
        first_some (Method_map.find_opt (`Other "*") trie.handlers) try_splat)
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

  let router' trie request =
    let target = Request.target request in
    match
      match_trie (Path_cursor.create target) (Request.meth request) trie
    with
    | Some (Flat_route { path; finish; run; middlewares; prefix_len; _ }) ->
      let cursor = Path_cursor.create target |> Path_cursor.skip prefix_len in
      let service request =
        match match_path path cursor with
        | Some apply_captures -> finish (apply_captures (run request))
        | None -> raise Not_found
      in
      Middleware.apply_all middlewares service request
    | None -> raise Not_found

  let router routes =
    let trie = compile routes in
    router' trie
end

let match' routes =
  let trie = Trie.compile routes in
  fun request -> try Some (Trie.router' trie request) with Not_found -> None

let of_list ?(not_found = Handler.not_found) route_list =
  let matcher = Trie.router route_list in
  fun req -> try matcher req with Not_found -> not_found req
