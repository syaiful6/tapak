exception Not_found

type (_, _) path =
  | Nil : ('a, 'a) path
  | Literal : string * ('a, 'b) path -> ('a, 'b) path
  | Int : ('a, 'b) path -> (int -> 'a, 'b) path
  | Int32 : ('a, 'b) path -> (int32 -> 'a, 'b) path
  | Int64 : ('a, 'b) path -> (int64 -> 'a, 'b) path
  | String : ('a, 'b) path -> (string -> 'a, 'b) path
  | Bool : ('a, 'b) path -> (bool -> 'a, 'b) path
  | Splat : ('a, 'b) path -> (string list -> 'a, 'b) path
  | Custom :
      { parse : string -> 'param option
      ; format : 'param -> string
      ; rest : ('a, 'b) path
      }
      -> ('param -> 'a, 'b) path
  | Method : Piaf.Method.t * ('a, 'b) path -> ('a, 'b) path
  | Guard : 'g Request_guard.t * ('a, 'b) path -> ('g -> 'a, 'b) path

let int = Int Nil
let int32 = Int32 Nil
let int64 = Int64 Nil
let str = String Nil
let bool = Bool Nil
let splat = Splat Nil
let s literal = Literal (literal, Nil)

let custom : type a.
  parse:(string -> 'param option)
  -> format:('param -> string)
  -> ('param -> a, a) path
  =
 fun ~parse ~format -> Custom { parse; format; rest = Nil }

type route =
  | Route :
      { method_ : Piaf.Method.t
      ; pattern : ('a, Request.t -> Response.t) path
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

let slug =
  Custom
    { parse = (fun s -> if is_slug s then Some s else None)
    ; format = Fun.id
    ; rest = Nil
    }

let rec ( / ) : type a b c. (a, c) path -> (c, b) path -> (a, b) path =
 fun left right ->
  match left with
  | Nil -> right
  | Literal (lit, rest) -> Literal (lit, rest / right)
  | Int rest -> Int (rest / right)
  | Int32 rest -> Int32 (rest / right)
  | Int64 rest -> Int64 (rest / right)
  | String rest -> String (rest / right)
  | Bool rest -> Bool (rest / right)
  | Splat rest -> Splat (rest / right)
  | Custom { parse; format; rest } ->
    Custom { parse; format; rest = rest / right }
  | Method (m, rest) -> Method (m, rest / right)
  | Guard _ ->
    failwith
      "Guards cannot be composed with / - apply guards to complete paths \
       instead"

let get pattern = Method (`GET, pattern)
let post pattern = Method (`POST, pattern)
let put pattern = Method (`PUT, pattern)
let patch pattern = Method (`Other "PATCH", pattern)
let delete pattern = Method (`DELETE, pattern)
let head pattern = Method (`HEAD, pattern)
let any pattern = Method (`Other "*", pattern)

let ( >=> ) : type a b g. g Request_guard.t -> (a, b) path -> (g -> a, b) path =
 fun guard pattern -> Guard (guard, pattern)

let parse_int s = try Some (int_of_string s) with Failure _ -> None
let parse_int32 s = try Some (Int32.of_string s) with Failure _ -> None
let parse_int64 s = try Some (Int64.of_string s) with Failure _ -> None

let parse_bool = function
  | "true" -> Some true
  | "false" -> Some false
  | _ -> None

let split_path path =
  let only_path =
    match String.index_opt path '?' with
    | Some idx -> String.sub path 0 idx
    | None -> path
  in
  let segments = String.split_on_char '/' only_path in
  List.filter (fun s -> s <> "") segments

(* Check if a path pattern can match given segments without executing guards *)
let rec can_match_path : type a b. (a, b) path -> string list -> bool =
 fun pattern segments ->
  match pattern, segments with
  | Nil, [] -> true
  | Nil, _ :: _ -> false
  | Literal ("", rest), [] -> can_match_path rest []
  | Literal (expected, rest), seg :: segs when String.equal expected seg ->
    can_match_path rest segs
  | Int rest, seg :: segs ->
    (match parse_int seg with
    | Some _ -> can_match_path rest segs
    | None -> false)
  | Int32 rest, seg :: segs ->
    (match parse_int32 seg with
    | Some _ -> can_match_path rest segs
    | None -> false)
  | Int64 rest, seg :: segs ->
    (match parse_int64 seg with
    | Some _ -> can_match_path rest segs
    | None -> false)
  | String rest, _ :: segs -> can_match_path rest segs
  | Bool rest, seg :: segs ->
    (match parse_bool seg with
    | Some _ -> can_match_path rest segs
    | None -> false)
  | Splat rest, _ -> can_match_path rest []
  | Custom { parse; rest; _ }, seg :: segs ->
    (match parse seg with Some _ -> can_match_path rest segs | None -> false)
  | Method (_, rest), segs -> can_match_path rest segs
  | Guard (_, rest), segs -> can_match_path rest segs
  | _ -> false

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
  | Int rest, seg :: segs ->
    (match parse_int seg with
    | Some n -> match_pattern rest segs request (k n)
    | None -> None)
  | Int32 rest, seg :: segs ->
    (match parse_int32 seg with
    | Some n -> match_pattern rest segs request (k n)
    | None -> None)
  | Int64 rest, seg :: segs ->
    (match parse_int64 seg with
    | Some n -> match_pattern rest segs request (k n)
    | None -> None)
  | String rest, seg :: segs -> match_pattern rest segs request (k seg)
  | Bool rest, seg :: segs ->
    (match parse_bool seg with
    | Some b -> match_pattern rest segs request (k b)
    | None -> None)
  | Splat rest, segs -> match_pattern rest [] request (k segs)
  | Custom { parse; rest; _ }, seg :: segs ->
    (match parse seg with
    | Some v -> match_pattern rest segs request (k v)
    | None -> None)
  | Method (_, rest), segs -> match_pattern rest segs request k
  | Guard (guard, rest), segs ->
    if can_match_path rest segs
    then
      match guard request with
      | Ok value -> match_pattern rest segs request (k value)
      | Error err -> raise (Request_guard.Failed err)
    else None
  | _ -> None

let rec get_method : type a b. (a, b) path -> Piaf.Method.t option = function
  | Method (m, _) -> Some m
  | Literal (_, rest) -> get_method rest
  | Int rest -> get_method rest
  | Int32 rest -> get_method rest
  | Int64 rest -> get_method rest
  | String rest -> get_method rest
  | Bool rest -> get_method rest
  | Splat rest -> get_method rest
  | Custom { rest; _ } -> get_method rest
  | Guard (_, rest) -> get_method rest
  | Nil -> None

let ( @-> ) : type a. (a, Request.t -> Response.t) path -> a -> route =
 fun pattern handler_fn ->
  let method_ = get_method pattern |> Option.value ~default:`GET in
  Route { method_; pattern; handler = handler_fn }

let rec match_prefix : type a. (a, a) path -> string list -> string list option =
 fun pattern segments ->
  match pattern, segments with
  | Nil, segs -> Some segs
  | Literal ("", rest), segs -> match_prefix rest segs
  | Literal (expected, rest), seg :: segs when String.equal expected seg ->
    match_prefix rest segs
  | Method (_, rest), segs -> match_prefix rest segs
  | _ -> None

let rec match_route ?(middlewares = []) route segments request =
  match route with
  | Route route ->
    let method_matches =
      match route.method_ with
      | `Other "*" -> true
      | _ ->
        Piaf.Method.to_string route.method_
        = Piaf.Method.to_string (Request.meth request)
    in
    if not method_matches
    then None
    else (
      match match_pattern route.pattern segments request route.handler with
      | Some handler ->
        let service =
          Middleware.apply_all middlewares (fun req -> handler req)
        in
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
  | Int rest -> fun n -> sprintf' rest (acc ^ "/" ^ string_of_int n)
  | Int32 rest -> fun n -> sprintf' rest (acc ^ "/" ^ Int32.to_string n)
  | Int64 rest -> fun n -> sprintf' rest (acc ^ "/" ^ Int64.to_string n)
  | String rest -> fun s -> sprintf' rest (acc ^ "/" ^ s)
  | Bool rest -> fun b -> sprintf' rest (acc ^ "/" ^ string_of_bool b)
  | Splat rest ->
    fun segments ->
      let splat_path = String.concat "/" segments in
      sprintf' rest (if splat_path = "" then acc else acc ^ "/" ^ splat_path)
  | Custom { format; rest; _ } -> fun v -> sprintf' rest (acc ^ "/" ^ format v)
  | Method (_, rest) -> sprintf' rest acc
  | Guard (_, rest) -> fun _ -> sprintf' rest acc

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
    [ get (s "") @-> R.index
    ; get (s "new") @-> R.new_
    ; post (s "") @-> R.create
    ; get (R.id_path ()) @-> R.get
    ; get (R.id_path () / s "edit") @-> R.edit
    ; put (R.id_path ()) @-> R.update
    ; delete (R.id_path ()) @-> R.delete
    ]
