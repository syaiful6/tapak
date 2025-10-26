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

let get pattern = Method (`GET, pattern)
let post pattern = Method (`POST, pattern)
let put pattern = Method (`PUT, pattern)
let patch pattern = Method (`Other "PATCH", pattern)
let delete pattern = Method (`DELETE, pattern)
let head pattern = Method (`HEAD, pattern)
let any pattern = Method (`Other "*", pattern)
let parse_int s = try Some (int_of_string s) with Failure _ -> None
let parse_int32 s = try Some (Int32.of_string s) with Failure _ -> None
let parse_int64 s = try Some (Int64.of_string s) with Failure _ -> None

let parse_bool = function
  | "true" -> Some true
  | "false" -> Some false
  | _ -> None

let split_path path =
  let segments = String.split_on_char '/' path in
  List.filter (fun s -> s <> "") segments

let rec prepend_path : type a b c. (a, a) path -> (b, c) path -> (b, c) path =
 fun prefix pattern ->
  match prefix with
  | Nil -> pattern
  | Literal (s, rest) -> Literal (s, prepend_path rest pattern)
  | Method (m, rest) -> Method (m, prepend_path rest pattern)
  | Int _ | Int32 _ | Int64 _ | String _ | Bool _ | Splat _ | Custom _ ->
    failwith "Scope prefix cannot contain parameter extractors"

let rec match_pattern : type a b. (a, b) path -> string list -> a -> b option =
 fun pattern segments k ->
  match pattern, segments with
  | Nil, [] -> Some k
  | Nil, _ :: _ -> None (* Reject trailing segments *)
  | Literal ("", rest), [] -> match_pattern rest [] k
  | Literal (expected, rest), seg :: segs when String.equal expected seg ->
    match_pattern rest segs k
  | Int rest, seg :: segs ->
    (match parse_int seg with
    | Some n -> match_pattern rest segs (k n)
    | None -> None)
  | Int32 rest, seg :: segs ->
    (match parse_int32 seg with
    | Some n -> match_pattern rest segs (k n)
    | None -> None)
  | Int64 rest, seg :: segs ->
    (match parse_int64 seg with
    | Some n -> match_pattern rest segs (k n)
    | None -> None)
  | String rest, seg :: segs -> match_pattern rest segs (k seg)
  | Bool rest, seg :: segs ->
    (match parse_bool seg with
    | Some b -> match_pattern rest segs (k b)
    | None -> None)
  | Splat rest, segs -> match_pattern rest [] (k segs)
  | Custom { parse; rest; _ }, seg :: segs ->
    (match parse seg with
    | Some v -> match_pattern rest segs (k v)
    | None -> None)
  | Method (_, rest), segs -> match_pattern rest segs k
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
  | Nil -> None

let ( @-> ) : type a. (a, Request.t -> Response.t) path -> a -> route =
 fun pattern handler_fn ->
  let method_ = get_method pattern |> Option.value ~default:`GET in
  Route { method_; pattern; handler = handler_fn }

let rec match_route ?(middlewares = []) route request =
  match route with
  | Route route ->
    let method_matches =
      match route.method_ with
      | `Other "*" -> true (* Match any HTTP method *)
      | _ ->
        Piaf.Method.to_string route.method_
        = Piaf.Method.to_string (Request.meth request)
    in
    if not method_matches
    then None
    else
      let segments = split_path (Request.target request) in
      (match match_pattern route.pattern segments route.handler with
      | Some handler ->
        let service =
          Middleware.apply_all middlewares (fun req -> handler req)
        in
        Some (service request)
      | None -> None)
  | Scope scope ->
    let accumulated_middlewares = middlewares @ scope.middlewares in
    let expanded_routes =
      List.map
        (fun r ->
           match r with
           | Route route ->
             Route
               { route with pattern = prepend_path scope.prefix route.pattern }
           | Scope inner_scope ->
             Scope
               { inner_scope with
                 prefix = prepend_path scope.prefix inner_scope.prefix
               })
        scope.routes
    in
    List.find_map
      (fun route ->
         match_route ~middlewares:accumulated_middlewares route request)
      expanded_routes

let match' routes request =
  List.find_map (fun route -> match_route route request) routes

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

let sprintf pattern = sprintf' pattern ""

let scope ?(middlewares = []) prefix routes =
  Scope { prefix; routes; middlewares }

module type Resource = sig
  type id

  val id_path : (id -> 'a, 'a) path
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
    ; get R.id_path @-> R.get
    ; get (R.id_path / s "edit") @-> R.edit
    ; put R.id_path @-> R.update
    ; delete R.id_path @-> R.delete
    ]
