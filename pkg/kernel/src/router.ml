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

(* Path builder with rank-2 polymorphism

   This is the key to avoiding value restriction!
   - The build method is universally quantified over both type parameters
   - This allows the same builder to create fresh polymorphic paths each time
*)
type ('input, 'output) path_builder =
  { build : 'a. unit -> ('input, 'output) path }

let int = { build = (fun () -> Int Nil) }
let int32 = { build = (fun () -> Int32 Nil) }
let int64 = { build = (fun () -> Int64 Nil) }
let str = { build = (fun () -> String Nil) }
let bool = { build = (fun () -> Bool Nil) }
let splat = { build = (fun () -> Splat Nil) }
let s literal = { build = (fun () -> Literal (literal, Nil)) }

let custom ~parse ~format =
  { build = (fun () -> Custom { parse; format; rest = Nil }) }

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

let slug : (string -> 'a, 'a) path_builder =
  { build =
      (fun () ->
        Custom
          { parse = (fun s -> if is_slug s then Some s else None)
          ; format = Fun.id
          ; rest = Nil
          })
  }

(* Path concatenation - works on path GADT *)
let rec path_concat : type a b c. (a, c) path -> (c, b) path -> (a, b) path =
 fun left right ->
  match left with
  | Nil -> right
  | Literal (lit, rest) -> Literal (lit, path_concat rest right)
  | Int rest -> Int (path_concat rest right)
  | Int32 rest -> Int32 (path_concat rest right)
  | Int64 rest -> Int64 (path_concat rest right)
  | String rest -> String (path_concat rest right)
  | Bool rest -> Bool (path_concat rest right)
  | Splat rest -> Splat (path_concat rest right)
  | Custom { parse; format; rest } ->
    Custom { parse; format; rest = path_concat rest right }
  | Method (m, rest) -> Method (m, path_concat rest right)

(* Path builder combinator - combines two builders *)
let ( / ) : type a b c.
  (a, c) path_builder -> (c, b) path_builder -> (a, b) path_builder
  =
 fun left_builder right_builder ->
  { build =
      (fun () ->
        let left = left_builder.build () in
        let right = right_builder.build () in
        path_concat left right)
  }

(* HTTP method combinators - work with path_builder *)
let get : type a b. (a, b) path_builder -> (a, b) path_builder =
 fun builder -> { build = (fun () -> Method (`GET, builder.build ())) }

let post : type a b. (a, b) path_builder -> (a, b) path_builder =
 fun builder -> { build = (fun () -> Method (`POST, builder.build ())) }

let put : type a b. (a, b) path_builder -> (a, b) path_builder =
 fun builder -> { build = (fun () -> Method (`PUT, builder.build ())) }

let patch : type a b. (a, b) path_builder -> (a, b) path_builder =
 fun builder ->
  { build = (fun () -> Method (`Other "PATCH", builder.build ())) }

let delete : type a b. (a, b) path_builder -> (a, b) path_builder =
 fun builder -> { build = (fun () -> Method (`DELETE, builder.build ())) }

let head : type a b. (a, b) path_builder -> (a, b) path_builder =
 fun builder -> { build = (fun () -> Method (`HEAD, builder.build ())) }

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

(* Handler attachment - builds the path from builder *)
let ( @-> ) : type a. (a, Request.t -> Response.t) path_builder -> a -> route =
 fun builder handler_fn ->
  let pattern = builder.build () in
  let method_ = get_method pattern |> Option.value ~default:`GET in
  Route { method_; pattern; handler = handler_fn }

let rec match_route ?(middlewares = []) route request =
  match route with
  | Route route ->
    let method_matches =
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

(* sprintf_path works on path GADT *)
let rec sprintf_path : type a. (a, string) path -> string -> a =
 fun pattern acc ->
  match pattern with
  | Nil -> acc
  | Literal ("", rest) -> sprintf_path rest (if acc = "" then "/" else acc)
  | Literal (s, rest) -> sprintf_path rest (acc ^ "/" ^ s)
  | Int rest -> fun n -> sprintf_path rest (acc ^ "/" ^ string_of_int n)
  | Int32 rest -> fun n -> sprintf_path rest (acc ^ "/" ^ Int32.to_string n)
  | Int64 rest -> fun n -> sprintf_path rest (acc ^ "/" ^ Int64.to_string n)
  | String rest -> fun s -> sprintf_path rest (acc ^ "/" ^ s)
  | Bool rest -> fun b -> sprintf_path rest (acc ^ "/" ^ string_of_bool b)
  | Splat rest ->
    fun segments ->
      let splat_path = String.concat "/" segments in
      sprintf_path
        rest
        (if splat_path = "" then acc else acc ^ "/" ^ splat_path)
  | Custom { format; rest; _ } ->
    fun v -> sprintf_path rest (acc ^ "/" ^ format v)
  | Method (_, rest) -> sprintf_path rest acc

(* sprintf works on path_builder - builds fresh path each time! *)
let sprintf : type a. (a, string) path_builder -> a =
 fun builder ->
  let pattern = builder.build () in
  sprintf_path pattern ""

let scope ?(middlewares = []) prefix_builder routes =
  let prefix = prefix_builder.build () in
  Scope { prefix; routes; middlewares }

module type Resource = sig
  type id

  val id_path : (id -> 'a, 'a) path_builder
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
