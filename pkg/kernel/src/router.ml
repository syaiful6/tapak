exception Not_found

module type Resource = sig
  val create : Handler.t
  val delete : Handler.t
  val edit : Handler.t
  val get : Handler.t
  val index : Handler.t
  val new_ : Handler.t
  val update : Handler.t
end

type t =
  | Scope of
      { name : string
      ; routes : t list
      ; middlewares : Middleware.t list
      }
  | Route of
      { meth : Piaf.Method.t
      ; path : string
      ; handler : Handler.t
      }

let rec pp fmt (t : t) =
  match t with
  | Scope { name; routes; middlewares } ->
    Format.fprintf fmt "scope %s [" name;
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ";\n")
      Middleware.pp
      fmt
      middlewares;
    Format.fprintf fmt ";\n";
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ";\n")
      pp
      fmt
      routes;
    Format.fprintf fmt "]"
  | Route { meth; path; _ } ->
    Format.fprintf fmt "%s %S" (Piaf.Method.to_string meth) path

let remove_trailing_slash s =
  let s =
    if String.starts_with ~prefix:"/" s
    then String.sub s 1 (String.length s - 1)
    else s
  in
  let s =
    if String.ends_with ~suffix:"/" s
    then String.sub s 0 (String.length s - 2)
    else s
  in
  if String.equal s "" then "/" else s

let scope ?(middlewares = []) name routes =
  Scope { name = remove_trailing_slash name; routes; middlewares }

let route meth path handler =
  Route { meth; path = remove_trailing_slash path; handler }

let delete path handler = route `DELETE path handler
let get path handler = route `GET path handler
let head path handler = route `HEAD path handler
let post path handler = route `POST path handler
let put path handler = route `PUT path handler

let resource ?(middlewares = []) name (module R : Resource) =
  scope
    ~middlewares
    name
    [ get "/" R.index
    ; get "/new" R.new_
    ; post "/" R.create
    ; get "/:id" R.get
    ; get "/:id/edit" R.edit
    ; put "/:id" R.update
    ; delete "/:id" R.delete
    ]

module Matcher = struct
  module Method = Piaf.Method

  type t =
    | Root
    | Part of string
    | Var of string
    | Splat
    | Full_splat
    | End of Method.t * App.t

  let pp_one fmt (t : t) =
    match t with
    | Root -> Format.fprintf fmt "Root"
    | Part s -> Format.fprintf fmt "Part %S" s
    | Var s -> Format.fprintf fmt "Var %S" s
    | Splat -> Format.fprintf fmt "Splat"
    | Full_splat -> Format.fprintf fmt "Full_splat"
    | End (m, _) -> Format.fprintf fmt "End %s" (Method.to_string m)

  let pp fmt (t : t list) =
    Format.fprintf fmt "[";
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
      pp_one
      fmt
      t;
    Format.fprintf fmt "]"

  let of_path_internal ?(middlewares = []) path meth handler =
    let parts = String.split_on_char '/' path in
    let parts = match parts with "" :: _ -> parts | _ -> "" :: parts in
    (parts
    |> List.map (fun part ->
      match part with
      | "" -> Root
      | "**" -> Full_splat
      | "*" -> Splat
      | part when String.starts_with ~prefix:":" part ->
        Var (String.sub part 1 (String.length part - 1))
      | part -> Part part))
    @ [ End (meth, App.create ~middlewares ~handler ()) ]

  let rec of_router router parent =
    match router with
    | Scope { name; routes; middlewares } ->
      let accumulated = parent @ middlewares in
      let prefix = [ Root; Part name ] in
      List.concat_map
        (fun r -> List.map (fun r -> prefix @ r) (of_router r accumulated))
        routes
    | Route { meth; path; handler } ->
      [ of_path_internal ~middlewares:parent path meth handler ]

  let method_equal s1 s2 = Method.to_string s1 = Method.to_string s2

  let rec equal a b =
    match a, b with
    | End (m1, _h1) :: [], End (m2, _) :: [] when method_equal m1 m2 -> true
    | Root :: t1, Root :: t2 -> equal t1 t2
    | Part p1 :: t1, Part p2 :: t2 when String.equal p1 p2 -> equal t1 t2
    | Var v1 :: t1, Var v2 :: t2 when String.equal v1 v2 -> equal t1 t2
    | Splat :: t1, Splat :: t2 -> equal t1 t2
    | Full_splat :: t1, Full_splat :: t2 -> equal t1 t2
    | _ -> false

  let rec compress_once (matcher : t list) =
    match matcher with
    | [] -> []
    | Part "/" :: rest -> Root :: compress_once rest
    | Root :: Root :: rest -> Root :: compress_once rest
    | Root :: Part "/" :: rest -> Root :: compress_once rest
    | Part p :: Root :: rest -> Part p :: compress_once rest
    | part :: rest -> part :: compress_once rest

  let rec compress matcher =
    let matcher' = compress_once matcher in
    if equal matcher matcher' then matcher' else compress matcher'

  let of_router r = List.map compress (of_router r [])

  let of_path p m =
    compress (of_path_internal p m Piaf.Server.Handler.not_found)

  let rec try_match a b captures splat =
    match a, b with
    | End (m1, h1) :: [], End (m2, _) :: [] when method_equal m1 m2 ->
      Some (h1, captures, splat)
    | Root :: t1, Root :: t2 -> try_match t1 t2 captures splat
    | Part p1 :: t1, Part p2 :: t2 when String.equal p1 p2 ->
      try_match t1 t2 captures splat
    | Var v :: t1, Part p :: t2 ->
      let captures = (v, p) :: captures in
      try_match t1 t2 captures splat
    | Splat :: t1, Part p :: t2 ->
      let splat = p :: splat in
      try_match t1 t2 captures splat
    | [ Full_splat; End (m1, h1) ], End (m2, _) :: _ when method_equal m1 m2 ->
      Some (h1, captures, splat)
    | Full_splat :: t1, Part p :: t2 ->
      let splat = p :: splat in
      try_match (Full_splat :: t1) t2 captures splat
    | Full_splat :: t1, Root :: t2 ->
      try_match (Full_splat :: t1) t2 captures splat
    | _ -> None

  let try_match (matchers : t list list) (pattern : t list) =
    List.nth_opt
      (List.filter_map
         (fun (matcher : t list) -> try_match matcher pattern [] [])
         matchers)
      0
end

type matches =
  { params : (string * string) list
  ; splat : string list
  }

let pp_matches fmt (m : matches) =
  Format.fprintf
    fmt
    "{ params = [%a]; splat = [%a] }"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt (key, value) -> Format.fprintf fmt "(%s, %s)" key value))
    m.params
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt s -> Format.fprintf fmt "%S" s))
    m.splat

let show_matches = Format.asprintf "%a" pp_matches

let key : matches Context.key =
  Context.Key.create
    { name = Option.some "router.matches"; show = Option.some show_matches }

let route_params request =
  let env = Request.context request in
  Context.find key env |> Option.map (fun m -> m.params)

let route_splat request =
  let env = Request.context request in
  Context.find key env |> Option.map (fun m -> m.splat)

let make t request =
  let path = Matcher.of_path (Request.target request) (Request.meth request) in
  let matcher = Matcher.of_router t in
  match Matcher.try_match matcher path with
  | None -> raise Not_found
  | Some (app, params, splat) ->
    let matches = { params; splat = List.rev splat } in
    let request =
      Request.with_context
        (Context.add key matches (Request.context request))
        request
    in
    App.call app request

let router t request = make (scope "/" t) request
