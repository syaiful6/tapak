module Constraint = Schema_constraint

type _ input =
  | Json : Yojson.Safe.t input
  | Urlencoded : Form.Urlencoded.t input
  | Multipart : Form.Multipart.t input

type _ field =
  | Str :
      { default : string option
      ; constraint_ : string Constraint.t option
      }
      -> string field
  | Int :
      { default : int option
      ; constraint_ : int Constraint.t option
      }
      -> int field
  | Int32 :
      { default : int32 option
      ; constraint_ : int32 Constraint.t option
      }
      -> int32 field
  | Int64 :
      { default : int64 option
      ; constraint_ : int64 Constraint.t option
      }
      -> int64 field
  | Bool : { default : bool option } -> bool field
  | Float :
      { default : float option
      ; constraint_ : float Constraint.t option
      }
      -> float field
  (* Container types *)
  | Option : 'a field -> 'a option field
  | List :
      { default : 'a list option
      ; item : 'a field
      ; constraint_ : 'a list Constraint.t option
      }
      -> 'a list field
  (* Special types *)
  | File : Form.Multipart.part field
  (* Choices/enums with default selection
       (identifier, value) pairs, the return value has the actual value as well as the index in the list *)
  | Choice :
      { choices : (string * 'a) list
      ; default : 'a option
      }
      -> ('a * int) list field
  (* deep object *)
  | Object : 'a t -> 'a field

and _ t =
  | Pure : 'a -> 'a t
  | Field :
      { field : 'a field
      ; name : string
      }
      -> 'a t
  | App : (('b -> 'a) t * 'b t) -> 'a t
  | Map :
      { transform : 'b -> ('a, string list) result
      ; tree : 'b t
      }
      -> 'a t

type 'input interpreter =
  { eval : 'a. 'a t -> 'input -> ('a, string list) result }

module type FIELD_INTERPRETER = sig
  type input

  val get_input : string -> input -> (input, string list) result
  (** Extract input value by field name. If the field is missing, return a default
      empty/null value for this input type. Return Error only if the input isn't
      a valid object/container. *)

  val eval : input interpreter -> 'a field -> input -> ('a, string list) result
  (** Evaluate a field against an input. Takes a tree evaluator (interpreter) that
      can evaluate nested Object fields. *)
end

module Field = struct
  type 'a t = 'a field

  let str ?default ?constraint_ () = Str { default; constraint_ }
  let int ?default ?constraint_ () = Int { default; constraint_ }
  let int32 ?default ?constraint_ () = Int32 { default; constraint_ }
  let int64 ?default ?constraint_ () = Int64 { default; constraint_ }
  let bool ?default () = Bool { default }
  let float ?default ?constraint_ () = Float { default; constraint_ }
  let list ?default ?constraint_ item = List { default; item; constraint_ }
  let option field = Option field
  let choice ?default choices = Choice { choices; default }
  let file () = File
end

let validate : type a b. (a -> (b, string list) result) -> a t -> b t =
 fun f t ->
  match t with
  | Map { transform; tree } ->
    Map
      { transform =
          (fun x -> match transform x with Ok v -> f v | Error e -> Error e)
      ; tree
      }
  | tree -> Map { transform = f; tree }

let map : type a b. (a -> b) -> a t -> b t =
 fun f -> validate (fun v -> Ok (f v))

let return : type a. a -> a t = fun v -> Pure v
let field name field = Field { field; name }

let int ?default ?constraint_ name =
  field name (Field.int ?default ?constraint_ ())

let str ?default ?constraint_ name =
  field name (Field.str ?default ?constraint_ ())

let int32 ?default ?constraint_ name =
  field name (Field.int32 ?default ?constraint_ ())

let int64 ?default ?constraint_ name =
  field name (Field.int64 ?default ?constraint_ ())

let bool ?default name = field name (Field.bool ?default ())

let float ?default ?constraint_ name =
  field name (Field.float ?default ?constraint_ ())

let option name fld = field name (Field.option fld)

let list ?default ?constraint_ name item =
  field name (Field.list ?default ?constraint_ item)

let choice ?default name choices = field name (Field.choice ?default choices)
let file name = field name (Field.file ())
let obj name schema = field name (Object schema)

module Yojson_interpreter : FIELD_INTERPRETER with type input = Yojson.Safe.t =
struct
  type input = Yojson.Safe.t

  let get_input name json =
    match json with
    | `Assoc _ ->
      let value = Yojson.Safe.Util.member name json in
      Ok value
    | _ -> Error [ "Expected JSON object for form fields" ]

  let rec eval : type a.
    Yojson.Safe.t interpreter
    -> a field
    -> Yojson.Safe.t
    -> (a, string list) result
    =
   fun interp field json ->
    let { eval = object_eval } = interp in
    match field, json with
    | Str { default = _; constraint_ }, `String s ->
      Constraint.apply_constraint constraint_ s
    | Str { default = Some d; _ }, `Null -> Ok d
    | Str _, `Null -> Error [ "Expected string value" ]
    | Int { default = _; constraint_ }, `Int n ->
      Constraint.apply_constraint constraint_ n
    | Int { default = _; constraint_ }, `String s ->
      (match
         try Ok (int_of_string s) with
         | Failure _ -> Error [ Printf.sprintf "Invalid integer: %s" s ]
       with
      | Error _ as e -> e
      | Ok n -> Constraint.apply_constraint constraint_ n)
    | Int { default = Some d; _ }, `Null -> Ok d
    | Int _, `Null -> Error [ "Expected integer value" ]
    | Int32 { default = _; constraint_ }, `Int n ->
      Constraint.apply_constraint constraint_ (Int32.of_int n)
    | Int32 { default = _; constraint_ }, `String s ->
      (match
         try Ok (Int32.of_string s) with
         | Failure _ -> Error [ Printf.sprintf "Invalid int32: %s" s ]
       with
      | Error _ as e -> e
      | Ok n -> Constraint.apply_constraint constraint_ n)
    | Int32 { default = Some d; _ }, `Null -> Ok d
    | Int32 _, `Null -> Error [ "Expected int32 value" ]
    | Int64 { default = _; constraint_ }, `Int n ->
      Constraint.apply_constraint constraint_ (Int64.of_int n)
    | Int64 { default = Some d; _ }, `Null -> Ok d
    | Int64 { default = _; constraint_ }, `String s ->
      (match
         try Ok (Int64.of_string s) with
         | Failure _ -> Error [ Printf.sprintf "Invalid int64: %s" s ]
       with
      | Error _ as e -> e
      | Ok n -> Constraint.apply_constraint constraint_ n)
    | Int64 _, `Null -> Error [ "Expected int64 value" ]
    | Bool _, `Bool b -> Ok b
    | Bool _, `String s ->
      (match String.lowercase_ascii s with
      | "true" | "1" | "yes" | "on" -> Ok true
      | "false" | "0" | "no" | "off" -> Ok false
      | _ -> Error [ Printf.sprintf "Invalid boolean: %s" s ])
    | Bool { default = Some d }, `Null -> Ok d
    | Bool _, `Null -> Error [ "Expected boolean value" ]
    | Float { default = _; constraint_ }, `Float f ->
      Constraint.apply_constraint constraint_ f
    | Float { default = _; constraint_ }, `Int n ->
      Constraint.apply_constraint constraint_ (float_of_int n)
    | Float { default = _; constraint_ }, `String s ->
      (match
         try Ok (float_of_string s) with
         | Failure _ -> Error [ Printf.sprintf "Invalid float: %s" s ]
       with
      | Error _ as e -> e
      | Ok f -> Constraint.apply_constraint constraint_ f)
    | Float { default = Some d; _ }, `Null -> Ok d
    | Float _, `Null -> Error [ "Expected float value" ]
    | Option _, `Null -> Ok None
    | Option field, _ -> eval interp field json |> Result.map Option.some
    | List { default = Some d; _ }, `Null -> Ok d
    | List _, `Null -> Error [ "Expected list/array value" ]
    | List { item; constraint_; _ }, `List lst ->
      let rec aux acc = function
        | [] -> Ok (List.rev acc)
        | x :: xs ->
          (match eval interp item x with
          | Ok v -> aux (v :: acc) xs
          | Error e -> Error e)
      in
      (match aux [] lst with
      | Error _ as e -> e
      | Ok list_val -> Constraint.apply_constraint constraint_ list_val)
    | Choice { default = Some d; _ }, `Null -> Ok [ d, 0 ]
    | Choice { choices; _ }, `String id ->
      (match
         List.mapi
           (fun i (cid, v) -> if cid = id then Some (v, i) else None)
           choices
         |> List.filter_map Fun.id
       with
      | (v, i) :: _ -> Ok [ v, i ]
      | [] -> Error [ Printf.sprintf "Invalid choice: %s" id ])
    | Choice { choices; _ }, `List ids ->
      let rec aux acc = function
        | [] -> Ok (List.rev acc)
        | `String id :: xs ->
          (match
             List.mapi
               (fun i (cid, v) -> if cid = id then Some (v, i) else None)
               choices
             |> List.filter_map Fun.id
           with
          | (v, i) :: _ -> aux ((v, i) :: acc) xs
          | [] -> Error [ Printf.sprintf "Invalid choice: %s" id ])
        | _ -> Error [ "Choice values must be strings" ]
      in
      aux [] ids
    | Choice _, `Null -> Error [ "Expected choice value" ]
    | Object tree, json_value -> object_eval tree json_value
    | _ -> Error [ "Unsupported field type or JSON value" ]
end

module Multipart_interpreter :
  FIELD_INTERPRETER with type input = Form.Multipart.node = struct
  type input = Form.Multipart.node

  let get_input name node =
    match node with
    | Form.Multipart.Object h ->
      (match Hashtbl.find_opt h name with
      | Some value_node -> Ok value_node
      | None ->
        (* Return empty object for missing fields - let field evaluator handle defaults *)
        Ok (Form.Multipart.Object (Hashtbl.create 0)))
    | _ -> Error [ "Expected object structure" ]

  let rec eval : type a.
    Form.Multipart.node interpreter
    -> a field
    -> Form.Multipart.node
    -> (a, string list) result
    =
   fun interp field node ->
    let { eval = object_eval } = interp in
    match field, node with
    | File, Form.Multipart.Value part -> Ok part
    | Str { default = _; constraint_ }, Form.Multipart.Value { body; _ } ->
      (match Body.to_string body with
      | Ok s -> Constraint.apply_constraint constraint_ s
      | Error (`Msg msg) -> Error [ msg ]
      | Error _ -> Error [ "Failed to read field body" ])
    | Str { default = Some d; _ }, _ -> Ok d
    | Int { default = _; constraint_ }, Form.Multipart.Value { body; _ } ->
      (match Body.to_string body with
      | Ok s ->
        (match
           try Ok (int_of_string s) with
           | Failure _ -> Error [ Printf.sprintf "Invalid integer: %s" s ]
         with
        | Error _ as e -> e
        | Ok n -> Constraint.apply_constraint constraint_ n)
      | Error (`Msg msg) -> Error [ msg ]
      | Error _ -> Error [ "Failed to read field body" ])
    | Int { default = Some d; _ }, _ -> Ok d
    | Int32 { default = _; constraint_ }, Form.Multipart.Value { body; _ } ->
      (match Body.to_string body with
      | Ok s ->
        (match
           try Ok (Int32.of_string s) with
           | Failure _ -> Error [ Printf.sprintf "Invalid int32: %s" s ]
         with
        | Error _ as e -> e
        | Ok n -> Constraint.apply_constraint constraint_ n)
      | Error (`Msg msg) -> Error [ msg ]
      | Error _ -> Error [ "Failed to read field body" ])
    | Int32 { default = Some d; _ }, _ -> Ok d
    | Int64 { default = _; constraint_ }, Form.Multipart.Value { body; _ } ->
      (match Body.to_string body with
      | Ok s ->
        (match
           try Ok (Int64.of_string s) with
           | Failure _ -> Error [ Printf.sprintf "Invalid int64: %s" s ]
         with
        | Error _ as e -> e
        | Ok n -> Constraint.apply_constraint constraint_ n)
      | Error (`Msg msg) -> Error [ msg ]
      | Error _ -> Error [ "Failed to read field body" ])
    | Int64 { default = Some d; _ }, _ -> Ok d
    | Bool _, Form.Multipart.Value { body; _ } ->
      (match Body.to_string body with
      | Ok s ->
        (match String.lowercase_ascii s with
        | "true" | "1" | "yes" | "on" -> Ok true
        | "false" | "0" | "no" | "off" -> Ok false
        | _ -> Error [ Printf.sprintf "Invalid boolean: %s" s ])
      | Error (`Msg msg) -> Error [ msg ]
      | Error _ -> Error [ "Failed to read field body" ])
    | Bool { default = Some d }, _ -> Ok d
    | Float { default = _; constraint_ }, Form.Multipart.Value { body; _ } ->
      (match Body.to_string body with
      | Ok s ->
        (match
           try Ok (float_of_string s) with
           | Failure _ -> Error [ Printf.sprintf "Invalid float: %s" s ]
         with
        | Error _ as e -> e
        | Ok f -> Constraint.apply_constraint constraint_ f)
      | Error (`Msg msg) -> Error [ msg ]
      | Error _ -> Error [ "Failed to read field body" ])
    | Float { default = Some d; _ }, _ -> Ok d
    | Option inner, _ ->
      (match eval interp inner node with
      | Ok v -> Ok (Some v)
      | Error _ -> Ok None)
    | List { default = Some d; _ }, _ -> Ok d
    | List { item; constraint_; _ }, Form.Multipart.Array l ->
      let rec aux acc = function
        | [] -> Ok (List.rev acc)
        | x :: xs ->
          (match eval interp item x with
          | Ok v -> aux (v :: acc) xs
          | Error e -> Error e)
      in
      (match aux [] !l with
      | Error _ as e -> e
      | Ok list_val -> Constraint.apply_constraint constraint_ list_val)
    | Choice { default = Some d; _ }, _ -> Ok [ d, 0 ]
    | Choice { choices; _ }, Form.Multipart.Value { body; _ } ->
      (match Body.to_string body with
      | Ok id ->
        (match
           List.mapi
             (fun i (cid, v) -> if cid = id then Some (v, i) else None)
             choices
           |> List.filter_map Fun.id
         with
        | (v, i) :: _ -> Ok [ v, i ]
        | [] -> Error [ Printf.sprintf "Invalid choice: %s" id ])
      | Error (`Msg msg) -> Error [ msg ]
      | Error _ -> Error [ "Failed to read field body" ])
    | Object tree, node_value -> object_eval tree node_value
    | _ -> Error [ "Required field is missing or type mismatch" ]
end

module Header_interpreter : FIELD_INTERPRETER with type input = Yojson.Safe.t =
struct
  type input = Yojson.Safe.t

  let get_input name json =
    match json with
    | `Assoc fields ->
      let value =
        List.find_opt (fun (k, _) -> Schema_headers.CI.equal k name) fields
        |> Option.map snd
        |> Option.value ~default:`Null
      in
      Ok value
    | _ -> Error [ "Expected JSON object for header fields" ]

  let eval = Yojson_interpreter.eval
end

let rec eval_tree_internal : type a b.
  (module FIELD_INTERPRETER with type input = b)
  -> a t
  -> b
  -> (a, string list) result
  =
 fun (type b) (module FI : FIELD_INTERPRETER with type input = b) tree input ->
  let interp =
    { eval = (fun tree input -> eval_tree_internal (module FI) tree input) }
  in
  match tree with
  | Pure v -> Ok v
  | Field { field; name } ->
    (match FI.get_input name input with
    | Ok value -> FI.eval interp field value
    | Error msgs -> Error msgs)
  | App (f_tree, x_tree) ->
    (match
       ( eval_tree_internal (module FI) f_tree input
       , eval_tree_internal (module FI) x_tree input )
     with
    | Ok f, Ok x -> Ok (f x)
    | Error e1, Error e2 -> Error (e1 @ e2)
    | Error e, _ | _, Error e -> Error e)
  | Map { transform; tree } ->
    (match eval_tree_internal (module FI) tree input with
    | Error e -> Error e
    | Ok v -> (match transform v with Ok v -> Ok v | Error msgs -> Error msgs))

let rec evaluate : type a b.
  (module FIELD_INTERPRETER with type input = b)
  -> a t
  -> b
  -> (a, (string * string) list) result
  =
 fun (type b) (module FI : FIELD_INTERPRETER with type input = b) tree input ->
  match tree with
  | Pure v -> Ok v
  | Field { field; name } ->
    (match eval_tree_internal (module FI) (Field { field; name }) input with
    | Ok v -> Ok v
    | Error msgs -> Error (List.map (fun msg -> name, msg) msgs))
  | App (f_tree, x_tree) ->
    (match
       evaluate (module FI) f_tree input, evaluate (module FI) x_tree input
     with
    | Ok f, Ok x -> Ok (f x)
    | Error e1, Error e2 -> Error (e1 @ e2)
    | Error e, _ | _, Error e -> Error e)
  | Map { transform; tree } ->
    (match evaluate (module FI) tree input with
    | Error e -> Error e
    | Ok v ->
      (match transform v with
      | Ok v -> Ok v
      | Error msgs ->
        let field_name =
          match tree with Field { name; _ } -> name | _ -> ""
        in
        Error (List.map (fun msg -> field_name, msg) msgs)))

let eval : type a b. b input -> a t -> b -> (a, (string * string) list) result =
 fun input tree value ->
  match input with
  | Json -> evaluate (module Yojson_interpreter) tree value
  | Urlencoded ->
    evaluate (module Yojson_interpreter) tree (Form.Urlencoded.to_yojson value)
  | Multipart ->
    evaluate (module Multipart_interpreter) tree (Form.Multipart.to_tree value)

module Syntax = struct
  let ( <*> ) f x = App (f, x)
  let ( let+ ) x f = map f x
  let pair a b = a, b
  let ( and+ ) fa fb = App (map pair fa, fb)
end
