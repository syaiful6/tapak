type t =
  [ `Null
  | `String of string
  | `Float of float
  | `Int of int
  | `Bool of bool
  | `List of t list
  | `Assoc of (string * t) list
  ]

let classify value =
  let rec decode json =
    match Js.Json.classify json with
    | JSONNull -> `Null
    | JSONString s -> `String s
    | JSONNumber n ->
      let i = int_of_float n in
      if float_of_int i = n then `Int i else `Float n
    | JSONFalse -> `Bool false
    | JSONTrue -> `Bool true
    | JSONArray arr -> `List (Array.to_list (Array.map decode arr))
    | JSONObject dict ->
      let is_undefined json = Js.typeof json = "undefined" in
      `Assoc
        (Array.fold_right
           (fun (k, v) acc ->
              if is_undefined v then acc else (k, decode v) :: acc)
           (Js.Dict.entries dict)
           [])
  in
  decode value

let declassify value =
  let rec encode = function
    | `Null -> Js.Json.null
    | `String str -> Js.Json.string str
    | `Float f -> Js.Json.number f
    | `Int i -> Js.Json.number (Js.Int.toFloat i)
    | `Bool b -> Js.Json.boolean b
    | `List li -> Js.Json.array (Array.of_list (List.map encode li))
    | `Assoc assoc ->
      Js.Json.object_
        (Js.Dict.fromList (List.map (fun (k, v) -> k, encode v) assoc))
  in
  encode value
