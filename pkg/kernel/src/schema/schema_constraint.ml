type _ num_t =
  | Int_ty : int num_t
  | Int32_ty : int32 num_t
  | Int64_ty : int64 num_t
  | Float_ty : float num_t

type _ num_constraint =
  | Min : 'a -> 'a num_constraint
  | Max : 'a -> 'a num_constraint
  | Exclusive_min : 'a -> 'a num_constraint
  | Exclusive_max : 'a -> 'a num_constraint
  | Multiple_of : 'a -> 'a num_constraint

type string_format =
  [ `Email
  | `Uri
  | `Uuid
  | `Date
  | `Date_time
  | `Ipv4
  | `Ipv6
  | `Custom of string
  ]

type _ t =
  | Min_length : int -> string t
  | Max_length : int -> string t
  | Pattern : string -> string t
  | Format : string_format -> string t
  | Numeric : ('a num_t * 'a num_constraint list) -> 'a t
  | Min_items : int -> 'a list t
  | Max_items : int -> 'a list t
  | Unique_items : 'a list t
  | Any_of : 'a t list -> 'a t
  | All_of : 'a t list -> 'a t
  | One_of : 'a t list -> 'a t
  | Not : 'a t -> 'a t

let int_min n = Numeric (Int_ty, [ Min n ])
let int_max n = Numeric (Int_ty, [ Max n ])
let int_range min max = Numeric (Int_ty, [ Min min; Max max ])
let int_multiple_of n = Numeric (Int_ty, [ Multiple_of n ])
let int32_min n = Numeric (Int32_ty, [ Min n ])
let int32_max n = Numeric (Int32_ty, [ Max n ])
let int32_range min max = Numeric (Int32_ty, [ Min min; Max max ])
let int64_min n = Numeric (Int64_ty, [ Min n ])
let int64_max n = Numeric (Int64_ty, [ Max n ])
let int64_range min max = Numeric (Int64_ty, [ Min min; Max max ])
let float_min n = Numeric (Float_ty, [ Min n ])
let float_max n = Numeric (Float_ty, [ Max n ])
let float_range min max = Numeric (Float_ty, [ Min min; Max max ])
let min_length n = Min_length n
let max_length n = Max_length n
let length_range min max = All_of [ Min_length min; Max_length max ]
let pattern p = Pattern p
let format f = Format f
let min_items n = Min_items n
let max_items n = Max_items n
let unique_items = Unique_items
let any_of constraints = Any_of constraints
let all_of constraints = All_of constraints
let one_of constraints = One_of constraints
let not_ constraint_ = Not constraint_

let eval_num : type a.
  a num_t -> a num_constraint -> a -> (a, string list) result
  =
 fun ty num_constraint value ->
  match ty, num_constraint with
  | Int_ty, Min min ->
    if value < min
    then Error [ Printf.sprintf "Integer %d is less than minimum %d" value min ]
    else Ok value
  | Int_ty, Max max ->
    if value > max
    then Error [ Printf.sprintf "Integer %d exceeds maximum %d" value max ]
    else Ok value
  | Int_ty, Exclusive_min min ->
    if value <= min
    then
      Error
        [ Printf.sprintf
            "Integer %d is not greater than exclusive minimum %d"
            value
            min
        ]
    else Ok value
  | Int_ty, Exclusive_max max ->
    if value >= max
    then
      Error
        [ Printf.sprintf
            "Integer %d is not less than exclusive maximum %d"
            value
            max
        ]
    else Ok value
  | Int_ty, Multiple_of n ->
    if value mod n <> 0
    then Error [ Printf.sprintf "Integer %d is not a multiple of %d" value n ]
    else Ok value
  | Int32_ty, Min min ->
    if Int32.compare value min < 0
    then Error [ Printf.sprintf "Int32 %ld is less than minimum %ld" value min ]
    else Ok value
  | Int32_ty, Max max ->
    if Int32.compare value max > 0
    then Error [ Printf.sprintf "Int32 %ld exceeds maximum %ld" value max ]
    else Ok value
  | Int32_ty, Exclusive_min min ->
    if Int32.compare value min <= 0
    then
      Error
        [ Printf.sprintf
            "Int32 %ld is not greater than exclusive minimum %ld"
            value
            min
        ]
    else Ok value
  | Int32_ty, Exclusive_max max ->
    if Int32.compare value max >= 0
    then
      Error
        [ Printf.sprintf
            "Int32 %ld is not less than exclusive maximum %ld"
            value
            max
        ]
    else Ok value
  | Int32_ty, Multiple_of n ->
    if Int32.rem value n <> Int32.zero
    then Error [ Printf.sprintf "Int32 %ld is not a multiple of %ld" value n ]
    else Ok value
  | Int64_ty, Min min ->
    if Int64.compare value min < 0
    then Error [ Printf.sprintf "Int64 %Ld is less than minimum %Ld" value min ]
    else Ok value
  | Int64_ty, Max max ->
    if Int64.compare value max > 0
    then Error [ Printf.sprintf "Int64 %Ld exceeds maximum %Ld" value max ]
    else Ok value
  | Int64_ty, Exclusive_min min ->
    if Int64.compare value min <= 0
    then
      Error
        [ Printf.sprintf
            "Int64 %Ld is not greater than exclusive minimum %Ld"
            value
            min
        ]
    else Ok value
  | Int64_ty, Exclusive_max max ->
    if Int64.compare value max >= 0
    then
      Error
        [ Printf.sprintf
            "Int64 %Ld is not less than exclusive maximum %Ld"
            value
            max
        ]
    else Ok value
  | Int64_ty, Multiple_of n ->
    if Int64.rem value n <> Int64.zero
    then Error [ Printf.sprintf "Int64 %Ld is not a multiple of %Ld" value n ]
    else Ok value
  | Float_ty, Min min ->
    if Float.compare value min < 0
    then Error [ Printf.sprintf "Float %f is less than minimum %f" value min ]
    else Ok value
  | Float_ty, Max max ->
    if Float.compare value max > 0
    then Error [ Printf.sprintf "Float %f exceeds maximum %f" value max ]
    else Ok value
  | Float_ty, Exclusive_min min ->
    if Float.compare value min <= 0
    then
      Error
        [ Printf.sprintf
            "Float %f is not greater than exclusive minimum %f"
            value
            min
        ]
    else Ok value
  | Float_ty, Exclusive_max max ->
    if Float.compare value max >= 0
    then
      Error
        [ Printf.sprintf
            "Float %f is not less than exclusive maximum %f"
            value
            max
        ]
    else Ok value
  | Float_ty, Multiple_of n ->
    let remainder = Float.rem value n in
    (* Use a small epsilon for floating point comparison *)
    if Float.abs remainder < epsilon_float *. Float.abs n
    then Ok value
    else Error [ Printf.sprintf "Float %f is not a multiple of %f" value n ]

let rec eval : type a. a t -> a -> (a, string list) result =
 fun constraint_ value ->
  match constraint_ with
  | Min_length min_len ->
    let len = String.length value in
    if len < min_len
    then
      Error
        [ Printf.sprintf "String length %d is less than minimum %d" len min_len
        ]
    else Ok value
  | Max_length max_len ->
    let len = String.length value in
    if len > max_len
    then
      Error [ Printf.sprintf "String length %d exceeds maximum %d" len max_len ]
    else Ok value
  | Pattern pattern -> Schema_patterns.validate_pattern pattern value
  | Format format -> Schema_patterns.validate_format format value
  | Numeric (ty, constraints) ->
    let results =
      List.map
        (fun num_constraint -> eval_num ty num_constraint value)
        constraints
    in
    let errors =
      List.filter_map (function Error e -> Some e | Ok _ -> None) results
    in
    if List.length errors > 0 then Error (List.concat errors) else Ok value
  | Min_items len ->
    if List.length value < len
    then
      Error
        [ Printf.sprintf
            "List length %d is less than minimum %d"
            (List.length value)
            len
        ]
    else Ok value
  | Max_items _n ->
    if List.length value > _n
    then
      Error
        [ Printf.sprintf
            "List length %d exceeds maximum %d"
            (List.length value)
            _n
        ]
    else Ok value
  | Unique_items ->
    let tbl = Hashtbl.create (List.length value) in
    let rec check_unique = function
      | [] -> Ok value
      | x :: xs ->
        if Hashtbl.mem tbl x
        then Error [ "List contains duplicate items" ]
        else (
          Hashtbl.add tbl x ();
          check_unique xs)
    in
    check_unique value
  | Any_of constraints ->
    (* At least one constraint must pass *)
    let results = List.map (fun c -> eval c value) constraints in
    let has_success =
      List.exists (function Ok _ -> true | Error _ -> false) results
    in
    if has_success
    then Ok value
    else
      let errors =
        List.filter_map (function Error e -> Some e | Ok _ -> None) results
      in
      Error ([ "None of the constraints matched:" ] @ List.concat errors)
  | All_of constraints ->
    (* All constraints must pass *)
    let results = List.map (fun c -> eval c value) constraints in
    let errors =
      List.filter_map (function Error e -> Some e | Ok _ -> None) results
    in
    if List.length errors > 0 then Error (List.concat errors) else Ok value
  | One_of constraints ->
    (* Exactly one constraint must pass *)
    let results = List.map (fun c -> eval c value) constraints in
    let successes =
      List.filter (function Ok _ -> true | Error _ -> false) results
    in
    let success_count = List.length successes in
    if success_count = 1
    then Ok value
    else if success_count = 0
    then Error [ "None of the constraints matched (expected exactly one)" ]
    else
      Error
        [ Printf.sprintf
            "Multiple constraints matched (expected exactly one, got %d)"
            success_count
        ]
  | Not constraint_ ->
    (* Constraint must NOT pass *)
    (match eval constraint_ value with
    | Ok _ -> Error [ "Constraint should not have matched" ]
    | Error _ -> Ok value)

let apply_constraints : type a. a t list -> a -> (a, string list) result =
 fun constraints value ->
  let results = List.map (fun c -> eval c value) constraints in
  let errors =
    List.filter_map (function Error e -> Some e | Ok _ -> None) results
  in
  if List.length errors > 0 then Error (List.concat errors) else Ok value

let apply_constraint : type a. a t option -> a -> (a, string list) result =
 fun constraint_ value ->
  match constraint_ with None -> Ok value | Some c -> eval c value
