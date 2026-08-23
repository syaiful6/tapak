include struct
  module Fmt = Sch_schema_constraint_fmt
  include Sch_schema_constraint
end

let string_format = function Format f -> Some (Fmt.to_string f) | _ -> None

let rec to_json_schema_obj : type a. a t -> Sch_json_schema.schema_obj =
 fun constraint_ ->
  match constraint_ with
  | Min_length n -> { Sch_json_schema.empty with min_length = Some n }
  | Max_length n -> { Sch_json_schema.empty with max_length = Some n }
  | Pattern p -> { Sch_json_schema.empty with pattern = Some p }
  | Format f -> { Sch_json_schema.empty with format = Some (Fmt.to_string f) }
  | Numeric (num_ty, constraints) ->
    (match num_ty with
    | Int_ty ->
      let constraints = (constraints : int num_constraint list) in
      List.fold_left
        (fun sch constraint_ ->
           match constraint_ with
           | Min n ->
             { sch with Sch_json_schema.minimum = Some (float_of_int n) }
           | Max n -> { sch with maximum = Some (float_of_int n) }
           | Exclusive_min n ->
             { sch with exclusive_minimum = Some (float_of_int n) }
           | Exclusive_max n ->
             { sch with exclusive_maximum = Some (float_of_int n) }
           | Multiple_of n -> { sch with multiple_of = Some (float_of_int n) })
        Sch_json_schema.empty
        constraints
    | Int32_ty ->
      let constraints = (constraints : int32 num_constraint list) in
      List.fold_left
        (fun sch constraint_ ->
           match constraint_ with
           | Min n ->
             { sch with Sch_json_schema.minimum = Some (Int32.to_float n) }
           | Max n -> { sch with maximum = Some (Int32.to_float n) }
           | Exclusive_min n ->
             { sch with exclusive_minimum = Some (Int32.to_float n) }
           | Exclusive_max n ->
             { sch with exclusive_maximum = Some (Int32.to_float n) }
           | Multiple_of n -> { sch with multiple_of = Some (Int32.to_float n) })
        Sch_json_schema.empty
        constraints
    | Int64_ty ->
      let constraints = (constraints : int64 num_constraint list) in
      List.fold_left
        (fun sch constraint_ ->
           match constraint_ with
           | Min n ->
             { sch with Sch_json_schema.minimum = Some (Int64.to_float n) }
           | Max n -> { sch with maximum = Some (Int64.to_float n) }
           | Exclusive_min n ->
             { sch with exclusive_minimum = Some (Int64.to_float n) }
           | Exclusive_max n ->
             { sch with exclusive_maximum = Some (Int64.to_float n) }
           | Multiple_of n -> { sch with multiple_of = Some (Int64.to_float n) })
        Sch_json_schema.empty
        constraints
    | Float_ty ->
      let constraints = (constraints : float num_constraint list) in
      List.fold_left
        (fun sch constraint_ ->
           match constraint_ with
           | Min n -> { sch with Sch_json_schema.minimum = Some n }
           | Max n -> { sch with maximum = Some n }
           | Exclusive_min n -> { sch with exclusive_minimum = Some n }
           | Exclusive_max n -> { sch with exclusive_maximum = Some n }
           | Multiple_of n -> { sch with multiple_of = Some n })
        Sch_json_schema.empty
        constraints)
  | Min_items n -> { Sch_json_schema.empty with min_items = Some n }
  | Max_items n -> { Sch_json_schema.empty with max_items = Some n }
  | Unique_items -> { Sch_json_schema.empty with unique_items = Some true }
  | Any_of ts ->
    let schemas = List.map to_json_schema ts in
    { Sch_json_schema.empty with any_of = Some schemas }
  | All_of ts ->
    let has_complex =
      ts
      |> List.exists (fun c ->
        match c with Any_of _ | One_of _ | Not _ -> true | _ -> false)
    in
    if has_complex
    then
      { Sch_json_schema.empty with all_of = Some (List.map to_json_schema ts) }
    else
      ts
      |> List.map to_json_schema_obj
      |> List.fold_left Sch_json_schema.merge Sch_json_schema.empty
  | One_of ts ->
    let schemas = List.map to_json_schema ts in
    { Sch_json_schema.empty with one_of = Some schemas }
  | Not t ->
    let schema = to_json_schema t in
    { Sch_json_schema.empty with not_ = Some schema }

and to_json_schema : type a. a t -> Sch_json_schema.schema =
 fun constraint_ ->
  Sch_json_schema.(
    Or_bool.Schema (Or_ref.Value (to_json_schema_obj constraint_)))
