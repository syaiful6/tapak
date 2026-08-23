include struct
  module Fmt = Sch_schema_constraint_fmt
  include Sch_schema_constraint
end

let string_format = function Format f -> Some (Fmt.to_string f) | _ -> None
