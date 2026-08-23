open Jest
open Expect

let decode_str_result schema str =
  Sch.Json.decode_string schema str |> Sch.Validation.to_result

let () =
  describe "decode string" (fun () ->
    test "single field" (fun () ->
      let schema = Sch.Object.(define @@ mem "name" Sch.string) in
      let result = decode_str_result schema {|{"name": "Alice"}|} in
      expect result |> toEqual (Ok "Alice"));

    test "multiple fields" (fun () ->
      let schema =
        Sch.Object.(
          define
          @@
          let+ name = mem "name" Sch.string
          and+ age = mem "age" Sch.int in
          name, age)
      in
      let result = decode_str_result schema {|{"name": "Alice", "age": 30}|} in
      expect result |> toEqual (Ok ("Alice", 30)));

    test "default field" (fun () ->
      let schema =
        Sch.Object.(
          define
          @@
          let+ name = mem "name" Sch.string
          and+ role = mem ~default:"customer" "role" Sch.string in
          name, role)
      in
      let result = decode_str_result schema {|{"name": "Bob"}|} in
      expect result |> toEqual (Ok ("Bob", "customer")));

    test "cross field validation" (fun () ->
      let schema =
        let base =
          Sch.Object.(
            define
            @@
            let+ password = mem "password" Sch.string
            and+ confirm_password = mem "confirm_password" Sch.string in
            password, confirm_password)
        in
        Sch.custom
          ~enc:(fun pwd -> pwd, pwd)
          ~dec:(fun (pwd, conf) ->
            if pwd = conf then Ok pwd else Error [ "Password do not match" ])
          base
      in
      expect
        (decode_str_result
           schema
           {|{"password": "secret", "confirm_password": "secret"}|})
      |> toEqual (Ok "secret")
      |> ignore;
      (* try invalid case *)
      expect
        (decode_str_result
           schema
           {|{"password": "secret", "confirm_password": "not_secret"}|})
      |> toEqual (Result.error [ "", "Password do not match" ])));

  describe "constraint duration" (fun () ->
    let schema_duration =
      Sch.Object.(
        define
        @@ mem
             "duration"
             Sch.(with_ ~constraint_:(Constraint.format `Duration) string))
    in
    test "duration constraint" (fun () ->
      expect
        (decode_str_result schema_duration {|{"duration": "P1Y2M3DT4H5M6S"}|})
      |> toEqual (Ok "P1Y2M3DT4H5M6S"));

    test "duration constraint 2" (fun () ->
      expect (decode_str_result schema_duration {|{"duration": "P1Y"}|})
      |> toEqual (Ok "P1Y"));

    test "duration constraint 3" (fun () ->
      expect (decode_str_result schema_duration {|{"duration": "P2M"}|})
      |> toEqual (Ok "P2M"));

    test "duration constraint 4" (fun () ->
      expect (decode_str_result schema_duration {|{"duration": "P3W"}|})
      |> toEqual (Ok "P3W"));

    test "duration constraint 5" (fun () ->
      expect (decode_str_result schema_duration {|{"duration": "P4D"}|})
      |> toEqual (Ok "P4D"));

    test "duration constraint 6" (fun () ->
      expect (decode_str_result schema_duration {|{"duration": "PT5H"}|})
      |> toEqual (Ok "PT5H"));

    test "duration constraint 7" (fun () ->
      expect (decode_str_result schema_duration {|{"duration": "PT6M"}|})
      |> toEqual (Ok "PT6M"));

    test "constraint validation" (fun () ->
      let schema =
        Sch.Object.(
          define ~kind:"user"
          @@
          let+ name =
            mem "name" Sch.(with_ ~constraint_:(Constraint.min_length 3) string)
          and+ email =
            mem
              "email"
              Sch.(with_ ~constraint_:(Constraint.format `Email) string)
          and+ age =
            mem "age" Sch.(with_ ~constraint_:(Constraint.int_range 17 20) int)
          in
          name, email, age)
      in
      let invalid_json =
        {|{"name": "Al", "email": "invalid_email", "age": 25}|}
      in
      expect (decode_str_result schema invalid_json)
      |> toEqual
           (Result.error
              [ "age", "Integer 25 exceeds maximum 20"
              ; "email", "Invalid email address"
              ; "name", "String length 2 is less than minimum 3"
              ])));

  describe "date constraint" (fun () ->
    test "standard valid date" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Date "2023-01-25" in
      expect (Result.is_ok result) |> toBe true);

    test "leap year" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Date "1900-02-29" in
      expect (Result.is_error result) |> toBe true);

    test "leap year valid" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Date "2000-02-29" in
      expect (Result.is_ok result) |> toBe true);

    test "earliest possible date" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Date "0001-01-01" in
      expect (Result.is_ok result) |> toBe true);

    test "latest possible date" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Date "9999-12-31" in
      expect (Result.is_ok result) |> toBe true);

    test "december, single digit day" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Date "2023-12-01" in
      expect (Result.is_ok result) |> toBe true);

    test "month with 30 days" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Date "2023-09-30" in
      expect (Result.is_ok result) |> toBe true);

    test "empty string should invalid" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Date "" in
      expect (Result.is_error result) |> toBe true);

    test "invalid month 13 retuurn error" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Date "2023-13-01" in
      expect (Result.is_error result) |> toBe true);

    test "invalid day 32 return error" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Date "2023-01-32" in
      expect (Result.is_error result) |> toBe true);

    test "Invalid day for february return error" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Date "2023-02-30" in
      expect (Result.is_error result) |> toBe true);

    test "missing day return error" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Date "2023-01" in
      expect (Result.is_error result) |> toBe true);

    test "completely invalid string return error" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Date "invalid-date" in
      expect (Result.is_error result) |> toBe true));

  describe "time constraint" (fun () ->
    test "is time invalid (x)" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "x" in
      expect (Result.is_error result) |> toBe true);

    test "time valid value" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "00:00:00" in
      expect (Result.is_ok result) |> toBe true);

    test "time valid value 2" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "23:59:59" in
      expect (Result.is_ok result) |> toBe true);

    test "time valid value 3" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "12:34:56" in
      expect (Result.is_ok result) |> toBe true);

    test "time valid value 4" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "12:34:56.789" in
      expect (Result.is_ok result) |> toBe true);

    test "time valid value 5" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "12:34:56Z" in
      expect (Result.is_ok result) |> toBe true);

    test "time valid value 6" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "12:34:56+01:00" in
      expect (Result.is_ok result) |> toBe true);

    test "time valid value 7" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "12:34:56-01:00" in
      expect (Result.is_ok result) |> toBe true);

    test "time valid value 8" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "12:34:56.789Z" in
      expect (Result.is_ok result) |> toBe true);

    test "hour out of range" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "24:00:00" in
      expect (Result.is_error result) |> toBe true);

    test "minute out of range" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "12:60:00" in
      expect (Result.is_error result) |> toBe true);

    test "second out of range" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "12:34:60" in
      expect (Result.is_error result) |> toBe true);

    test "missing seconds" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "12:34" in
      expect (Result.is_error result) |> toBe true);

    test "tz hour out of range" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "12:34:56+24:00" in
      expect (Result.is_error result) |> toBe true);

    test "tz minute out of range" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "12:34:56+01:60" in
      expect (Result.is_error result) |> toBe true);

    test "leap second at 23:59:60 UTC is valid" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "23:59:60" in
      expect (Result.is_ok result) |> toBe true);

    test "leap second matching 23:59:60 UTC via offset is valid" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "00:59:60+01:00" in
      expect (Result.is_ok result) |> toBe true);

    test "leap second elsewhere is invalid" (fun () ->
      let result = Sch.Constraint.Fmt.validate `Time "12:34:60" in
      expect (Result.is_error result) |> toBe true))
