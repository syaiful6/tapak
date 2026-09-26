open Jest
open Expect

let decode_str_result schema str =
  Sch.Json.decode_string schema str |> Sch.Validation.to_result

let encode_str schema value = Sch.Json.encode_string schema value

let encode_roundtrip schema value =
  decode_str_result schema (encode_str schema value)

type shape =
  | Circle of float
  | Rectangle of float * float

let circle_schema =
  Sch.Object.(
    define ~unknown:Error_on_unknown @@ mem ~enc:Fun.id "radius" Sch.float)

let rectangle_schema =
  Sch.Object.(
    define ~unknown:Error_on_unknown
    @@ let+ width = mem ~enc:Stdlib.fst "width" Sch.float
       and+ height = mem ~enc:Stdlib.snd "height" Sch.float in
       width, height)

let shape_union_schema =
  Sch.Union.(
    define
      ~discriminator:"type"
      [ case
          ~tag:"circle"
          ~inj:(fun c -> Circle c)
          ~proj:(function Circle c -> Some c | _ -> None)
          circle_schema
      ; case
          ~tag:"rectangle"
          ~inj:(fun (w, h) -> Rectangle (w, h))
          ~proj:(function Rectangle (w, h) -> Some (w, h) | _ -> None)
          rectangle_schema
      ])

type tagless_payload =
  | Text of string
  | Items of string list

let tagless_payload_schema =
  Sch.Union.(
    tagless
      [ case
          ~tag:"text"
          ~inj:(fun s -> Text s)
          ~proj:(function Text s -> Some s | _ -> None)
          Sch.string
      ; case
          ~tag:"items"
          ~inj:(fun xs -> Items xs)
          ~proj:(function Items xs -> Some xs | _ -> None)
          Sch.(list string)
      ])

type dual = Dual of int

let dual_tagless_schema =
  Sch.Union.(
    tagless
      [ case
          ~tag:"even"
          ~inj:(fun n -> Dual n)
          ~proj:(function Dual n when n mod 2 = 0 -> Some n | _ -> None)
          Sch.int
      ; case
          ~tag:"any"
          ~inj:(fun n -> Dual n)
          ~proj:(function Dual n -> Some n)
          Sch.int
      ])

type loose =
  | Loose_text of string
  | Loose_other

let loose_tagless_schema =
  Sch.Union.(
    tagless
      [ case
          ~tag:"text"
          ~inj:(fun s -> Loose_text s)
          ~proj:(function Loose_text s -> Some s | _ -> None)
          Sch.string
      ])

type tree =
  { value : int
  ; children : tree list
  }

let () =
  describe "encode primitives" (fun () ->
    test "string" (fun () ->
      expect (encode_str Sch.string "hello") |> toEqual {|"hello"|});

    test "int" (fun () -> expect (encode_str Sch.int 42) |> toEqual "42");

    test "bool" (fun () -> expect (encode_str Sch.bool true) |> toEqual "true");

    test "float" (fun () ->
      expect (encode_str Sch.float 42.5) |> toEqual "42.5");

    test "list" (fun () ->
      expect (encode_str (Sch.list Sch.int) [ 1; 2; 3 ]) |> toEqual "[1,2,3]");

    test "option none as null" (fun () ->
      expect (encode_str (Sch.option Sch.int) None) |> toEqual "null");

    test "option some" (fun () ->
      expect (encode_str (Sch.option Sch.int) (Some 5)) |> toEqual "5");

    test "int64 encodes as JSON string" (fun () ->
      expect (encode_str Sch.int64 42L) |> toEqual {|"42"|});

    test "int64 large value stays exact as a string" (fun () ->
      expect (encode_str Sch.int64 9223372036854775807L)
      |> toEqual {|"9223372036854775807"|}));

  describe "encode object" (fun () ->
    test "multiple fields" (fun () ->
      let schema =
        Sch.Object.(
          define
          @@ let+ name = mem "name" ~enc:Stdlib.fst Sch.string
             and+ age = mem "age" ~enc:Stdlib.snd Sch.int in
             name, age)
      in
      expect (encode_str schema ("Alice", 30))
      |> toEqual {|{"name":"Alice","age":30}|});

    test "default field omitted when value equals default" (fun () ->
      let schema =
        Sch.Object.(
          define
          @@ let+ name = mem "name" ~enc:Stdlib.fst Sch.string
             and+ role =
               mem
                 ~enc:Stdlib.snd
                 ~default:"customer"
                 ~omit:(fun v -> v = "customer")
                 "role"
                 Sch.string
             in
             name, role)
      in
      expect (encode_str schema ("Bob", "customer"))
      |> toEqual {|{"name":"Bob"}|});

    test "default field included when value differs from default" (fun () ->
      let schema =
        Sch.Object.(
          define
          @@ let+ name = mem "name" ~enc:Stdlib.fst Sch.string
             and+ role =
               mem
                 ~enc:Stdlib.snd
                 ~default:"customer"
                 ~omit:(fun v -> v = "customer")
                 "role"
                 Sch.string
             in
             name, role)
      in
      expect (encode_str schema ("Bob", "admin"))
      |> toEqual {|{"name":"Bob","role":"admin"}|}));

  describe "encode union" (fun () ->
    test "object case merges discriminator with fields" (fun () ->
      expect (encode_str shape_union_schema (Rectangle (2., 4.)))
      |> toEqual {|{"type":"rectangle","width":2,"height":4}|});

    test "roundtrip through decode" (fun () ->
      let shapes = [ Circle 3.5; Rectangle (10., 20.) ] in
      let roundtripped =
        List.map (encode_roundtrip shape_union_schema) shapes
      in
      expect roundtripped |> toEqual (List.map Result.ok shapes)));

  describe "encode tagless union" (fun () ->
    test "encodes the scalar case with no wrapper" (fun () ->
      expect (encode_str tagless_payload_schema (Text "hello"))
      |> toEqual {|"hello"|});

    test "encodes the array case with no wrapper" (fun () ->
      expect (encode_str tagless_payload_schema (Items [ "a"; "b" ]))
      |> toEqual {|["a","b"]|});

    test "roundtrip through decode" (fun () ->
      let values = [ Text "hello"; Items [ "a"; "b"; "c" ]; Items [] ] in
      let roundtripped =
        List.map (encode_roundtrip tagless_payload_schema) values
      in
      expect roundtripped |> toEqual (List.map Result.ok values));

    test "raises on ambiguous projection match" (fun () ->
      expect (fun () -> ignore (encode_str dual_tagless_schema (Dual 4)))
      |> toThrow);

    test "encodes the unambiguous case" (fun () ->
      expect (encode_str dual_tagless_schema (Dual 3)) |> toEqual "3");

    test "raises when no case's projection matches" (fun () ->
      expect (fun () -> ignore (encode_str loose_tagless_schema Loose_other))
      |> toThrow));

  describe "tagless union constructor validation" (fun () ->
    test "rejects an empty case list" (fun () ->
      expect (fun () -> ignore (Sch.Union.tagless [])) |> toThrow);

    test "rejects duplicate tags" (fun () ->
      expect (fun () ->
        ignore
          Sch.Union.(
            tagless
              [ case
                  ~tag:"dup"
                  ~inj:(fun s -> Text s)
                  ~proj:(function Text s -> Some s | _ -> None)
                  Sch.string
              ; case
                  ~tag:"dup"
                  ~inj:(fun xs -> Items xs)
                  ~proj:(function Items xs -> Some xs | _ -> None)
                  Sch.(list string)
              ]))
      |> toThrow));

  describe "encode Iso" (fun () ->
    let bool_str =
      Sch.custom
        ~enc:(fun b -> if b then "yes" else "no")
        ~dec:(fun s ->
          match s with
          | "yes" -> Ok true
          | "no" -> Ok false
          | _ -> Error [ "invalid" ])
        Sch.string
    in
    test "encodes through bwd" (fun () ->
      expect (encode_str bool_str true) |> toEqual {|"yes"|});

    test "roundtrip" (fun () ->
      expect (encode_roundtrip bool_str false) |> toEqual (Ok false)));

  describe "encode Rec" (fun () ->
    let rec tree_codec =
      lazy
        Sch.Object.(
          define ~kind:"tree"
          @@ let+ value = mem "value" ~enc:(fun t -> t.value) Sch.int
             and+ children =
               mem
                 "children"
                 ~enc:(fun t -> t.children)
                 (Sch.list (Sch.rec' tree_codec))
             in
             { value; children })
    in
    let v =
      { value = 1
      ; children =
          [ { value = 2; children = [] }; { value = 3; children = [] } ]
      }
    in
    test "roundtrip nested recursive schema" (fun () ->
      expect (encode_roundtrip (Sch.rec' tree_codec) v) |> toEqual (Ok v)));

  describe "encode File" (fun () ->
    test "raises, files are rejected by JSON encoding" (fun () ->
      expect (fun () ->
        ignore
          (encode_str
             Sch.file
             { Sch.File.name = "f"
             ; filename = None
             ; content_type = "text/plain"
             ; body = Obj.magic Js.Json.null
             }))
      |> toThrow))
