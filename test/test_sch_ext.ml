open Tapak

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

let decode_result node =
  Sch_ext.Multipart_decoder.decode tagless_payload_schema node
  |> Sch.Validation.to_result

let test_multipart_decode_json_part () =
  let node =
    Form.Multipart.Part
      { name = "payload"
      ; filename = None
      ; content_type = "application/json"
      ; body = Form.Multipart.preload {|"hello"|}
      }
  in
  match decode_result node with
  | Ok (Text s) -> Alcotest.(check string) "text payload" "hello" s
  | Ok _ -> Alcotest.fail "Expected Text"
  | Error errs ->
    Alcotest.failf
      "Unexpected errors: %a"
      Fmt.(list (pair ~sep:comma string string))
      errs

let test_multipart_decode_json_array_part () =
  let node =
    Form.Multipart.Part
      { name = "payload"
      ; filename = None
      ; content_type = "application/json"
      ; body = Form.Multipart.preload {|["a","b"]|}
      }
  in
  match decode_result node with
  | Ok (Items xs) ->
    Alcotest.(check (list string)) "items payload" [ "a"; "b" ] xs
  | Ok _ -> Alcotest.fail "Expected Items"
  | Error errs ->
    Alcotest.failf
      "Unexpected errors: %a"
      Fmt.(list (pair ~sep:comma string string))
      errs

let test_multipart_decode_non_json_part_rejected () =
  let node =
    Form.Multipart.Part
      { name = "payload"
      ; filename = None
      ; content_type = "text/plain"
      ; body = Form.Multipart.preload "hello"
      }
  in
  match decode_result node with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Expected unsupported multipart error"

let test_multipart_decode_object_node_rejected () =
  let node = Form.Multipart.Object (Hashtbl.create 1) in
  match decode_result node with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Expected unsupported multipart error"

let tests =
  [ ( "Sch_ext: multipart tagless union"
    , [ Alcotest.test_case
          "decode tagless union from json multipart part"
          `Quick
          test_multipart_decode_json_part
      ; Alcotest.test_case
          "decode tagless union array shape from json multipart part"
          `Quick
          test_multipart_decode_json_array_part
      ; Alcotest.test_case
          "rejects non-json multipart part"
          `Quick
          test_multipart_decode_non_json_part_rejected
      ; Alcotest.test_case
          "rejects non-json multipart object node"
          `Quick
          test_multipart_decode_object_node_rejected
      ] )
  ]
