include Sch_dsl

module Union = struct
  type 'a case = 'a union_case

  let case ?(doc = "") ~tag ~inj ~proj codec =
    if tag = ""
    then invalid_arg "Sch.Union.case: tag cannot be empty"
    else Case { tag; doc; codec; inject = inj; project = proj }

  let ensure_unique_tags cases =
    let seen = Js.Set.make () in
    let check (Case c) =
      if Js.Set.has ~value:c.tag seen
      then invalid_arg "Sch.Union.define: duplicate case tag"
      else Js.Set.add ~value:c.tag seen |> ignore
    in
    List.iter check cases

  let define ?(doc = "") ?(discriminator = "type") cases =
    match cases with
    | [] -> invalid_arg "Sch.Union.define: at least one case required"
    | _ when String.equal discriminator "" ->
      invalid_arg "Sch.Union.define: discriminator cannot be empty"
    | _ ->
      ensure_unique_tags cases;
      Union { doc; discriminator; cases }
end

module Json = struct
  module Decoder = struct
    module V = Sig.Make (struct
        type 'a t = 'a Validation.t
      end)

    let validation_applicative : V.t Sig.applicative =
      { pure = (fun a -> V.inj (Validation.pure a))
      ; map = (fun f va -> V.inj (Validation.map f (V.prj va)))
      ; apply = (fun vf va -> V.inj (Validation.apply (V.prj vf) (V.prj va)))
      }

    let apply_constraint constraint_ value =
      match Constraint.apply constraint_ value with
      | Ok v -> Validation.Success v
      | Error msgs -> Validation.Error (errors msgs)

    let coerce_string : type a. a t -> string -> a Validation.t =
     fun codec s ->
      match codec with
      | Str { constraint_; _ } -> apply_constraint constraint_ s
      | Password { constraint_; _ } -> apply_constraint constraint_ s
      | Int { constraint_; _ } ->
        (match int_of_string_opt s with
        | Some i -> apply_constraint constraint_ i
        | None ->
          Validation.Error [ error (Printf.sprintf "Invalid integer: %s" s) ])
      | Int32 { constraint_; _ } ->
        (match Int32.of_string_opt s with
        | Some i -> apply_constraint constraint_ i
        | None ->
          Validation.Error [ error (Printf.sprintf "Invalid int32: %s" s) ])
      | Int64 { constraint_; _ } ->
        (match Int64.of_string_opt s with
        | Some i -> apply_constraint constraint_ i
        | None ->
          Validation.Error [ error (Printf.sprintf "Invalid int64: %s" s) ])
      | Bool _ ->
        (match Js.String.toLowerCase s with
        | "true" | "1" | "yes" | "on" -> Validation.Success true
        | "false" | "0" | "no" | "off" -> Validation.Success false
        | _ ->
          Validation.Error [ error (Printf.sprintf "Invalid boolean: %s" s) ])
      | Float { constraint_; _ } ->
        (match float_of_string_opt s with
        | Some f -> apply_constraint constraint_ f
        | None ->
          Validation.Error [ error (Printf.sprintf "Invalid number: %s" s) ])
      | Double { constraint_; _ } ->
        (match float_of_string_opt s with
        | Some f -> apply_constraint constraint_ f
        | None ->
          Validation.Error [ error (Printf.sprintf "Invalid number: %s" s) ])
      | _ -> Validation.Error [ error "Cannot coerce string to this type" ]

    let rec decode : type a. a t -> Sch_json_classify.t -> a Validation.t =
     fun codec json ->
      match codec with
      | Str { constraint_; _ } ->
        (match json with
        | `String s -> apply_constraint constraint_ s
        | _ -> Validation.Error [ error "Expected a string" ])
      | Password { constraint_; _ } ->
        (match json with
        | `String s -> apply_constraint constraint_ s
        | _ -> Validation.Error [ error "Expected a string" ])
      | Int { constraint_; _ } ->
        (match json with
        | `Int i -> apply_constraint constraint_ i
        | `Float f when Float.is_integer f ->
          apply_constraint constraint_ (int_of_float f)
        | `String s -> coerce_string codec s
        | _ -> Validation.Error [ error "Expected an integer" ])
      | Int32 { constraint_; _ } ->
        (match json with
        | `Int i -> apply_constraint constraint_ (Int32.of_int i)
        | `Float f when Float.is_integer f ->
          apply_constraint constraint_ (Int32.of_float f)
        | `String s -> coerce_string codec s
        | _ -> Validation.Error [ error "Expected an int32" ])
      | Int64 { constraint_; _ } ->
        (match json with
        | `Int i -> apply_constraint constraint_ (Int64.of_int i)
        | `Float f when Float.is_integer f ->
          apply_constraint constraint_ (Int64.of_float f)
        | `String s -> coerce_string codec s
        | _ -> Validation.Error [ error "Expected an int64" ])
      | Bool _ ->
        (match json with
        | `Bool b -> Validation.Success b
        | `String s -> coerce_string codec s
        | _ -> Validation.Error [ error "Expected a boolean" ])
      | Float { constraint_; _ } ->
        (match json with
        | `Float f -> apply_constraint constraint_ f
        | `Int i -> apply_constraint constraint_ (float_of_int i)
        | `String s -> coerce_string codec s
        | _ -> Validation.Error [ error "Expected a number" ])
      | Double { constraint_; _ } ->
        (match json with
        | `Float f -> apply_constraint constraint_ f
        | `Int i -> apply_constraint constraint_ (float_of_int i)
        | `String s -> coerce_string codec s
        | _ -> Validation.Error [ error "Expected a number" ])
      | File -> Validation.Error [ error "File cannot be decoded from JSON" ]
      | Option inner ->
        (match json with
        | `Null -> Validation.Success None
        | _ -> Validation.map Option.some (decode inner json))
      | List { item; constraint_; _ } ->
        (match json with
        | `List lst ->
          (match Validation.traverse (fun v -> decode item v) lst with
          | Success items -> apply_constraint constraint_ items
          | Error errs -> Error errs)
        | _ -> Validation.Error [ error "Expected an array" ])
      | Map { item; constraint_; _ } ->
        (match json with
        | `Assoc assoc ->
          (match
             Validation.traverse
               (fun (k, v) ->
                  match decode item v with
                  | Validation.Success a -> Success (k, a)
                  | Error errs -> Error errs)
               assoc
           with
          | Success kvs -> apply_constraint constraint_ kvs
          | Error errs -> Error errs)
        | _ -> Validation.Error [ error "Expected an object" ])
      | Object { members; unknown; _ } ->
        (match json with
        | `Assoc mems ->
          let known_fields = Js.Set.make () in
          let nat = object_nat mems known_fields in
          let result = V.prj (Free.run validation_applicative nat members) in
          (match unknown with
          | Skip -> result
          | Error_on_unknown ->
            let unknown_fields =
              List.filter
                (fun (k, _) -> not (Js.Set.has ~value:k known_fields))
                mems
            in
            (match unknown_fields, result with
            | [], _ -> result
            | keys, Validation.Error errs ->
              Validation.Error
                (errs
                @ List.map (fun (k, _) -> error ("Unknown field: " ^ k)) keys)
            | keys, Validation.Success _ ->
              Validation.Error
                (List.map (fun (k, _) -> error ("Unknown field: " ^ k)) keys)))
        | _ -> Validation.Error [ error "Expected an object" ])
      | Union { discriminator; cases; _ } ->
        decode_union discriminator cases json
      | Rec t -> decode (Lazy.force t) json
      | Iso { fwd; repr; _ } ->
        (match decode repr json with
        | Validation.Success b ->
          (match fwd b with
          | Ok a -> Validation.Success a
          | Error msgs -> Validation.Error (errors msgs))
        | Error errs -> Validation.Error errs)

    and decode_union : type a.
      string -> a union_case list -> Sch_json_classify.t -> a Validation.t
      =
     fun discriminator cases json ->
      match json with
      | `Assoc mems ->
        (match List.assoc_opt discriminator mems with
        | None ->
          Validation.Error
            (in_field discriminator [ error "Missing discriminator field" ])
        | Some (`String tag) ->
          (match find_case_by_tag tag cases with
          | None ->
            let expected = String.concat ", " (case_tags cases) in
            Validation.Error
              (in_field
                 discriminator
                 [ error
                     (Printf.sprintf
                        "Unknown discriminator value: %s. Expected one of: %s"
                        tag
                        expected)
                 ])
          | Some (Case case) ->
            let filtered =
              List.filter (fun (k, _) -> k <> discriminator) mems
            in
            let case_result =
              if is_object_codec case.codec
              then decode case.codec (`Assoc filtered)
              else
                match List.assoc_opt "value" filtered with
                | Some v -> decode case.codec v
                | None ->
                  Validation.Error
                    (in_field "value" [ error "Missing required field" ])
            in
            Validation.map case.inject case_result)
        | Some _ ->
          Validation.Error
            (in_field
               discriminator
               [ error "Discriminator field must be a string" ]))
      | _ -> Validation.Error [ error "Expected an object for union type" ]

    and object_nat : type a.
      (string * Sch_json_classify.t) list
      -> string Js.Set.t
      -> (a fieldk, V.t) Sig.nat
      =
     fun mems known ->
      { Sig.run =
          (fun (type b) (fa : (b, a fieldk) Sig.app) ->
            let field = Object.prj fa in
            Js.Set.add ~value:field.name known |> ignore;
            V.inj
              (match List.assoc_opt field.name mems with
              | Some v ->
                (match decode field.codec v with
                | Validation.Success a -> Success a
                | Error errs -> Error (in_field field.name errs))
              | None ->
                (match field.default with
                | Some d -> Validation.Success d
                | None ->
                  Error (in_field field.name [ error "Missing required field" ]))))
      }
  end

  let coerce_string = Decoder.coerce_string
  let decode codec json = Decoder.decode codec (Sch_json_classify.classify json)

  let decode_string codec s =
    match Js.Json.parseExn s with
    | json -> decode codec json
    | exception _ -> Validation.Error [ error "Invalid JSON string" ]

  module Encoder = struct
    module Fc = Sig.Make (struct
        type 'a t = (string * Sch_json_classify.t) list
      end)

    let fc_applicative : Fc.t Sig.applicative =
      { pure = (fun _ -> Fc.inj [])
      ; map = (fun _f va -> Fc.inj (Fc.prj va))
      ; apply = (fun vf va -> Fc.inj (Fc.prj va @ Fc.prj vf))
      }

    type 'a object_case =
      | Object_case :
          { members : ('o fieldk, 'o) Free.t
          ; extract : 'a -> 'o
          }
          -> 'a object_case

    let rec object_case_of_codec : type a. a t -> a object_case option =
      function
      | Object { members; _ } ->
        Some (Object_case { members; extract = Fun.id })
      | Rec t -> object_case_of_codec (Lazy.force t)
      | Iso { bwd; repr; _ } ->
        (match object_case_of_codec repr with
        | Some (Object_case data) ->
          Some
            (Object_case
               { members = data.members
               ; extract = (fun a -> data.extract (bwd a))
               })
        | None -> None)
      | _ -> None

    let rec to_json : type a. a t -> a -> Sch_json_classify.t =
     fun codec a ->
      match codec with
      | Str _ -> `String a
      | Password _ -> `String a
      | Int _ -> `Int a
      | Int32 _ -> `Int (Int32.to_int a)
      | Int64 _ -> `String (Int64.to_string a)
      | Bool _ -> `Bool a
      | Float _ -> `Float a
      | Double _ -> `Float a
      | File -> failwith "Cannot encode File to JSON"
      | Option ta -> (match a with None -> `Null | Some v -> to_json ta v)
      | List { item; _ } -> `List (List.map (to_json item) a)
      | Map { item; _ } -> `Assoc (List.map (fun (k, v) -> k, to_json item v) a)
      | Object { members; _ } ->
        `Assoc (Fc.prj @@ Free.run fc_applicative (object_member_nat a) members)
      | Union { discriminator; cases; _ } ->
        `Assoc (union_to_fields a discriminator cases)
      | Rec t -> to_json (Lazy.force t) a
      | Iso { bwd; repr; _ } -> to_json repr (bwd a)

    and union_to_fields : type a.
      a -> string -> a union_case list -> (string * Sch_json_classify.t) list
      =
     fun obj discriminator cases ->
      match find_case_for_value obj cases with
      | Some (Projected { tag; codec; payload }) ->
        (match object_case_of_codec codec with
        | Some (Object_case { members; extract }) ->
          let obj = extract payload in
          (discriminator, `String tag)
          :: Fc.prj (Free.run fc_applicative (object_member_nat obj) members)
        | None -> [ discriminator, `String tag; "value", to_json codec payload ])
      | None ->
        invalid_arg "Sch.Json.encode: value does not match any union case"

    and object_member_nat : type a. a -> (a fieldk, Fc.t) Sig.nat =
     fun obj ->
      { Sig.run =
          (fun (type b) (fa : (b, a fieldk) Sig.app) ->
            let field = Object.prj fa in
            let v = field.get obj in
            if not (field.omit v)
            then Fc.inj [ field.name, to_json field.codec v ]
            else Fc.inj [])
      }
  end

  type format =
    | Minify
    | Indent of int

  let encode codec a = Sch_json_classify.declassify (Encoder.to_json codec a)

  let encode_string ?(format = Minify) codec a =
    match format with
    | Minify -> Js.Json.stringify (encode codec a)
    | Indent n -> Js.Json.stringifyWithSpace (encode codec a) n
end
