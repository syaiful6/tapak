open Imports

(** shamelessly copied from anmonteiro's httpun, which is Piaf
    depends *)
module CI = struct
  (* Convert codes to upper case and compare them. This is a port of assembly
     code from the page:

     http://www.azillionmonkeys.com/qed/asmexample.html *)
  let[@inline always] char_code_equal_ci x y =
    let codes = (x lsl 8) lor y in
    let b = 0x8080 lor codes in
    let c = b - 0x6161 in
    let d = lnot (b - 0x7b7b) in
    let e = c land d land (lnot codes land 0x8080) in
    let upper = codes - (e lsr 2) in
    upper lsr 8 = upper land 0xff

  let equal x y =
    let len = String.length x in
    len = String.length y
    &&
    let equal_so_far = ref true in
    let i = ref 0 in
    while !equal_so_far && !i < len do
      let c1 = Char.code (String.unsafe_get x !i) in
      let c2 = Char.code (String.unsafe_get y !i) in
      equal_so_far := char_code_equal_ci c1 c2;
      incr i
    done;
    !equal_so_far
end

let case_tags (cases : _ Sch.union_case list) =
  List.map (fun (Sch.Case c) -> c.tag) cases

let find_case_by_tag tag cases =
  List.find_opt (fun (Sch.Case c) -> String.equal c.tag tag) cases

module Header = struct
  let to_forms h =
    let xs = Piaf.Headers.to_list h in
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun (key, value) ->
         let existing_key =
           Hashtbl.fold
             (fun k _ acc -> if CI.equal k key then Some k else acc)
             tbl
             None
         in
         match existing_key with
         | Some k ->
           let values = Hashtbl.find tbl k in
           Hashtbl.replace tbl k (value :: values)
         | None -> Hashtbl.add tbl key [ value ])
      xs;
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []

  let to_json h =
    let forms = to_forms h in
    let fields =
      List.map
        (fun (key, values) ->
           match values with
           | [] -> Jsont.Json.name key, Jsont.Json.null ()
           | [ single ] -> Jsont.Json.name key, Jsont.Json.string single
           | multiple ->
             ( Jsont.Json.name key
             , Jsont.Json.list
                 (List.map (fun v -> Jsont.Json.string v) (List.rev multiple)) ))
        forms
    in
    Jsont.Json.object' fields
end

module Validation_syntax = struct
  let ( let+ ) va f = Sch.Validation.map f va
  let ( and+ ) fa fb = Sch.Validation.(apply (map (fun a b -> a, b) fa) fb)

  let ( let* ) fa f =
    match fa with
    | Sch.Validation.Success a -> f a
    | Sch.Validation.Error e -> Sch.Validation.Error e
end

module Multipart_decoder = struct
  module V = Sch.Sig.Make (struct
      type 'a t = 'a Sch.Validation.t
    end)

  let validation_applicative : V.t Sch.Sig.applicative =
    { pure = (fun a -> V.inj (Sch.Validation.pure a))
    ; map = (fun f va -> V.inj (Sch.Validation.map f (V.prj va)))
    ; apply = (fun vf va -> V.inj (Sch.Validation.apply (V.prj vf) (V.prj va)))
    }

  let multipart_to_string : Form.Multipart.part -> string Sch.Validation.t =
   fun { body; _ } ->
    match Body.to_string body with
    | Ok s -> Sch.Validation.Success s
    | Error _ ->
      Sch.Validation.Error [ Sch.error "Can't read multipart part as string" ]

  let apply_constraint constraint_ value =
    match Sch.Constraint.apply constraint_ value with
    | Ok v -> Sch.Validation.Success v
    | Error msgs -> Sch.Validation.Error (Sch.errors msgs)

  let content_type_is_json content_type =
    let media_type =
      match String.split_on_char ~sep:';' content_type with
      | media :: _ -> String.trim media
      | [] -> content_type
    in
    CI.equal media_type "application/json"

  let rec decode : type a. a Sch.t -> Form.Multipart.node -> a Sch.Validation.t =
   fun codec node ->
    let open Validation_syntax in
    match codec with
    | Sch.Str _ ->
      (match node with
      | Form.Multipart.Part part ->
        let* str = multipart_to_string part in
        Sch.Json.coerce_string codec str
      | _ ->
        Sch.Validation.Error [ Sch.error "Expected multipart part for string" ])
    | Password _ ->
      (match node with
      | Form.Multipart.Part part ->
        let* str = multipart_to_string part in
        Sch.Json.coerce_string codec str
      | _ ->
        Sch.Validation.Error
          [ Sch.error "Expected multipart part for password" ])
    | Int _ ->
      (match node with
      | Form.Multipart.Part part ->
        let* str = multipart_to_string part in
        Sch.Json.coerce_string codec str
      | _ ->
        Sch.Validation.Error [ Sch.error "Expected multipart part for int" ])
    | Int32 _ ->
      (match node with
      | Form.Multipart.Part part ->
        let* str = multipart_to_string part in
        Sch.Json.coerce_string codec str
      | _ ->
        Sch.Validation.Error [ Sch.error "Expected multipart part for int32" ])
    | Int64 _ ->
      (match node with
      | Form.Multipart.Part part ->
        let* str = multipart_to_string part in
        Sch.Json.coerce_string codec str
      | _ ->
        Sch.Validation.Error [ Sch.error "Expected multipart part for int64" ])
    | Bool _ ->
      (match node with
      | Form.Multipart.Part part ->
        let* str = multipart_to_string part in
        Sch.Json.coerce_string codec str
      | _ ->
        Sch.Validation.Error [ Sch.error "Expected multipart part for bool" ])
    | Float _ ->
      (match node with
      | Form.Multipart.Part part ->
        let* str = multipart_to_string part in
        Sch.Json.coerce_string codec str
      | _ ->
        Sch.Validation.Error [ Sch.error "Expected multipart part for float" ])
    | Double _ ->
      (match node with
      | Form.Multipart.Part part ->
        let* str = multipart_to_string part in
        Sch.Json.coerce_string codec str
      | _ ->
        Sch.Validation.Error [ Sch.error "Expected multipart part for double" ])
    | File ->
      (match node with
      | Form.Multipart.Part part ->
        Sch.Validation.Success
          Sch.File.
            { name = part.name
            ; filename = part.filename
            ; content_type = part.content_type
            ; body = Bytesrw_util.reader_of_stream (Body.to_stream part.body)
            }
      | _ ->
        Sch.Validation.Error [ Sch.error "Expected multipart part for file" ])
    | Option ta ->
      (match decode ta node with
      | Sch.Validation.Success a -> Sch.Validation.Success (Some a)
      | Sch.Validation.Error _ -> Sch.Validation.Success None)
    | List { item; constraint_; _ } ->
      (match node with
      | Form.Multipart.Array xs ->
        let* ys = Sch.Validation.traverse (decode item) xs in
        apply_constraint constraint_ ys
      | _ ->
        Sch.Validation.Error [ Sch.error "Expected multipart array for list" ])
    | Object { members; unknown; _ } ->
      (match node with
      | Form.Multipart.Part { body; content_type; _ }
        when content_type_is_json content_type ->
        Sch.Json.decode_reader
          codec
          (Bytesrw_util.reader_of_stream (Body.to_stream body))
      | Form.Multipart.Object htbl ->
        let known = Hashtbl.create 16 in
        let nat = object_nat htbl known in
        let result = V.prj (Sch.Free.run validation_applicative nat members) in
        (match unknown with
        | Skip -> result
        | Error_on_unknown ->
          let unknown_keys =
            Seq.filter
              (fun (name, _) -> not (Hashtbl.mem known name))
              (Hashtbl.to_seq htbl)
            |> List.of_seq
          in
          (match unknown_keys, result with
          | [], _ -> result
          | keys, Sch.Validation.Error errs ->
            Sch.Validation.Error
              (errs
              @ List.map
                  (fun (k, _) ->
                     Sch.error (Printf.sprintf "Unknown field: %s" k))
                  keys)
          | keys, Sch.Validation.Success _ ->
            Sch.Validation.Error
              (List.map
                 (fun (k, _) ->
                    Sch.error (Printf.sprintf "Unknown field: %s" k))
                 keys)))
      | _ -> Sch.Validation.Error [ Sch.error "Expected object" ])
    | Union { discriminator; cases; _ } ->
      (match node with
      | Form.Multipart.Part { body; content_type; _ }
        when content_type_is_json content_type ->
        Sch.Json.decode_reader
          codec
          (Bytesrw_util.reader_of_stream (Body.to_stream body))
      | Form.Multipart.Object htbl -> decode_union discriminator cases htbl
      | _ -> Sch.Validation.Error [ Sch.error "Expected object" ])
    | Rec t -> decode (Lazy.force t) node
    | Iso { fwd; repr; _ } ->
      (match decode repr node with
      | Sch.Validation.Success b ->
        (match fwd b with
        | Ok a -> Sch.Validation.Success a
        | Error msgs -> Sch.Validation.Error (Sch.errors msgs))
      | Sch.Validation.Error errs -> Sch.Validation.Error errs)

  and decode_union : type a.
    string
    -> a Sch.union_case list
    -> (string, Form.Multipart.node) Hashtbl.t
    -> a Sch.Validation.t
    =
   fun discriminator cases node ->
    let open Validation_syntax in
    match Hashtbl.find_opt node discriminator with
    | Some (Form.Multipart.Part part) ->
      let* tag = multipart_to_string part in
      (match find_case_by_tag tag cases with
      | None ->
        let expected = String.concat ~sep:", " (case_tags cases) in
        Sch.Validation.Error
          (Sch.in_field
             discriminator
             [ Sch.error
                 (Printf.sprintf
                    "Unknown discriminator value %s (expected one of: %s)"
                    tag
                    expected)
             ])
      | Some (Sch.Case case) ->
        let case_result =
          if Sch.is_object_codec case.codec
          then (
            let filtered = Hashtbl.copy node in
            Hashtbl.remove filtered discriminator;
            decode case.codec (Form.Multipart.Object filtered))
          else
            match Hashtbl.find_opt node "value" with
            | Some v -> decode case.codec v
            | None ->
              Sch.Validation.Error
                (Sch.in_field "value" [ Sch.error "Missing required field" ])
        in
        Sch.Validation.map case.inject case_result)
    | _ ->
      Sch.Validation.Error
        (Sch.in_field discriminator [ Sch.error "Expected object" ])

  and object_nat : type a.
    (string, Form.Multipart.node) Hashtbl.t
    -> (string, unit) Hashtbl.t
    -> (a Sch.fieldk, V.t) Sch.Sig.nat
    =
   fun mems known ->
    { Sch.Sig.run =
        (fun (type b) (fa : (b, a Sch.fieldk) Sch.Sig.app) ->
          let field = Sch.Object.prj fa in
          Hashtbl.replace known field.name ();
          V.inj
            (match Hashtbl.find_opt mems field.name with
            | Some v ->
              (match decode field.codec v with
              | Sch.Validation.Success a -> Sch.Validation.Success a
              | Sch.Validation.Error errs ->
                Sch.Validation.Error (Sch.in_field field.name errs))
            | None ->
              (match field.default with
              | Some d -> Sch.Validation.Success d
              | None ->
                Sch.Validation.Error
                  (Sch.in_field
                     field.name
                     [ Sch.error "Missing required field" ]))))
    }
end
