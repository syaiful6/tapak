include Sch_dsl
module Json_schema = Sch_json_schema

module Union = struct
  type 'a case = 'a union_case

  let case ?(doc = "") ~tag ~inj ~proj codec =
    if String.equal tag ""
    then invalid_arg "Sch.Union.case: tag cannot be empty"
    else Case { tag; doc; codec; inject = inj; project = proj }

  let define ?(doc = "") ?(discriminator = "type") cases =
    ensure_nonempty_cases "Sch.Union.define: at least one case required" cases;
    if String.equal discriminator ""
    then invalid_arg "Sch.Union.define: discriminator cannot be empty";
    ensure_unique_tags "Sch.Union.define: duplicate case tag" cases;
    Union { doc; discriminator; cases }

  let tagless ?(doc = "") cases =
    ensure_nonempty_cases "Sch.Union.tagless: at least one case required" cases;
    ensure_unique_tags "Sch.Union.tagless: duplicate case tag" cases;
    Tagless_union { doc; cases }
end

type mem_lookup = string -> Jsont.object' -> Jsont.json option
(** Member lookup strategies for object decoding. *)

let mem_exact : mem_lookup =
 fun name mems ->
  match Jsont.Json.find_mem name mems with
  | Some (_name, json) -> Some json
  | None -> None

let mem_ci : mem_lookup =
 fun name mems ->
  List.find_map
    (fun ((k, _), v) -> if Sch_ci.equal name k then Some v else None)
    mems

(** Is a member name considered "known" under the given lookup? For
    [Error_on_unknown] checking we need the reverse: given a key from
    the input, is it known to the schema? *)
let mem_is_known_exact known ((k, _) : Jsont.name) = Hashtbl.mem known k

let mem_is_known_ci known ((k, _) : Jsont.name) =
  Hashtbl.mem known (String.lowercase_ascii k)

module Diflist = struct
  type 'a t = 'a list -> 'a list

  let[@inline] single : 'a -> 'a t = fun x -> fun xs -> x :: xs
  let empty : 'a t = fun xs -> xs
  let[@inline] concat : 'a t -> 'a t -> 'a t = fun a b -> fun xs -> a (b xs)
  let[@inline] to_list : 'a t -> 'a list = fun dl -> dl []
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

    (** Coerce a string to a typed value. Used when the source is inherently
      string-typed (headers, query params) but the schema expects a non-string
      type. *)
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
        (match String.lowercase_ascii s with
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

    let decode ?(lookup = mem_exact) =
      let is_known =
        if lookup == mem_ci then mem_is_known_ci else mem_is_known_exact
      in
      let rec go : type a. a t -> Jsont.json -> a Validation.t =
       fun codec json ->
        match codec with
        | Str { constraint_; _ } ->
          (match json with
          | String (s, _) -> apply_constraint constraint_ s
          | _ -> Validation.Error [ error "Expected string" ])
        | Password { constraint_; _ } ->
          (match json with
          | String (s, _) -> apply_constraint constraint_ s
          | _ -> Validation.Error [ error "Expected string" ])
        | Int { constraint_; _ } ->
          (match json with
          | Number (f, _) -> apply_constraint constraint_ (Float.to_int f)
          | String (s, _) -> coerce_string codec s
          | _ -> Validation.Error [ error "Expected integer" ])
        | Int32 { constraint_; _ } ->
          (match json with
          | Number (f, _) -> apply_constraint constraint_ (Int32.of_float f)
          | String (s, _) -> coerce_string codec s
          | _ -> Validation.Error [ error "Expected int32" ])
        | Int64 { constraint_; _ } ->
          (match json with
          | Number (f, _) -> apply_constraint constraint_ (Int64.of_float f)
          | String (s, _) -> coerce_string codec s
          | _ -> Validation.Error [ error "Expected int64" ])
        | Bool _ ->
          (match json with
          | Bool (b, _) -> Validation.Success b
          | String (s, _) -> coerce_string codec s
          | _ -> Validation.Error [ error "Expected boolean" ])
        | Float { constraint_; _ } ->
          (match json with
          | Number (f, _) -> apply_constraint constraint_ f
          | String (s, _) -> coerce_string codec s
          | _ -> Validation.Error [ error "Expected number" ])
        | Double { constraint_; _ } ->
          (match json with
          | Number (f, _) -> apply_constraint constraint_ f
          | String (s, _) -> coerce_string codec s
          | _ -> Validation.Error [ error "Expected number" ])
        | File -> Validation.Error [ error "File cannot be decoded from JSON" ]
        | Option inner ->
          (match json with
          | Null _ -> Validation.Success None
          | _ -> Validation.map Option.some (go inner json))
        | List { item; constraint_; _ } ->
          (match json with
          | Array (items, _) ->
            (match Validation.traverse (go item) items with
            | Success lst -> apply_constraint constraint_ lst
            | Error errs -> Error errs)
          | _ -> Validation.Error [ error "Expected array" ])
        | Map { item; constraint_; _ } ->
          (match json with
          | Object (mems, _) ->
            let decode_one ((k, _), v) =
              match go item v with
              | Validation.Success a -> Validation.Success (k, a)
              | Validation.Error errs -> Validation.Error (in_field k errs)
            in
            (match Validation.traverse decode_one mems with
            | Success kvs -> apply_constraint constraint_ kvs
            | Error errs -> Error errs)
          | _ -> Validation.Error [ error "Expected object" ])
        | Object { members; unknown; _ } ->
          (match json with
          | Object (mems, _) ->
            let known = Hashtbl.create 16 in
            let nat = object_nat mems known in
            let result = V.prj (Free.run validation_applicative nat members) in
            (match unknown with
            | Skip -> result
            | Error_on_unknown ->
              let unknown_keys =
                List.filter (fun (name, _) -> not (is_known known name)) mems
              in
              (match unknown_keys, result with
              | [], _ -> result
              | keys, Validation.Error errs ->
                Validation.Error
                  (errs
                  @ List.map
                      (fun ((k, _), _) ->
                         error (Printf.sprintf "Unknown field: %s" k))
                      keys)
              | keys, Validation.Success _ ->
                Validation.Error
                  (List.map
                     (fun ((k, _), _) ->
                        error (Printf.sprintf "Unknown field: %s" k))
                     keys)))
          | _ -> Validation.Error [ error "Expected object" ])
        | Union { discriminator; cases; _ } -> go_union discriminator cases json
        | Tagless_union { cases; _ } -> go_tagless_union cases json
        | Rec t -> go (Lazy.force t) json
        | Iso { fwd; repr; _ } ->
          (match go repr json with
          | Validation.Success b ->
            (match fwd b with
            | Ok a -> Validation.Success a
            | Error msgs -> Validation.Error (errors msgs))
          | Validation.Error errs -> Validation.Error errs)
      and go_union : type a.
        string -> a union_case list -> Jsont.json -> a Validation.t
        =
       fun discriminator cases json ->
        match json with
        | Object (mems, meta) ->
          (match lookup discriminator mems with
          | None ->
            Validation.Error
              (in_field discriminator [ error "Missing discriminator field" ])
          | Some (String (tag, _)) ->
            (match find_case_by_tag tag cases with
            | None ->
              let expected = String.concat ", " (case_tags cases) in
              Validation.Error
                (in_field
                   discriminator
                   [ error
                       (Printf.sprintf
                          "Unknown discriminator value %s (expected one of: %s)"
                          tag
                          expected)
                   ])
            | Some (Case case) ->
              let filtered =
                List.filter
                  (fun ((k, _), _) -> not (String.equal k discriminator))
                  mems
              in
              let case_result =
                if is_object_codec case.codec
                then go case.codec (Object (filtered, meta))
                else
                  match lookup "value" filtered with
                  | Some v -> go case.codec v
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
        | _ -> Validation.Error [ error "Expected object" ]
      and go_tagless_union : type a.
        a union_case list -> Jsont.json -> a Validation.t
        =
       fun cases json ->
        let attempts =
          List.map
            (fun (Case c) ->
               match go c.codec json with
               | Validation.Success v -> Either.Left (c.tag, c.inject v)
               | Validation.Error errs -> Either.Right (c.tag, errs))
            cases
        in
        let matches =
          List.filter_map
            (function Either.Left m -> Some m | _ -> None)
            attempts
        in
        match matches with
        | [ (_, v) ] -> Validation.Success v
        | [] ->
          let errs =
            List.concat_map
              (function
                | Either.Right (tag, errs) -> in_field tag errs
                | Either.Left _ -> [])
              attempts
          in
          Validation.Error errs
        | many ->
          Validation.Error
            [ error
                (Printf.sprintf
                   "Ambiguous tagless union: matched cases %s"
                   (String.concat ", " (List.map fst many)))
            ]
      and object_nat : type a.
        Jsont.object' -> (string, unit) Hashtbl.t -> (a fieldk, V.t) Sig.nat
        =
       fun mems known ->
        { Sig.run =
            (fun (type b) (fa : (b, a fieldk) Sig.app) ->
              let field = Object.prj fa in
              Hashtbl.replace known field.name ();
              V.inj
                (match lookup field.name mems with
                | Some v ->
                  (match go field.codec v with
                  | Validation.Success a -> Validation.Success a
                  | Validation.Error errs ->
                    Validation.Error (in_field field.name errs))
                | None ->
                  (match field.default with
                  | Some d -> Validation.Success d
                  | None ->
                    Validation.Error
                      (in_field field.name [ error "Missing required field" ]))))
        }
      in
      go

    let decode_string ?lookup codec s =
      match Jsont_bytesrw.decode_string Jsont.json s with
      | Error msg -> Validation.Error [ error msg ]
      | Ok json -> decode ?lookup codec json

    let decode_reader ?lookup codec reader =
      match Jsont_bytesrw.decode Jsont.json reader with
      | Error msg -> Validation.Error [ error msg ]
      | Ok json -> decode ?lookup codec json
  end

  module Encoder = struct
    (* based on jsont bytesrw encoder by Daniel Bünzli *)

    open Bytesrw

    type 'a codec = 'a t

    type format =
      | Minify
      | Indent of int

    type t =
      { writer : Bytes.Writer.t
      ; buf : Bytes.t (* Buffer for slice *)
      ; buf_max : int (* Max index in buf *)
      ; mutable buf_next : int (* Next writable index in buf *)
      ; format : format
      }

    module U = Sig.Make (struct
        type 'a t = unit
      end)

    let unit_applicative : U.t Sig.applicative =
      { pure = (fun _ -> U.inj ())
      ; map = (fun _ _ -> U.inj ())
      ; apply = (fun _ _ -> U.inj ())
      }

    let make ?buf ?(format = Minify) writer =
      let buf =
        match buf with
        | Some b -> b
        | None -> Bytes.create (Bytes.Writer.slice_length writer)
      in
      let len = Bytes.length buf in
      let buf_max = len - 1 in
      { writer; buf; buf_max; buf_next = 0; format }

    let[@inline] rem_len t = t.buf_max - t.buf_next + 1

    let flush t =
      Bytes.Writer.write
        t.writer
        (Bytes.Slice.make t.buf ~first:0 ~length:t.buf_next);
      t.buf_next <- 0

    let write_eod ~eod t =
      flush t;
      if eod then Bytes.Writer.write_eod t.writer

    let write_char t c =
      if t.buf_next > t.buf_max then flush t;
      Stdlib.Bytes.set t.buf t.buf_next c;
      t.buf_next <- t.buf_next + 1

    let rec write_substring t s first length =
      if length = 0
      then ()
      else
        let len = Int.min (rem_len t) length in
        if len = 0
        then (
          flush t;
          write_substring t s first length)
        else begin
          Bytes.blit_string s first t.buf t.buf_next len;
          t.buf_next <- t.buf_next + len;
          write_substring t s (first + len) (length - len)
        end

    let write_bytes t s = write_substring t s 0 (String.length s)
    let write_sep t = write_char t ','

    let write_indent t ~nest =
      for _ = 1 to nest do
        write_char t ' '
      done

    let write_newline t =
      match t.format with Minify -> () | Indent _ -> write_char t '\n'

    let indent_step t = match t.format with Minify -> 0 | Indent n -> n

    let write_colon_sep w =
      write_char w ':';
      match w.format with Minify -> () | Indent _ -> write_char w ' '

    let write_json_null t = write_bytes t "null"
    let write_json_bool t b = write_bytes t (if b then "true" else "false")

    let write_json_float t f =
      if Float.is_finite f
      then write_bytes t (Printf.sprintf "%.7g" f)
      else write_json_null t

    let write_json_double t f =
      if Float.is_finite f
      then write_bytes t (Printf.sprintf "%.15g" f)
      else write_json_null t

    let write_json_string t s =
      let is_control = function
        | '\x00' .. '\x1F' | '\x7F' -> true
        | _ -> false
      in
      let len = String.length s in
      let flush t start i max =
        if start <= max then write_substring t s start (i - start)
      in
      let rec loop start i max =
        if i > max
        then flush t start i max
        else
          let next = i + 1 in
          match String.unsafe_get s i with
          | '\"' ->
            flush t start i max;
            write_bytes t "\\\"";
            loop next next max
          | '\\' ->
            flush t start i max;
            write_bytes t "\\\\";
            loop next next max
          | '\n' ->
            flush t start i max;
            write_bytes t "\\n";
            loop next next max
          | '\r' ->
            flush t start i max;
            write_bytes t "\\r";
            loop next next max
          | '\t' ->
            flush t start i max;
            write_bytes t "\\t";
            loop next next max
          | c when is_control c ->
            flush t start i max;
            write_bytes t "\\u";
            write_bytes t (Printf.sprintf "%04x" (Char.code c));
            loop next next max
          | _ -> loop start next max
      in
      write_char t '"';
      loop 0 0 (len - 1);
      write_char t '"'

    type 'a object_case =
      | Object_case :
          { members : ('o fieldk, 'o) Free.t
          ; extract : 'a -> 'o
          }
          -> 'a object_case

    let rec object_case_of_codec : type a. a codec -> a object_case option =
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

    let rec is_object_item : type a. a codec -> bool = function
      | Object _ -> true
      | Map _ -> true
      | Rec t -> is_object_item (Lazy.force t)
      | Iso { repr; _ } -> is_object_item repr
      | _ -> false

    let rec write_value : type a. t -> nest:int -> a codec -> a -> unit =
     fun w ~nest t a ->
      match t with
      | Str _ -> write_json_string w a
      | Password _ -> write_json_string w a
      | Int _ -> write_bytes w (Int.to_string a)
      | Int32 _ -> write_bytes w (Int32.to_string a)
      | Int64 _ -> write_json_string w (Int64.to_string a)
      | Bool _ -> write_json_bool w a
      | Float _ -> write_json_float w a
      | Double _ -> write_json_double w a
      | File -> failwith "Cannot encode File to JSON"
      | Option ta ->
        (match a with
        | None -> write_json_null w
        | Some v -> write_value w ~nest ta v)
      | List { item; _ } ->
        write_char w '[';
        (match a with
        | [] -> ()
        | _ when not (is_object_item item) ->
          let rec loop = function
            | [] -> ()
            | [ x ] -> write_value w ~nest item x
            | x :: xs ->
              write_value w ~nest item x;
              write_sep w;
              (match w.format with
              | Minify -> ()
              | Indent _ -> write_char w ' ');
              loop xs
          in
          loop a
        | _ ->
          let inner = nest + indent_step w in
          let rec loop = function
            | [] -> ()
            | [ x ] ->
              write_newline w;
              write_indent w ~nest:inner;
              write_value w ~nest:inner item x
            | x :: xs ->
              write_newline w;
              write_indent w ~nest:inner;
              write_value w ~nest:inner item x;
              write_sep w;
              loop xs
          in
          loop a;
          write_newline w;
          write_indent w ~nest);
        write_char w ']'
      | Map { item; _ } ->
        write_char w '{';
        let written = ref false in
        let inner = nest + indent_step w in
        List.iter
          (fun (k, v) ->
             if !written then write_sep w;
             written := true;
             write_newline w;
             write_indent w ~nest:inner;
             write_json_string w k;
             write_colon_sep w;
             write_value w ~nest:inner item v)
          a;
        if !written
        then begin
          write_newline w;
          write_indent w ~nest
        end;
        write_char w '}'
      | Object { members; _ } ->
        write_char w '{';
        let written = ref false in
        let inner = nest + indent_step w in
        ignore
        @@ Free.run
             unit_applicative
             (write_members written w ~nest:inner a)
             members;
        if !written
        then begin
          write_newline w;
          write_indent w ~nest
        end;
        write_char w '}'
      | Union { discriminator; cases; _ } ->
        write_char w '{';
        let inner = nest + indent_step w in
        let written = ref false in
        write_newline w;
        write_indent w ~nest:inner;
        write_json_string w discriminator;
        write_colon_sep w;
        (match find_case_for_value a cases with
        | Some (Projected { tag; codec; payload }) ->
          write_json_string w tag;
          written := true;
          write_union_case_fields w ~nest:inner written codec payload
        | None ->
          invalid_arg "Sch.Json_encoder: value does not match any union case");
        if !written
        then begin
          write_newline w;
          write_indent w ~nest
        end;
        write_char w '}'
      | Tagless_union { cases; _ } ->
        (match find_all_cases_for_value a cases with
        | [ Projected { codec; payload; _ } ] ->
          write_value w ~nest codec payload
        | [] ->
          invalid_arg "Sch.Json_encoder: value does not match any union case"
        | many ->
          let tags = List.map (fun (Projected { tag; _ }) -> tag) many in
          invalid_arg
            (Printf.sprintf
               "Ambiguous tagless union: matched cases %s"
               (String.concat ", " tags)))
      | Rec t -> write_value w ~nest (Lazy.force t) a
      | Iso { bwd; repr; _ } ->
        let b = bwd a in
        write_value w ~nest repr b

    and write_members : type a.
      bool ref -> t -> nest:int -> a -> (a fieldk, U.t) Sig.nat
      =
     fun rf w ~nest obj ->
      { Sig.run =
          (fun (type b) (fa : (b, a fieldk) Sig.app) ->
            let field = Object.prj fa in
            let v = field.get obj in
            if not (field.omit v)
            then begin
              if !rf then write_sep w;
              rf := true;
              write_newline w;
              write_indent w ~nest;
              write_json_string w field.name;
              write_colon_sep w;
              write_value w ~nest field.codec v
            end;
            U.inj ())
      }

    and write_union_case_fields : type a.
      t -> nest:int -> bool ref -> a codec -> a -> unit
      =
     fun w ~nest rf codec value ->
      match object_case_of_codec codec with
      | Some (Object_case { members; extract }) ->
        let obj = extract value in
        ignore
        @@ Free.run unit_applicative (write_members rf w ~nest obj) members
      | None ->
        if !rf then write_sep w;
        rf := true;
        write_newline w;
        write_indent w ~nest;
        write_json_string w "value";
        write_colon_sep w;
        write_value w ~nest codec value

    let encode_writer ?buf ?(eod = true) ?(format = Minify) t a w =
      let encoder = make ?buf ~format w in
      write_value encoder ~nest:0 t a;
      write_eod ~eod encoder

    let encode_string ?(format = Minify) t a =
      let buf = Buffer.create 256 in
      let writer = Bytes.Writer.of_buffer buf in
      encode_writer ~eod:true ~format t a writer;
      Buffer.contents buf

    module Jsont_tree = struct
      type jsonfield = (Jsont.name * Jsont.json) Diflist.t

      module Fc = Sig.Make (struct
          type 'a t = jsonfield
        end)

      let fc_applicative : Fc.t Sig.applicative =
        { pure = (fun _ -> Fc.inj Diflist.empty)
        ; map = (fun _f va -> Fc.inj (Fc.prj va))
        ; apply = (fun vf va -> Fc.inj (Diflist.concat (Fc.prj va) (Fc.prj vf)))
        }

      let rec to_json : type a. a codec -> a -> Jsont.json =
       fun codec a ->
        match codec with
        | Str _ -> Jsont.Json.string a
        | Password _ -> Jsont.Json.string a
        | Int _ -> Jsont.Json.number (Float.of_int a)
        | Int32 _ -> Jsont.Json.number (Float.of_int (Int32.to_int a))
        | Int64 _ -> Jsont.Json.string (Int64.to_string a)
        | Bool _ -> Jsont.Json.bool a
        | Float _ -> Jsont.Json.number a
        | Double _ -> Jsont.Json.number a
        | File -> failwith "Cannot encode File to JSON"
        | Option ta ->
          (match a with None -> Jsont.Json.null () | Some v -> to_json ta v)
        | List { item; _ } -> Jsont.Json.list (List.map (to_json item) a)
        | Map { item; _ } ->
          Jsont.Json.object'
            (List.map (fun (k, v) -> Jsont.Json.name k, to_json item v) a)
        | Object { members; _ } ->
          let fields =
            Fc.prj @@ Free.run fc_applicative (object_member_nat a) members
          in
          Jsont.Json.object' (Diflist.to_list fields)
        | Union { discriminator; cases; _ } ->
          Jsont.Json.object' (union_to_fields a discriminator cases)
        | Tagless_union { cases; _ } ->
          (match find_all_cases_for_value a cases with
          | [ Projected { codec; payload; _ } ] -> to_json codec payload
          | [] ->
            invalid_arg "Sch.Json_encoder: value does not match any union case"
          | many ->
            let tags = List.map (fun (Projected { tag; _ }) -> tag) many in
            invalid_arg
              (Printf.sprintf
                 "Ambiguous tagless union: matched cases %s"
                 (String.concat ", " tags)))
        | Rec t -> to_json (Lazy.force t) a
        | Iso { bwd; repr; _ } ->
          let b = bwd a in
          to_json repr b

      and union_to_fields : type a.
        a -> string -> a union_case list -> (Jsont.name * Jsont.json) list
        =
       fun obj discriminator cases ->
        match find_case_for_value obj cases with
        | Some (Projected { tag; codec; payload }) ->
          (match object_case_of_codec codec with
          | Some (Object_case { members; extract }) ->
            let obj = extract payload in
            let fields =
              Fc.prj @@ Free.run fc_applicative (object_member_nat obj) members
            in
            Diflist.to_list
              (Diflist.concat
                 (Diflist.single
                    (Jsont.Json.name discriminator, Jsont.Json.string tag))
                 fields)
          | None ->
            let value = to_json codec payload in
            [ Jsont.Json.name discriminator, Jsont.Json.string tag
            ; Jsont.Json.name "value", value
            ])
        | None ->
          invalid_arg "Sch.Json_encoder: value does not match any union case"

      and object_member_nat : type a. a -> (a fieldk, Fc.t) Sig.nat =
       fun obj ->
        { Sig.run =
            (fun (type b) (fa : (b, a fieldk) Sig.app) ->
              let field = Object.prj fa in
              let v = field.get obj in
              if not (field.omit v)
              then
                Fc.inj
                  (Diflist.single
                     (Jsont.Json.name field.name, to_json field.codec v))
              else Fc.inj Diflist.empty)
        }
    end

    let encode = Jsont_tree.to_json
  end

  let coerce_string = Decoder.coerce_string
  let decode = Decoder.decode
  let decode_string = Decoder.decode_string
  let decode_reader = Decoder.decode_reader

  type format = Encoder.format =
    | Minify
    | Indent of int

  let encode_writer = Encoder.encode_writer
  let encode_string = Encoder.encode_string
  let encode = Encoder.encode
end

module To_json_schema = struct
  module Json_type = Json_schema.Json_type

  type state =
    { defs : (string * Json_schema.schema) list ref
    ; seen : (Obj.t * string) list ref
    ; next_id : int ref
    }

  type fieldc =
    { properties : (string * Json_schema.schema) Diflist.t
    ; required : string Diflist.t
    }
  (* field collector, use difference list for appends operation for efficiency,
     it called inside fc_applicative by free applicative *)

  let empty = { properties = Diflist.empty; required = Diflist.empty }

  let combine a b =
    { properties = Diflist.concat b.properties a.properties
    ; required = Diflist.concat b.required a.required
    }

  module Fc = Sig.Make (struct
      type nonrec 'a t = fieldc
    end)

  let fc_applicative : Fc.t Sig.applicative =
    { pure = (fun _ -> Fc.inj empty)
    ; map = (fun _f va -> Fc.inj (Fc.prj va))
    ; apply = (fun vf va -> Fc.inj (combine (Fc.prj vf) (Fc.prj va)))
    }

  let create_state () = { defs = ref []; seen = ref []; next_id = ref 0 }

  let find_seen state lazy_val =
    let key = Obj.repr lazy_val in
    List.find_map
      (fun (k, name) -> if k == key then Some name else None)
      !(state.seen)

  let gen_name state hint =
    if hint <> ""
    then hint
    else begin
      let id = !(state.next_id) in
      state.next_id := id + 1;
      Printf.sprintf "def_%d" id
    end

  let basemap_schema_obj (b : _ base_map) : Json_schema.t =
    let base =
      match b.constraint_ with
      | Some c -> Constraint.to_json_schema_obj c
      | None -> Json_schema.empty
    in
    if b.doc <> "" then { base with description = Some b.doc } else base

  let wrap obj : Json_schema.schema =
    Json_schema.Or_bool.Schema (Json_schema.Or_ref.Value obj)

  let rec to_schema : type a. state -> a t -> Json_schema.schema =
   fun state codec ->
    match codec with
    | Str b ->
      let obj = basemap_schema_obj b in
      wrap { obj with type_ = Json_type.string }
    | Password b ->
      let obj = basemap_schema_obj b in
      wrap
        { obj with
          type_ = Json_type.string
        ; format = obj.format <|> Some "password"
        }
    | Int b ->
      let obj = basemap_schema_obj b in
      wrap { obj with type_ = Json_type.integer; format = Some "int32" }
    | Int32 b ->
      let obj = basemap_schema_obj b in
      wrap { obj with type_ = Json_type.integer; format = Some "int32" }
    | Int64 b ->
      let obj = basemap_schema_obj b in
      wrap { obj with type_ = Json_type.integer; format = Some "int64" }
    | Bool { doc } ->
      wrap
        { Json_schema.empty with
          type_ = Json_type.boolean
        ; description = (if doc <> "" then Some doc else None)
        }
    | Float b ->
      let obj = basemap_schema_obj b in
      wrap { obj with type_ = Json_type.number; format = Some "float" }
    | Double b ->
      let obj = basemap_schema_obj b in
      wrap { obj with type_ = Json_type.number; format = Some "double" }
    | File ->
      wrap
        { Json_schema.empty with
          type_ = Json_type.string
        ; format = Some "binary"
        }
    | Option inner -> option_schema state inner
    | List { doc; item; constraint_ } ->
      let item_schema = to_schema state item in
      let base =
        match constraint_ with
        | Some c -> Constraint.to_json_schema_obj c
        | None -> Json_schema.empty
      in
      wrap
        { base with
          type_ = Json_type.array
        ; items = Some item_schema
        ; description = (if doc <> "" then Some doc else None)
        }
    | Map { doc; item; constraint_ } ->
      let item_schema = to_schema state item in
      let base =
        match constraint_ with
        | Some c ->
          let obj = Constraint.to_json_schema_obj c in
          { obj with
            min_items = None
          ; max_items = None
          ; min_properties = obj.min_items
          ; max_properties = obj.max_items
          }
        | None -> Json_schema.empty
      in
      wrap
        { base with
          type_ = Json_type.object_
        ; additional_properties = Some item_schema
        ; description = (if doc <> "" then Some doc else None)
        }
    | Object { doc; unknown; members; _ } ->
      object_schema state doc unknown members
    | Union { doc; discriminator; cases } ->
      union_schema state doc discriminator cases
    | Tagless_union { doc; cases } -> tagless_union_schema state doc cases
    | Rec lazy_t -> rec_schema state lazy_t
    | Iso { repr; _ } -> to_schema state repr

  and option_schema : type a. state -> a t -> Json_schema.schema =
   fun state inner ->
    let inner_s = to_schema state inner in
    match inner_s with
    | Json_schema.Or_bool.Schema (Json_schema.Or_ref.Value obj) ->
      wrap { obj with type_ = Json_type.union obj.type_ Json_type.null }
    | _ ->
      let null_s = wrap { Json_schema.empty with type_ = Json_type.null } in
      wrap { Json_schema.empty with any_of = Some [ inner_s; null_s ] }

  and object_schema : type o.
    state
    -> string
    -> unknown_handling
    -> (o fieldk, o) Free.t
    -> Json_schema.schema
    =
   fun state doc unknown members ->
    let nat : (o fieldk, Fc.t) Sig.nat =
      { Sig.run =
          (fun (type b) (fa : (b, o fieldk) Sig.app) ->
            let field = Object.prj fa in
            let field_schema = to_schema state field.codec in
            let properties = Diflist.single (field.name, field_schema) in
            let required =
              if Option.is_none field.default
              then Diflist.single field.name
              else Diflist.empty
            in
            Fc.inj { properties; required })
      }
    in
    let fieldc = Fc.prj (Free.run fc_applicative nat members) in
    let additional_properties =
      match unknown with
      | Skip -> None
      | Error_on_unknown -> Some (Json_schema.Or_bool.Bool false)
    in
    wrap
      { Json_schema.empty with
        type_ = Json_type.object_
      ; properties = Some (Diflist.to_list fieldc.properties)
      ; required =
          (match Diflist.to_list fieldc.required with
          | [] -> None
          | r -> Some r)
      ; additional_properties
      ; description = (if doc <> "" then Some doc else None)
      }

  and union_schema : type a.
    state -> string -> string -> a union_case list -> Json_schema.schema
    =
   fun state doc discriminator cases ->
    let discriminator_values =
      List.map (fun tag -> Jsont.Json.string tag) (case_tags cases)
    in
    let discriminator_property =
      ( discriminator
      , wrap
          { Json_schema.empty with
            type_ = Json_type.string
          ; enum = Some discriminator_values
          } )
    in
    let case_schema (Case c) =
      let base = to_schema state c.codec in
      let guard =
        wrap
          { Json_schema.empty with
            type_ = Json_type.object_
          ; properties =
              Some
                [ ( discriminator
                  , wrap
                      { Json_schema.empty with
                        type_ = Json_type.string
                      ; const = Some (Jsont.Json.string c.tag)
                      } )
                ]
          ; required = Some [ discriminator ]
          }
      in
      wrap { Json_schema.empty with all_of = Some [ base; guard ] }
    in
    wrap
      { Json_schema.empty with
        type_ = Json_type.object_
      ; properties = Some [ discriminator_property ]
      ; required = Some [ discriminator ]
      ; one_of = Some (List.map case_schema cases)
      ; description = (if not (String.equal doc "") then Some doc else None)
      }

  and tagless_union_schema : type a.
    state -> string -> a union_case list -> Json_schema.schema
    =
   fun state doc cases ->
    let case_schema (Case c) = to_schema state c.codec in
    wrap
      { Json_schema.empty with
        one_of = Some (List.map case_schema cases)
      ; description = (if not (String.equal doc "") then Some doc else None)
      }

  and rec_schema : type a. state -> a t Lazy.t -> Json_schema.schema =
   fun state lazy_t ->
    match find_seen state lazy_t with
    | Some name ->
      Json_schema.Or_bool.Schema (Json_schema.Or_ref.Ref ("#/$defs/" ^ name))
    | None ->
      let forced = Lazy.force lazy_t in
      let name =
        match forced with
        | Object { kind; _ } when kind <> "" -> kind
        | _ -> gen_name state ""
      in
      let key = Obj.repr lazy_t in
      state.seen := (key, name) :: !(state.seen);
      let s = to_schema state forced in
      state.defs := (name, s) :: !(state.defs);
      Json_schema.Or_bool.Schema (Json_schema.Or_ref.Ref ("#/$defs/" ^ name))

  let convert ?(draft = Json_schema.Draft.Draft2020_12) codec =
    let state = create_state () in
    let s = to_schema state codec in
    let defs =
      match !(state.defs) with [] -> None | defs -> Some (List.rev defs)
    in
    match s with
    | Json_schema.Or_bool.Schema (Json_schema.Or_ref.Value obj) ->
      { obj with schema = Some draft; defs }
    | Json_schema.Or_bool.Schema (Json_schema.Or_ref.Ref ref_str) ->
      { Json_schema.empty with schema = Some draft; ref_ = Some ref_str; defs }
    | Json_schema.Or_bool.Bool _ ->
      { Json_schema.empty with schema = Some draft; defs }
end

let to_json_schema ?draft codec = To_json_schema.convert ?draft codec
