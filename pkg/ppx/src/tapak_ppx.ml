open Ppxlib

module Route_parser = struct
  open Angstrom

  type segment =
    | Literal of string
    | Parameter of string * string option (* name, optional type *)
    | Splat

  let literal_chars =
    take_while1 (fun c ->
      c <> '/' && c <> ':' && c <> '*' && c <> '<' && c <> '\000')

  let identifier =
    take_while1 (fun c ->
      (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      || (c >= '0' && c <= '9')
      || c = '_')

  let django_parameter =
    char '<' *> identifier >>= fun type_name ->
    char ':' *> identifier <* char '>' >>| fun param_name ->
    Parameter (param_name, Some type_name)

  let simple_parameter =
    char ':' *> identifier >>| fun name -> Parameter (name, None)

  let splat = char '*' *> char '*' >>| fun _ -> Splat

  let segment =
    choice
      [ django_parameter
      ; simple_parameter
      ; splat
      ; (literal_chars >>| fun s -> Literal s)
      ]

  let route_path = char '/' *> sep_by (char '/') segment <* end_of_input

  let parse pattern =
    match parse_string ~consume:All route_path pattern with
    | Ok segments -> Ok segments
    | Error msg -> Error msg
end

let route_attr =
  Attribute.declare
    "route"
    Attribute.Context.value_binding
    Ast_pattern.(
      pstr
        (pstr_eval
           (pexp_tuple
              (pexp_construct (lident __) none
              ^:: pexp_constant (pconst_string __ __ __)
              ^:: nil))
           nil
        ^:: nil))
    (fun method_str path _loc _str_loc -> method_str, path)

let extract_handler_name ~loc:_ pat =
  match pat.ppat_desc with
  | Ppat_var { txt; _ } -> txt
  | _ ->
    Location.raise_errorf
      ~loc:pat.ppat_loc
      "Route handler must be a simple named function"

let validate_http_method ~loc method_str =
  match method_str with
  | "GET" | "POST" | "PUT" | "DELETE" | "PATCH" | "HEAD" | "ANY" -> ()
  | _ ->
    Location.raise_errorf
      ~loc
      "Invalid HTTP method '%s'. Supported methods: GET, POST, PUT, DELETE, \
       PATCH, HEAD, ANY"
      method_str

let parse_route_pattern ~loc pattern =
  if String.length pattern = 0 || pattern.[0] <> '/'
  then
    Location.raise_errorf
      ~loc
      "Route pattern must start with '/': \"%s\""
      pattern;

  match Route_parser.parse pattern with
  | Ok segments ->
    if List.length segments = 0 then [ Route_parser.Literal "" ] else segments
  | Error msg ->
    Location.raise_errorf ~loc "Invalid route pattern \"%s\": %s" pattern msg

let type_to_path_expr ~loc type_name =
  let open Ast_builder.Default in
  match type_name with
  | "int" -> [%expr Tapak.Router.int]
  | "int32" -> [%expr Tapak.Router.int32]
  | "int64" -> [%expr Tapak.Router.int64]
  | "string" | "str" -> [%expr Tapak.Router.str]
  | "slug" -> [%expr Tapak.Router.slug]
  | "bool" -> [%expr Tapak.Router.bool]
  | custom -> [%expr [%e evar ~loc custom] ()]

let generate_path_expr ~loc segments =
  let open Ast_builder.Default in
  let open Route_parser in
  let rec build_path segs =
    match segs with
    | [] -> [%expr Tapak.Router.s ""]
    | [ Literal "" ] -> [%expr Tapak.Router.s ""]
    | [ Literal s ] -> [%expr Tapak.Router.s [%e estring ~loc s]]
    | [ Parameter (_name, type_opt) ] ->
      (match type_opt with
      | Some type_name -> type_to_path_expr ~loc type_name
      | None -> [%expr Tapak.Router.int] (* Default to int for now *))
    | [ Splat ] -> [%expr Tapak.Router.splat]
    | Literal s :: rest ->
      let rest_expr = build_path rest in
      [%expr
        Tapak.Router.( / ) (Tapak.Router.s [%e estring ~loc s]) [%e rest_expr]]
    | Parameter (_name, type_opt) :: rest ->
      let param_expr =
        match type_opt with
        | Some type_name -> type_to_path_expr ~loc type_name
        | None -> [%expr Tapak.Router.int]
        (* Default to int for now *)
      in
      let rest_expr = build_path rest in
      [%expr Tapak.Router.( / ) [%e param_expr] [%e rest_expr]]
    | Splat :: rest ->
      let rest_expr = build_path rest in
      [%expr Tapak.Router.( / ) Tapak.Router.splat [%e rest_expr]]
  in
  build_path segments

let method_to_expr ~loc method_str =
  let method_name =
    Format.sprintf "Tapak.Router.%s" (String.lowercase_ascii method_str)
  in
  Ast_builder.Default.evar ~loc method_name

let extract_param_names segments =
  let open Route_parser in
  List.filter_map
    (function
      | Parameter (name, _type_opt) -> Some name
      | Splat -> Some "splat"
      | Literal _ -> None)
    segments

let generate_route_binding ~loc ~handler_name ~method_str ~route_pattern =
  let open Ast_builder.Default in
  validate_http_method ~loc method_str;
  let segments = parse_route_pattern ~loc route_pattern in
  let path_expr = generate_path_expr ~loc segments in
  let method_expr = method_to_expr ~loc method_str in
  let path_var_name = handler_name ^ "_path" in
  let route_var_name = handler_name ^ "_route" in

  let param_names = extract_param_names segments in

  let handler_expr =
    if List.length param_names = 0
    then evar ~loc handler_name
    else
      let params_pattern = List.map (fun name -> pvar ~loc name) param_names in
      let request_pattern = pvar ~loc "request" in
      let all_patterns = params_pattern @ [ request_pattern ] in

      let handler_call =
        let base_call = evar ~loc handler_name in
        let call_with_params =
          List.fold_left
            (fun acc param_name ->
               pexp_apply ~loc acc [ Labelled param_name, evar ~loc param_name ])
            base_call
            param_names
        in
        pexp_apply ~loc call_with_params [ Nolabel, evar ~loc "request" ]
      in

      List.fold_right
        (fun pattern acc -> [%expr fun [%p pattern] -> [%e acc]])
        all_patterns
        handler_call
  in

  (* Generate both path and route variables
     - handler_path: polymorphic path for sprintf
     - handler_route: complete route with method and handler

     Important: We duplicate the path expression to avoid specialization.
     When the same path value is used in both route and sprintf contexts,
     it gets specialized to the route type. By generating it twice,
     each can have its own polymorphic type. *)
  [ [%stri let [%p pvar ~loc path_var_name] = [%e path_expr]]
  ; [%stri
    let [%p pvar ~loc route_var_name] =
      Tapak.Router.( @-> ) ([%e method_expr] [%e path_expr]) [%e handler_expr]]
  ]

let expand_str_item str_item =
  match str_item.pstr_desc with
  | Pstr_value (_rec_flag, bindings) ->
    let routes_and_handlers =
      List.concat_map
        (fun vb ->
           match Attribute.get route_attr vb with
           | None -> []
           | Some (method_str, route_pattern) ->
             let loc = vb.pvb_loc in
             let handler_name = extract_handler_name ~loc vb.pvb_pat in
             let generated =
               generate_route_binding
                 ~loc
                 ~handler_name
                 ~method_str
                 ~route_pattern
             in
             (match generated with
             | [ path_binding; route_binding ] ->
               [ path_binding, Some route_binding ]
             | _ -> []))
        bindings
    in
    if List.length routes_and_handlers > 0
    then
      let paths = List.map fst routes_and_handlers in
      let routes =
        List.filter_map (fun (_, route) -> route) routes_and_handlers
      in
      paths @ [ str_item ] @ routes
    else [ str_item ]
  | _ -> [ str_item ]

let transform_structure structure =
  let route_infos_and_items =
    List.map
      (fun str_item ->
         match str_item.pstr_desc with
         | Pstr_value (_rec_flag, bindings) ->
           let infos =
             List.filter_map
               (fun vb ->
                  match Attribute.get route_attr vb with
                  | None -> None
                  | Some (method_str, route_pattern) ->
                    let loc = vb.pvb_loc in
                    let handler_name = extract_handler_name ~loc vb.pvb_pat in
                    Some (loc, handler_name, method_str, route_pattern))
               bindings
           in
           infos, str_item
         | _ -> [], str_item)
      structure
  in
  let all_route_infos = List.concat_map fst route_infos_and_items in
  let all_paths =
    List.map
      (fun (loc, handler_name, _method_str, route_pattern) ->
         let segments = parse_route_pattern ~loc route_pattern in
         let path_expr = generate_path_expr ~loc segments in
         let path_var_name = handler_name ^ "_path" in
         let open Ast_builder.Default in
         [%stri let [%p pvar ~loc path_var_name] = [%e path_expr]])
      all_route_infos
  in
  let original_structure = List.map snd route_infos_and_items in
  let all_routes =
    List.map
      (fun (loc, handler_name, method_str, route_pattern) ->
         validate_http_method ~loc method_str;
         let segments = parse_route_pattern ~loc route_pattern in
         let path_expr = generate_path_expr ~loc segments in
         let method_expr = method_to_expr ~loc method_str in
         let route_var_name = handler_name ^ "_route" in
         let param_names = extract_param_names segments in
         let open Ast_builder.Default in
         let handler_expr =
           if List.length param_names = 0
           then evar ~loc handler_name
           else
             let params_pattern =
               List.map (fun name -> pvar ~loc name) param_names
             in
             let request_pattern = pvar ~loc "request" in
             let all_patterns = params_pattern @ [ request_pattern ] in
             let handler_call =
               let base_call = evar ~loc handler_name in
               let call_with_params =
                 List.fold_left
                   (fun acc param_name ->
                      pexp_apply
                        ~loc
                        acc
                        [ Labelled param_name, evar ~loc param_name ])
                   base_call
                   param_names
               in
               pexp_apply ~loc call_with_params [ Nolabel, evar ~loc "request" ]
             in
             List.fold_right
               (fun pattern acc -> [%expr fun [%p pattern] -> [%e acc]])
               all_patterns
               handler_call
         in
         [%stri
         let [%p pvar ~loc route_var_name] =
           Tapak.Router.( @-> )
             ([%e method_expr] [%e path_expr])
             [%e handler_expr]])
      all_route_infos
  in
  let rec find_last_handler_index items index last_handler_index =
    match items with
    | [] -> last_handler_index
    | (infos, _) :: rest ->
      let new_last =
        if List.length infos > 0 then index else last_handler_index
      in
      find_last_handler_index rest (index + 1) new_last
  in
  let last_handler_idx = find_last_handler_index route_infos_and_items 0 (-1) in
  if last_handler_idx >= 0
  then
    let before_routes, after_routes =
      let rec split lst idx acc =
        match lst with
        | [] -> List.rev acc, []
        | hd :: tl ->
          if idx = 0 then List.rev acc, lst else split tl (idx - 1) (hd :: acc)
      in
      split original_structure (last_handler_idx + 1) []
    in
    all_paths @ before_routes @ all_routes @ after_routes
  else original_structure

let () = Driver.register_transformation "tapak_ppx" ~impl:transform_structure
