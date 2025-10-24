open Tapak

(* Example of Django-style typed parameters with custom types *)

(* Define custom path types using rank-2 polymorphism to avoid value restriction.

   The Router module now uses path_builder internally, which provides rank-2 polymorphism.
   This means we can define reusable path patterns that work for both routing AND URL generation!
*)

let uuid_parse s =
  (* Simple validation: UUID format is 8-4-4-4-12 hex digits *)
  let parts = String.split_on_char '-' s in
  match parts with
  | [ a; b; c; d; e ]
    when String.length a = 8
         && String.length b = 4
         && String.length c = 4
         && String.length d = 4
         && String.length e = 12 ->
    Some s
  | _ -> None

let hex_color_parse s =
  if
    String.length s = 6
    && String.for_all
         (fun c ->
            (c >= '0' && c <= '9')
            || (c >= 'a' && c <= 'f')
            || (c >= 'A' && c <= 'F'))
         s
  then Some s
  else None

(* Define custom path patterns as functions to work around value restriction.

   The issue: Router.custom ~parse ~format creates a non-expansive expression,
   which means even with type annotations, OCaml assigns weak type variables.

   The solution: Wrap in a function so each call creates a fresh polymorphic value.
*)
let uuid () : (string -> 'a, 'a) Router.path_builder =
  Router.custom ~parse:uuid_parse ~format:Fun.id

let hex_color () : (string -> 'a, 'a) Router.path_builder =
  Router.custom ~parse:hex_color_parse ~format:String.lowercase_ascii

(* Demonstrate URL generation with rank-2 polymorphic paths *)
let () =
  Printf.printf "=== URL Generation Demo (Rank-2 Polymorphism) ===\n";

  (* Custom types - call as functions to get fresh polymorphic values! *)
  let article_url =
    Router.sprintf
      Router.(s "articles" / uuid ())
      "550e8400-e29b-41d4-a716-446655440000"
  in
  Printf.printf "Article URL: %s\n" article_url;

  let color_url = Router.sprintf Router.(s "colors" / hex_color ()) "ff5733" in
  Printf.printf "Color URL: %s\n" color_url

let get_user ~id _request =
  Response.of_string' (Printf.sprintf "User ID: %Ld" id)
[@@route GET, "/users/<int64:id>"]

let get_post ~slug _request =
  Response.of_string' (Printf.sprintf "Post slug: %s" slug)
[@@route GET, "/posts/<slug:slug>"]

let search ~query _request =
  Response.of_string' (Printf.sprintf "Search: %s" query)
[@@route GET, "/search/<string:query>"]

let toggle_feature ~enabled _request =
  Response.of_string' (Printf.sprintf "Feature enabled: %b" enabled)
[@@route GET, "/features/<bool:enabled>"]

let get_article ~id _request =
  Response.of_string' (Printf.sprintf "Article UUID: %s" id)
[@@route GET, "/articles/<uuid:id>"]

let get_color ~color _request =
  Response.of_string' (Printf.sprintf "Color: #%s" color)
[@@route GET, "/colors/<hex_color:color>"]

let get_page ~num _request = Response.of_string' (Printf.sprintf "Page: %d" num)
[@@route GET, "/page/:num"]

let routes =
  [ get_user_route
  ; get_post_route
  ; search_route
  ; toggle_feature_route
  ; get_article_route
  ; get_color_route
  ; get_page_route
  ]

(* Demonstrate URL generation with generated path variables *)
let () =
  Printf.printf "\n=== Generated Path Variables ===\n";

  (* Built-in types work fine - the generated paths stay polymorphic *)
  let user_url = Router.sprintf get_user_path 123L in
  Printf.printf "User URL: %s\n" user_url;

  let post_url = Router.sprintf get_post_path "hello-world-2024" in
  Printf.printf "Post URL: %s\n" post_url;

  let search_url = Router.sprintf search_path "tapak framework" in
  Printf.printf "Search URL: %s\n" search_url;

  let feature_url = Router.sprintf toggle_feature_path true in
  Printf.printf "Feature URL: %s\n" feature_url;

  (* Simple param *)
  let page_url = Router.sprintf get_page_path 5 in
  Printf.printf "Page URL: %s\n" page_url;

  let article_url2 =
    Router.sprintf get_article_path "550e8400-e29b-41d4-a716-446655440000"
  in
  Printf.printf "Article URL (from generated path): %s\n" article_url2;

  let color_url2 = Router.sprintf get_color_path "ff5733" in
  Printf.printf "Color URL (from generated path): %s\n" color_url2;

  Printf.printf
    "\n\
     Success! Both routing and URL generation work with the same path \
     definitions.\n"
