(** Type-safe routing with GADT-based combinators.

    This module provides a type-safe routing API where route parameters are
    automatically parsed and typed. Unlike string-based routing, parameter
    types are enforced at compile time.

    {[
      open Route

      (* Define a typed route *)
      let user_route =
        get (s "users" / int64 / s "posts" / str)
        @-> fun user_id slug request ->
          (* user_id: int64, slug: string, request: Request.t *)
          Response.of_string
            ~body:(Printf.sprintf "User %Ld post %s" user_id slug)
            `OK
    ]}
*)

(** {1 Exceptions} *)

exception Not_found
(** Raised when no route matches the request. *)

(** {1 Core Types} *)

type (_, _) path
(** The type of a route pattern (internal GADT).
    ['a] is the accumulated function type for extracted parameters.
    ['b] is the final return type. *)

type route
(** The type of a complete route with handler attached. *)

(** {1 Combinators} *)

val int : (int -> 'a, 'a) path
(** [int] matches an integer path segment and extracts it as [int]. *)

val int32 : (int32 -> 'a, 'a) path
(** [int32] matches a 32-bit integer path segment. *)

val int64 : (int64 -> 'a, 'a) path
(** [int64] matches a 64-bit integer path segment. *)

val str : (string -> 'a, 'a) path
(** [str] matches any path segment and extracts it as [string]. *)

val bool : (bool -> 'a, 'a) path
(** [bool] matches "true" or "false" and extracts as [bool]. *)

val splat : (string list -> 'a, 'a) path
(** [splat] matches all remaining path segments and extracts them as [string list].
    This is useful for catch-all routes, serving SPAs, or file browsers.
    {[
      (* Matches: /files/docs/readme.txt -> ["docs"; "readme.txt"] *)
      get (s "files" / splat) @-> fun segments req -> ...
    ]} *)

val custom :
   parse:(string -> 'param option)
  -> format:('param -> string)
  -> ('param -> 'a, 'a) path
(** [custom ~parse ~format] creates a custom parameter extractor.

    {b Custom Type Pattern (OCaml Value Restriction):}

    Due to OCaml's value restriction, custom types must be defined as {b functions}
    to be reusable for both routing and URL generation:

    {[
      (* Define as a function - note the () *)
      let uuid () =
        custom
          ~parse:(fun s -> if is_valid_uuid s then Some s else None)
          ~format:Fun.id

      (* Manual usage - call it *)
      get (s "articles" / uuid ()) @-> fun uuid_str req -> ...
      let url = sprintf (s "articles" / uuid ()) "550e8400-..."

      (* PPX usage - automatically calls it *)
      let get_article ~id _req = ...[@@route GET, "/articles/<uuid:id>"]
      (* ^^^ PPX generates: get (s "articles" / uuid ()) *)
    ]}

    {b Why functions?}
    The expression [custom ~parse ~format] is non-expansive (calling a function
    with function arguments), so OCaml assigns weak type variables that cannot
    be reused polymorphically. Wrapping in a function [let uuid () = ...] makes
    each call produce a fresh polymorphic value.

    {b For PPX users:} The PPX automatically handles calling custom types as
    functions, so you can use them naturally in route annotations.

    {b For manual users:} Remember to call custom types: [uuid ()] not [uuid].
*)

val slug : (string -> 'a, 'a) path
(** [slug] matches URL-friendly slugs (lowercase letters, numbers, hyphens).
    This is useful for article URLs, product slugs, etc.
    {[
      (* Matches: /posts/my-awesome-post-123 *)
      get (s "posts" / slug) @-> fun slug req -> ...
    ]} *)

val s : string -> ('a, 'a) path
(** [s literal] matches a literal string segment. *)

val ( / ) : ('a, 'c) path -> ('c, 'b) path -> ('a, 'b) path
(** [( / )] chains two path segments together.
    {[
      s "users" / int64 / s "posts" / str
    ]} *)

(** {1 Handler Attachment} *)

val ( @-> ) : ('a, Request.t -> Response.t) path -> 'a -> route
(** [pattern @-> handler] attaches a handler to a route pattern.
    The handler must accept all extracted parameters followed by [Request.t]. *)

(** {1 HTTP Methods} *)

val get : ('a, 'b) path -> ('a, 'b) path
(** [get pattern] creates a GET route with the given pattern. *)

val post : ('a, 'b) path -> ('a, 'b) path
(** [post pattern] creates a POST route with the given pattern. *)

val put : ('a, 'b) path -> ('a, 'b) path
(** [put pattern] creates a PUT route with the given pattern. *)

val patch : ('a, 'b) path -> ('a, 'b) path
(** [patch pattern] creates a PATCH route with the given pattern. *)

val delete : ('a, 'b) path -> ('a, 'b) path
(** [delete pattern] creates a DELETE route with the given pattern. *)

val head : ('a, 'b) path -> ('a, 'b) path
(** [head pattern] creates a HEAD route with the given pattern. *)

val any : ('a, 'b) path -> ('a, 'b) path
(** [any pattern] creates a route that matches any HTTP method.
    This is useful for catch-all endpoints like webhooks.
    {[
      (* Matches GET, POST, PUT, DELETE, etc. on /api/webhook *)
      any (s "api" / s "webhook") @-> fun req -> ...
    ]} *)

(** {1 Request Guards} *)

val ( >=> ) : 'g Request_guard.t -> ('a, 'b) path -> ('g -> 'a, 'b) path
(** [guard >=> pattern] composes a request guard with a route pattern.
    The guard extracts a value from the request and adds it as the first parameter
    to the handler, before any path parameters.

    This reads naturally left-to-right: "guard then path pattern".

    Guards provide type-safe request validation and data extraction:
    - If the guard succeeds, the extracted value is passed to the handler
    - If the guard fails, a [Request_guard.Failed] exception is raised

    {[
      (* Define a guard *)
      let user_guard : User.t Request_guard.t = fun req ->
        match get_auth_token req with
        | Some token -> verify_user token  (* Returns Ok user or Error err *)
        | None -> Error Request_guard.Invalid_bearer_token

      (* Use with router - reads "user guard then users path" *)
      get (user_guard >=> s "users" / int64) @-> fun user id req ->
        (* user : User.t, id : int64, req : Request.t *)
        Response.of_string ~body:(Printf.sprintf "User %s viewing %Ld" user.name id) `OK

      (* Multiple guards - compose with &&& *)
      let authenticated_json = Request_guard.(user_guard &&& json_guard) in
      post (authenticated_json >=> s "posts") @-> fun (user, ()) req -> ...

      (* Chain multiple guards *)
      get (user_guard >=> admin_guard >=> s "admin" / str) @->
        fun user admin_role path req -> ...
    ]}

    See {!Request_guard} module for guard combinators and common guards. *)

(** {1 Routing} *)

val match' : route list -> Request.t -> Response.t option
(** [match' routes request] attempts to match the request against the list of routes.
    Returns [Some response] if a route matches, [None] otherwise. *)

val router : route list -> Request.t -> Response.t
(** [router routes] creates a request handler from a list of routes.
    Raises [Not_found] if no route matches. *)

(** {1 URL Generation} *)

val sprintf : ('a, string) path -> 'a
(** [sprintf pattern args...] generates a URL from a route pattern and arguments.

    Works with any path pattern, extracting just the structure for URL generation:
    {[
      (* Define patterns *)
      let user_path = s "users" / int64
      let user_route_path = get (s "users" / int64)

      (* Use for URL generation - works with both! *)
      let url1 = sprintf user_path 42L  (* "/users/42" *)
      let url2 = sprintf user_route_path 42L  (* "/users/42" *)
    ]} *)

(** {1 Scoping} *)

val scope :
   ?middlewares:Middleware.t list
  -> ('a, 'a) path
  -> route list
  -> route
(** [scope ?middlewares prefix routes] groups routes under a common prefix.
    {[
      scope ~middlewares:[auth; logging] (s "api" / s "v1") [
        get (s "users") @-> users_handler;
        get (s "posts") @-> posts_handler;
      ]
      (* Matches: /api/v1/users, /api/v1/posts *)
    ]} *)

(** {1 RESTful Resources} *)

(** Module signature for defining RESTful resource handlers.

    The Resource module allows you to specify the ID type and pattern for your
    resource, making it flexible for different ID schemes (int64, UUID, slugs, etc.).

    {[
      (* Example: Resource with int64 IDs *)
      module UserResource : Router.Resource = struct
        type id = int64
        let id_path () = Router.int64

        let index _req = Response.of_string ~body:"User list" `OK
        let new_ _req = Response.of_string ~body:"New user form" `OK
        let create _req = Response.of_string ~body:"Create user" `OK
        let get id _req =
          Response.of_string ~body:(Printf.sprintf "User %Ld" id) `OK
        let edit id _req =
          Response.of_string ~body:(Printf.sprintf "Edit user %Ld" id) `OK
        let update id _req =
          Response.of_string ~body:(Printf.sprintf "Update user %Ld" id) `OK
        let delete id _req =
          Response.of_string ~body:(Printf.sprintf "Delete user %Ld" id) `OK
      end

      (* Example: Resource with UUID (string) IDs *)
      module ArticleResource : Router.Resource = struct
        type id = string
        let id_path () = Router.custom
          ~parse:(fun s -> if is_valid_uuid s then Some s else None)
          ~format:Fun.id

        let index _req = Response.of_string ~body:"Article list" `OK
        let get uuid _req =
          Response.of_string ~body:(Printf.sprintf "Article %s" uuid) `OK
        (* ... other handlers *)
      end

      (* Example: Resource with slug IDs *)
      module PostResource : Router.Resource = struct
        type id = string
        let id_path () = Router.slug

        let index _req = Response.of_string ~body:"Post list" `OK
        let get slug _req =
          Response.of_string ~body:(Printf.sprintf "Post %s" slug) `OK
        (* ... other handlers *)
      end
    ]} *)
module type Resource = sig
  type id
  (** The type of the resource identifier. *)

  val id_path : unit -> (id -> 'a, 'a) path
  (** The path pattern for extracting the resource ID.
      This allows you to specify custom validation and formatting for IDs.

      {b Note:} Due to OCaml's value restriction, [id_path] must be a function
      to allow reuse in both routing (within [resource]) and URL generation.
      Define it as: [let id_path () = Router.int64] (note the [unit] parameter). *)

  val index : Handler.t
  (** GET /resource - List all resources *)

  val new_ : Handler.t
  (** GET /resource/new - Show form to create a new resource *)

  val create : Handler.t
  (** POST /resource - Create a new resource *)

  val get : id -> Handler.t
  (** GET /resource/:id - Show a specific resource *)

  val edit : id -> Handler.t
  (** GET /resource/:id/edit - Show form to edit a resource *)

  val update : id -> Handler.t
  (** PUT /resource/:id - Update a specific resource *)

  val delete : id -> Handler.t
  (** DELETE /resource/:id - Delete a specific resource *)
end

val resource :
   ?middlewares:Middleware.t list
  -> ('a, 'a) path
  -> (module Resource)
  -> route
(** [resource ?middlewares prefix module] creates RESTful routes for a resource.

    This generates 7 standard RESTful routes with proper HTTP methods:
    - GET    /prefix          -> index
    - GET    /prefix/new      -> new_
    - POST   /prefix          -> create
    - GET    /prefix/:id      -> get
    - GET    /prefix/:id/edit -> edit
    - PUT    /prefix/:id      -> update
    - DELETE /prefix/:id      -> delete

    {[
      (* Using with int64 IDs *)
      let routes = resource (s "users") (module UserResource)

      (* Using with UUID IDs *)
      let routes = resource (s "articles") (module ArticleResource)

      (* With middleware *)
      let routes = resource ~middlewares:[auth] (s "admin" / s "posts") (module PostResource)

      (* URL generation for resource routes *)
      let user_path = s "users" / UserResource.id_path ()
      let url = sprintf user_path 42L  (* "/users/42" *)

      let edit_path = s "users" / UserResource.id_path () / s "edit"
      let edit_url = sprintf edit_path 42L  (* "/users/42/edit" *)
    ]} *)
