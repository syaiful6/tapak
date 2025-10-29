(** Request guards for type-safe request data extraction and validation.

    Request guards provide a composable way to extract and validate data from
    requests before they reach your handlers. Guards return [Result.t] with
    extensible error variants, allowing type-safe error handling.

    {[
      (* Define custom error types *)
      type Request_guard.error +=
        | Json_required
        | Invalid_bearer_token

      let json_guard : string Request_guard.t = fun req ->
        match Piaf.Headers.get (Request.headers req) "content-type" with
        | Some ct when String.starts_with ~prefix:"application/json" ct -> Ok ct
        | _ -> Error Json_required

      post (json_guard >=> s "api" / s "data") @-> fun content_type req ->
        (* content_type : string - extracted by guard *)
        ...

      (* Handle errors in middleware *)
      let guard_error_handler : (Request.t, Response.t) Filter.simple = fun next req ->
        try next req with
        | Request_guard.Failed Json_required ->
          Response.of_json ~status:`Bad_request
            {|{"error": "Content-Type must be application/json"}|}
        | Request_guard.Failed Invalid_bearer_token ->
          Response.of_json ~status:`Unauthorized
            {|{"error": "Invalid token"}|}
    ]}

    Guards compose naturally:
    {[
      (* Multiple guards *)
      post (content_type_guard >=> (size_guard >=> s "upload")) @->
        fun ct size req -> ...

      (* Combine with &&& *)
      let combined = Request_guard.(user_guard &&& json_guard) in
      post (combined >=> s "data") @-> fun (user, ct) req -> ...
    ]}
*)

type error = ..
(** Extensible error type for guard failures.
    Guards can extend this with their own error variants. *)

exception Failed of error
(** Exception raised when a guard fails with an error. *)

type 'a t = Request.t -> ('a, error) result
(** The type of a request guard that extracts a value of type ['a].
    Returns [Ok value] if successful, [Error err] if it fails. *)

(** {1 Built-in Errors} *)

type error +=
  | Missing_header of string  (** Required header not found *)
  | Missing_query_param of string  (** Required query parameter not found *)
  | Invalid_bearer_token  (** Bearer token missing or malformed *)

(** {1 Combinators} *)

val return : 'a -> 'a t
(** [return x] creates a guard that always succeeds with value [x].

    {[
      let always_admin = Request_guard.return { id = 0; role = "admin" }
    ]} *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind guard f] is the monadic bind operation. Extracts ['a] with [guard],
    then applies [f] to get a new guard. The continuation [f] has access to
    both the extracted value and the request.

    This is useful for composing guards where the second guard depends on
    the result of the first guard AND needs access to the request.

    {[
      (* Extract user, then check their permissions based on request path *)
      let authorized_user =
        bind user_guard (fun user ->
          fun req ->
            let path = Request.target req in
            if has_permission user path
            then Ok user
            else Error Permission_denied)
    ]} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f guard] transforms the extracted value using function [f].

    {[
      (* Extract user, return only their ID *)
      let user_id_guard = Request_guard.map (fun user -> user.id) user_guard
    ]} *)

val map_error : (error -> error) -> 'a t -> 'a t
(** [map_error f guard] transforms guard errors using function [f].
    Useful for wrapping errors in a more specific context.

    {[
      type Request_guard.error += Database_error of string

      let db_guard =
        Request_guard.map_error
          (fun _ -> Database_error "Connection failed")
          connection_guard
    ]} *)

val and_then : ('a -> ('b, error) result) -> 'a t -> 'b t
(** [and_then f guard] chains validation. The guard extracts ['a], then [f]
    validates/transforms it, returning [Ok b] or [Error err].

    Unlike {!bind}, the continuation [f] does NOT have access to the request,
    making it simpler when you only need to validate the extracted value.

    {[
      (* Extract user, then validate they're an admin *)
      let admin_guard =
        Request_guard.and_then
          (fun user -> if user.role = "admin" then Ok user else Error Not_admin)
          user_guard
    ]} *)

val ( &&& ) : 'a t -> 'b t -> ('a * 'b) t
(** [g1 &&& g2] combines two guards. Both must succeed.
    Returns a tuple of extracted values. Returns the first guard's error if it fails.

    {[
      let combined = user_guard &&& json_guard in
      post (combined >=> s "data") @-> fun (user, _) req -> ...
    ]} *)

val ( ||| ) : 'a t -> 'a t -> 'a t
(** [g1 ||| g2] tries [g1] first. If it fails, tries [g2].
    Useful for fallback authentication methods.

    {[
      let auth = cookie_auth ||| bearer_auth in
      get (auth >=> s "profile") @-> fun user req -> ...
    ]} *)

(** {1 Common Guards} *)

val bearer_token : string t
(** [bearer_token] extracts the bearer token from the Authorization header.
    Expects header format: "Bearer <token>".
    Returns [Error Invalid_bearer_token] if missing or malformed. *)

val header : string -> string t
(** [header name] extracts a required header value.
    Returns [Error (Missing_header name)] if not found. *)

val optional_header : string -> string option t
(** [optional_header name] extracts an optional header value.
    Always succeeds with [Ok (Some value)] or [Ok None]. *)

val query_param : string -> string t
(** [query_param name] extracts a required query parameter.
    Returns [Error (Missing_query_param name)] if not found. *)

val optional_query_param : string -> string option t
(** [optional_query_param name] extracts an optional query parameter.
    Always succeeds with [Ok (Some value)] or [Ok None]. *)
