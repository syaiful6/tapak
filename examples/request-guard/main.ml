open Tapak

(**
   Request Guard Examples

   This example demonstrates type-safe request guards in Tapak.
   Guards extract and validate data from requests before handlers execute.
*)

module Log = (val Logs.src_log Logs.default : Logs.LOG)

(* Define custom error types *)
type Request_guard.error += Not_admin | Invalid_user_token

let api_key_guard : string Request_guard.t = Request_guard.header "x-api-key"

type user =
  { id : int
  ; name : string
  ; role : string
  }

let user_guard : user Request_guard.t =
 fun req ->
  match Request.header "Authorization" req with
  | Some auth when String.starts_with ~prefix:"Bearer " auth ->
    let token = String.sub auth 7 (String.length auth - 7) in
    if token = "valid-token"
    then Ok { id = 1; name = "Alice"; role = "admin" }
    else if token = "user-token"
    then Ok { id = 2; name = "Bob"; role = "user" }
    else Error Invalid_user_token
  | _ -> Error Invalid_user_token

let admin_guard : user Request_guard.t =
  Request_guard.and_then
    (fun user -> if user.role = "admin" then Ok user else Error Not_admin)
    user_guard

type Request_guard.error += Json_required

let json_content_guard : unit Request_guard.t =
 fun req ->
  match Request.header "content-type" req with
  | Some ct when String.starts_with ~prefix:"application/json" ct -> Ok ()
  | _ -> Error Json_required

let authenticated_json_guard : (user * unit) Request_guard.t =
  Request_guard.(user_guard &&& json_content_guard)

type pagination =
  { page : int
  ; per_page : int
  }

type Request_guard.error += Invalid_pagination

let pagination_guard : pagination Request_guard.t =
 fun req ->
  let uri = Request.uri req in
  match Uri.get_query_param uri "page", Uri.get_query_param uri "per_page" with
  | Some page_str, Some per_page_str ->
    (match int_of_string_opt page_str, int_of_string_opt per_page_str with
    | Some page, Some per_page when page > 0 && per_page > 0 && per_page <= 100
      ->
      Ok { page; per_page }
    | _ -> Error Invalid_pagination)
  | Some page_str, None ->
    (match int_of_string_opt page_str with
    | Some page when page > 0 -> Ok { page; per_page = 20 }
    | _ -> Error Invalid_pagination)
  | None, Some per_page_str ->
    (match int_of_string_opt per_page_str with
    | Some per_page when per_page > 0 && per_page <= 100 ->
      Ok { page = 1; per_page }
    | _ -> Error Invalid_pagination)
  | None, None -> Ok { page = 1; per_page = 20 }

let api_info_handler api_key _req =
  let body =
    Printf.sprintf {|{"message": "API access granted", "key": "%s"}|} api_key
  in
  Response.of_string' ~status:`OK body

let profile_handler user _req =
  let body =
    Printf.sprintf
      {|{"id": %d, "name": "%s", "role": "%s"}|}
      user.id
      user.name
      user.role
  in
  Response.of_string' ~status:`OK body

let admin_handler admin _req =
  let body =
    Printf.sprintf
      {|{"message": "Admin access granted", "admin": "%s"}|}
      admin.name
  in
  Response.of_string' ~status:`OK body

let create_post_handler ((user, ()) : user * unit) (req : Request.t) :
  Response.t
  =
  let body_content =
    Result.fold
      ~ok:Fun.id
      ~error:(fun _ -> "{}")
      (Body.to_string (Request.body req))
  in
  let response =
    Printf.sprintf
      {|{"message": "Post created", "author": "%s", "data": %s}|}
      user.name
      body_content
  in
  Response.of_string' ~status:`OK response

let update_user_handler user user_id req =
  if Int64.of_int user.id = user_id || user.role = "admin"
  then
    let body_content =
      Result.fold
        ~ok:Fun.id
        ~error:(fun _ -> "{}")
        (Body.to_string (Request.body req))
    in
    let response =
      Printf.sprintf
        {|{"message": "User %Ld updated by %s", "data": %s}|}
        user_id
        user.name
        body_content
    in
    Response.of_string' ~status:`OK response
  else Response.of_string' ~status:`Forbidden {|{"error": "Forbidden"}|}

let user_list_handler admin pagination _req =
  (* Only admins can list users with pagination *)
  let response =
    Printf.sprintf
      {|{"message": "User list", "admin": "%s", "page": %d, "per_page": %d, "users": [{"id": 1, "name": "Alice"}, {"id": 2, "name": "Bob"}]}|}
      admin.name
      pagination.page
      pagination.per_page
  in
  Response.of_string' ~status:`OK response

(* TODO: PPX support for [@guard] attribute not yet implemented *)

(*
   (** PPX Example 1: Simple guard with attribute *)
let api_status (api_key : string) (_req : Request.t) : Response.t =
  let body = Printf.sprintf
    {|{"status": "operational", "api_key": "%s"}|}
    api_key
  in
  Response.of_string' ~status:`OK body
[@@route GET, "/api/status"]
[@@guard api_key_guard]

(** PPX Example 2: User profile with path param *)
let get_user ~id (user : user) (_req : Request.t) : Response.t =
  let body = Printf.sprintf
    {|{"requested_id": %Ld, "authenticated_user": "%s"}|}
    id user.name
  in
  Response.of_string' ~status:`OK body
[@@route GET, "/users/:id"]
[@@guard user_guard]

(** PPX Example 3: Admin dashboard *)
let admin_dashboard (admin : user) (_req : Request.t) : Response.t =
  Response.of_string'
    ~status:`OK
    {|{"message": "Admin dashboard", "admin": true}|}
[@@route GET, "/admin/dashboard"]
[@@guard admin_guard]
*)

let home_handler _req =
  Response.of_html
    ~status:`OK
    {|<h1>Request Guard Examples</h1>
<p>This example demonstrates type-safe request guards using extensible variants.</p>

<h2>Guard Syntax</h2>
<p>Guards use the <code>&gt;=&gt;</code> (Kleisli) operator for natural left-to-right composition:</p>
<pre>
(* Single guard *)
get (user_guard &gt;=&gt; s "users" / int64) @-&gt; fun user id req -&gt; ...
     ^^^^^^^^^^ guard extracts User.t first
                         ^^^^^^ path extracts int64 second
                                          ^^^^ ^^  parameters in order!

(* Multiple guards - use parentheses for clarity *)
get (admin_guard &gt;=&gt; (pagination_guard &gt;=&gt; (s "users" / s "list"))) @-&gt;
  fun admin pagination req -&gt; ...
      ^^^^^ first guard result
            ^^^^^^^^^^ second guard result
                             ^^^ request (always last)
Note: &gt;=&gt; is left-associative, so parentheses help readability
</pre>

<h2>Available Routes</h2>
<ul>
  <li><code>GET /manual/api-info</code> - Single guard: API key</li>
  <li><code>GET /manual/profile</code> - Single guard: User auth</li>
  <li><code>GET /manual/admin</code> - Single guard: Admin auth</li>
  <li><code>POST /manual/posts</code> - Combined guard: User + JSON</li>
  <li><code>PUT /manual/users/:id</code> - Guard + path param</li>
  <li><code>GET /manual/users/list?page=1&amp;per_page=20</code> - <strong>Multiple guards chained: Admin + Pagination</strong></li>
</ul>

<h2>Test Credentials</h2>
<pre>
API Key:      X-API-Key: any-value
User Token:   Authorization: Bearer valid-token  (admin user: Alice)
User Token:   Authorization: Bearer user-token   (regular user: Bob)
</pre>

<h2>Example cURL commands</h2>
<pre>
# API key example (any key works)
curl -H "X-API-Key: my-secret-key" http://localhost:8080/manual/api-info

# User profile (Alice - admin)
curl -H "Authorization: Bearer valid-token" http://localhost:8080/manual/profile

# Admin endpoint (requires admin role - only Alice)
curl -H "Authorization: Bearer valid-token" http://localhost:8080/manual/admin

# Try with Bob (not admin - should fail)
curl -H "Authorization: Bearer user-token" http://localhost:8080/manual/admin

# JSON post with auth
curl -X POST \
  -H "Authorization: Bearer valid-token" \
  -H "Content-Type: application/json" \
  -d '{"title": "Hello"}' \
  http://localhost:8080/manual/posts

# Update user (Alice can update anyone, Bob can only update himself)
curl -X PUT \
  -H "Authorization: Bearer valid-token" \
  -H "Content-Type: application/json" \
  -d '{"name": "Updated"}' \
  http://localhost:8080/manual/users/2

# Multiple guards - admin + pagination (only Alice is admin)
curl -H "Authorization: Bearer valid-token" \
  'http://localhost:8080/manual/users/list?page=2&per_page=10'

# Try pagination with invalid params
curl -H "Authorization: Bearer valid-token" \
  'http://localhost:8080/manual/users/list?page=0&per_page=200'
</pre>

<h2>Error Handling</h2>
<p>Guards use extensible variants for type-safe error handling:</p>
<pre>
type Request_guard.error +=
  | Not_admin
  | Invalid_user_token

(* Caught by middleware *)
try next req with
| Request_guard.Failed Not_admin -&gt; ... custom response
</pre>
|}

let not_found _req =
  Response.of_string' ~status:`Not_found {|{"error": "Not found"}|}

let guard_error_middleware :
  (Request.t -> Response.t) -> Request.t -> Response.t
  =
 fun next req ->
  try next req with
  | Request_guard.Failed (Request_guard.Missing_header name) ->
    Response.of_string'
      ~status:`Bad_request
      (Printf.sprintf {|{"error": "Missing required header: %s"}|} name)
  | Request_guard.Failed Request_guard.Invalid_bearer_token ->
    Response.of_string'
      ~status:`Unauthorized
      {|{"error": "Invalid or missing Bearer token"}|}
  | Request_guard.Failed Invalid_user_token ->
    Response.of_string'
      ~status:`Unauthorized
      {|{"error": "Invalid user token"}|}
  | Request_guard.Failed Not_admin ->
    Response.of_string'
      ~status:`Forbidden
      {|{"error": "Admin access required"}|}
  | Request_guard.Failed Json_required ->
    Response.of_string'
      ~status:`Bad_request
      {|{"error": "Content-Type must be application/json"}|}
  | Request_guard.Failed Invalid_pagination ->
    Response.of_string'
      ~status:`Bad_request
      {|{"error": "Invalid pagination parameters (page > 0, per_page 1-100)"}|}
  | Request_guard.Failed _ ->
    Response.of_string'
      ~status:`Bad_request
      {|{"error": "Request validation failed"}|}

let setup_app () =
  let open Router in
  App.(
    routes
      ~not_found
      [ get (s "") |> into home_handler
      ; (* Examples using request guard with >=> operator *)
        scope
          (s "manual")
          [ get (s "api-info") |> guard api_key_guard |> into api_info_handler
          ; get (s "profile") |> guard user_guard |> into profile_handler
          ; get (s "admin") |> guard admin_guard |> into admin_handler
          ; post (s "posts")
            |> guard authenticated_json_guard
            |> into create_post_handler
          ; put (s "users" / int64)
            |> guard user_guard
            |> into update_user_handler
          ; (* Multiple guards chained *)
            get (s "users" / s "list")
            |> guard pagination_guard
            |> guard admin_guard
            |> into user_list_handler
          ]
      ]
      ()
    <++> [ guard_error_middleware ])

let setup_log level =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let () =
  setup_log (Some Logs.Debug);
  Log.info (fun m -> m "Starting Request Guard Example Server");
  Log.info (fun m -> m "Server running at http://localhost:8080");

  Eio_main.run @@ fun env ->
  let port = 8080 in
  let address = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let config = Piaf.Server.Config.create ~domains:1 address in

  let app = setup_app () in
  ignore (Tapak.Server.run_with ~config ~env app)
