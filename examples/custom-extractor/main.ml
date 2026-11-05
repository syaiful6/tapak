(**
   Custom Extractors Example

   This example demonstrates custom extractors in Tapak, similar to
   extractors in Axum/Warp (Rust). Extractors allow you to define custom types
   that can be "extracted" from requests with type-safe error handling.

   Routes are written against the applicative [Router2]: a handler is built by
   parsing the request, and anything it doesn't need (a gate) never reaches it.

   Use cases:
   - Authentication: Extract authenticated user from token
   - Authorization: Extract user with specific roles/permissions
   - Rate limiting: Extract rate limit info
   - Custom validation: Extract and validate domain-specific data
*)

module Log = (val Logs.src_log Logs.default : Logs.LOG)
module Router = Tapak.Private.Router2

type user =
  { id : int
  ; name : string
  ; role : string
  }

let user_schema =
  Sch.Object.(
    define ~kind:"User"
    @@
    let+ id = mem ~enc:(fun u -> u.id) "id" Sch.int
    and+ name = mem ~enc:(fun u -> u.name) "name" Sch.string
    and+ role = mem ~enc:(fun u -> u.role) "role" Sch.string in
    { id; name; role })

type Router.extractor_error +=
  | Missing_auth_header
  | Invalid_bearer_token
  | Invalid_token
  | Forbidden_not_admin
  | Invalid_api_key

let user : user Router.schemaf =
  Router.(secured (Security.bearer ())) (fun req ->
    match Tapak.Request.header "Authorization" req with
    | Some auth when String.starts_with ~prefix:"Bearer " auth ->
      let token = String.sub auth 7 (String.length auth - 7) in
      (* In real app: validate JWT, query database, etc. *)
      if token = "valid-token"
      then Ok { id = 1; name = "Alice"; role = "admin" }
      else if token = "user-token"
      then Ok { id = 2; name = "Bob"; role = "user" }
      else Error Invalid_token
    | Some _ -> Error Invalid_bearer_token
    | None -> Error Missing_auth_header)

let admin : user Router.schemaf =
  user
  |> Router.refine (fun u ->
    if u.role = "admin" then Ok u else Error Forbidden_not_admin)

let api_key_schema =
  Sch.Object.(
    define ~kind:"ApiKey"
    @@ mem
         ~enc:Fun.id
         "x-api-key"
         Sch.(with_ ~constraint_:(Constraint.min_length 1) string))

(** API key extractor example, built on a header schema so it is documented as
    required. [fail] replaces the validation error with our own. *)
let api_key : string Router.schemaf =
  Router.(header api_key_schema <|> fail Invalid_api_key)

let json_to_string json =
  Jsont_bytesrw.encode_string Jsont.json json |> Result.get_ok

let api_info_handler key =
  Tapak.json
    (json_to_string
       Jsont.Json.(
         object'
           [ name "message", string "API access granted"
           ; name "key", string key
           ]))

let profile_handler user = Tapak.json (Sch.Json.encode_string user_schema user)

(** Handler receives extracted Admin directly - guaranteed to be admin *)
let admin_dashboard_handler admin =
  Tapak.json
    (json_to_string
       Jsont.Json.(
         object'
           [ name "message", string "Welcome to admin dashboard"
           ; name "admin", string admin.name
           ]))

let get_user_handler user user_id =
  Tapak.json
    (json_to_string
       Jsont.Json.(
         object'
           [ name "requested_user_id", string (Int64.to_string user_id)
           ; ( name "authenticated_as"
             , string (Printf.sprintf "%s (id=%d)" user.name user.id) )
           ]))

let admin_stats_handler _req =
  Tapak.json
    (json_to_string Jsont.Json.(object' [ name "active_users", int 42 ]))

let home_handler _req =
  Tapak.html
    {|<h1>Custom Extractors Example</h1>
<p>This example demonstrates custom extractors similar to Axum/Warp in Rust.</p>

<h2>What are Extractors?</h2>
<p>Extractors allow you to define custom types that can be "extracted" from requests.
Common use cases:</p>
<ul>
  <li><strong>Authentication</strong>: Extract authenticated User from token</li>
  <li><strong>Authorization</strong>: Extract users with specific roles (Admin)</li>
  <li><strong>API Keys</strong>: Extract and validate API keys</li>
  <li><strong>Custom validation</strong>: Extract domain-specific data</li>
</ul>

<h2>Available Routes</h2>
<ul>
  <li><code>GET /api-info</code> - Extracts API key from header</li>
  <li><code>GET /profile</code> - Extracts authenticated User</li>
  <li><code>GET /admin/dashboard</code> - Extracts Admin (validates role)</li>
  <li><code>GET /users/:id</code> - Combines User extractor + path param</li>
  <li><code>GET /admin/stats</code> - Gated by Admin, handler doesn't receive it</li>
  <li><code>GET /openapi.json</code> - Generated from the same routes</li>
</ul>

<h2>Test Credentials</h2>
<pre>
API Key:      X-API-Key: any-non-empty-value
User Token:   Authorization: Bearer valid-token  (Alice, admin)
User Token:   Authorization: Bearer user-token   (Bob, regular user)
</pre>

<h2>Example cURL commands</h2>
<pre>
# Extract API key
curl -H "X-API-Key: my-secret-key" http://localhost:8080/api-info

# Extract User (Alice)
curl -H "Authorization: Bearer valid-token" http://localhost:8080/profile

# Extract User (Bob)
curl -H "Authorization: Bearer user-token" http://localhost:8080/profile

# Extract Admin - only Alice works
curl -H "Authorization: Bearer valid-token" http://localhost:8080/admin/dashboard

# Try with Bob (not admin - will fail with 403)
curl -H "Authorization: Bearer user-token" http://localhost:8080/admin/dashboard

# Extract User + path parameter
curl -H "Authorization: Bearer valid-token" http://localhost:8080/users/123
</pre>

<h2>Error Handling</h2>
<p>Extractors use extensible error variants for type-safe error handling:</p>
<pre>
type Router.extractor_error +=
  | Missing_auth_header
  | Invalid_token
  | Forbidden_not_admin

(* Caught by middleware *)
try next req with
| Router.Extraction_failed Forbidden_not_admin -&gt;
    Response.of_string ~status:`Forbidden "..."
</pre>
|}

let error_schema =
  Sch.Object.(define ~kind:"Error" @@ mem ~enc:Fun.id "error" Sch.string)

let not_found _req =
  Tapak.json
    ~status:`Not_found
    (Sch.Json.encode_string error_schema "Not found")

let extractor_error_middleware : Tapak.middleware =
 fun next req ->
  try next req with
  | Router.Extraction_failed Missing_auth_header ->
    Tapak.json
      ~status:`Unauthorized
      (Sch.Json.encode_string error_schema "Missing Authorization header")
  | Router.Extraction_failed Invalid_bearer_token ->
    Tapak.json
      ~status:`Unauthorized
      (Sch.Json.encode_string error_schema "Invalid Bearer token format")
  | Router.Extraction_failed Invalid_token ->
    Tapak.json
      ~status:`Unauthorized
      (Sch.Json.encode_string error_schema "Invalid token")
  | Router.Extraction_failed Forbidden_not_admin ->
    Tapak.json
      ~status:`Forbidden
      (Sch.Json.encode_string error_schema "Admin access required")
  | Router.Extraction_failed Invalid_api_key ->
    Tapak.json
      ~status:`Unauthorized
      (Sch.Json.encode_string error_schema "Invalid or missing API key")
  | Router.Extraction_failed _ ->
    Tapak.json
      ~status:`Bad_request
      (Sch.Json.encode_string error_schema "Extractor failed")

let routes =
  Router.
    [ get (s "") >> home_handler
    ; get (s "api-info") >-> map api_info_handler api_key
    ; get (s "profile") >-> map profile_handler user
    ; get (s "admin" / s "dashboard") >-> map admin_dashboard_handler admin
    ; get (s "admin" / s "stats") |> gate admin >> admin_stats_handler
    ; get (s "users" / int64) >-> map get_user_handler user
    ]

let openapi_route =
  let document =
    Tapak.Private.Openapi.generate_router2_string
      ~title:"Custom Extractors"
      routes
  in
  Router.(get (s "openapi.json") >> fun _req -> Tapak.json document)

let setup_app () =
  Router.of_list ~not_found (openapi_route :: routes)
  |> extractor_error_middleware

let setup_log level =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let () =
  setup_log (Some Logs.Debug);
  Log.info (fun m -> m "Starting Custom Extractors Example Server");
  Log.info (fun m -> m "Server running at http://localhost:8080");

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let port = 8080 in
  let address = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let app = setup_app () in
  let socket =
    Eio.Net.listen ~reuse_addr:true ~backlog:1024 ~sw env#net address
  in
  Tapak.run
    ~on_error:(fun exn ->
      Logs.warn (fun f -> f "Uncaught exception %s" (Printexc.to_string exn)))
    socket
    app
