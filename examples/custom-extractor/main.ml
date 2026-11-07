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

type Router.extractor_error += Forbidden_not_admin

let verify_token = function
  (* In real app: validate JWT, query database, etc. *)
  | "valid-token" -> Some { id = 1; name = "Alice"; role = "admin" }
  | "user-token" -> Some { id = 2; name = "Bob"; role = "user" }
  | _ -> None

let user : user Router.schemaf = Router.(secured (bearer verify_token))

let admin : user Router.schemaf =
  user
  |> Router.refine (fun u ->
    if u.role = "admin" then Ok u else Error Forbidden_not_admin)

(** API key extractor example, documented as an apiKey security scheme *)
let api_key_auth : string Router.schemaf =
  Router.(secured (api_key ~in_:`Header "x-api-key" Option.some))

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
type Router.extractor_error += Forbidden_not_admin

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

(* The router returns a 401 by default and adds WWW-Authenticate for us, so this
   only chooses the body. *)
let unauthorized _req scheme reason =
  let message =
    match scheme, (reason : Router.Security.reason) with
    | Router.Security.Api_key _, _ -> "Invalid or missing API key"
    | _, Missing -> "Missing Authorization header"
    | _, Malformed -> "Invalid Bearer token format"
    | _, Invalid -> "Invalid token"
  in
  Tapak.json ~status:`Unauthorized (Sch.Json.encode_string error_schema message)

let extractor_error_middleware : Tapak.middleware =
 fun next req ->
  try next req with
  | Router.Extraction_failed Forbidden_not_admin ->
    Tapak.json
      ~status:`Forbidden
      (Sch.Json.encode_string error_schema "Admin access required")
  | Router.Extraction_failed _ ->
    Tapak.json
      ~status:`Bad_request
      (Sch.Json.encode_string error_schema "Extractor failed")

let routes =
  Router.
    [ get (s "") |> handle_request home_handler
    ; get (s "api-info") |> handle (map api_info_handler api_key_auth)
    ; get (s "profile") |> handle (map profile_handler user)
    ; get (s "admin" / s "dashboard")
      |> handle (map admin_dashboard_handler admin)
    ; get (s "admin" / s "stats")
      |> gate admin
      |> handle_request admin_stats_handler
    ; get (s "users" / int64) |> handle (map get_user_handler user)
    ]

let openapi_route =
  let document =
    Tapak.Private.Openapi.generate_router2_string
      ~title:"Custom Extractors"
      routes
  in
  Router.(
    get (s "openapi.json") |> handle_request (fun _req -> Tapak.json document))

let setup_app () =
  Router.of_list ~not_found ~unauthorized (openapi_route :: routes)
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
