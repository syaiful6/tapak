(**
   Custom Extractors Example

   This example demonstrates custom extractors in Tapak, similar to
   extractors in Axum/Warp (Rust). Extractors allow you to define custom types
   that can be "extracted" from requests with type-safe error handling.

   Use cases:
   - Authentication: Extract authenticated user from token
   - Authorization: Extract user with specific roles/permissions
   - Rate limiting: Extract rate limit info
   - Custom validation: Extract and validate domain-specific data
*)

module Log = (val Logs.src_log Logs.default : Logs.LOG)

type user =
  { id : int
  ; name : string
  ; role : string
  }

type admin = user

type Tapak.Router.extractor_error +=
  | Missing_auth_header
  | Invalid_bearer_token
  | Invalid_token
  | Forbidden_not_admin
  | Invalid_api_key

let user : user Tapak.Router.extractor =
 fun req ->
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
  | None -> Error Missing_auth_header

let admin : admin Tapak.Router.extractor =
 fun req ->
  match user req with
  | Ok u when u.role = "admin" -> Ok u
  | Ok _ -> Error Forbidden_not_admin
  | Error e -> Error e

(** API key extractor example *)
let api_key : string Tapak.Router.extractor =
 fun req ->
  match Tapak.Request.header "x-api-key" req with
  | Some key when String.length key > 0 -> Ok key
  | _ -> Error Invalid_api_key

let api_info_handler key =
  Tapak.json
    (`Assoc [ "message", `String "API access granted"; "key", `String key ])

let profile_handler user =
  Tapak.json
    (`Assoc
        [ "id", `Int user.id
        ; "name", `String user.name
        ; "role", `String user.role
        ])

(** Handler receives extracted Admin directly - guaranteed to be admin *)
let admin_dashboard_handler admin =
  Tapak.json
    (`Assoc
        [ "meessage", `String "Welcome to admin dashboard"
        ; "admin", `String admin.name
        ])

let get_user_handler user user_id =
  Tapak.json
    (`Assoc
        [ "requested_user_id", `String (Int64.to_string user_id)
        ; ( "authenticated_as"
          , `String (Printf.sprintf "%s (id=%d)" user.name user.id) )
        ])

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

let not_found _req =
  Tapak.json ~status:`Not_found (`Assoc [ "error", `String "Not found" ])

let extractor_error_middleware : Tapak.middleware =
 fun next req ->
  try next req with
  | Tapak.Router.Extraction_failed Missing_auth_header ->
    Tapak.json
      ~status:`Unauthorized
      (`Assoc [ "error", `String "Missing Authorization header" ])
  | Tapak.Router.Extraction_failed Invalid_bearer_token ->
    Tapak.json
      ~status:`Unauthorized
      (`Assoc [ "error", `String "Invalid Bearer token format" ])
  | Tapak.Router.Extraction_failed Invalid_token ->
    Tapak.json
      ~status:`Unauthorized
      (`Assoc [ "error", `String "Invalid token" ])
  | Tapak.Router.Extraction_failed Forbidden_not_admin ->
    Tapak.json
      ~status:`Forbidden
      (`Assoc [ "error", `String "Admin access required" ])
  | Tapak.Router.Extraction_failed Invalid_api_key ->
    Tapak.json
      ~status:`Unauthorized
      (`Assoc [ "error", `String "Invalid or missing API key" ])
  | Tapak.Router.Extraction_failed _ ->
    Tapak.json
      ~status:`Bad_request
      (`Assoc [ "error", `String "Extractor failed" ])

let setup_app () =
  Tapak.Router.(
    routes
      ~not_found
      [ get (s "") |> unit |> into home_handler
      ; get (s "api-info") |> extract api_key |> into api_info_handler
      ; get (s "profile") |> extract user |> into profile_handler
      ; get (s "admin" / s "dashboard")
        |> extract admin
        |> into admin_dashboard_handler
      ; get (s "users" / int64) |> extract user |> into get_user_handler
      ])
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
  let port = 8080 in
  let address = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let config = Piaf.Server.Config.create ~domains:1 address in

  let app = setup_app () in
  ignore (Tapak.run_with ~config ~env app)
