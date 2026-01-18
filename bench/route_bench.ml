open Tapak

let create_mock_request ~meth ~path =
  Request.create
    ~scheme:`HTTP
    ~version:Piaf.Versions.HTTP.HTTP_1_1
    ~meth
    ~body:Piaf.Body.empty
    path

let ok body = Response.of_string ~body `OK

(* Google+ API - 13 routes *)
let gplus_api =
  let open Router in
  [ get (s "people" / str) |> into (fun user_id -> ok user_id)
  ; get (s "people") |> unit |> into (fun () -> ok "people")
  ; get (s "activities" / str / s "people" / str)
    |> into (fun activity_id collection -> ok (activity_id ^ collection))
  ; get (s "people" / str / s "people" / str)
    |> into (fun user_id collection -> ok (user_id ^ collection))
  ; get (s "people" / str / s "openIdConnect")
    |> into (fun user_id -> ok user_id)
  ; get (s "people" / str / s "activities" / str)
    |> into (fun user_id collection -> ok (user_id ^ collection))
  ; get (s "activities" / str) |> into (fun activity_id -> ok activity_id)
  ; get (s "activities") |> unit |> into (fun () -> ok "activities")
  ; get (s "activities" / str / s "comments")
    |> into (fun activity_id -> ok activity_id)
  ; get (s "comments" / str) |> into (fun comment_id -> ok comment_id)
  ; post (s "people" / str / s "moments" / str)
    |> into (fun user_id collection -> ok (user_id ^ collection))
  ; get (s "people" / str / s "moments" / str)
    |> into (fun user_id collection -> ok (user_id ^ collection))
  ; delete (s "moments" / str) |> into (fun id -> ok id)
  ]

(* GitHub API - 203 routes *)
let github_api =
  let open Router in
  [ get (s "authorizations") |> unit |> into (fun () -> ok "authorizations")
  ; get (s "authorizations" / str) |> into (fun id -> ok id)
  ; post (s "authorizations") |> unit |> into (fun () -> ok "create")
  ; delete (s "authorizations" / str) |> into (fun id -> ok id)
  ; get (s "applications" / str / s "tokens" / str)
    |> into (fun client_id token -> ok (client_id ^ token))
  ; delete (s "applications" / str / s "tokens")
    |> into (fun client_id -> ok client_id)
  ; delete (s "applications" / str / s "tokens" / str)
    |> into (fun client_id token -> ok (client_id ^ token))
  ; get (s "events") |> unit |> into (fun () -> ok "events")
  ; get (s "repos" / str / str / s "events")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "networks" / str / str / s "events")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "orgs" / str / s "events") |> into (fun org -> ok org)
  ; get (s "users" / str / s "received_events") |> into (fun user -> ok user)
  ; get (s "users" / str / s "received_events" / s "public")
    |> into (fun user -> ok user)
  ; get (s "users" / str / s "events") |> into (fun user -> ok user)
  ; get (s "users" / str / s "events" / s "public")
    |> into (fun user -> ok user)
  ; get (s "users" / str / s "events" / s "orgs" / str)
    |> into (fun user org -> ok (user ^ org))
  ; get (s "feeds") |> unit |> into (fun () -> ok "feeds")
  ; get (s "notifications") |> unit |> into (fun () -> ok "notifications")
  ; get (s "repos" / str / str / s "notifications")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; put (s "notifications") |> unit |> into (fun () -> ok "put")
  ; put (s "repos" / str / str / s "notifications")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "notifications" / s "threads" / str) |> into (fun id -> ok id)
  ; get (s "notifications" / s "threads" / str / s "subscription")
    |> into (fun id -> ok id)
  ; put (s "notifications" / s "threads" / str / s "subscription")
    |> into (fun id -> ok id)
  ; delete (s "notifications" / s "threads" / str / s "subscription")
    |> into (fun id -> ok id)
  ; get (s "repos" / str / str / s "stargazers")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "users" / str / s "starred") |> into (fun user -> ok user)
  ; get (s "user" / s "starred") |> unit |> into (fun () -> ok "starred")
  ; get (s "user" / s "starred" / str / str)
    |> into (fun owner repo -> ok (owner ^ repo))
  ; put (s "user" / s "starred" / str / str)
    |> into (fun owner repo -> ok (owner ^ repo))
  ; delete (s "user" / s "starred" / str / str)
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "subscribers")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "users" / str / s "subscriptions") |> into (fun user -> ok user)
  ; get (s "user" / s "subscriptions")
    |> unit
    |> into (fun () -> ok "subscriptions")
  ; get (s "repos" / str / str / s "subscription")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; put (s "repos" / str / str / s "subscription")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; delete (s "repos" / str / str / s "subscription")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "user" / s "subscriptions" / str / str)
    |> into (fun owner repo -> ok (owner ^ repo))
  ; put (s "user" / s "subscriptions" / str / str)
    |> into (fun owner repo -> ok (owner ^ repo))
  ; delete (s "user" / s "subscriptions" / str / str)
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "users" / str / s "gists") |> into (fun user -> ok user)
  ; get (s "gists") |> unit |> into (fun () -> ok "gists")
  ; get (s "gists" / str) |> into (fun id -> ok id)
  ; post (s "gists") |> unit |> into (fun () -> ok "create")
  ; put (s "gists" / str / s "star") |> into (fun id -> ok id)
  ; delete (s "gists" / str / s "star") |> into (fun id -> ok id)
  ; get (s "gists" / str / s "star") |> into (fun id -> ok id)
  ; post (s "gists" / str / s "forks") |> into (fun id -> ok id)
  ; delete (s "gists" / str) |> into (fun id -> ok id)
  ; get (s "repos" / str / str / s "git" / s "blobs" / str)
    |> into (fun owner repo sha -> ok (owner ^ repo ^ sha))
  ; post (s "repos" / str / str / s "git" / s "blobs")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "git" / s "commits" / str)
    |> into (fun owner repo sha -> ok (owner ^ repo ^ sha))
  ; post (s "repos" / str / str / s "git" / s "commits")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "git" / s "refs")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; post (s "repos" / str / str / s "git" / s "refs")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "git" / s "tags" / str)
    |> into (fun owner repo sha -> ok (owner ^ repo ^ sha))
  ; post (s "repos" / str / str / s "git" / s "tags")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "git" / s "trees" / str)
    |> into (fun owner repo sha -> ok (owner ^ repo ^ sha))
  ; post (s "repos" / str / str / s "git" / s "trees")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "issues") |> unit |> into (fun () -> ok "issues")
  ; get (s "user" / s "issues") |> unit |> into (fun () -> ok "user-issues")
  ; get (s "orgs" / str / s "issues") |> into (fun org -> ok org)
  ; get (s "repos" / str / str / s "issues")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "issues" / str)
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; post (s "repos" / str / str / s "issues")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "assignees")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "assignees" / str)
    |> into (fun owner repo assignee -> ok (owner ^ repo ^ assignee))
  ; get (s "repos" / str / str / s "issues" / str / s "comments")
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; post (s "repos" / str / str / s "issues" / str / s "comments")
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "repos" / str / str / s "issues" / str / s "events")
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "repos" / str / str / s "labels")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "labels" / str)
    |> into (fun owner repo name -> ok (owner ^ repo ^ name))
  ; post (s "repos" / str / str / s "labels")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; delete (s "repos" / str / str / s "labels" / str)
    |> into (fun owner repo name -> ok (owner ^ repo ^ name))
  ; get (s "repos" / str / str / s "issues" / str / s "labels")
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; post (s "repos" / str / str / s "issues" / str / s "labels")
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; delete (s "repos" / str / str / s "issues" / str / s "labels" / str)
    |> into (fun owner repo number name -> ok (owner ^ repo ^ number ^ name))
  ; put (s "repos" / str / str / s "issues" / str / s "labels")
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; delete (s "repos" / str / str / s "issues" / str / s "labels")
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "repos" / str / str / s "milestones" / str / s "labels")
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "repos" / str / str / s "milestones")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "milestones" / str)
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; post (s "repos" / str / str / s "milestones")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; delete (s "repos" / str / str / s "milestones" / str)
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "emojis") |> unit |> into (fun () -> ok "emojis")
  ; get (s "gitignore" / s "templates")
    |> unit
    |> into (fun () -> ok "templates")
  ; get (s "gitignore" / s "templates" / str) |> into (fun name -> ok name)
  ; post (s "markdown") |> unit |> into (fun () -> ok "markdown")
  ; post (s "markdown" / s "raw") |> unit |> into (fun () -> ok "raw")
  ; get (s "meta") |> unit |> into (fun () -> ok "meta")
  ; get (s "rate_limit") |> unit |> into (fun () -> ok "rate_limit")
  ; get (s "users" / str / s "orgs") |> into (fun user -> ok user)
  ; get (s "user" / s "orgs") |> unit |> into (fun () -> ok "orgs")
  ; get (s "orgs" / str) |> into (fun org -> ok org)
  ; get (s "orgs" / str / s "members") |> into (fun org -> ok org)
  ; get (s "orgs" / str / s "members" / str)
    |> into (fun org user -> ok (org ^ user))
  ; delete (s "orgs" / str / s "members" / str)
    |> into (fun org user -> ok (org ^ user))
  ; get (s "orgs" / str / s "public_members") |> into (fun org -> ok org)
  ; get (s "orgs" / str / s "public_members" / str)
    |> into (fun org user -> ok (org ^ user))
  ; put (s "orgs" / str / s "public_members" / str)
    |> into (fun org user -> ok (org ^ user))
  ; delete (s "orgs" / str / s "public_members" / str)
    |> into (fun org user -> ok (org ^ user))
  ; get (s "orgs" / str / s "teams") |> into (fun org -> ok org)
  ; get (s "teams" / str) |> into (fun id -> ok id)
  ; post (s "orgs" / str / s "teams") |> into (fun org -> ok org)
  ; delete (s "teams" / str) |> into (fun id -> ok id)
  ; get (s "teams" / str / s "members") |> into (fun id -> ok id)
  ; get (s "teams" / str / s "members" / str)
    |> into (fun id user -> ok (id ^ user))
  ; put (s "teams" / str / s "members" / str)
    |> into (fun id user -> ok (id ^ user))
  ; delete (s "teams" / str / s "members" / str)
    |> into (fun id user -> ok (id ^ user))
  ; get (s "teams" / str / s "repos") |> into (fun id -> ok id)
  ; get (s "teams" / str / s "repos" / str / str)
    |> into (fun id owner repo -> ok (id ^ owner ^ repo))
  ; put (s "teams" / str / s "repos" / str / str)
    |> into (fun id owner repo -> ok (id ^ owner ^ repo))
  ; delete (s "teams" / str / s "repos" / str / str)
    |> into (fun id owner repo -> ok (id ^ owner ^ repo))
  ; get (s "user" / s "teams") |> unit |> into (fun () -> ok "teams")
  ; get (s "repos" / str / str / s "pulls")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "pulls" / str)
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; post (s "repos" / str / str / s "pulls")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "pulls" / str / s "commits")
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "repos" / str / str / s "pulls" / str / s "files")
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "repos" / str / str / s "pulls" / str / s "merge")
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; put (s "repos" / str / str / s "pulls" / str / s "merge")
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "repos" / str / str / s "pulls" / str / s "comments")
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; put (s "repos" / str / str / s "pulls" / str / s "comments")
    |> into (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "user" / s "repos") |> unit |> into (fun () -> ok "repos")
  ; get (s "users" / str / s "repos") |> into (fun user -> ok user)
  ; get (s "orgs" / str / s "repos") |> into (fun org -> ok org)
  ; get (s "repositories") |> unit |> into (fun () -> ok "repositories")
  ; post (s "user" / s "repos") |> unit |> into (fun () -> ok "create")
  ; post (s "orgs" / str / s "repos") |> into (fun org -> ok org)
  ; get (s "repos" / str / str) |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "contributors")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "languages")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "teams")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "tags")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "branches")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "branches" / str)
    |> into (fun owner repo branch -> ok (owner ^ repo ^ branch))
  ; delete (s "repos" / str / str) |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "collaborators")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "collaborators" / str)
    |> into (fun owner repo user -> ok (owner ^ repo ^ user))
  ; put (s "repos" / str / str / s "collaborators" / str)
    |> into (fun owner repo user -> ok (owner ^ repo ^ user))
  ; delete (s "repos" / str / str / s "collaborators" / str)
    |> into (fun owner repo user -> ok (owner ^ repo ^ user))
  ; get (s "repos" / str / str / s "comments")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "commits" / str / s "comments")
    |> into (fun owner repo sha -> ok (owner ^ repo ^ sha))
  ; post (s "repos" / str / str / s "commits" / str / s "comments")
    |> into (fun owner repo sha -> ok (owner ^ repo ^ sha))
  ; get (s "repos" / str / str / s "comments" / str)
    |> into (fun owner repo id -> ok (owner ^ repo ^ id))
  ; delete (s "repos" / str / str / s "comments" / str)
    |> into (fun owner repo id -> ok (owner ^ repo ^ id))
  ; get (s "repos" / str / str / s "commits")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "commits" / str)
    |> into (fun owner repo sha -> ok (owner ^ repo ^ sha))
  ; get (s "repos" / str / str / s "readme")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "keys")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "keys" / str)
    |> into (fun owner repo id -> ok (owner ^ repo ^ id))
  ; post (s "repos" / str / str / s "keys")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; delete (s "repos" / str / str / s "keys" / str)
    |> into (fun owner repo id -> ok (owner ^ repo ^ id))
  ; get (s "repos" / str / str / s "downloads")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "downloads" / str)
    |> into (fun owner repo id -> ok (owner ^ repo ^ id))
  ; delete (s "repos" / str / str / s "downloads" / str)
    |> into (fun owner repo id -> ok (owner ^ repo ^ id))
  ; get (s "repos" / str / str / s "forks")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; post (s "repos" / str / str / s "forks")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "hooks")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "hooks" / str)
    |> into (fun owner repo id -> ok (owner ^ repo ^ id))
  ; post (s "repos" / str / str / s "hooks")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; post (s "repos" / str / str / s "hooks" / str / s "tests")
    |> into (fun owner repo id -> ok (owner ^ repo ^ id))
  ; delete (s "repos" / str / str / s "hooks" / str)
    |> into (fun owner repo id -> ok (owner ^ repo ^ id))
  ; post (s "repos" / str / str / s "merges")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "releases")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "releases" / str)
    |> into (fun owner repo id -> ok (owner ^ repo ^ id))
  ; post (s "repos" / str / str / s "releases")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; delete (s "repos" / str / str / s "releases" / str)
    |> into (fun owner repo id -> ok (owner ^ repo ^ id))
  ; get (s "repos" / str / str / s "releases" / str / s "assets")
    |> into (fun owner repo id -> ok (owner ^ repo ^ id))
  ; get (s "repos" / str / str / s "stats" / s "contributors")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "stats" / s "commit_activity")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "stats" / s "code_frequency")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "stats" / s "participation")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "stats" / s "punch_card")
    |> into (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "statuses" / str)
    |> into (fun owner repo ref_ -> ok (owner ^ repo ^ ref_))
  ; post (s "repos" / str / str / s "statuses" / str)
    |> into (fun owner repo ref_ -> ok (owner ^ repo ^ ref_))
  ; get (s "search" / s "repositories")
    |> unit
    |> into (fun () -> ok "search-repos")
  ; get (s "search" / s "code") |> unit |> into (fun () -> ok "search-code")
  ; get (s "search" / s "issues") |> unit |> into (fun () -> ok "search-issues")
  ; get (s "search" / s "users") |> unit |> into (fun () -> ok "search-users")
  ; get (s "legacy" / s "issues" / s "search" / str / str / str / str)
    |> into (fun owner repo state keyword ->
      ok (owner ^ repo ^ state ^ keyword))
  ; get (s "legacy" / s "repos" / s "search" / str)
    |> into (fun keyword -> ok keyword)
  ; get (s "legacy" / s "user" / s "search" / str)
    |> into (fun keyword -> ok keyword)
  ; get (s "legacy" / s "user" / s "email" / str)
    |> into (fun email -> ok email)
  ; get (s "users" / str) |> into (fun user -> ok user)
  ; get (s "user") |> unit |> into (fun () -> ok "user")
  ; get (s "users") |> unit |> into (fun () -> ok "users")
  ; get (s "user" / s "emails") |> unit |> into (fun () -> ok "emails")
  ; post (s "user" / s "emails") |> unit |> into (fun () -> ok "create")
  ; delete (s "user" / s "emails") |> unit |> into (fun () -> ok "delete")
  ; get (s "users" / str / s "followers") |> into (fun user -> ok user)
  ; get (s "user" / s "followers") |> unit |> into (fun () -> ok "followers")
  ; get (s "users" / str / s "following") |> into (fun user -> ok user)
  ; get (s "user" / s "following") |> unit |> into (fun () -> ok "following")
  ; get (s "user" / s "following" / str) |> into (fun user -> ok user)
  ; get (s "users" / str / s "following" / str)
    |> into (fun user target -> ok (user ^ target))
  ; put (s "user" / s "following" / str) |> into (fun user -> ok user)
  ; delete (s "user" / s "following" / str) |> into (fun user -> ok user)
  ; get (s "users" / str / s "keys") |> into (fun user -> ok user)
  ; get (s "user" / s "keys") |> unit |> into (fun () -> ok "keys")
  ; get (s "user" / s "keys" / str) |> into (fun id -> ok id)
  ; post (s "user" / s "keys") |> unit |> into (fun () -> ok "create")
  ; delete (s "user" / s "keys" / str) |> into (fun id -> ok id)
  ]

(* Test requests matching the Go benchmark *)
let github_requests =
  [ `GET, "/user/repos"
  ; `GET, "/repos/julienschmidt/httprouter/events"
  ; `GET, "/repos/julienschmidt/httprouter/git/commits/abc123"
  ; `GET, "/repos/julienschmidt/httprouter/issues/1"
  ; `GET, "/repos/julienschmidt/httprouter/pulls/1/files"
  ; `GET, "/users/julienschmidt"
  ; `GET, "/user"
  ; `GET, "/authorizations"
  ; `GET, "/teams/123/repos/foo/bar"
  ; `GET, "/search/repositories"
  ]

let gplus_requests =
  [ `GET, "/people/me"
  ; `GET, "/people"
  ; `GET, "/people/me/activities/public"
  ; `GET, "/activities/123"
  ; `GET, "/activities/123/comments"
  ; `POST, "/people/me/moments/vault"
  ; `GET, "/comments/123"
  ]

let bench_single_route (router, req) =
  let _resp = router req in
  ()

let bench_all_routes (router, requests) =
  List.iter
    (fun (meth, path) ->
       let req = create_mock_request ~meth ~path in
       let _resp = router req in
       ())
    requests

let () =
  let open Benchmark in
  Printf.printf " Google+ API (13 routes)\n";

  let gplus_router = Router.routes gplus_api in

  Printf.printf "--- GPlus Static (no params): GET /people ---\n";
  let req = create_mock_request ~meth:`GET ~path:"/people" in
  let results =
    throughputN
      ~repeat:3
      2
      [ "GPlus Static", bench_single_route, (gplus_router, req) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf "--- GPlus Param: GET /people/:userId ---\n";
  let req = create_mock_request ~meth:`GET ~path:"/people/me" in
  let results =
    throughputN
      ~repeat:3
      2
      [ "GPlus Param", bench_single_route, (gplus_router, req) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf
    "--- GPlus 2 Params: GET /people/:userId/activities/:collection ---\n";
  let req =
    create_mock_request ~meth:`GET ~path:"/people/me/activities/public"
  in
  let results =
    throughputN
      ~repeat:3
      2
      [ "GPlus 2 Params", bench_single_route, (gplus_router, req) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf "--- GPlus All (13 routes, 7 requests) ---\n";
  let results =
    throughputN
      ~repeat:3
      2
      [ "GPlus All", bench_all_routes, (gplus_router, gplus_requests) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf " GitHub API (203 routes)\n";

  let github_router = Router.routes github_api in

  Printf.printf "--- GitHub Static (no params): GET /user ---\n";
  let req = create_mock_request ~meth:`GET ~path:"/user" in
  let results =
    throughputN
      ~repeat:3
      2
      [ "GitHub Static", bench_single_route, (github_router, req) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf "--- GitHub Param: GET /users/:user ---\n";
  let req = create_mock_request ~meth:`GET ~path:"/users/julienschmidt" in
  let results =
    throughputN
      ~repeat:3
      2
      [ "GitHub Param", bench_single_route, (github_router, req) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf "--- GitHub 2 Params: GET /repos/:owner/:repo ---\n";
  let req =
    create_mock_request ~meth:`GET ~path:"/repos/julienschmidt/httprouter"
  in
  let results =
    throughputN
      ~repeat:3
      2
      [ "GitHub 2 Params", bench_single_route, (github_router, req) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf
    "--- GitHub Deep (5 segments): GET /repos/:owner/:repo/pulls/:number/files \
     ---\n";
  let req =
    create_mock_request
      ~meth:`GET
      ~path:"/repos/julienschmidt/httprouter/pulls/1/files"
  in
  let results =
    throughputN
      ~repeat:3
      2
      [ "GitHub Deep", bench_single_route, (github_router, req) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf "--- GitHub All (203 routes, 10 requests) ---\n";
  let results =
    throughputN
      ~repeat:3
      2
      [ "GitHub All", bench_all_routes, (github_router, github_requests) ]
  in
  tabulate results;
  Printf.printf "\n"
