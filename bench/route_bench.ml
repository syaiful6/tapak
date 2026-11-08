open Tapak

let create_mock_request ~meth ~path =
  Request.make
    ~version:`HTTP_1_1
    ~meth
    ~body:(Bytesrw.Bytes.Reader.empty ())
    path

let ok body = Response.of_string ~status:`OK body

(* Google+ API - 13 routes *)
let gplus_api =
  let open Router in
  [ get (s "people" / str) >> pure (fun user_id -> ok user_id)
  ; get (s "people") |> handle_request (fun _ -> ok "people")
  ; get (s "activities" / str / s "people" / str)
    >> pure (fun activity_id collection -> ok (activity_id ^ collection))
  ; get (s "people" / str / s "people" / str)
    >> pure (fun user_id collection -> ok (user_id ^ collection))
  ; get (s "people" / str / s "openIdConnect")
    >> pure (fun user_id -> ok user_id)
  ; get (s "people" / str / s "activities" / str)
    >> pure (fun user_id collection -> ok (user_id ^ collection))
  ; get (s "activities" / str) >> pure (fun activity_id -> ok activity_id)
  ; get (s "activities") |> handle_request (fun _ -> ok "activities")
  ; get (s "activities" / str / s "comments")
    >> pure (fun activity_id -> ok activity_id)
  ; get (s "comments" / str) >> pure (fun comment_id -> ok comment_id)
  ; post (s "people" / str / s "moments" / str)
    >> pure (fun user_id collection -> ok (user_id ^ collection))
  ; get (s "people" / str / s "moments" / str)
    >> pure (fun user_id collection -> ok (user_id ^ collection))
  ; delete (s "moments" / str) >> pure (fun id -> ok id)
  ]

(* GitHub API - 203 routes *)
let github_api =
  let open Router in
  [ get (s "authorizations") |> handle_request (fun _ -> ok "authorizations")
  ; get (s "authorizations" / str) >> pure (fun id -> ok id)
  ; post (s "authorizations") |> handle_request (fun _ -> ok "create")
  ; delete (s "authorizations" / str) >> pure (fun id -> ok id)
  ; get (s "applications" / str / s "tokens" / str)
    >> pure (fun client_id token -> ok (client_id ^ token))
  ; delete (s "applications" / str / s "tokens")
    >> pure (fun client_id -> ok client_id)
  ; delete (s "applications" / str / s "tokens" / str)
    >> pure (fun client_id token -> ok (client_id ^ token))
  ; get (s "events") |> handle_request (fun _ -> ok "events")
  ; get (s "repos" / str / str / s "events")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "networks" / str / str / s "events")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "orgs" / str / s "events") >> pure (fun org -> ok org)
  ; get (s "users" / str / s "received_events") >> pure (fun user -> ok user)
  ; get (s "users" / str / s "received_events" / s "public")
    >> pure (fun user -> ok user)
  ; get (s "users" / str / s "events") >> pure (fun user -> ok user)
  ; get (s "users" / str / s "events" / s "public")
    >> pure (fun user -> ok user)
  ; get (s "users" / str / s "events" / s "orgs" / str)
    >> pure (fun user org -> ok (user ^ org))
  ; get (s "feeds") |> handle_request (fun _ -> ok "feeds")
  ; get (s "notifications") |> handle_request (fun _ -> ok "notifications")
  ; get (s "repos" / str / str / s "notifications")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; put (s "notifications") |> handle_request (fun _ -> ok "put")
  ; put (s "repos" / str / str / s "notifications")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "notifications" / s "threads" / str) >> pure (fun id -> ok id)
  ; get (s "notifications" / s "threads" / str / s "subscription")
    >> pure (fun id -> ok id)
  ; put (s "notifications" / s "threads" / str / s "subscription")
    >> pure (fun id -> ok id)
  ; delete (s "notifications" / s "threads" / str / s "subscription")
    >> pure (fun id -> ok id)
  ; get (s "repos" / str / str / s "stargazers")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "users" / str / s "starred") >> pure (fun user -> ok user)
  ; get (s "user" / s "starred") |> handle_request (fun _ -> ok "starred")
  ; get (s "user" / s "starred" / str / str)
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; put (s "user" / s "starred" / str / str)
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; delete (s "user" / s "starred" / str / str)
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "subscribers")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "users" / str / s "subscriptions") >> pure (fun user -> ok user)
  ; get (s "user" / s "subscriptions")
    |> handle_request (fun _ -> ok "subscriptions")
  ; get (s "repos" / str / str / s "subscription")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; put (s "repos" / str / str / s "subscription")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; delete (s "repos" / str / str / s "subscription")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "user" / s "subscriptions" / str / str)
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; put (s "user" / s "subscriptions" / str / str)
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; delete (s "user" / s "subscriptions" / str / str)
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "users" / str / s "gists") >> pure (fun user -> ok user)
  ; get (s "gists") |> handle_request (fun _ -> ok "gists")
  ; get (s "gists" / str) >> pure (fun id -> ok id)
  ; post (s "gists") |> handle_request (fun _ -> ok "create")
  ; put (s "gists" / str / s "star") >> pure (fun id -> ok id)
  ; delete (s "gists" / str / s "star") >> pure (fun id -> ok id)
  ; get (s "gists" / str / s "star") >> pure (fun id -> ok id)
  ; post (s "gists" / str / s "forks") >> pure (fun id -> ok id)
  ; delete (s "gists" / str) >> pure (fun id -> ok id)
  ; get (s "repos" / str / str / s "git" / s "blobs" / str)
    >> pure (fun owner repo sha -> ok (owner ^ repo ^ sha))
  ; post (s "repos" / str / str / s "git" / s "blobs")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "git" / s "commits" / str)
    >> pure (fun owner repo sha -> ok (owner ^ repo ^ sha))
  ; post (s "repos" / str / str / s "git" / s "commits")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "git" / s "refs")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; post (s "repos" / str / str / s "git" / s "refs")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "git" / s "tags" / str)
    >> pure (fun owner repo sha -> ok (owner ^ repo ^ sha))
  ; post (s "repos" / str / str / s "git" / s "tags")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "git" / s "trees" / str)
    >> pure (fun owner repo sha -> ok (owner ^ repo ^ sha))
  ; post (s "repos" / str / str / s "git" / s "trees")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "issues") |> handle_request (fun _ -> ok "issues")
  ; get (s "user" / s "issues") |> handle_request (fun _ -> ok "user-issues")
  ; get (s "orgs" / str / s "issues") >> pure (fun org -> ok org)
  ; get (s "repos" / str / str / s "issues")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "issues" / str)
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; post (s "repos" / str / str / s "issues")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "assignees")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "assignees" / str)
    >> pure (fun owner repo assignee -> ok (owner ^ repo ^ assignee))
  ; get (s "repos" / str / str / s "issues" / str / s "comments")
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; post (s "repos" / str / str / s "issues" / str / s "comments")
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "repos" / str / str / s "issues" / str / s "events")
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "repos" / str / str / s "labels")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "labels" / str)
    >> pure (fun owner repo name -> ok (owner ^ repo ^ name))
  ; post (s "repos" / str / str / s "labels")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; delete (s "repos" / str / str / s "labels" / str)
    >> pure (fun owner repo name -> ok (owner ^ repo ^ name))
  ; get (s "repos" / str / str / s "issues" / str / s "labels")
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; post (s "repos" / str / str / s "issues" / str / s "labels")
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; delete (s "repos" / str / str / s "issues" / str / s "labels" / str)
    >> pure (fun owner repo number name -> ok (owner ^ repo ^ number ^ name))
  ; put (s "repos" / str / str / s "issues" / str / s "labels")
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; delete (s "repos" / str / str / s "issues" / str / s "labels")
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "repos" / str / str / s "milestones" / str / s "labels")
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "repos" / str / str / s "milestones")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "milestones" / str)
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; post (s "repos" / str / str / s "milestones")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; delete (s "repos" / str / str / s "milestones" / str)
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "emojis") |> handle_request (fun _ -> ok "emojis")
  ; get (s "gitignore" / s "templates")
    |> handle_request (fun _ -> ok "templates")
  ; get (s "gitignore" / s "templates" / str) >> pure (fun name -> ok name)
  ; post (s "markdown") |> handle_request (fun _ -> ok "markdown")
  ; post (s "markdown" / s "raw") |> handle_request (fun _ -> ok "raw")
  ; get (s "meta") |> handle_request (fun _ -> ok "meta")
  ; get (s "rate_limit") |> handle_request (fun _ -> ok "rate_limit")
  ; get (s "users" / str / s "orgs") >> pure (fun user -> ok user)
  ; get (s "user" / s "orgs") |> handle_request (fun _ -> ok "orgs")
  ; get (s "orgs" / str) >> pure (fun org -> ok org)
  ; get (s "orgs" / str / s "members") >> pure (fun org -> ok org)
  ; get (s "orgs" / str / s "members" / str)
    >> pure (fun org user -> ok (org ^ user))
  ; delete (s "orgs" / str / s "members" / str)
    >> pure (fun org user -> ok (org ^ user))
  ; get (s "orgs" / str / s "public_members") >> pure (fun org -> ok org)
  ; get (s "orgs" / str / s "public_members" / str)
    >> pure (fun org user -> ok (org ^ user))
  ; put (s "orgs" / str / s "public_members" / str)
    >> pure (fun org user -> ok (org ^ user))
  ; delete (s "orgs" / str / s "public_members" / str)
    >> pure (fun org user -> ok (org ^ user))
  ; get (s "orgs" / str / s "teams") >> pure (fun org -> ok org)
  ; get (s "teams" / str) >> pure (fun id -> ok id)
  ; post (s "orgs" / str / s "teams") >> pure (fun org -> ok org)
  ; delete (s "teams" / str) >> pure (fun id -> ok id)
  ; get (s "teams" / str / s "members") >> pure (fun id -> ok id)
  ; get (s "teams" / str / s "members" / str)
    >> pure (fun id user -> ok (id ^ user))
  ; put (s "teams" / str / s "members" / str)
    >> pure (fun id user -> ok (id ^ user))
  ; delete (s "teams" / str / s "members" / str)
    >> pure (fun id user -> ok (id ^ user))
  ; get (s "teams" / str / s "repos") >> pure (fun id -> ok id)
  ; get (s "teams" / str / s "repos" / str / str)
    >> pure (fun id owner repo -> ok (id ^ owner ^ repo))
  ; put (s "teams" / str / s "repos" / str / str)
    >> pure (fun id owner repo -> ok (id ^ owner ^ repo))
  ; delete (s "teams" / str / s "repos" / str / str)
    >> pure (fun id owner repo -> ok (id ^ owner ^ repo))
  ; get (s "user" / s "teams") |> handle_request (fun _ -> ok "teams")
  ; get (s "repos" / str / str / s "pulls")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "pulls" / str)
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; post (s "repos" / str / str / s "pulls")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "pulls" / str / s "commits")
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "repos" / str / str / s "pulls" / str / s "files")
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "repos" / str / str / s "pulls" / str / s "merge")
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; put (s "repos" / str / str / s "pulls" / str / s "merge")
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "repos" / str / str / s "pulls" / str / s "comments")
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; put (s "repos" / str / str / s "pulls" / str / s "comments")
    >> pure (fun owner repo number -> ok (owner ^ repo ^ number))
  ; get (s "user" / s "repos") |> handle_request (fun _ -> ok "repos")
  ; get (s "users" / str / s "repos") >> pure (fun user -> ok user)
  ; get (s "orgs" / str / s "repos") >> pure (fun org -> ok org)
  ; get (s "repositories") |> handle_request (fun _ -> ok "repositories")
  ; post (s "user" / s "repos") |> handle_request (fun _ -> ok "create")
  ; post (s "orgs" / str / s "repos") >> pure (fun org -> ok org)
  ; get (s "repos" / str / str) >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "contributors")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "languages")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "teams")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "tags")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "branches")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "branches" / str)
    >> pure (fun owner repo branch -> ok (owner ^ repo ^ branch))
  ; delete (s "repos" / str / str) >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "collaborators")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "collaborators" / str)
    >> pure (fun owner repo user -> ok (owner ^ repo ^ user))
  ; put (s "repos" / str / str / s "collaborators" / str)
    >> pure (fun owner repo user -> ok (owner ^ repo ^ user))
  ; delete (s "repos" / str / str / s "collaborators" / str)
    >> pure (fun owner repo user -> ok (owner ^ repo ^ user))
  ; get (s "repos" / str / str / s "comments")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "commits" / str / s "comments")
    >> pure (fun owner repo sha -> ok (owner ^ repo ^ sha))
  ; post (s "repos" / str / str / s "commits" / str / s "comments")
    >> pure (fun owner repo sha -> ok (owner ^ repo ^ sha))
  ; get (s "repos" / str / str / s "comments" / str)
    >> pure (fun owner repo id -> ok (owner ^ repo ^ id))
  ; delete (s "repos" / str / str / s "comments" / str)
    >> pure (fun owner repo id -> ok (owner ^ repo ^ id))
  ; get (s "repos" / str / str / s "commits")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "commits" / str)
    >> pure (fun owner repo sha -> ok (owner ^ repo ^ sha))
  ; get (s "repos" / str / str / s "readme")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "keys")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "keys" / str)
    >> pure (fun owner repo id -> ok (owner ^ repo ^ id))
  ; post (s "repos" / str / str / s "keys")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; delete (s "repos" / str / str / s "keys" / str)
    >> pure (fun owner repo id -> ok (owner ^ repo ^ id))
  ; get (s "repos" / str / str / s "downloads")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "downloads" / str)
    >> pure (fun owner repo id -> ok (owner ^ repo ^ id))
  ; delete (s "repos" / str / str / s "downloads" / str)
    >> pure (fun owner repo id -> ok (owner ^ repo ^ id))
  ; get (s "repos" / str / str / s "forks")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; post (s "repos" / str / str / s "forks")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "hooks")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "hooks" / str)
    >> pure (fun owner repo id -> ok (owner ^ repo ^ id))
  ; post (s "repos" / str / str / s "hooks")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; post (s "repos" / str / str / s "hooks" / str / s "tests")
    >> pure (fun owner repo id -> ok (owner ^ repo ^ id))
  ; delete (s "repos" / str / str / s "hooks" / str)
    >> pure (fun owner repo id -> ok (owner ^ repo ^ id))
  ; post (s "repos" / str / str / s "merges")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "releases")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "releases" / str)
    >> pure (fun owner repo id -> ok (owner ^ repo ^ id))
  ; post (s "repos" / str / str / s "releases")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; delete (s "repos" / str / str / s "releases" / str)
    >> pure (fun owner repo id -> ok (owner ^ repo ^ id))
  ; get (s "repos" / str / str / s "releases" / str / s "assets")
    >> pure (fun owner repo id -> ok (owner ^ repo ^ id))
  ; get (s "repos" / str / str / s "stats" / s "contributors")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "stats" / s "commit_activity")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "stats" / s "code_frequency")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "stats" / s "participation")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "stats" / s "punch_card")
    >> pure (fun owner repo -> ok (owner ^ repo))
  ; get (s "repos" / str / str / s "statuses" / str)
    >> pure (fun owner repo ref_ -> ok (owner ^ repo ^ ref_))
  ; post (s "repos" / str / str / s "statuses" / str)
    >> pure (fun owner repo ref_ -> ok (owner ^ repo ^ ref_))
  ; get (s "search" / s "repositories")
    |> handle_request (fun _ -> ok "search-repos")
  ; get (s "search" / s "code") |> handle_request (fun _ -> ok "search-code")
  ; get (s "search" / s "issues")
    |> handle_request (fun _ -> ok "search-issues")
  ; get (s "search" / s "users") |> handle_request (fun _ -> ok "search-users")
  ; get (s "legacy" / s "issues" / s "search" / str / str / str / str)
    >> pure (fun owner repo state keyword ->
      ok (owner ^ repo ^ state ^ keyword))
  ; get (s "legacy" / s "repos" / s "search" / str)
    >> pure (fun keyword -> ok keyword)
  ; get (s "legacy" / s "user" / s "search" / str)
    >> pure (fun keyword -> ok keyword)
  ; get (s "legacy" / s "user" / s "email" / str)
    >> pure (fun email -> ok email)
  ; get (s "users" / str) >> pure (fun user -> ok user)
  ; get (s "user") |> handle_request (fun _ -> ok "user")
  ; get (s "users") |> handle_request (fun _ -> ok "users")
  ; get (s "user" / s "emails") |> handle_request (fun _ -> ok "emails")
  ; post (s "user" / s "emails") |> handle_request (fun _ -> ok "create")
  ; delete (s "user" / s "emails") |> handle_request (fun _ -> ok "delete")
  ; get (s "users" / str / s "followers") >> pure (fun user -> ok user)
  ; get (s "user" / s "followers") |> handle_request (fun _ -> ok "followers")
  ; get (s "users" / str / s "following") >> pure (fun user -> ok user)
  ; get (s "user" / s "following") |> handle_request (fun _ -> ok "following")
  ; get (s "user" / s "following" / str) >> pure (fun user -> ok user)
  ; get (s "users" / str / s "following" / str)
    >> pure (fun user target -> ok (user ^ target))
  ; put (s "user" / s "following" / str) >> pure (fun user -> ok user)
  ; delete (s "user" / s "following" / str) >> pure (fun user -> ok user)
  ; get (s "users" / str / s "keys") >> pure (fun user -> ok user)
  ; get (s "user" / s "keys") |> handle_request (fun _ -> ok "keys")
  ; get (s "user" / s "keys" / str) >> pure (fun id -> ok id)
  ; post (s "user" / s "keys") |> handle_request (fun _ -> ok "create")
  ; delete (s "user" / s "keys" / str) >> pure (fun id -> ok id)
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

  let gplus_router = Router.of_list gplus_api in

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

  let github_router = Router.of_list github_api in

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
