open Tapak
open Bench_routes

let create_mock_request ?(headers = Headers.empty) ~meth ~path () =
  Request.make
    ~version:`HTTP_1_1
    ~meth
    ~headers
    ~body:(Bytesrw.Bytes.Reader.empty ())
    path

let iterations = ref 1_000_000
let samples = ref 5
let metrics_only = ref false
let only = ref None

let parse_args () =
  let usage =
    "tapak-route-bench [--iterations N] [--samples K] [--only NAME] \
     [--metrics-only]"
  in
  Arg.parse
    [ ( "--iterations"
      , Arg.Set_int iterations
      , "N Requests per sample (default: 1000000)" )
    ; ( "--samples"
      , Arg.Set_int samples
      , "K Timed samples per benchmark; the median is reported (default: 5)" )
    ; ( "--only"
      , Arg.String (fun name -> only := Some name)
      , "NAME Run a single metrics benchmark (implies --metrics-only); use \
         with perf stat to get cycles/request" )
    ; ( "--metrics-only"
      , Arg.Set metrics_only
      , " Skip the legacy multi-second route benchmark suite" )
    ]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg)))
    usage;
  if !iterations <= 0 then raise (Arg.Bad "--iterations must be positive");
  if !samples <= 0 then raise (Arg.Bad "--samples must be positive");
  if Option.is_some !only then metrics_only := true

let run_n n f x =
  for _ = 1 to n do
    f x
  done

type sample =
  { ns : float (* nanoseconds per request *)
  ; bytes : float (* bytes allocated per request *)
  }

let measure f x =
  Gc.full_major ();
  let before = Benchmark.make 0L in
  run_n !iterations f x;
  let r = Benchmark.sub (Benchmark.make (Int64.of_int !iterations)) before in
  let requests = float !iterations in
  (* [major_words] already includes [promoted_words]; subtract them so words
     that survive a minor GC are not counted twice. [Benchmark] reports these
     fields in bytes. *)
  let allocated = r.minor_words +. r.major_words -. r.promoted_words in
  { ns = r.wall *. 1e9 /. requests; bytes = allocated /. requests }

let median_sample samples =
  let sorted = List.sort (fun a b -> Float.compare a.ns b.ns) samples in
  List.nth sorted (List.length sorted / 2)

(* Benchmarks that ran in this process, for computing deltas. *)
let results : (string, sample) Hashtbl.t = Hashtbl.create 16

let benchmark_request ?baseline ~name f x =
  if match !only with None -> true | Some n -> String.equal n name
  then begin
    (* Warm up caches and branch predictors before timing. *)
    run_n (max 10_000 (!iterations / 10)) f x;
    let all = List.init !samples (fun _ -> measure f x) in
    let m = median_sample all in
    let lo = List.fold_left (fun acc s -> Float.min acc s.ns) infinity all in
    let hi = List.fold_left (fun acc s -> Float.max acc s.ns) 0. all in
    Hashtbl.replace results name m;
    let delta =
      match Option.bind baseline (Hashtbl.find_opt results) with
      | Some b ->
        Printf.sprintf
          "  %+9.1f ns  %+9.1f B"
          (m.ns -. b.ns)
          (m.bytes -. b.bytes)
      | None -> ""
    in
    Printf.printf
      "%-24s %10.1f ns  [%8.1f..%8.1f]  %10.1f B  %12.0f req/s%s\n%!"
      name
      m.ns
      lo
      hi
      m.bytes
      (1e9 /. m.ns)
      delta
  end

let print_metrics_header () =
  Printf.printf
    "%-24s %13s  %20s  %12s  %18s  %s\n"
    "benchmark"
    "median"
    "min..max (ns)"
    "alloc"
    "throughput"
    "vs input/baseline";
  Printf.printf
    "(in-process router dispatch; req/s = 1e9 / median ns; %d samples x %d \
     requests)\n"
    !samples
    !iterations

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
       let req = create_mock_request ~meth ~path () in
       let _resp = router req in
       ())
    requests

let () =
  parse_args ();
  print_metrics_header ();
  let gplus_router = Router.of_list gplus_api in
  let github_router = Router.of_list github_api in
  let input_router = Router.of_list input_api in
  (* A realistic header count, so header lookup is not trivially cheap. *)
  let input_headers =
    Headers.of_list
      [ "host", "api.example.com"
      ; "user-agent", "Mozilla/5.0 (X11; Linux x86_64) Gecko/20100101"
      ; "accept", "application/json"
      ; "accept-encoding", "gzip, deflate, br"
      ; "accept-language", "en-US,en;q=0.9"
      ; "connection", "keep-alive"
      ; "x-forwarded-for", "203.0.113.7"
      ; "x-api-key", "secret-key"
      ; "x-request-id", "request-123"
      ; "cookie", "session=abcdef0123456789; theme=dark"
      ]
  in
  let input_request prefix =
    create_mock_request
      ~headers:input_headers
      ~meth:`GET
      ~path:("/" ^ prefix ^ "/users?q=alice&limit=25&offset=10")
      ()
  in
  let check_ok name (router, req) =
    match Response.status (router req) with
    | `OK -> ()
    | status ->
      Printf.eprintf
        "%s: expected 200 OK, got %s\n"
        name
        (Http.Status.to_string status);
      exit 1
  in
  benchmark_request
    ~name:"path/static"
    bench_single_route
    (gplus_router, create_mock_request ~meth:`GET ~path:"/people" ());
  benchmark_request
    ~name:"path/2-params"
    bench_single_route
    ( gplus_router
    , create_mock_request ~meth:`GET ~path:"/people/me/activities/public" () );
  benchmark_request
    ~name:"path/203-route-trie"
    bench_single_route
    ( github_router
    , create_mock_request
        ~meth:`GET
        ~path:"/repos/julienschmidt/httprouter/pulls/1/files"
        () );
  (* The delta column needs [input/baseline] to have run in this process, so it
     is empty under --only; --only runs exactly one benchmark so that perf
     counts nothing else. *)
  let baseline = "input/baseline" in
  List.iter
    (fun prefix ->
       let name = "input/" ^ prefix in
       let x = input_router, input_request prefix in
       check_ok name x;
       benchmark_request ~baseline ~name bench_single_route x)
    [ "baseline"; "query"; "headers"; "cookies"; "all" ];
  Printf.printf
    "\n\
     CPU cycles/request: run one benchmark under perf and divide by the \
     request count\n\
     (samples x iterations, plus warm-up of max(10000, iterations/10)):\n\
    \  perf stat -e cycles:u _build/default/bench/route_bench.exe --only \
     input/query --iterations %d --samples 1\n\n"
    !iterations;

  if !metrics_only then exit 0;

  (* Keep the original benchmark suite below for detailed route comparisons. *)
  let open Benchmark in
  Printf.printf " Google+ API (13 routes)\n";

  Printf.printf "--- GPlus Static (no params): GET /people ---\n";
  let req = create_mock_request ~meth:`GET ~path:"/people" () in
  let results =
    throughputN
      ~repeat:3
      2
      [ "GPlus Static", bench_single_route, (gplus_router, req) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf "--- GPlus Param: GET /people/:userId ---\n";
  let req = create_mock_request ~meth:`GET ~path:"/people/me" () in
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
    create_mock_request ~meth:`GET ~path:"/people/me/activities/public" ()
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

  Printf.printf "--- GitHub Static (no params): GET /user ---\n";
  let req = create_mock_request ~meth:`GET ~path:"/user" () in
  let results =
    throughputN
      ~repeat:3
      2
      [ "GitHub Static", bench_single_route, (github_router, req) ]
  in
  tabulate results;
  Printf.printf "\n";

  Printf.printf "--- GitHub Param: GET /users/:user ---\n";
  let req = create_mock_request ~meth:`GET ~path:"/users/julienschmidt" () in
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
    create_mock_request ~meth:`GET ~path:"/repos/julienschmidt/httprouter" ()
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
      ()
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
