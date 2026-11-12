(* HTTP server for whole-stack benchmarks, driven by bench/http_bench.sh.

   It listens on three ports: - [port] every benchmark route behind
   [Tapak.Router]; - [port + 1] a control handler that skips the router
   entirely, giving the cost floor of the socket + cohttp-eio stack; - [port +
   2] stats, outside the measured ports: [GET /gc] reports allocation counters
   so the script can compute bytes/request.

   Nothing is logged and errors are ignored, so the hot path does only the work
   being measured. *)

open Tapak

let port = ref 8080
let domains = ref 1

let parse_args () =
  Arg.parse
    [ "--port", Arg.Set_int port, "PORT Router port (default 8080)"
    ; ( "--domains"
      , Arg.Set_int domains
      , "N Total domains serving requests (default 1)" )
    ]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg)))
    "tapak-server-bench [--port PORT] [--domains N]";
  if !domains <= 0 then raise (Arg.Bad "--domains must be positive")

(* [Gc.quick_stat] sums the allocation counters of every domain, which is what
   we need with --domains > 1. Counters are synchronised at minor collections,
   so they lag by at most one minor heap per domain; over a run of millions of
   requests that error is negligible. *)
let gc_json () =
  let s = Gc.quick_stat () in
  let bytes w = w *. float (Sys.word_size / 8) in
  Printf.sprintf
    {|{"allocated_bytes":%.0f,"minor_collections":%d,"major_collections":%d}|}
    (bytes (s.minor_words +. s.major_words -. s.promoted_words))
    s.minor_collections
    s.major_collections

let stats_handler request =
  match Request.target request with
  | "/gc" ->
    Response.of_string
      ~headers:(Headers.of_list [ "content-type", "application/json" ])
      ~status:`OK
      (gc_json ())
  | _ -> Response.of_string ~status:`Not_found ""

let control_handler _request = Response.of_string ~status:`OK ""

let () =
  parse_args ();
  let router =
    Router.of_list
      (Bench_routes.gplus_api @ Bench_routes.github_api @ Bench_routes.input_api)
  in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let listen port =
    Eio.Net.listen
      ~reuse_addr:true
      ~backlog:4096
      ~sw
      env#net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let router_socket = listen !port in
  let control_socket = listen (!port + 1) in
  let stats_socket = listen (!port + 2) in
  let on_error _ = () in
  let additional_domains =
    if !domains > 1
    then Some (Eio.Stdenv.domain_mgr env, !domains - 1)
    else None
  in
  Printf.printf
    "server_bench pid=%d router=%d control=%d stats=%d domains=%d\n%!"
    (Unix.getpid ())
    !port
    (!port + 1)
    (!port + 2)
    !domains;
  Eio.Fiber.all
    [ (fun () -> run ?additional_domains ~on_error router_socket router)
    ; (fun () ->
        run ?additional_domains ~on_error control_socket control_handler)
    ; (fun () -> run ~on_error stats_socket stats_handler)
    ]
