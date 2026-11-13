(* Compare the GADT Sch_free with the CPS implementation it replaced. The
   workload matches the structures produced by Sch and the router: an N-field,
   left-nested record built with [let+] and [and+], then interpreted with [run].
   Each field adds one [map] and one [apply], followed by a final [map].

   The interpreters model their production counterparts: - decode uses
   [Validation] and creates a natural transformation for each input object, as
   [Sch.Json.decode] does for [Object]; - encode uses a unit applicative and a
   side-effecting JSON-writer-style natural transformation; - reader compiles
   once to [env -> a], then runs once per request, like
   [Router.compile_route]. *)

module Validation = Sch.Validation

(* Effect: one record field, decoded from an int-valued object. *)
type field = { name : string } [@@boxed]
type fieldk

external inj : field -> ('a, fieldk) Sch_sig.app = "%identity"
external prj : ('a, fieldk) Sch_sig.app -> field = "%identity"

module V = Sch_sig.Make (struct
    type 'a t = 'a Validation.t
  end)

let validation : V.t Sch_sig.applicative =
  { pure = (fun a -> V.inj (Validation.pure a))
  ; map = (fun f a -> V.inj (Validation.map f (V.prj a)))
  ; apply = (fun f a -> V.inj (Validation.apply (V.prj f) (V.prj a)))
  }

module U = Sch_sig.Make (struct
    type 'a t = unit
  end)

let unit_applicative : U.t Sch_sig.applicative =
  { pure = (fun _ -> U.inj ())
  ; map = (fun _ _ -> U.inj ())
  ; apply = (fun _ _ -> U.inj ())
  }

module R = Sch_sig.Make (struct
    type 'a t = (string * int) list -> 'a
  end)

(* Evaluate both sides. Their order does not affect the sum. *)
let reader : R.t Sch_sig.applicative =
  { pure = (fun a -> R.inj (fun _ -> a))
  ; map = (fun f a -> R.inj (fun env -> f (R.prj a env)))
  ; apply =
      (fun f a ->
        R.inj (fun env ->
          let x = R.prj a env in
          (R.prj f env) x))
  }

(* These natural transformations coerce [int] to ['a]. This is safe here because
   the benchmark only constructs [int] fields. *)
let decode_nat (obj : (string * int) list) : (fieldk, V.t) Sch_sig.nat =
  { run =
      (fun fa ->
        let { name } = prj fa in
        match List.assoc_opt name obj with
        | Some v -> V.inj (Validation.Success (Obj.magic v))
        | None -> V.inj (Validation.Error []))
  }

let encode_nat (count : int ref) : (fieldk, U.t) Sch_sig.nat =
  { run =
      (fun fa ->
        let { name } = prj fa in
        count := !count + String.length name;
        U.inj ())
  }

let reader_nat : (fieldk, R.t) Sch_sig.nat =
  { run =
      (fun fa ->
        let { name } = prj fa in
        R.inj (fun env -> Obj.magic (List.assoc name env)))
  }

module type FREE = sig
  type ('f, 'a) t

  val pure : 'a -> ('f, 'a) t [@@warning "-32"]
  val map : ('a -> 'b) -> ('f, 'a) t -> ('f, 'b) t
  val apply : ('f, 'a -> 'b) t -> ('f, 'a) t -> ('f, 'b) t
  val lift : ('a, 'f) Sch_sig.app -> ('f, 'a) t

  val run :
     'g Sch_sig.applicative
    -> ('f, 'g) Sch_sig.nat
    -> ('f, 'a) t
    -> ('a, 'g) Sch_sig.app
end

module Workload (F : FREE) = struct
  let pair a b = a, b

  let field i : (fieldk, int) F.t =
    F.lift (inj { name = "f" ^ string_of_int i })

  (* Model a left-nested [let+ ... and+ ...] expression. Each [and+] expands to
     [apply (map pair a) b]. Fold each pair into a sum to keep the accumulator
     type at [int], then add the final [map]. *)
  let record n =
    let acc = ref (field 1) in
    for i = 2 to n do
      acc := F.map (fun (a, b) -> a + b) (F.apply (F.map pair !acc) (field i))
    done;
    F.map Fun.id !acc

  let decode schema obj = V.prj (F.run validation (decode_nat obj) schema)

  let encode schema =
    let count = ref 0 in
    ignore (U.prj (F.run unit_applicative (encode_nat count) schema));
    !count

  let compile schema = R.prj (F.run reader reader_nat schema)
end

type sample =
  { ns : float
  ; bytes : float
  }

let iterations = ref 100_000
let samples = ref 7
let sizes = ref [ 1; 5; 20; 100 ]
let only = ref None

let parse_args () =
  let usage =
    "sch-free-bench [--iterations N] [--samples K] [--sizes 1,5,20] [--only \
     SUBSTRING]"
  in
  Arg.parse
    [ "--iterations", Arg.Set_int iterations, "N operations per sample"
    ; "--samples", Arg.Set_int samples, "K samples; median is reported"
    ; ( "--sizes"
      , Arg.String
          (fun s ->
            sizes := String.split_on_char ',' s |> List.map int_of_string)
      , "Comma-separated record sizes (number of fields)" )
    ; ( "--only"
      , Arg.String (fun name -> only := Some name)
      , "Run benchmarks whose name contains SUBSTRING" )
    ]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg)))
    usage;
  if !iterations <= 0 then raise (Arg.Bad "--iterations must be positive");
  if !samples <= 0 then raise (Arg.Bad "--samples must be positive");
  if List.exists (fun n -> n <= 0) !sizes
  then raise (Arg.Bad "--sizes must be positive")

let run_n n f =
  for _ = 1 to n do
    Sys.opaque_identity (f ()) |> ignore
  done

let bytes_per_word = Float.of_int (Sys.word_size / 8)

let measure f =
  Gc.full_major ();
  let before = Benchmark.make 0L in
  run_n !iterations f;
  let result =
    Benchmark.sub (Benchmark.make (Int64.of_int !iterations)) before
  in
  let operations = Float.of_int !iterations in
  let words =
    result.minor_words +. result.major_words -. result.promoted_words
  in
  { ns = result.wall *. 1e9 /. operations
  ; bytes = words *. bytes_per_word /. operations
  }

let median samples =
  let samples = List.sort (fun a b -> Float.compare a.ns b.ns) samples in
  List.nth samples (List.length samples / 2)

let contains ~sub s =
  let n = String.length sub
  and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  go 0

let selected name =
  Option.fold ~none:true ~some:(fun sub -> contains ~sub name) !only

let bench _name f =
  run_n (max 100 (!iterations / 10)) f;
  median (List.init !samples (fun _ -> measure f))

let check what expected actual =
  if actual <> expected
  then failwith (Printf.sprintf "%s: expected %d, got %d" what expected actual)

module type CASES = sig
  val label : string

  (* Field names observed by the natural transformation during [run]. *)
  val nat_order : int -> string list
  val cases : int -> (string * (unit -> Obj.t)) list
end

module Cases
    (L : sig
       val label : string
     end)
    (F : FREE) : CASES = struct
  module W = Workload (F)

  let label = L.label

  let nat_order n =
    let seen = ref [] in
    let nat : (fieldk, U.t) Sch_sig.nat =
      { run =
          (fun fa ->
            seen := (prj fa).name :: !seen;
            U.inj ())
      }
    in
    ignore (F.run unit_applicative nat (W.record n));
    List.rev !seen

  let cases n =
    let obj = List.init n (fun i -> "f" ^ string_of_int (i + 1), i + 1) in
    let expected = n * (n + 1) / 2 in
    let schema = W.record n in
    let value = function
      | Validation.Success v -> v
      | Validation.Error _ -> -1
    in
    check (label ^ " decode") expected (value (W.decode schema obj));
    let reader = W.compile schema in
    check (label ^ " reader") expected (reader obj);
    let any f () = Obj.repr (f ()) in
    [ "build", any (fun () -> W.record n)
    ; "decode", any (fun () -> W.decode schema obj)
    ; "encode", any (fun () -> W.encode schema)
    ; "reader/compile", any (fun () -> W.compile schema)
    ; "reader/call", any (fun () -> reader obj)
    ]
end

let implementations : (module CASES) list =
  [ (module Cases
              (struct
                let label = "Sch_free (GADT)"
              end)
              (Sch_free))
  ; (module Cases
              (struct
                let label = "Cps_free (old)"
              end)
              (Cps_free))
  ]

let () =
  parse_args ();
  Printf.printf
    "Free applicative benchmark (%d samples x %d iterations, median)\n\n"
    !samples
    !iterations;
  let declared = List.init 4 (fun i -> "f" ^ string_of_int (i + 1)) in
  List.iter
    (fun (module I : CASES) ->
       let order = I.nat_order 4 in
       Printf.printf
         "%-16s nat order: %s (%s)\n"
         I.label
         (String.concat "," order)
         (if order = declared then "declared" else "NOT declared"))
    implementations;
  Printf.printf "\n%-20s" "benchmark";
  List.iter
    (fun (module I : CASES) -> Printf.printf " %24s" I.label)
    implementations;
  print_newline ();
  List.iter
    (fun n ->
       let all =
         List.map (fun (module I : CASES) -> I.cases n) implementations
       in
       List.iter
         (fun (kind, _) ->
            let name = Printf.sprintf "%s/%d" kind n in
            if selected name
            then begin
              Printf.printf "%-20s" name;
              let base = ref None in
              List.iter
                (fun cases ->
                   let r = bench name (List.assoc kind cases) in
                   let ratio =
                     match !base with
                     | None ->
                       base := Some r.ns;
                       1.
                     | Some b -> r.ns /. b
                   in
                   Printf.printf " %8.0fns %6.0fB %5.2fx" r.ns r.bytes ratio)
                all;
              print_newline ()
            end)
         (List.hd all))
    !sizes
