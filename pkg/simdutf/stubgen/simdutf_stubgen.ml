let prefix = "simdutf_stubgen"

let prolugue = {|
#include "simdutf_capi.h"
|}

let () =
  let generate_ml, generate_c = ref false, ref false in
  let () =
    Arg.(
      parse
        [ "-ml", Set generate_ml, " Generate ML"
        ; "-c", Set generate_c, " Generate C"
        ])
      (fun _ -> failwith "Unexpected anonymous argument")
      "stubgen [-ml] [-c]"
  in
  match !generate_ml, !generate_c with
  | false, false | true, true ->
    failwith "Exactly one of -ml or -c must be specified"
  | true, false ->
    Cstubs.write_ml Format.std_formatter ~prefix (module Simdutf_bindings.M)
  | false, true ->
    print_endline prolugue;
    Cstubs.write_c Format.std_formatter ~prefix (module Simdutf_bindings.M)
