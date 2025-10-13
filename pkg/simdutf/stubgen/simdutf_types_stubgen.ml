let prologue = {|
#include "simdutf_capi.h"
|}

let () =
  print_endline prologue;
  Cstubs.Types.write_c Format.std_formatter (module Simdutf_bindings_types.M)
