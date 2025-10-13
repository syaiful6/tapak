type os =
  | Linux
  | Mac
  | Windows

module Configurator = Configurator.V1

let find_xcode_sysroot sdk =
  let ic =
    Unix.open_process_in (Format.sprintf "xcrun --sdk %s --show-sdk-path" sdk)
  in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () -> input_line ic

let detect_system_header =
  {|
  #if __APPLE__
    #include <TargetConditionals.h>
    #define PLATFORM_NAME "mac"
  #elif __linux__
    #define PLATFORM_NAME "linux"
  #elif _WIN32
    #define PLATFORM_NAME "windows"
  #endif
  |}

let get_os t =
  let header =
    let file = Filename.temp_file "discover" ".os.h" in
    let fd = open_out file in
    output_string fd detect_system_header;
    close_out fd;
    file
  in
  let platform =
    Configurator.C_define.import
      t
      ~includes:[ header ]
      [ "PLATFORM_NAME", String ]
  in
  match platform with
  | [ (_, String "mac") ] -> Mac
  | [ (_, String "linux") ] -> Linux
  | [ (_, String "windows") ] -> Windows
  | _ -> failwith "Could not detect OS"

let ccopt s = [ "-ccopt"; s ]
let cclib s = [ "-cclib"; s ]

let cflags = function
  | Mac ->
    let sdk = find_xcode_sysroot "macosx" in
    [ "-isysroot"; sdk ]
  | Linux -> [ "fPIC" ]
  | Windows -> []

let cxxflags = function
  | Mac ->
    let sdk = find_xcode_sysroot "macosx" in
    [ "-isysroot"; sdk; "-std=c++17" ]
  | Linux -> [ "-std=c++17"; "-fPIC" ]
  | Windows -> [ "-std=c++17" ]

let c_library_flags = function
  | Mac -> [ "-lc++"; "-lc++abi" ]
  | Linux -> [ "-lstdc++" ]
  | Windows -> []

let flags os =
  (cflags os |> List.map ccopt |> List.flatten)
  @ (c_library_flags os |> List.map cclib |> List.flatten)

let () =
  Configurator.main ~name:"simdutf" @@ fun t ->
  let os = get_os t in
  let flags = flags os in

  Configurator.Flags.write_sexp "flags.sexp" flags;
  Configurator.Flags.write_sexp "c_flags.sexp" (cflags os);
  Configurator.Flags.write_lines "c_flags.txt" (cflags os);
  Configurator.Flags.write_sexp "c_library_flags.sexp" (c_library_flags os);
  Configurator.Flags.write_sexp "cxx_flags.sexp" (cxxflags os);
  Configurator.Flags.write_lines "cxx_flags.txt" (cxxflags os)
