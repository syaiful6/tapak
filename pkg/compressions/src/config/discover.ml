module C = Configurator.V1

let () =
  C.main ~name:"tapak-compressions" (fun c ->
    let default_cflags = [] in
    let default_libs = [ "-lbrotlidec"; "-lbrotlienc"; "-lzstd" ] in

    let cflags, libs =
      match C.Pkg_config.get c with
      | None -> default_cflags, default_libs
      | Some pc ->
        (* Try to get flags from pkg-config *)
        let brotli_cflags =
          match C.Pkg_config.query pc ~package:"libbrotlidec" with
          | None -> []
          | Some conf -> conf.cflags
        in
        let brotlidec_libs =
          match C.Pkg_config.query pc ~package:"libbrotlidec" with
          | None -> [ "-lbrotlidec" ]
          | Some conf -> conf.libs
        in
        let brotlienc_libs =
          match C.Pkg_config.query pc ~package:"libbrotlienc" with
          | None -> [ "-lbrotlienc" ]
          | Some conf -> conf.libs
        in
        let brotli_libs = brotlidec_libs @ brotlienc_libs in
        let zstd_cflags =
          match C.Pkg_config.query pc ~package:"libzstd" with
          | None -> []
          | Some conf -> conf.cflags
        in
        let zstd_libs =
          match C.Pkg_config.query pc ~package:"libzstd" with
          | None -> [ "-lzstd" ]
          | Some conf -> conf.libs
        in
        brotli_cflags @ zstd_cflags, brotli_libs @ zstd_libs
    in

    C.Flags.write_sexp "c_flags.sexp" cflags;
    C.Flags.write_sexp "c_library_flags.sexp" libs)
