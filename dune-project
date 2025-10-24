(lang dune 3.19)

(name tapak)

(generate_opam_files true)

(source
 (github syaiful6/tapak))

(authors "Syaiful Bahri <syai.bahri6@gmail.com>")

(maintainers "Syaiful Bahri <syai.bahri6@gmail.com>")

(license MIT)

(documentation https://syaiful6.github.io/tapak/)

(package
 (name simdutf)
 (synopsis "OCaml bindings for simdutf")
 (description
  "simdutf is a high-performance library for validating and transcoding UTF-8, UTF-16, and UTF-32 text. This package provides OCaml bindings to the simdutf library, allowing OCaml developers to leverage its capabilities for efficient text processing.")
 (depends
  (ocaml
   (>= 5.2.0))
  (ctypes
   (>= 0.14.0))
  dune
  dune-configurator
  (alcotest :with-test)
  (odoc :with-doc)))

(package
 (name tapak)
 (synopsis "Composable framework for building web applications in OCaml")
 (description
  "Tapak is a modular and extensible framework for building web applications in OCaml. It provides a set of tools and abstractions to help developers create scalable and maintainable web applications.")
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  angstrom
  hmap
  ipaddr
  logs
  (eio
   (>= 1.3))
  (piaf
   (>= 0.2.0))
  (uri
   (>= 4.4.0))
  yojson
  mirage-crypto
  mirage-crypto-rng
  (alcotest :with-test)
  (odoc :with-doc))
 (tags
  ("web" "framework" "ocaml" "eio" "http" "modular")))

(package
 (name tapak-compressions)
 (synopsis "Compression support for Tapak web framework")
 (description
  "Provides compression and decompression support for the Tapak web framework. Includes implementations for gzip, deflate, brotli, and zstd compression algorithms.")
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  dune-configurator
  (piaf
   (>= 0.2.0))
  (camlzip
   (>= 1.11))
  conf-brotli
  conf-zstd
  (alcotest :with-test)
  (odoc :with-doc))
 (tags
  ("web" "compression" "brotli" "zstd" "gzip" "deflate")))

(package
 (name tapak-ppx)
 (synopsis "PPX extension for type-safe route generation in Tapak")
 (description
  "tapak-ppx is a PPX rewriter that generates type-safe route definitions from annotated handler functions. It eliminates boilerplate when defining routes in the Tapak web framework.")
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  (ppxlib
   (>= 0.32.0))
  angstrom
  (odoc :with-doc))
 (tags
  ("ppx" "web" "framework" "tapak" "routes" "type-safe")))
