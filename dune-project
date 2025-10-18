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
  hmap
  logs
  (eio
   (>= 1.3))
  (piaf
   (>= 0.2.0))
  (uri
   (>= 4.4.0))
  mirage-crypto
  mirage-crypto-rng
  mirage-crypto-rng-unix
  (alcotest :with-test)
  (odoc :with-doc))
 (tags
  ("web" "framework" "ocaml" "eio" "http" "modular")))
