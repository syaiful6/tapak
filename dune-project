(lang dune 3.19)

(name tapak)

(generate_opam_files true)

(cram enable)

(source
 (github syaiful6/tapak))

(authors "Syaiful Bahri <syai.bahri6@gmail.com>")

(maintainers "Syaiful Bahri <syai.bahri6@gmail.com>")

(license MIT)

(documentation https://syaiful6.github.io/tapak/)

(package
 (name sch)
 (synopsis
  "Sch is schema validation, serialization and deserialization OCaml data structures")
 (description
  "Sch is schema validation, serialization and deserialization OCaml data structures\n  ")
 (depends
  (ocaml
   (>= 5.2))
  dune
  (bytesrw
   (>= 0.3.0))
  jsont
  (re
   (>= 1.7.2))
  (uri (>= 2.0.0))
  ipaddr
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
  (dune (>= 3.0.0))
  (angstrom (>= 0.15.0))
  (multipart_form (>= 0.7.0))
  cows
  hmap
  ptime
  ipaddr
  logs
  (re (>= 1.7.2))
  (eio
   (>= 1.3))
  (cohttp-eio
   (>= 6.0.0))
  (bytesrw
   (>= 0.3.0))
  (uri
   (>= 2.0.0))
  (saturn (>= 1.0.0))
  (sch (= :version))
  magic-mime
  mirage-crypto
  (mirage-crypto-rng (>= 1.2.0))
  (reason
   (and
    (>= 3.12)
    :with-dev-setup))
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat
   (and
    (= 0.28.1)
    :with-dev-setup))
  (alcotest :with-test)
  (benchmark :with-test)
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
  (dune (>= 3.0.0))
  bytesrw
  conf-zlib
  conf-zstd
  dune-configurator
  (tapak
   (= :version))
  (alcotest :with-test)
  (odoc :with-doc))
 (tags
  ("web" "compression" "brotli" "zstd" "gzip" "deflate")))
