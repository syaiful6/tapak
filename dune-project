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
 (name tapak)
 (synopsis "Composable framework for building web applications in OCaml")
 (description
  "Tapak is a modular and extensible framework for building web applications in OCaml. It provides a set of tools and abstractions to help developers create scalable and maintainable web applications.")
 (depends
  (ocaml
   (>= 5.2.0))
  (dune (>= 3.0.0))
  (angstrom (>= 0.15.0))
  hmap
  ptime
  ipaddr
  logs
  (re (>= 1.7.2))
  (eio
   (>= 1.3))
  (piaf
   (>= 0.2.0))
  (uri
   (>= 4.4.0))
  yojson
  (saturn (>= 1.0.0))
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
  dune-configurator
  (tapak
   (= :version))
  (piaf
   (>= 0.2.0))
  (camlzip
   (>= 1.11))
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
  (dune (>= 3.0.0))
  (tapak
   (= :version))
  (ppxlib
   (>= 0.36.0))
  (ocamlformat
   (and
    (= 0.28.1)
    :with-test))
  angstrom
  (odoc :with-doc))
 (tags
  ("ppx" "web" "framework" "tapak" "routes" "type-safe")))
