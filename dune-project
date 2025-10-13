(lang dune 3.19)

(name tapak)
(generate_opam_files true)
(source (github syaiful6/tapak))

(authors "Syaiful Bahri <syai.bahri6@gmail.com>")
(maintainers "Syaiful Bahri <syai.bahri6@gmail.com>")

(license MIT)
; TODO: replace with your project documentation URL
(documentation https://url/to/your/documentation)

(package
  (name tapak)
  (synopsis "Composable framework for building web applications in OCaml")
  (description "Tapak is a modular and extensible framework for building web applications in OCaml. It provides a set of tools and abstractions to help developers create scalable and maintainable web applications.")
  (depends
    (ocaml (>= 5.2.0))
    dune
    hmap
    logs
    (eio (>= 1.3))
    (piaf (>= 0.2.0)))
  (tags ("web" "framework" "ocaml" "eio" "http" "modular")))
