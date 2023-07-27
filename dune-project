(lang dune 3.7)
(name solver-service)

(formatting (enabled_for dune))
(generate_opam_files true)
(source (github ocurrent/solver-service))
(authors "Thomas Leonard <talex5@gmail.com>" "Patrick Ferris <patrick@sirref.org>")
(maintainers "alpha@tarides.com" "Tim McGilchrist <timmcgil@gmail.com>")

(package
 (name solver-service)
 (synopsis "Choose package versions to test")
 (depends
  (ocaml (>= 5.0.0))
  (logs (>= 0.7.0))
  (fmt (>= 0.9.0))
  (dune-build-info (>= 3.8.0))
  (opam-0install (>= 0.4.3))))
