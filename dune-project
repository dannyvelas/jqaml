(lang dune 3.15)

(name jqaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name jqaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

(using menhir 2.0)
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
