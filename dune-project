(lang dune 2.7)
(name BitarrayExt)

(generate_opam_files true)

(source (github adaminsky/Bitarray))
(license MIT)
(authors "Adam Stein")
(maintainers "adaminsky@gmail.com")

(package
    (name BitarrayExt)
    (synopsis "Bitarray types for Ocaml")
    (depends
        (alcotest :with-test)
        (ocaml
            (>= 4.02.3))
        base
        dune))
