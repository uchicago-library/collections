(lang dune 3.8)
(generate_opam_files true)
(package
 (name collections)
 (description "UChicago DLDC digital collections Rest API")
 (synopsis "Rest API for communicating with digital collections database")
 (maintainers "UChicago Developer <some-developer@uchicago.edu>")
 (authors "UChicago Developer <some-developer@uchicago.edu>")
 (homepage "https://github.com/uchicago-library/collections")
 (bug_reports "https://github.com/uchicago-library/collections")
 (depends
  (ocaml (>= 4.13.0))
   dune
   camlp-streams
   prelude
   mattlude
   utop
   ocp-index
   restful
   netclient
   threads
   nettls-gnutls
   data-encoding
   uri
   merlin))
