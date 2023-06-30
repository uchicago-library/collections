(lang dune 3.0)
(generate_opam_files true)
(package
 (name collections)
 (description "Insert project description here.")
 (synopsis "Insert project synopsis, which is supposedly different, here")
 (maintainers "Your Name <youremail@gmail.com>")
 (authors "Your Name <youremail@gmail.com>")
 (homepage "https://your.website.here")
 (bug_reports "https://your.website.here")
 (depends
  (ocaml (>= 4.13.0))
   dune
   camlp-streams
   prelude
   mattlude
   utop
   ocp-index
   merlin))
