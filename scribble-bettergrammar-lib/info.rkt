#lang info
(define collection 'multi)
(define deps
  '("base"
    "scribble-lib"
    ("sexp-diff" #:version "0.2")))
(define pkg-desc "A Scribble library for typesetting diffs between `racketgrammar` (implementation only, no docs or tests).")
(define pkg-authors '(wilbowma))
(define version "1.4.1")
