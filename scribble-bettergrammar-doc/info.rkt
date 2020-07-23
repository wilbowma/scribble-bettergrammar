#lang info
(define build-deps
  '("scribble-bettergrammar-lib"
    "base"
    "scribble-lib"
    "racket-doc"
    "scribble-doc"
    ("sexp-diff" #:version "0.2")))
(define pkg-desc "Documentation for scribble-bettergrammar")
(define pkg-authors '(wilbowma))
(define version "1.4")
(define deps '("base"))
