#lang info
(define collection "tree")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/tree.scrbl" ())))
(define pkg-desc "My tree implementation")
(define version "0.0")
(define pkg-authors '(hin))
(define license '(Apache-2.0 OR MIT))
