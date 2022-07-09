#lang info
(define collection "module-reader-lang")
(define deps '("base"
               "parser-tools-lib"
               "functional-lib"
               "megaparsack-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/module-reader-lang.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(dannypsnl))
(define license '(Apache-2.0 OR MIT))
