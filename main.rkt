#lang racket/base
(provide (rename-out [lang-module-begin #%module-begin])
         #%datum
         #%top
         println)
(require (for-syntax syntax/parse
                     racket/base))

(module reader syntax/module-reader
  #:language 'module-reader-lang
  #:read (lambda (in) (list (syntax->datum (parse-all in))))
  #:read-syntax (lambda (src in) (list (parse-all in #:source src)))
  #:whole-body-readers? #t
  (require "parser.rkt"))

(define-syntax (lang-module-begin stx)
  (syntax-parse stx
    [(_ (content ...))
     #`(#%module-begin
        content ...)]))
