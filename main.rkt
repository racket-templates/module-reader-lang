#lang racket/base
(provide (rename-out [lang-module-begin #%module-begin])
         #%top-interaction
         #%app
         #%datum)
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
        content
        ...
        (void))]))

;; splices content of any block as its own top-level group:
(define-syntax (#%top-interaction stx)
  (syntax-parse stx
    #:datum-literals (group block)
    [(form-id . (top form ... (group (block inner-form ...)) . content))
     #'(form-id . (top form ... inner-form ... . content))]
    [(_ . (top . content))
     #'(lang-top . content)]))
