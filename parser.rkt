#lang racket
(provide parse-all)
(require megaparsack/parser-tools/lex
         data/monad
         data/applicative
         megaparsack
         "lexer.rkt")

(define (parse-all in
                   #:source [src ""])
  (parse-result!
   (parse-tokens (many/p statement/p) (lex-simple in)
                 src)))

(define number/p (syntax/p (token/p 'NUMBER)))
(define identifier/p (syntax/p (token/p 'IDENTIFIER)))
; a simple function invokation
(define funcall/p
  (syntax/p
   (do [func <- identifier/p]
     (token/p 'OPEN-PAREN)
     [args <- (many/p expression/p #:sep (token/p 'COMMA))]
     (token/p 'CLOSE-PAREN)
     (pure (list* func args)))))
; an expression can be a number or a function invokation
(define expression/p
  (or/p number/p
        identifier/p
        funcall/p))

(define let/p
  (do (token/p 'LET)
    [name <- identifier/p]
    (token/p 'ASSIGN)
    [expr <- expression/p]
    (pure #`(define #,name #,expr))))

(define statement/p
  (or/p let/p
        #;fun/p
        expression/p))
