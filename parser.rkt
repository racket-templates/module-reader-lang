#lang racket
(provide parse-all)
(require megaparsack/parser-tools/lex
         data/monad
         data/applicative
         megaparsack
         megaparsack/text
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
(define term/p
  (or/p number/p
        identifier/p
        funcall/p))

(define (binary/p high-level/p op-list)
  (define (op/p)
    (define (make-op/p op)
      (do [_ <- (try/p (token/p op))]
        (pure op)))
    (apply or/p (map make-op/p op-list)))
  (do [e <- high-level/p]
    [es <- (many/p (try/p (do [op <- (op/p)]
                            [e <- high-level/p]
                            (pure (list op e)))))]
    (pure (foldl (λ (op+rhs lhs)
                   (match op+rhs
                     [(list op rhs)
                      #`(#,op #,lhs #,rhs)]))
                 e es))))
(define (table/p base/p list-of-op-list)
  (if (empty? list-of-op-list)
      base/p
      (table/p (binary/p base/p (car list-of-op-list))
               (cdr list-of-op-list))))
(define expression/p
  (table/p term/p
           '((* /)
             (+ -))))

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
