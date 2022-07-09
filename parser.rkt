#lang racket
(provide parse-all)
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         megaparsack/parser-tools/lex
         data/monad
         data/applicative
         megaparsack
         megaparsack/text)

(define (parse-all in
                   #:source [src ""])
  (define module-str (port->string in))
  (parse-result!
   (parse-tokens expression/p (lex-simple module-str)
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

(define-tokens simple [IDENTIFIER NUMBER])
(define-empty-tokens simple* [OPEN-PAREN CLOSE-PAREN COMMA])
(define simple-lexer
  (lexer-src-pos
   [#\( (token-OPEN-PAREN)]
   [#\) (token-CLOSE-PAREN)]
   [#\, (token-COMMA)]
   [(:+ (:or (:/ #\a #\z) (:/ #\A #\Z)))
    (token-IDENTIFIER (string->symbol lexeme))]
   [(:+ (:/ #\0 #\9))
    (token-NUMBER (string->number lexeme))]
   [(:or whitespace blank iso-control) (void)]
   [(eof) eof]))

(define (lex-simple str)
  (define in (open-input-string str))
  (port-count-lines! in)
  (let loop ([v (simple-lexer in)])
    (cond [(void? (position-token-token v)) (loop (simple-lexer in))]
          [(eof-object? (position-token-token v)) '()]
          [else (cons v (loop (simple-lexer in)))])))
