#lang racket/base
(provide lex-simple)
(require (prefix-in : parser-tools/lex-sre)
         parser-tools/lex)

(define-tokens simple [IDENTIFIER NUMBER])
(define-empty-tokens simple*
  [OPEN-PAREN CLOSE-PAREN
              COMMA ASSIGN])
(define-empty-tokens keyword*
  [LET])
(define simple-lexer
  (lexer-src-pos
   [#\( (token-OPEN-PAREN)]
   [#\) (token-CLOSE-PAREN)]
   [#\, (token-COMMA)]
   [#\= (token-ASSIGN)]
   ["let" (token-LET)]
   [(:+ (:or (:/ #\a #\z) (:/ #\A #\Z) #\-))
    (token-IDENTIFIER (string->symbol lexeme))]
   [(:+ (:/ #\0 #\9))
    (token-NUMBER (string->number lexeme))]
   [(:or whitespace blank iso-control) (void)]
   [(eof) eof]))

(define (lex-simple in)
  (port-count-lines! in)
  (let loop ([v (simple-lexer in)])
    (cond [(void? (position-token-token v)) (loop (simple-lexer in))]
          [(eof-object? (position-token-token v)) '()]
          [else (cons v (loop (simple-lexer in)))])))
