#lang racket/base

(require waxeye/ast
         waxeye/parser
         "dfa.rkt"
         "gen.rkt"
         "racket.rkt"
         "util.rkt")

(provide dynamic-parser
         interpreter)


(define (dynamic-parser grammar)
  (make-parser *start-index* *eof-check* (make-automata grammar)))


(define (interpreter grammar input)
  (let ((input-ast ((dynamic-parser grammar) input)))
    (if (parse-error? input-ast)
        (display-parse-error input-ast)
        (display-ast input-ast))))
