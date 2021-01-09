#lang racket

(require "parser.rkt")

;; Parse our input
(let ((ast (parser "42")))
  ;; Print our AST
  (display-ast ast))
