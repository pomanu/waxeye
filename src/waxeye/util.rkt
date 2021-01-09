#lang racket/base

(provide (all-defined-out))


;; Prints the value followed by a newline
(define (print-ln . e)
  (for-each print e)
  (newline))


;; Displays the value followed by a newline
(define (display-ln . e)
  (for-each display e)
  (newline))


;; Concatenates a list of lists
(define (list-concat sl)
  (foldr append '() sl))


;; Concatenates a list of strings
(define (string-concat sl)
  (foldr string-append "" sl))
