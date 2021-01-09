#lang racket/base

(provide debug
         *debug*
         debug!)


(define *debug* #f)

(define (debug! v)
  (set! *debug* v))

(define-syntax debug
  (syntax-rules ()
    ((_ a ...)
     (when *debug*
           a ...))))
