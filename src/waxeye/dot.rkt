#lang racket/base

(require "util.rkt")


(define (display-dot name state)
  (let ((visited (make-hash)))
    (hash-set! visited state "match")
    (display-ln "digraph " name " {")
    (display-state visited state)
    (display-ln "\"match\" [ label = \"match\" ];")
    (display-ln "}")))


(define (display-state visited state)
  (define (get-state-name table state)
    (let ((val (hash-ref table state #f)))
      (if val
          val
          (begin
            (let ((v2 (gensym)))
              (hash-set! table state v2)
              v2)))))
  (unless (state-match state)
          (display-ln "\"" (get-state-name visited state) "\""
                      "[ label = \"\" ];")
          (for-each (lambda (a)
                      (display-ln "\"" (get-state-name visited state) "\""
                                  "->"
                                  "\"" (get-state-name visited (cdr a)) "\""
                                  "[ label = \"" (car a) "\" ];")
                      (display-state visited (cdr a)))
                    (state-edges state))))