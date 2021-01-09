#lang racket/base

(provide within-set?)


;; Is 'b' within set 'a'?
;;
;; The elements of the set are assumed to be in ascending order.
(define (within-set? a b)
  (if (null? a)
      #f
      (let ((aa (car a)))
        (if (char? aa)
            (if (char=? aa b)
                #t
                (if (char<? aa b)
                    (within-set? (cdr a) b)
                    #f))
            (if (char-within-range? (car aa) (cdr aa) b)
                #t
                (if (char<? (cdr aa) b)
                    (within-set? (cdr a) b)
                    #f))))))

;; Is the character within the inclusive character range?
(define (char-within-range? start end char)
  (if (and (char<=? start char) (char<=? char end))
      #t
      #f))


(module+ test
  (require rackunit)

  (check-true (within-set? '(#\a) #\a))
  (check-true (within-set? '(#\a #\b) #\a))
  (check-true (within-set? '(#\a #\b) #\b))
  (check-true (within-set? '(#\a #\b #\c) #\a))
  (check-true (within-set? '(#\a #\b #\c) #\b))
  (check-true (within-set? '(#\a #\b #\c) #\c))

  (check-false (within-set? '() #\a))
  (check-false (within-set? '() #\space))

  (check-false (within-set? '(#\a) #\space))
  (check-false (within-set? '(#\a) #\b))
  (check-false (within-set? '(#\a #\b) #\d))
  (check-false (within-set? '(#\a #\b) #\E))
  (check-false (within-set? '(#\space #\a #\b) #\tab))
  (check-false (within-set? '(#\a #\A #\e #\E) #\æ))
  (check-false (within-set? '(#\e #\E #\o #\O) #\ø))
  (check-false (within-set? '(#\e #\E #\o #\O) #\ø))

  ;; test that set is assumed to be in ascending order
  (check-false (within-set? '(#\c #\a #\b) #\a))
  (check-false (within-set? '(#\c #\a #\b) #\b))
  (check-false (within-set? '(#\a #\c #\b) #\b))

  (check-true (within-set? '((#\a . #\a)) #\a))
  (check-true (within-set? '((#\a . #\z)) #\a))
  (check-true (within-set? '((#\A . #\Z) (#\a . #\z)) #\c))
  (check-true (within-set? '((#\A . #\Z) (#\a . #\z)) #\C))
  (check-true (within-set? '((#\0 . #\9) (#\A . #\Z) (#\a . #\z)) #\e))
  (check-true (within-set? '((#\0 . #\9) (#\A . #\Z) (#\a . #\z)) #\F))
  (check-true (within-set? '((#\0 . #\9) (#\A . #\Z) (#\a . #\z)) #\7))
  (check-true (within-set? '((#\d . #\g)) #\g))
  (check-true (within-set? '((#\d . #\h)) #\g))
  (check-true (within-set? '(#\a (#\b . #\z)) #\a))
  (check-true (within-set? '(#\a (#\c . #\z)) #\a))
  (check-true (within-set? '(#\a (#\b . #\z)) #\b))
  (check-true (within-set? '(#\a (#\c . #\z)) #\c))
  (check-true (within-set? '(#\a (#\c . #\z)) #\z))
  (check-true (within-set? '(#\a (#\c . #\z)) #\x))
  (check-true (within-set? '(#\a #\b #\c (#\w . #\y)) #\x))
  (check-true (within-set? '(#\a (#\b . #\h) #\c (#\w . #\y)) #\d))

  (check-false (within-set? '((#\a . #\a)) #\b))
  (check-false (within-set? '((#\d . #\g)) #\h))
  (check-false (within-set? '((#\d . #\h)) #\b))
  (check-false (within-set? '((#\a . #\z)) #\A))
  (check-false (within-set? '(#\a (#\d . #\z)) #\c))
  (check-false (within-set? '(#\c (#\D . #\Z)) #\C))
  (check-false (within-set? '((#\0 . #\9) (#\A . #\Z)) #\e))
  (check-false (within-set? '((#\A . #\z)) #\4))
  (check-false (within-set? '((#\0 . #\5) (#\6 . #\Z)) #\t))
  (check-false (within-set? '(#\a #\b #\c (#\w . #\y)) #\r))
  (check-false (within-set? '(#\a (#\b . #\h) #\c (#\w . #\y)) #\j))

  ;; test that set is assumed to be in ascending order
  (check-false (within-set? '((#\a . #\z) (#\A . #\Z)) #\A))
  (check-false (within-set? '((#\a . #\z) (#\A . #\Z)) #\F))
  (check-false (within-set? '((#\a . #\z) #\A) #\A))
  (check-false (within-set? '((#\A . #\Z) (#\0 . #\9) (#\a . #\z)) #\1))
  (check-false (within-set? '((#\j . #\m) (#\a . #\c)) #\a))
  (check-false (within-set? '(#\a (#\w . #\y) #\b #\c) #\b))
  (check-false (within-set? '((#\b . #\h) #\a #\c (#\w . #\y)) #\a)))
