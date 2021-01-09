#lang racket/base

(require (only-in racket/cmdline command-line)
         waxeye/ast
         "debug.rkt"
         "file.rkt"
         "gen.rkt"
         "interp.rkt"
         "load.rkt"
         "racket.rkt"
         "tester.rkt"
         "transform.rkt"
         "util.rkt"
         "version.rkt")

(provide main)


(define *grammar-path* #f)
(define *grammar-test* #f)
(define *header-path* #f)
(define *interpret* #f)
(define *output-path* #f)
(define *target-lang* #f)


(define (main args)
  (process-args args)
  (when *grammar-path*
        (let ((grammar-tree (load-grammar *grammar-path*)))
          (transform-grammar grammar-tree)
          (start-nt! *start-name* grammar-tree)
          (cond
           (*interpret* (interpreter grammar-tree (input-as-string (current-input-port))))
           (*grammar-test* (tester grammar-tree *grammar-test*))
           ((and *target-lang* *output-path*)
            (begin
              (when *header-path*
                    (file-header! (file-as-string-lines *header-path*)))
              (display-version)
              (for-each (lambda (a)
                          (display-ln "generated: " a))
                        (*target-lang* grammar-tree *output-path*))))
           (else (display-help))))))


(define (process-args args)
  (if (member "--version" args)
      (display-version)
      (if (null? args)
          (begin
            (display-version)
            (newline)
            (display-help))
          (parse-args args))))


(define (parse-args args)
  (command-line
   #:program "waxeye"
   #:argv args

   #:help-labels "Waxeye modes:"

   #:once-any
   ("-g" language dir
    "Generate"
    (set! *target-lang* (case (string->symbol language)
                          ((racket) gen-racket)
                          (else #f)))
    (set! *output-path* (if (equal? (string-ref dir (- (string-length dir) 1)) #\/)
                            dir
                            (string-append dir "/"))))
   ("-i" "Interpret"
    (set! *interpret* #t))
   ("-t" test
    "Test"
    (set! *grammar-test* test))

   #:help-labels "Grammar options:"

   #:once-each
   ("-m"
    "Modular Grammar - default: false"
    (modular-grammar! #t))
   ("-s" start
    "Starting non-terminal - default: first non-terminal"
    (start-name! start))

   #:help-labels "Parser options:"

   #:once-each
   ("-c" comment
    "Header comment for generated files - default: none"
    (set! *header-path* comment))
   ("-e" eof
    "Check parser consumes all input - default: true"
    (eof-check! (equal? eof "true")))
   ("-n" namespace
    "Module or package namespace - default: none"
    (module-name! namespace))
   ("-p" prefix
    "Name prefix for generated files - default: none"
    (name-prefix! prefix))

   #:help-labels "Misc options:"

   #:once-each
   ("--debug" "Activates debug information"
    (debug! #t))
   ("--version" "Prints version number and copyright notice"
    (void))

   ;; expects one grammar path
   #:args (grammar)
   ;; set the grammar path when done
   (set! *grammar-path* grammar)))


(define (display-version)
  (display-ln "Waxeye Parser Generator v" *version*))


(define (display-help)
  (parse-args '("--help")))
