(module
interp
mzscheme

(require (lib "ast.ss" "waxeye")
         (lib "parser.ss" "waxeye")
         "dfa.scm"
         "gen.scm"
         "scheme.scm"
         "util.scm")

(provide dynamic-parser interpreter)


(define (dynamic-parser grammar)
  (make-parser *start-index* *eof-check* (make-automata grammar)))


(define (interpreter grammar input)
  (let ((input-ast ((dynamic-parser grammar) input)))
    (if (parse-error? input-ast)
        (display-parse-error input-ast)
        (display-ast input-ast))))

)
