(module
example
mzscheme

(require "parser.scm")

;; Parse our input
(let ((ast (parser "42")))
  ;; Print our AST
  (display-ast ast))

)
