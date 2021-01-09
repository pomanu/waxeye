#lang racket/base

(require "main.rkt")


(main (vector->list (current-command-line-arguments)))
