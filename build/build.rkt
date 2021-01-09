#lang racket/base

(require "make.rkt"
         "../src/waxeye/version.rkt")


(define *name* "waxeye")
(define *doc-book* "/usr/local/docbook")


(target clean (clean-book clean-dist clean-unix)
        (^ rm -rf tmp))


(target book (book-html))


(target book-html ()
        (^ asciidoc -a toc -n -o docs/manual.html docs/book/book))


(target book-pdf ()
        (^ mkdir -p tmp/book)
        (^ asciidoc -a toc -b docbook --doctype=book -o tmp/book/book.xml docs/book/book)
        ($ xsltproc '-o 'tmp/book/book.fo (++ *doc-book* "/fo/docbook.xsl") 'tmp/book/book.xml)
        (^ fop tmp/book/book.fo docs/manual.pdf))


(target clean-book ()
        (^ rm -rf tmp/book)
        (^ rm -f docs/manual.html docs/manual.pdf))


(target dist (clean dist-src dist-unix))


(define (cp-dist from)
  ($ cp '-r from (++ "dist/waxeye-" *version* "/")))


(target dist-base (book)

 ($ mkdir '-p (++ "dist/waxeye-" *version*))

 (cp-dist "build")
 (cp-dist "docs")
 (cp-dist "grammars")
 (cp-dist "lib")
 (cp-dist "LICENSE.md")
 (cp-dist "README.md")
 (cp-dist "src")
 (cp-dist "test")

 ($ chmod '755 (++ "dist/waxeye-" *version* "/build/make"))
 ($ chmod '755 (++ "dist/waxeye-" *version* "/build/unix"))
 ($ chmod '755 (++ "dist/waxeye-" *version* "/build/waxeye")))


(target dist-src (dist-base)
        (cd dist
            ($ zip '-r (++ "waxeye-" *version* "-src.zip waxeye-" *version*))
            ($ tar 'cjf (++ "waxeye-" *version* "-src.tar.bz2 waxeye-" *version*))))


(target dist-unix (dist-base)
        (cd$ (++ "dist/waxeye-" *version*)
             (^ ./build/unix))
        (cd dist
            ($ tar 'czf (++ "waxeye-" *version* "-unix.tar.gz waxeye-" *version*))
            ($ tar 'cjf (++ "waxeye-" *version* "-unix.tar.bz2 waxeye-" *version*))))


(target clean-dist ()
        (^ rm -rf dist))


(target clean-unix ()
        (^ rm -rf bin lib))


(run-make)
