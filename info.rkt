#lang info

(define collection 'multi)

(define deps '(("base" "6.8.0.2")
               "gui-lib"
               "scribble-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"))

(define version "0.1")
(define pkg-authors '(leif))
(define pkg-desc "Video Language")
