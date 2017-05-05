#lang racket/base

(provide (all-defined-out))
(require "mlt.rkt"
         "init-mlt.rkt"
         racket/runtime-path)

(define-runtime-path video-dir "..")

(when mlt-lib
  (void (dynamic-require (build-path video-dir "render.rkt") #f)))
