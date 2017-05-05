#lang racket/base

(provide (all-defined-out))
(require "mlt.rkt"
         "init-mlt.rkt"
         racket/runtime-path)

(define-runtime-path video-dir "..")

(define current-renderer (make-parameter #f))
(define current-profile (make-parameter #f))

(when mlt-lib
  (void (dynamic-require (build-path video-dir "render.rkt") #f)))
