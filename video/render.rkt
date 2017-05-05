#lang racket/base

(require racket/contract/base
         racket/match
         racket/math
         racket/dict
         racket/class
         racket/file
         (prefix-in file: file/convertible)
         (only-in pict pict? pict->bitmap)
         "private/init-mlt.rkt"
         "init.rkt"
         "private/mlt.rkt"
         "private/video.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse))

(provide (all-defined-out))

(define render%
  (class* object% ()
    (super-new)
    (init-field dest-dir
                [dest-filename #f]
                [prof-name #f]
                [width 720]
                [height 576]
                [fps 25])
    
    (define profile (mlt-profile-init prof-name))

    (define/public (get-profile)
      profile)))
    
;; Set the current renderer
(let ([r (new render% [dest-dir #f])])
  (current-renderer r)
  (current-profile (send r get-profile)))
