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
    
    (define res-counter 0)
    (define profile (mlt-profile-init prof-name))
    
    (define/private (get-current-filename)
      (begin0 (format "resource~a" res-counter)
              (set! res-counter (add1 res-counter))))

    (define/public (get-profile)
      profile)
    
    (define/public (setup-profile)
      (define fps* (rationalize (inexact->exact fps) 1/1000000))
      (set-mlt-profile-width! profile width)
      (set-mlt-profile-height! profile height)
      (set-mlt-profile-frame-rate-den! profile (denominator fps*))
      (set-mlt-profile-frame-rate-num! profile (numerator fps*)))))
    
;; Set the current renderer
(let ([r (new render% [dest-dir #f])])
  (send r setup-profile)
  (current-renderer r)
  (current-profile (send r get-profile)))
