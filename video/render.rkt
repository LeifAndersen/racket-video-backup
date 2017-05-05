#lang racket/base

(require racket/class
         "private/init-mlt.rkt"
         "init.rkt"
         "private/mlt.rkt"
         "private/video.rkt")

(provide (all-defined-out))

(define render%
  (class* object% ()
    (super-new)
    
    (define profile (mlt-profile-init #f))

    (define/public (get-profile)
      profile)))
    
;; Set the current renderer
(let ([r (new render%)])
  (current-renderer r)
  (current-profile (send r get-profile)))
