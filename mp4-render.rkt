#lang racket/base

(require racket/class
         "private/video.rkt"
         "private/mlt.rkt"
         "render.rkt")

(provide render-mixin)

(define render-mixin
  (mixin (render<%>) (render<%>)
    (super-new)
    (inherit-field dest-dir dest-filename)
    (define/override (render source)
      (define dest-filename* (or dest-filename "defaut"))
      (parameterize ([current-renderer this])
        (mlt-*-connect (make-consumer
                        #:type 'avformat
                        ;#:target (format "images%05d.jpg"))
                        #:target (build-path dest-dir
                                             (format "~a.mp4" dest-filename*)))
                       source)))))