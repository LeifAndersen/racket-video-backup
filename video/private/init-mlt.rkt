#lang racket/base

(require ffi/unsafe
         "mlt.rkt")

(define scheme_register_process_global
  (get-ffi-obj 'scheme_register_process_global #f (_fun _string _pointer -> _pointer)))

(let ([v (scheme_register_process_global "mlt-support-initialized"
                                         (cast 1 _racket _pointer))])
  (unless v
    (void (mlt_factory_init #f))))

(void
 (plumber-add-flush!
  (current-plumber) (λ (p)
                      (collect-garbage)
                      (mlt_factory_close))))
