#lang racket/base

;; Library to initialize the MLT framework.
;; Alternatively your program can just call `mlt-factory-init` on its own.
;;   If you do this though, do make sure to shut things down with mlt-factory-init

(require ffi/unsafe
         "mlt.rkt")

;; Because we currently can't rely on MLT being installed,
;;   only run this module if it is.
(when (ffi-lib? mlt-lib)
  ;; Init MLT factory (ONCE PER PROCESS)
  (define scheme_register_process_global
    (get-ffi-obj 'scheme_register_process_global #f (_fun _string _pointer -> _pointer)))

  (let ([v (scheme_register_process_global "mlt-support-initialized"
                                           (cast 1 _racket _pointer))])
    (unless v
      (void (mlt_factory_init #f))))

  ;; Close MLT factory on program exit
  (void
   (plumber-add-flush!
    (current-plumber) (λ (p)
                        (collect-garbage)
                        (mlt_factory_close)))))
