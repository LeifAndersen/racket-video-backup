#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define)
(provide (all-defined-out))

;; MLT Library
;; We currently cannot assume that MLT is installed and thus
;;  need to error at runtime (rather than compile time) if it is
;;  not installed.
(define mlt-lib (ffi-lib "libmlt" '("6")
                         #:fail (λ () #f)))
(define-ffi-definer define-mlt mlt-lib)

;; Types
(define-cpointer-type _mlt-repository)
(define-cpointer-type _mlt-profile-pointer)

;; Factory
(define-mlt mlt-factory-init (_fun _path -> _mlt-repository/null)
  #:c-id mlt_factory_init)
(define-mlt mlt-factory-close (_fun -> _void)
  #:c-id mlt_factory_close)

;; Profile
(define-mlt mlt-profile-init (_fun _string -> [v : _mlt-profile-pointer/null])
  #:c-id mlt_profile_init)
