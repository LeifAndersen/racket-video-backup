#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define)
(provide (all-defined-out))

(define mlt-lib (ffi-lib "libmlt" '("6")
                         #:fail (λ () #f)))
(define-ffi-definer define-mlt mlt-lib)

(define-cpointer-type _mlt-ptr)

(define-mlt mlt_factory_init (_fun _path -> _mlt-ptr/null))
(define-mlt mlt_factory_close (_fun -> _void))
(define-mlt mlt_profile_init (_fun _string -> _mlt-ptr/null))
