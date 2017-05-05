#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         racket/stxparam
         racket/splicing
         (for-syntax ffi/unsafe
                     racket/syntax
                     racket/base
                     racket/string
                     syntax/parse))
(provide (all-defined-out))

;; MLT Library
;; We currently cannot assume that MLT is installed and thus
;;  need to error at runtime (rather than compile time) if it is
;;  not installed.
(define mlt-lib (ffi-lib "libmlt" '("6")
                         #:fail (λ () #f)))
(define-for-syntax mlt-lib (ffi-lib "libmlt" '("6")
                                    #:fail (λ () #f)))
(define-ffi-definer define-mlt/internal mlt-lib)
(define-syntax (define-mlt stx)
  (syntax-parse stx
    [(_ name:id args ...)
     (if mlt-lib
         #'(define-mlt/internal name args ...)
         #'(define (name . rst) (raise-user-error 'video "MLT Not Installed")))]))

(define-syntax-parameter current-func-name #f)
(define-syntax (define-mlt* stx)
  (syntax-parse stx
    [(_ name:id args ...)
     #:with c-name (format-id stx "~a" (string-replace (symbol->string (syntax-e #'name)) "-" "_"))
     (syntax/loc stx
       (splicing-syntax-parameterize ([current-func-name (make-rename-transformer #'name)])
         (define-mlt name args ...
           #:c-id c-name)))]))

;; MLT Error
(struct exn:fail:mlt exn:fail ())
(struct exn:fail:mlt:warning exn:fail:mlt ())
(define raise-mlt-warnings (make-parameter #f))
(define-syntax (raise-mlt-warning stx)
  (syntax-parse stx
    [(_ function:id)
     #:with expanded-function (local-expand #'function 'expression #f)
     (syntax/loc stx
       (when (raise-mlt-warnings)
         (raise (exn:fail:mlt:warning (format "MLT: Function ~a returned warning" 'expanded-function)
                                      (current-continuation-marks)))))]))
(define-syntax (raise-mlt-error stx)
  (syntax-parse stx
    [(_ function:id (~optional (~seq #:msg msg:str)
                               #:defaults ([msg #'""])))
     #:with expanded-function (local-expand #'function 'expression #f)
     (syntax/loc stx
       (raise (exn:fail:mlt (format "MLT: Function ~a failed: ~a" 'expanded-function msg)
                            (current-continuation-marks))))]))
(define-syntax (null-error stx)
  (syntax-parse stx
    [(_ v)
     (quasisyntax/loc stx (or v (raise-mlt-error current-func-name)))]))

;; Types
(define-cpointer-type _mlt-repository)
(define-cstruct _mlt-profile
  ([description _string]
   [frame-rate-num _int]
   [frame-rate-den _int]
   [width _int]
   [height _int]
   [progressive _int]
   [sample-aspect-num _int]
   [sample-aspect-den _int]
   [display-aspect-num _int]
   [disaply-aspect-den _int]
   [colorspace _int]
   [is-explicit _int]))

;; Factory
(define-mlt* mlt-factory-init (_fun _path -> [v : _mlt-repository/null]
                                    -> (null-error v)))
(define-mlt* mlt-factory-close (_fun -> _void))

;; Profile
(define-mlt* mlt-profile-init (_fun _string -> [v : _mlt-profile-pointer/null]
                                    -> (null-error v)))
(define-mlt* mlt-profile-clone (_fun _mlt-profile-pointer -> [v : _mlt-profile-pointer]
                                     -> (null-error v)))
(define-mlt* mlt-profile-close (_fun _mlt-profile-pointer -> _void))
(define-mlt* mlt-profile-dar (_fun _mlt-profile-pointer -> _double))
(define-mlt* mlt-profile-fps (_fun _mlt-profile-pointer -> _double))
