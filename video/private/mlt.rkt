#lang racket/base

#|
   Copyright 2016-2017 Leif Andersen

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
|#

(require ffi/unsafe
         ffi/unsafe/define
         racket/stxparam
         racket/splicing
         (for-syntax racket/syntax
                     racket/base
                     racket/string
                     syntax/parse))
(provide (all-defined-out))

;; MLT Library
(define mlt-lib (ffi-lib "libmlt" '("6")))
(define-ffi-definer define-mlt mlt-lib)
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
(define-syntax (ret-error stx)
  (syntax-parse stx
    [(_ v)
     (quasisyntax/loc stx (when v (raise-mlt-error current-func-name)))]))
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

(define-mlt* mlt-factory-init (_fun _path -> [v : _mlt-repository/null]
                                    -> (null-error v)))
(define-mlt* mlt-profile-init (_fun _string -> [v : _mlt-profile-pointer/null]
                                    -> (null-error v)))
