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
(define _symbol-or-null
  (make-ctype _string
              (λ (v) (if (symbol? v) (symbol->string v) v))
              (λ (v) (if (string? v) (string->symbol v) v))))
(define _mlt-position _int32)
(define _mlt-destructor (_fun (_cpointer 'destruct) -> _void))
(define _mlt-image-format (_enum '(mlt-image-none = 0
                                   mlt-image-rgb24
                                   mlt-image-rgb24a
                                   mlt-image-yuv422
                                   mlt-image-yuv420p
                                   mlt-image-opengl
                                   mlt-image-glsl
                                   mlt-image-glsl-texture)))
(define _mlt-audio-format (_enum '(mlt-audio-none = 0
                                   mlt-audio-pcm
                                   mlt-audio-s16
                                   mlt-audio-s32
                                   mlt-audio-float
                                   mlt-audio-s32le
                                   mlt-audio-f32le
                                   mlt-audio-u8)))
(define _mlt-whence (_enum '(mlt-whence-relative-start = 0
                             mlt-whence-relative-current
                             mlt-whence-relative-end)))
(define _mlt-service-type (_enum '(invalid-type = 0
                                   unknown-type
                                   producer-type
                                   tractor-type
                                   playlist-type
                                   multitrack-type
                                   filter-type
                                   transition-type
                                   consumer-type
                                   field-type)))
(define _mlt-time-format (_enum '(mlt-time-frames = 0
                                  mlt-time-clock
                                  mlt-time-smpte-df
                                  mlt-time-smpte
                                  mlt-time-smpte-ndf)))
(define _mlt-keyframe-type (_enum '(mlt-keyframe-discrete = 0
                                    mlt-keyframe-linear
                                    mlt-keyframe-smooth)))
(define-cpointer-type _mlt-repository)
(define-cpointer-type _mlt-deque)
(define-cpointer-type _playlist-entry)
(define-cpointer-type _mlt-event)
(define-cpointer-type _mlt-field)
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
(define-cstruct _mlt-properties
  ([child (_cpointer/null 'child)]
   [local (_cpointer/null 'local)]
   [close _mlt-destructor]
   [close-object (_cpointer/null 'close-object)]))
(define-cstruct (_mlt-frame _mlt-properties)
  ([get-alpha-mask (_fun _mlt-frame-pointer -> (_ptr o _uint8))]
   [convert-image (_fun _mlt-frame-pointer
                        (_ptr io (_ptr io _uint8))
                        (_ptr io _mlt-image-format)
                        _mlt-image-format
                        -> [v : _bool]
                        -> (when v (raise-mlt-error mlt-frame-convert-image)))]
   [convert-audio (_fun _mlt-frame-pointer
                        (_ptr io (_ptr io _void))
                        (_ptr io _mlt-audio-format)
                        _mlt-audio-format
                        -> [v : _bool]
                        -> (when v (raise-mlt-error mlt-framework-convert-audio)))]
   [stack-image _mlt-deque]
   [stack-audio _mlt-deque]
   [stack-service _mlt-deque]
   [is-processing _int]))
(define-cstruct (_mlt-service _mlt-properties)
  ([get-frame (_fun _mlt-service-pointer _mlt-frame-pointer _int -> _int)]
   [close* _mlt-destructor]
   [close-object (_cpointer/null 'class-object)]
   [local (_cpointer/null 'local)]
   [child (_cpointer/null 'child)]))
(define-cstruct (_mlt-consumer _mlt-service)
  ([start* (_fun _mlt-consumer-pointer -> [v : _bool]
                 -> (when v (raise-mlt-error mlt-consumer-start)))]
   [stop* (_fun _mlt-consumer-pointer -> [v : _bool]
                -> (when v (raise-mlt-error mlt-consumer-stop)))]
   [is-stopped* (_fun _mlt-consumer-pointer -> [v : _bool]
                      -> (when v (raise-mlt-error mlt-consumer-is-stopped)))]
   [purge* (_fun _mlt-consumer-pointer -> [v : _bool]
                 -> (when v (raise-mlt-error mlt-consumer-purge)))]
   [close* (_fun _mlt-consumer-pointer -> _void)]
   [local (_cpointer/null 'local)]
   [child (_cpointer/null 'child)]))
(define-cstruct (_mlt-filter _mlt-service)
  ([close* (_fun _mlt-filter-pointer -> _void)]
   [process* (_fun _mlt-filter-pointer _mlt-frame-pointer -> _mlt-frame-pointer)]
   [child (_cpointer/null 'child)]))
(define-cstruct (_mlt-transition _mlt-service)
  ([close* (_fun _mlt-transition-pointer -> _void)]
   [process* (_fun _mlt-transition-pointer _mlt-frame-pointer _mlt-frame-pointer
                   -> _mlt-frame-pointer)]
   [child (_cpointer/null 'child)]
   [producer _mlt-service-pointer]
   [frames (_cpointer/null 'frames _mlt-frame-pointer)]
   [held _int]))
(define-cstruct (_mlt-producer _mlt-service)
  ([get-frame* (_fun _mlt-producer-pointer _mlt-frame-pointer _int -> _int)]
   [close* _mlt-destructor]
   [close-object (_cpointer/null 'close-object)]
   [local (_cpointer/null 'local)]
   [child (_cpointer/null 'child)]))
