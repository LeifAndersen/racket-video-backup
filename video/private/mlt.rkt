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
(define-cstruct (_mlt-playlist _mlt-producer)
  ([blank* _mlt-producer]
   [size* _int]
   [count* _int]
   [list* (_cpointer 'list (_cpointer 'list* _playlist-entry))]))
(define-cstruct _mlt-playlist-clip-info
  ([clip _int]
   [producer _mlt-producer-pointer/null]
   [cut _mlt-producer-pointer/null]
   [start _mlt-position]
   [resource _string]
   [frame-in _mlt-position]
   [frame-out _mlt-position]
   [frame-count _mlt-position]
   [length _mlt-position]
   [fps _float]
   [repeate _int]))
(define-cstruct (_mlt-tractor _mlt-producer)
  ([producer* _mlt-service-pointer]))
(define-cstruct _mlt-track
  ([producer _mlt-producer-pointer/null]
   [event _mlt-event/null]))
(define-cstruct (_mlt-multitrack _mlt-producer)
   ([list* _mlt-track-pointer]
   [size* _int]
   [count* _int]))

;; Factory
(define-mlt* mlt-factory-init (_fun _path -> [v : _mlt-repository/null]
                                    -> (null-error v)))
(define-mlt* mlt-factory-producer (_fun _mlt-profile-pointer _symbol-or-null _string
                                        -> [v : _mlt-producer-pointer/null]
                                        -> (null-error v)))
(define-mlt* mlt-factory-consumer (_fun _mlt-profile-pointer _symbol-or-null _string
                                        -> [v : _mlt-consumer-pointer/null]
                                        -> (null-error v)))
(define-mlt* mlt-factory-filter (_fun _mlt-profile-pointer _symbol-or-null _string
                                      -> [v : _mlt-filter-pointer/null]
                                      -> (null-error v)))
(define-mlt* mlt-factory-transition (_fun _mlt-profile-pointer _symbol-or-null _string
                                          -> [v : _mlt-transition-pointer/null]
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
(define-mlt* mlt-profile-from-producer (_fun _mlt-profile-pointer _mlt-producer-pointer -> _void))

;; Consumer
(define-mlt* mlt-consumer-connect (_fun _mlt-consumer-pointer _mlt-service-pointer -> (v : _int)
                                        ->
                                        (cond
                                          [(= 0 v) (void)]
                                          [(= 1 v)
                                           (raise-mlt-error mlt-consumer-connect
                                                            #:msg "Producer does not accept input")]
                                          [(= 2 v) (raise-mlt-error mlt-consumer-connect
                                                                    #:msg "Invalid Producer")]
                                          [(= 3 v) (raise-mlt-error
                                                    mlt-consumer-connect
                                                    #:msg "Producer has already been registered")]
                                          [else (raise-mlt-warning mlt-consumer-connect)])))
(define-mlt* mlt-consumer-start (_fun _mlt-consumer-pointer -> [v : _bool]
                                      -> (ret-error v))
  #:wrap (λ (func)
           (λ (cp)
             (dynamic-require 'racket/gui #f)
             (func cp))))
(define-mlt* mlt-consumer-stop (_fun _mlt-consumer-pointer -> [v : _bool]
                                     -> (ret-error v)))
(define-mlt* mlt-consumer-is-stopped (_fun _mlt-consumer-pointer -> _bool))
(define-mlt* mlt-consumer-close (_fun _mlt-consumer-pointer -> _void))
(define-mlt* mlt-consumer-purge (_fun _mlt-consumer-pointer -> _void))

;; Producer
(define-mlt* mlt-producer-close (_fun _mlt-producer-pointer -> _void))
(define-mlt* mlt-producer-service (_fun _mlt-producer-pointer -> _mlt-service-pointer))
(define-mlt* mlt-producer-optimise (_fun _mlt-producer-pointer -> [v : _bool]
                                         -> (ret-error v)))
(define-mlt* mlt-producer-set-in-and-out (_fun _mlt-producer-pointer _mlt-position _mlt-position
                                               -> [v : _bool]
                                               -> (ret-error v)))
(define-mlt* mlt-producer-set-speed (_fun _mlt-producer-pointer _double -> [v : _bool]
                                          -> (ret-error v)))
(define-mlt* mlt-producer-get-length (_fun _mlt-producer-pointer -> _mlt-position))
(define-mlt* mlt-producer-get-length-time (_fun _mlt-producer-pointer _mlt-time-format -> _string))
(define-mlt* mlt-producer-get-in (_fun _mlt-producer-pointer -> _mlt-position))
(define-mlt* mlt-producer-get-out (_fun _mlt-producer-pointer -> _mlt-position))
(define-mlt* mlt-producer-get-playtime (_fun _mlt-producer-pointer -> _mlt-position))
(define-mlt* mlt-producer-get-speed (_fun _mlt-producer-pointer -> _double))
(define-mlt* mlt-producer-get-fps (_fun _mlt-producer-pointer -> _double))
(define-mlt* mlt-producer-is-mix (_fun _mlt-producer-pointer -> _bool))
(define-mlt* mlt-producer-is-blank (_fun _mlt-producer-pointer -> _bool))
(define-mlt* mlt-producer-is-cut (_fun _mlt-producer-pointer -> _bool))
(define-mlt* mlt-producer-cut (_fun _mlt-producer-pointer _int _int
                                    -> [v : _mlt-producer-pointer/null]
                                    -> (null-error v)))
(define-mlt* mlt-producer-cut-parent (_fun _mlt-producer-pointer -> [v : _mlt-producer-pointer/null]
                                           -> (null-error v)))
(define-mlt* mlt-producer-seek (_fun _mlt-producer-pointer _mlt-position -> [v : _bool]
                                     -> (ret-error v)))
(define-mlt* mlt-producer-seek-time (_fun _mlt-producer-pointer _string -> [v : _bool]
                                          -> (ret-error v)))
(define-mlt* mlt-producer-frame (_fun _mlt-producer-pointer -> _mlt-position))
(define-mlt* mlt-producer-frame-time (_fun _mlt-producer-pointer -> _string))
(define-mlt* mlt-producer-position (_fun _mlt-producer-pointer -> _mlt-position))

;; Playlist
(define-mlt* mlt-playlist-init (_fun -> [v : _mlt-playlist-pointer/null]
                                     -> (null-error v)))
(define-mlt* mlt-playlist-append (_fun _mlt-playlist-pointer _mlt-producer-pointer -> [v : _bool]
                                       -> (ret-error v)))
(define-mlt* mlt-playlist-append-io (_fun _mlt-playlist-pointer
                                          _mlt-producer-pointer
                                          _mlt-position
                                          _mlt-position
                                          -> [v : _bool]
                                          -> (ret-error v)))
(define-mlt* mlt-playlist-close (_fun _mlt-playlist-pointer -> _void))
(define-mlt* mlt-playlist-clear (_fun _mlt-playlist-pointer -> [v : _bool]
                                      -> (ret-error v)))
(define-mlt* mlt-playlist-properties (_fun _mlt-playlist-pointer -> _mlt-properties-pointer))
(define-mlt* mlt-playlist-producer (_fun _mlt-playlist-pointer -> _mlt-producer-pointer))
(define-mlt* mlt-playlist-blank (_fun _mlt-playlist-pointer _mlt-position -> [v : _bool]
                                      -> (ret-error v)))
(define-mlt* mlt-playlist-blank-time (_fun _mlt-playlist-pointer _string -> [v : _bool]
                                           -> (ret-error v)))
(define-mlt* mlt-playlist-blanks-from (_fun _mlt-playlist-pointer _int _int -> _int))
(define-mlt* mlt-playlist-count (_fun _mlt-playlist-pointer -> _int))
(define-mlt* mlt-playlist-mix (_fun _mlt-playlist-pointer _int _int _mlt-transition-pointer
                                    -> [v : _bool]
                                    -> (ret-error v)))
(define-mlt* mlt-playlist-clip-length (_fun _mlt-playlist-pointer _int -> _int))
(define-mlt* mlt-playlist-clip-start (_fun _mlt-playlist-pointer _int -> _int))
(define-mlt* mlt-playlist-clip-is-mix (_fun _mlt-playlist-pointer _int -> _bool))
(define-mlt* mlt-playlist-clip (_fun _mlt-playlist-pointer _mlt-whence _int -> _mlt-position))
(define-mlt* mlt-playlist-get-clip-info (_fun _mlt-playlist-pointer
                                              [res : (_ptr o _mlt-playlist-clip-info)]
                                              _int
                                              -> [v : _bool]
                                              -> (begin
                                                   (ret-error v)
                                                   res)))
(define-mlt* mlt-playlist-service (_fun _mlt-playlist-pointer -> _mlt-service-pointer))

