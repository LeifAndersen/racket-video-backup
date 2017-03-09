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

(provide (all-defined-out))
(require racket/dict
         racket/match
         racket/set
         racket/class
         racket/list
         racket/generic
         file/convertible
         (prefix-in file: file/convertible)
         "utils.rkt"
         "mlt.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     racket/function
                     syntax/parse))

(define (register-mlt-close x) x)

(define current-renderer (make-parameter #f))
(define current-profile (make-parameter #f))

;; A helper function to convert videos to MLT object
;; Video (U Renderer% #f) -> _mlt-object
(define (convert source
                 #:renderer [renderer* #f])
  (define renderer (or renderer* (current-renderer)))
  (unless renderer
    (error 'current-renderer "No renderer set"))
  (send renderer prepare source))

;; Helper function to determine if a producer
;; can be potentially unbounded in length.
;; Producer -> Boolean
(define (unbounded-video? prod)
  (cond
    [else #f])) ;; Should only happen in playlists

;; DEBUG FUNCTION ONLY
;; Save a textual marshalization of a property's prop
;;  table to a file.
;; Properties Path -> Void
(define (debug/save-prop prop filepath)
  (mlt-properties-save (convert prop) (if (absolute-path? filepath)
                                          filepath
                                          (build-path (current-directory) filepath))))

;; Calls mlt-*-service on the correct data type
;;    (getting the service type)
;; Service -> _mlt-service
(define (mlt-*-service video-object)
  (cond
    [(link? video-object)
     (mlt-*-service (link-target video-object))]
    [else
     (define video-object* (convert video-object))
     (cond
       [(mlt-filter? video-object*)
        (mlt-filter-service video-object*)]
       [(mlt-playlist? video-object*)
        (mlt-playlist-service video-object*)]
       [(mlt-producer? video-object*)
        (mlt-producer-service video-object*)]
       [else (error 'video "Unsupported video: ~a" video-object)])]))

;; Connect target to source
;; Video-Object _mlt-service Integer -> _mlt-consumer
(define (mlt-*-connect target source-service [index #f])
  (define target* (convert target))
  (cond
    [(filter? target)
     (mlt-filter-connect target*
                         source-service
                         index)]
    [else (error 'video "Unsupported target ~a" target)])
  target*)

(define (finish-mlt-object-init! mlt-object video)
  ;; Set properties
  (when (properties? video)
    (for ([(k v) (in-dict (properties-prop video))])
      (cond
        [(integer? v) (mlt-properties-set-int64 mlt-object k v)]
        [(real? v) (mlt-properties-set-double mlt-object k v)]
        [(string? v) (mlt-properties-set mlt-object k v)]
        [(boolean? v) (mlt-properties-set/bool mlt-object k v)]
        [(anim-property? v)
         (match v
           [(struct* anim-property ([value value]
                                    [position position]
                                    [length length]))
            (cond
              [(string? value)
               (mlt-properties-anim-set mlt-object value position length)]
              [else (error 'video "Anim Property type ~a not currently supported" value)])])]
        [else (error 'video "Property type ~a not currently supported" v)])))
  ;; Attach filters
  (when (service? video)
    (for ([f (in-list (service-filters video))])
      (mlt-service-attach mlt-object (convert f))))
  ;; Optimise if possible
  #;
  (when (producer? video)
    (mlt-producer-optimise (video-mlt-object video))))

;; Dynamic Dispatch for Video Objects
(define-generics video-ops
  (copy-video-op video-ops to-copy))
(define copy-video
  (make-keyword-procedure
   (λ (kws kw-args . args)
     (unless (= 1 (length args))
       (error 'copy-video "copy-video requires exactly one non keyword argument"))
     (copy-video-op (first args)
                    (map cons
                         (map (compose string->symbol keyword->string) kws)
                         kw-args)))))

;; Constructor for video objects
(define-syntax subclass-empty '(() () ()))
(define-syntax (define-constructor stx)
  (syntax-parse stx
    [(_ name:id super* ([ids:id default] ...) body ...)
     #:with constructor (format-id stx "make-~a" #'name)
     #:with this (format-id stx "this")
     #:with new-supers (format-id stx "subclass-~a" #'name)
     #:with super (format-id stx "subclass-~a" (if (identifier? #'super*)
                                                   #'super*
                                                   #'empty))
     #:with predicate (format-id stx "~a?" #'name)
     #:with convert-name (format-id stx "convert-~a" #'name)
     (define super-vals (syntax-local-value #'super))
     (define all-structs (append (first super-vals) (make-list (length (syntax->list #'(ids ...)))
                                                               (syntax->datum #'name))))
     (define all-ids (append (second super-vals) (syntax->datum #'(ids ...))))
     (define all-defaults (append (third super-vals) (syntax-e #'(default ...))))
     (quasisyntax/loc stx
       (begin
         (struct name #,@(if (identifier? #'super*) (list #'super*) '())
           (ids ...)
           #:transparent
           #:methods gen:video-ops
           [(define (copy-video-op v to-copy)
              (name
               #,@(for/list ([i (in-list all-structs)]
                             [j (in-list all-ids)])
                    #`(if (dict-has-key? to-copy '#,j)
                          (dict-ref to-copy '#,j)
                          (#,(format-id stx "~a-~a" i j) v)))))]
           #:property prop:convertible
           (let ([memo-table (make-hasheq)])
             (λ (v request def)
               (match request
                 ['mlt
                  (hash-ref! memo-table v
                             (λ ()
                               (define ret (let ([this v])
                                             (let #,(for/list ([i (in-list all-structs)]
                                                               [j (in-list all-ids)])
                                                      #`[#,(datum->syntax stx j)
                                                         (#,(format-id stx "~a-~a" i j) v)])
                                               #f body ...)))
                               (when ret
                                 (finish-mlt-object-init! ret v))
                               ret))]
                 [_ def]))))
         (define (constructor #,@(append*
                                  (for/list ([i (in-list all-ids)]
                                             [j (in-list all-defaults)])
                                    `(,(datum->syntax stx (string->keyword (symbol->string i)))
                                      [,(datum->syntax stx i) ,j]))))
           (name #,@(map (curry datum->syntax stx) all-ids)))
         (define-syntax new-supers '#,(list all-structs all-ids all-defaults))))]))

;; Structs
(define-constructor video #f ())

(define-constructor link video ([source #f] [target #f] [index 0])
  (mlt-*-connect target (mlt-*-service source) index))

(define-constructor properties video ([prop (hash)]
                                      [prop-default-proc mlt-prop-default-proc]))

(define (get-property dict key
                      [extra-info #f])
  (dict-ref (properties-prop dict) key
            (λ () ((properties-prop-default-proc dict) dict key extra-info))))

(define (mlt-prop-default-proc dict key default-type)
  (define v (convert dict))
  (unless v
    (error 'properties "MLT object for ~a not created, cannot get default property" dict))
  (match default-type
    [(or 'string #f) (mlt-properties-get v key)]
    ['int (mlt-properties-get-int v key)]
    ['int64 (mlt-properties-get-int64 v key)]
    ['mlt-position (mlt-properties-get-position v key)]
    ['double (mlt-properties-get-double v key)]
    [else (error 'properties "Not a valid default-type ~a" default-type)]))

(define-constructor anim-property video ([value #f] [position #f] [length #f]))

(define-constructor frame properties ())

(define-constructor service properties ([filters '()]))

(define-constructor filter service ([type #f] [source #f])
  (define f (mlt-factory-filter (current-profile) type source))
  (register-mlt-close mlt-filter-close f))

(define-constructor transition service ([type #f] [source #f] [length #f])
  (define t (mlt-factory-transition (current-profile) type source))
  (register-mlt-close mlt-transition-close t))
