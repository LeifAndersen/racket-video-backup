#lang racket/base

(provide (all-defined-out))
(require racket/dict
         racket/match
         racket/set
         racket/class
         racket/list
         racket/generic
         file/convertible
         (prefix-in file: file/convertible)
         "mlt.rkt"
         "init-mlt.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     racket/function
                     syntax/parse))

(require racket/contract/base
         racket/runtime-path)

(define-runtime-path video-dir "..")

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

;; DEBUG FUNCTION ONLY
;; Save a textual marshalization of a property's prop
;;  table to a file.
;; Properties Path -> Void
(define (debug/save-prop prop filepath)
  (mlt-properties-save (convert prop) (if (absolute-path? filepath)
                                          filepath
                                          (build-path (current-directory) filepath))))

(define (finish-mlt-object-init! mlt-object video)
  (void))

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

(define-constructor video #f ())

;; Hack because we can't currently guarentee that mlt is installed
(when mlt-lib
  ;; The render module sets a parameter we rely on
  ;;  (Yes, we 'could' do it with units, but requires a large
  ;;   amount of boilerplate.)
  (void (dynamic-require (build-path video-dir "render.rkt") #f)))
