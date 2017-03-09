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

(require racket/contract/base
         racket/match
         racket/math
         racket/dict
         racket/class
         racket/file
         (prefix-in file: file/convertible)
         (only-in pict pict? pict->bitmap)
         "private/mlt.rkt"
         "private/video.rkt"
         "private/utils.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse))

(provide
 (contract-out
  ;; Render a video object (including the links
  [render (->* [any/c]
               [(or/c path-string? path? #f)
                #:dest-filename (or/c path-string? path? #f)
                #:render-mixin (-> class? class?)
                #:profile-name (or/c string? #f)
                #:width (and/c integer? positive?)
                #:height (and/c integer? positive?)
                #:fps number?
                #:start (or/c nonnegative-integer? #f)
                #:end (or/c nonnegative-integer? #f)
                #:speed (or/c number? #f)
                #:timeout (or/c number? #f)]
               void?)])
 render%
 render<%>)

(define (render video
                [dest #f]
                #:dest-filename [dest-filename #f]
                #:render-mixin [render-mixin values]
                #:profile-name [profile-name #f]
                #:width [width 720]
                #:height [height 576]
                #:start [start #f]
                #:end [end #f]
                #:fps [fps 25]
                #:speed [speed #f]
                #:timeout [timeout #f])
  (define dest* (or dest (make-temporary-file "rktvid~a" 'directory)))
  (define r% (render-mixin render%))
  (define renderer
    (new r%
         [dest-dir dest*]
         [dest-filename dest-filename]
         [width width]
         [height height]
         [fps fps]))
  (let* ([res (send renderer setup-profile)]
         [res (send renderer prepare video)]
         [target (send renderer render res)]
         [res (send renderer play res target start end speed timeout)])
    (void)))

(define render<%>
  (interface () get-profile setup-profile prepare render play))

(define render%
  (class* object% (render<%>)
    (super-new)
    (init-field dest-dir
                [dest-filename #f]
                [prof-name #f]
                [width 720]
                [height 576]
                [fps 25])
    
    (define res-counter 0)
    (define profile (mlt-profile-init prof-name))
    
    (define/private (get-current-filename)
      (void))

    (define/public (get-profile)
      (void))
    
    (define/public (setup-profile)
      (void))
    
    (define/public (prepare source)
      (void))
      
    (define/public (render source)
      (void))
    
    (define/public (play source target start end speed timeout)
      (void))))

;; Set the current renderer
(let ([r (new render% [dest-dir #f])])
  (send r setup-profile)
  (current-renderer r)
  (current-profile (send r get-profile)))
