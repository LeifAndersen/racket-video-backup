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

(provide render%)

(define render%
  (class object%
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
(send (new render% [dest-dir #f]) setup-profile)
