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

(provide logo)
(require racket/class
         racket/draw
         racket/path
         racket/gui/base
         racket/math
         images/icons/style
         mrlib/switchable-button
         racket/list)

(define height 1000)
(define width (* height 2))
(define film-ratio (/ height 3))
(define film-offset (/ width 15))
(define body-height (+ height film-ratio))
(define lens-dim 2/3)
(define camera (make-object bitmap% width height #f #t))

(let ()
  (define dc (make-object bitmap-dc% camera))
  (send dc set-pen (new pen% [width 0] [color (icon-color->outline-color run-icon-color)]))
  (send dc set-brush (new brush% [color run-icon-color]))

  (define path (new dc-path%))
  (send path move-to 0             0)
  (send path line-to 0             height)
  (send path line-to (* width lens-dim) height)
  (send path line-to (* width lens-dim) (* height 3/4))
  (send path line-to width         (* height 9/10))
  (send path line-to width         (* height 1/10))
  (send path line-to (* width lens-dim) (* height 1/4))
  (send path line-to (* width lens-dim) 0)
  (send path line-to 0             0)
  (send dc draw-path path))

(define camera-body (make-object bitmap% width (ceiling body-height) #f #t))

(let ()
  (define dc (make-object bitmap-dc% camera-body))
  (send dc draw-bitmap camera 0 film-ratio)
  (send dc set-brush (new brush% [color "blue"]))
  (send dc draw-arc film-offset 0 (* 2 film-ratio) (* 2 film-ratio) 0 pi)
  (send dc set-brush (new brush% [color "red"]))
  (send dc draw-arc (* 4 film-offset) 0 (* 2 film-ratio) (* 2 film-ratio) 0 pi))

(define plain-logo (make-object bitmap% width width #f #t))
(let ()
  (define dc (make-object bitmap-dc% plain-logo))
  (send dc draw-bitmap camera-body 0 (/ (- width body-height) 2))
  (void))

(define logo
  (bitmap-render-icon plain-logo))

logo
(send logo save-file "camera.png" 'png 100)
