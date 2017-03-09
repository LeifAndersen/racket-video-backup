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

;; Library to initialize the MLT framework.
;; Alternatively your program can just call `mlt-factory-init` on its own.
;;   If you do this though, do make sure to shut things down with mlt-factory-init

(provide mlt-executor
         register-mlt-close)
(require ffi/unsafe
         "mlt.rkt")

;; Init MLT factory (ONCE PER PROCESS)
(define scheme_register_process_global
  (get-ffi-obj 'scheme_register_process_global #f (_fun _string _pointer -> _pointer)))

(let ([v (scheme_register_process_global "mlt-support-initialized"
                                         (cast 1 _racket _pointer))])
  (unless v
    (void (mlt-factory-init #f))))

;; Close MLT factory on program exit
(void
 (plumber-add-flush!
  (current-plumber) (λ (p)
                      (collect-garbage)
                      (mlt-factory-close))))

;; Set up GC thread for MLT objects
(define mlt-executor (make-will-executor))
(void
 (thread
  (λ ()
    (let loop ()
      (will-execute mlt-executor)
      (loop)))))
;; Register an mlt-object to be closed on garbadge collection
;; For convience, return the mlt-object when done
;; (_mlt-properties -> any) _mlt-properties -> _mlt-properties
(define (register-mlt-close proc v)
  (will-register mlt-executor v (λ (v) (proc v)))
  v)
