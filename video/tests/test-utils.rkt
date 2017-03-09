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

(require video/base
         video/core
         "../private/utils.rkt")
(require rackunit)
(require (for-syntax racket/base
                     syntax/parse
                     "../private/utils.rkt"))
(provide check-producer check-transition check-filter)

(define-syntax (check-producer stx)
  (syntax-parse stx
    [(_ p (~or (~optional (~seq #:len len))))
     (quasisyntax/loc stx
       (begin
         #,(syntax/loc stx (check-pred producer? p))
         #,@(cond
              [(not (attribute len)) (list)]
              [else
               (list (syntax/loc stx (check-equal? (producer-length p) len)))])))]))

(define-syntax (check-transition stx)
  (syntax-parse stx
    [(_ p) (syntax/loc stx (check-pred transition? p))]))

(define-syntax (check-filter stx)
  (syntax-parse stx
    [(_ p) (syntax/loc stx (check-pred filter? p))]))
