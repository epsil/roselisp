;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Exceptions
;;;
;;; Exceptions thrown during evaluation.
;;;
;;; ## Description
;;;
;;; These exceptions are thrown by the evaluator. For example, a
;;; `BreakException` is thrown while evaluating a `BreakStatement`
;;; ESTree node, which is what a `(break)` form compiles to.
;;; Similarly, `ContinueException` is thrown when evaluating
;;; `ContinueStatement` and `(continue)`.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; `BreakException`.
;;;
;;; Used for evaluating `BreakStatement` and `(break)`.
(define-class BreakException (Error)
  (define/public (constructor)
    (super "BreakException")))

;;; `ContinueException`.
;;;
;;; Used for evaluating `ContinueStatement` and `(continue)`.
(define-class ContinueException (Error)
  (define/public (constructor)
    (super "ContinueException")))

;;; `YieldException`.
;;;
;;; Used for evaluating `YieldExpression` and `(yield ...)`.
(define-class YieldException (Error)
  (define/public value)

  (define/public (constructor value)
    (super "YieldException")
    (set-field! value this value)))

;;; `ReturnException`.
;;;
;;; Used for evaluating `ReturnStatement` and `(return ...)`.
(define-class ReturnException (Error)
  (define/public value)

  (define/public (constructor value)
    (super "ReturnException")
    (set-field! value this value)))

(provide
  BreakException
  ContinueException
  YieldException
  ReturnException)
