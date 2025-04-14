;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # JavaScript
;;;
;;; Basic JavaScript constructs.
;;;
;;; ## Description
;;;
;;; This file defines functions for various basic
;;; JavaScript constructs.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; JavaScript [strict equality][js:strict-equality],
;;; i.e., the [`===`][js:strict-equality-operator] operator.
;;;
;;; [js:strict-equality]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#strict_equality_using
;;; [js:strict-equality-operator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality
(define (js-is-strictly-equal?_ x y)
  (js/=== x y))

;;; JavaScript [loose equality][js:loose-equality],
;;; i.e., the [`==`][js:loose-equality-operator] operator.
;;;
;;; [js:loose-equality]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#loose_equality_using
;;; [js:loose-equality-operator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Equality
(define (js-is-loosely-equal?_ x y)
  (js/== x y))

;;; JavaScript [sameValue][js:same-value] equality.
;;;
;;; [js:same-value]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value_equality_using_object.is
(define (js-same-value?_ x y)
  (send Object is x y))

;;; JavaScript [sameValueZero][js:same-value-zero] equality.
;;;
;;; [js:same-value-zero]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value-zero_equality
(define (js-same-value-zero?_ x y)
  (or (js/=== x y)
      (and (send Number isNaN x)
           (send Number isNaN y))))

;;; JavaScript's [`typeof`][js:typeof] operator,
;;; as a function.
;;;
;;; [js:typeof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof
(define (js-typeof_ x)
  (js/typeof x))

;;; JavaScript's [`instanceof`][js:instanceof] operator,
;;; as a function.
;;;
;;; [js:instanceof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof
(define (js-instanceof_ x y)
  (js/instanceof x y))

;;; Variadic version of JavaScript's `+` operator.
;;;
;;; Performs [addition][js:add] or [string concatenation][js:concat]
;;; depending on the types.
;;;
;;; [js:add]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Unary_plus
;;; [js:concat]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_operators#string_operators
(define (js-plus_ . args)
  (let ((len (array-length args)))
    (cond
     ((zero? len)
      undefined)
     (else
      (define result
        (array-first args))
      (for ((i (range 1 len)))
        (set! result
              (js/+ result (aget args i))))
      result))))

;;; Whether `obj` is a JavaScript function.
(define (js-function?_ obj)
  ;; In JavaScript, every function is a
  ;; [`Function` object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function).
  (js/function-object? obj))

;;; Whether `obj` is a [`Function`][js:Function] object.
;;;
;;; [js:Function]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function
(define (js-function-object?_ obj)
  (is-a? obj Function))

;;; Whether `obj` is of type `"function"`.
(define (js-function-type?_ obj)
  (eq? (type-of obj) "function"))

;;; Whether something is JavaScript's `null`.
(define (js-null?_ obj)
  (eq? obj js/null))

;;; Find the index of a list element matching a predicate.
;;;
;;; Like `findf-index`, but returns `-1` rather than `#f`
;;; if there is no match.
(define (js-find-index_ proc seq)
  ;; This construct maps neatly onto
  ;; [`Array.prototype.findIndex()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findIndex).
  (send seq findIndex proc))

;;; JavaScript's [`eval` function][js:eval].
;;;
;;; [js:eval]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval
(define (js-eval_ str)
  (js/eval str))

;;; JavaScript's [`in`][js:in] operator,
;;; as a function.
;;;
;;; [js:in]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in
(define (js-in_ prop obj)
  (js/in prop obj))

;;; Placeholder function for JavaScript's
;;; [tagged template][js:tagged-template] construct.
;;;
;;; [js:tagged-template]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#tagged_templates
(define (js-tagged-template_ tag . args)
  tag)

;;; Placeholder function for JavaScript's
;;; [`delete`][js:delete] operator.
;;;
;;; [js:delete]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/delete
(define (js-delete_ x)
  ;; This function does nothing by itself, but a call to it
  ;; will be compiled to a `UnaryExpression` ESTree node
  ;; invoking `delete`.
  undefined)

(provide
  js-delete_
  js-eval_
  js-find-index_
  js-function-object?_
  js-function-type?_
  js-function?_
  js-in_
  js-instanceof_
  js-is-loosely-equal?_
  js-is-strictly-equal?_
  js-null?_
  js-plus_
  js-same-value-zero?_
  js-same-value?_
  js-tagged-template_
  js-typeof_)
