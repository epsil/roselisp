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
(define (js/strictly-equal?_ x y)
  (js/=== x y))

;;; JavaScript [loose equality][js:loose-equality],
;;; i.e., the [`==`][js:loose-equality-operator] operator.
;;;
;;; [js:loose-equality]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#loose_equality_using
;;; [js:loose-equality-operator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Equality
(define (js/loosely-equal?_ x y)
  (js/== x y))

;;; JavaScript [sameValue][js:same-value] equality.
;;;
;;; [js:same-value]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value_equality_using_object.is
(define (js/same-value?_ x y)
  (send Object is x y))

;;; JavaScript [sameValueZero][js:same-value-zero] equality.
;;;
;;; [js:same-value-zero]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value-zero_equality
(define (js/same-value-zero?_ x y)
  (or (js/=== x y)
      (and (js/nan? x)
           (js/nan? y))))

;;; JavaScript's [`typeof`][js:typeof] operator,
;;; as a function.
;;;
;;; [js:typeof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof
(define (js/type-of_ x)
  (js/type-of x))

;;; JavaScript's [`instanceof`][js:instanceof] operator,
;;; as a function.
;;;
;;; [js:instanceof]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof
(define (js/instance-of?_ x y)
  (js/instance-of? x y))

;;; Whether a number is [NaN][js:nan].
;;;
;;; [js:nan]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN
(define (js/nan?_ x y)
  (send Number isNaN x))

;;; Variadic version of JavaScript's `+` operator.
;;;
;;; Performs [addition][js:add] or [string concatenation][js:concat],
;;; depending on the types.
;;;
;;; [js:add]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Unary_plus
;;; [js:concat]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_operators#string_operators
(define (js/plus_ . args)
  (if (zero? (js/length args))
      #u
      (js/reduce args
                 (lambda (acc x)
                   (js/+ acc x)))))

;;; Whether `obj` is a JavaScript function.
(define (js/function?_ obj)
  ;; In JavaScript, every function is a
  ;; [`Function` object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function).
  (js/function-object? obj))

;;; Whether `obj` is a [`Function`][js:Function] object.
;;;
;;; [js:Function]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function
(define (js/function-object?_ obj)
  (is-a? obj Function))

;;; Whether `obj` is of type `"function"`.
(define (js/function-type?_ obj)
  (eq? (type-of obj) "function"))

;;; Whether something is JavaScript's `null`.
(define (js/null?_ obj)
  (eq? obj #n))

;;; Find the index of a list element matching a predicate.
;;;
;;; Like `findf-index`, but returns `-1` rather than `#f`
;;; if there is no match.
(define (js/find-index_ proc seq)
  ;; This construct maps neatly onto
  ;; [`Array.prototype.findIndex()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findIndex).
  (send seq findIndex proc))

;;; JavaScript's [`in`][js:in] operator,
;;; as a function.
;;;
;;; [js:in]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in
(define (js/in_ prop obj)
  (js/in prop obj))

;;; Placeholder function for JavaScript's
;;; [tagged template][js:tagged-template] construct.
;;;
;;; [js:tagged-template]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#tagged_templates
(define (js/tagged-template_ tag . args)
  tag)

;;; Placeholder function for JavaScript's
;;; [`delete`][js:delete] operator.
;;;
;;; [js:delete]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/delete
(define (js/delete_ x)
  ;; This function does nothing by itself, but a call to it
  ;; will be compiled to a `UnaryExpression` ESTree node
  ;; invoking `delete`.
  #u)

;;; Whether something is a JavaScript array.
(define (js/array?_ obj)
  (send Array isArray obj))

;;; Return the last element of a JavaScript array.
(define (js/last_ arr)
  (js/get arr (- (js/length arr) 1)))

;;; Return the length of a JavaScript string or array.
(define (js/length_ arr)
  (get-field length arr))

;;; Return the first element of a JavaScript array.
(define (js/first_ lst)
  (js/get lst 0))

;;; Return the second element of a JavaScript array.
(define (js/second_ lst)
  (js/get lst 1))

;;; Return the third element of a JavaScript array.
(define (js/third_ lst)
  (js/get lst 2))

;;; Return the fourth element of a JavaScript array.
(define (js/fourth_ lst)
  (js/get lst 3))

;;; Return the fifth element of a JavaScript array.
(define (js/fifth_ lst)
  (js/get lst 4))

;;; Return the sixth element of a JavaScript array.
(define (js/sixth_ lst)
  (js/get lst 5))

;;; Return the seventh element of a JavaScript array.
(define (js/seventh_ lst)
  (js/get lst 6))

;;; Return the eight element of a JavaScript array.
(define (js/eighth_ lst)
  (js/get lst 7))

;;; Return the ninth element of a JavaScript array.
(define (js/ninth_ lst)
  (js/get lst 8))

;;; Return the tenth element of a JavaScript array.
(define (js/tenth_ lst)
  (js/get lst 9))

;;; Look up the property `key` in the JavaScript object `obj`.
(define (js/get_ obj key)
  (js/get obj key))

;;; Look up the property `prop` in the JavaScript object `obj`.
(define (js/dot_ obj prop)
  (js/. obj prop))

;;; Look up properties `args` in the JavaScript object `obj`,
;;; using optional chaining.
(define (js/optional-chaining_ obj . args)
  (foldl (lambda (prop obj)
           (js/?. obj prop))
         obj
         args))

;;; Slice a JavaScript array.
(define (js/slice_ arr . args)
  (send/apply arr slice args))

;;; Return the tail of a JavaScript array.
(define (js/rest_ arr)
  (js/slice arr 1))

;;; Reverse the order of a JavaScript array.
;;; Returns a new array.
(define (js/reverse_ arr)
  (send arr reverse))

;;; Take the `n` first elements from
;;; the JavaScript array `arr`.
(define (js/take_ arr n)
  (js/slice arr 0 (- (js/length arr) n)))

;;; Fold up a JavaScript array left to right.
(define (js/reduce_ arr . args)
  (send/apply arr reduce args))

;;; Fold up a JavaScript array right to left.
(define (js/reduce-right_ arr . args)
  (send/apply arr reduceRight args))

;;; Create a JavaScript regular expression.
(define (js/regexp_ input (flags #u))
  (new RegExp input flags))

;;; Whether `obj` is a JavaScript regular expression.
(define (js/regexp?_ obj)
  (is-a? obj RegExp))

;;; Match a string or regular expression against
;;; a JavaScript string.
(define (js/regexp-match_ str pattern)
  (send str match pattern))

;;; Match a string or regular expression against
;;; a JavaScript string and replace the matches
;;; with a given string or replacement pattern.
(define (js/regexp-replace_ str pattern insert)
  (send str replace pattern insert))

;;; JavaScript's [`eval` function][js:eval].
;;;
;;; [js:eval]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval
(define (js/eval_ str)
  (js/eval str))

;;; Create a JavaScript `new` expression.
(define (js/new_ x . args)
  (new/apply x args))

;;; Create a JavaScript `return` statement.
(define (js/return_ (x #u))
  x)

;;; Create a JavaScript `yield` expression.
(define (js/yield_ (x #u))
  x)

(provide
  js/array?_
  js/delete_
  js/dot_
  js/eighth_
  js/eval_
  js/fifth_
  js/find-index_
  js/first_
  js/fourth_
  js/function-object?_
  js/function-type?_
  js/function?_
  js/get_
  js/in_
  js/instance-of?_
  js/last_
  js/length_
  js/loosely-equal?_
  js/nan?_
  js/new_
  js/ninth_
  js/null?_
  js/optional-chaining_
  js/plus_
  js/reduce-right_
  js/reduce_
  js/regexp-match_
  js/regexp-replace_
  js/regexp?_
  js/regexp_
  js/rest_
  js/return_
  js/reverse_
  js/same-value-zero?_
  js/same-value?_
  js/second_
  js/seventh_
  js/sixth_
  js/slice_
  js/strictly-equal?_
  js/tagged-template_
  js/take_
  js/tenth_
  js/third_
  js/type-of_
  js/yield_)
