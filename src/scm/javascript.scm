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

;;; JavaScript's [`eval` function][js:eval].
;;;
;;; [js:eval]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval
(define (js/eval_ str)
  (js/eval str))

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

;;; Whether something is JavaScript's `null`.
(define (js/null?_ obj)
  (eq? obj #n))

;;; Whether a number is JavaScript's [NaN][js:nan].
;;;
;;; [js:nan]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN
(define (js/nan?_ x y)
  (send Number isNaN x))

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

;;; JavaScript's [`in`][js:in] operator,
;;; as a function.
;;;
;;; [js:in]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in
(define (js/in_ prop obj)
  (js/op in prop obj))

;;; Make a JavaScript object.
;;;
;;; Similar to [`js-obj` in ClojureScript][cljs:js-obj].
;;;
;;; [cljs:js-obj]: https://cljs.github.io/api/cljs.core/#js-obj
(define (js/obj_ . args)
  (let ((entries '()))
    (for ((i (range 0 (length args) 2)))
      (push-right! entries
                   (list (list-ref args i)
                         (list-ref args (+ i 1)))))
    (send Object fromEntries entries)))

;;; Whether something is a JavaScript object.
(define (js/obj?_ x)
  ;; This function avoids regarding JavaScript's `null` value as an
  ;; object (even if JavaScript does), because it has no properties;
  ;; and unlike the empty object, attempting to access a property on
  ;; it causes an error to be thrown. This is more trouble than it is
  ;; worth, so only non-`null` object values are considered to be
  ;; proper objects here.
  (and (not (js/null? x))
       (js/object-type? x)))

;;; Whether something types as a JavaScript object.
;;;
;;; Note that this includes JavaScript's `null` value.
(define (js/object-type?_ x)
  (eq? (type-of x) "object"))

;;; Combine multiple JavaScript objects into a new JavaScript object.
;;;
;;; Like `append`, but for JavaScript objects.
(define (js/obj-append_ . args)
  (send/apply Object assign (js/obj) args))

;;; Spread a JavaScript object into another.
(define (js/obj-spread_ x)
  x)

;;; Return the keys for a JavaScript object.
;;;
;;; Similar to [`js-keys` in ClojureScript][cljs:js-keys].
;;; [cljs:js-keys]: https://cljs.github.io/api/cljs.core/#js-keys
(define (js/keys_ obj)
  (send Object keys obj))

;;; Variadic version of JavaScript's `+` operator.
;;;
;;; Performs [addition][js:add] or [string concatenation][js:concat],
;;; depending on the types.
;;;
;;; [js:add]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Unary_plus
;;; [js:concat]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_operators#string_operators
(define (js/plus_ . args)
  (cond
   ((zero? (length args))
    #u)
   (else
    (let ((result (first args)))
      (for ((x (rest args)))
        (set! result (js/+ result x)))
      result))))

;;; Return the absolute value of `x`.
(define (js/abs_ x)
  (send Math abs x))

;;; Find the index of a list element matching a predicate.
;;;
;;; Like `findf-index`, but returns `-1` rather than `#f`
;;; if there is no match.
(define (js/find-index_ proc seq)
  ;; This construct maps neatly onto
  ;; [`Array.prototype.findIndex()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findIndex).
  (send seq findIndex proc))

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
(define (js/array?_ x)
  (send Array isArray x))

;;; Return the length of a JavaScript string or array.
(define (js/length_ x)
  (get-field length x))

;;; Look up the property `key` in the JavaScript object `obj`.
(define (js/get_ obj key)
  (js/get obj key))

;;; Look up the property `prop` in the JavaScript object `obj`.
(define (js/dot_ obj prop)
  (js/. obj prop))

;;; Look up properties `args` in the JavaScript object `obj`,
;;; using optional chaining.
(define (js/optional-chaining_ obj . args)
  (let ((result obj))
    (for ((x args))
      (cond
       ((js/in x result)
        (set! result (js/get result x)))
       (else
        (set! result #u)
        (break))))
    result))

;;; Slice a JavaScript array.
(define (js/slice_ arr . args)
  (send/apply arr slice args))

;;; Fold up a JavaScript array left to right.
(define (js/reduce_ arr . args)
  (send/apply arr reduce args))

;;; Fold up a JavaScript array right to left.
(define (js/reduce-right_ arr . args)
  (send/apply arr reduceRight args))

;;; Whether something is a JavaScript string.
(define (js/string?_ x)
  (or (js/string-literal? x)
      (js/string-object? x)))

;;; Whether something is a JavaScript string literal.
(define (js/string-literal?_ x)
  (eq? (type-of x) "string"))

;;; Whether something is a JavaScript string object.
(define (js/string-object?_ x)
  (is-a? x String))

;;; Concatenate two or more JavaScript strings together.
(define (js/string-concat_ . args)
  (let ((result ""))
    (for ((x args))
      (set! result (js/op + result x)))
    result))

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

;;; Create a JavaScript `new` expression.
(define (js/new_ x . args)
  (new/apply x args))

;;; Create a JavaScript `return` statement.
(define (js/return_ (x #u))
  x)

;;; Create a JavaScript `yield` expression.
(define (js/yield_ (x #u))
  x)

;;; Less than comparison.
(define (js/lt_ . args)
  (cond
   ((< (length args) 2)
    #t)
   (else
    (for ((i (range 1 (length args))))
      ;; !(x < y) === (x >= y)
      (when (>= (list-ref args (- i 1))
                (list-ref args i))
        (return #f)))
    #t)))

;;; Less than or equal comparison.
(define (js/lte_ . args)
  (cond
   ((< (length args) 2)
    #t)
   (else
    (for ((i (range 1 (length args))))
      ;; !(x <= y) === (x > y)
      (when (> (list-ref args (- i 1))
               (list-ref args i))
        (return #f)))
    #t)))

;;; Greater than comparison.
(define (js/gt_ . args)
  (cond
   ((< (length args) 2)
    #t)
   (else
    (for ((i (range 1 (length args))))
      ;; !(x > y) === (x <= y)
      (when (<= (list-ref args (- i 1))
                (list-ref args i))
        (return #f)))
    #t)))

;;; Greater than or equal comparison.
(define (js/gte_ . args)
  (cond
   ((< (length args) 2)
    #t)
   (else
    (for ((i (range 1 (length args))))
      ;; !(x >= y) === (x < y)
      (when (< (list-ref args (- i 1))
               (list-ref args i))
        (return #f)))
    #t)))

;;; Modulo operation.
(define (js/mod_ x y)
  (js/% x y))

;;; Logical negation.
(define (js/not_ x)
  (js/! x))

;;; Logical AND.
(define (js/and_ . args)
  (js/op/apply && args :identity #t))

;;; Logical OR.
(define (js/or_ . args)
  (js/op/apply \|\| args :identity #f))

;;; Bitwise NOT.
(define (js/bitwise-not_ x)
  (js/op ~ x))

;;; Bitwise AND.
(define (js/bitwise-and_ . args)
  (js/op/apply & args))

;;; Bitwise OR.
(define (js/bitwise-or_ . args)
  (js/op/apply \| args))

;;; Bitwise XOR.
(define (js/bitwise-xor_ . args)
  (js/op/apply ^ args))

;;; Bitwise left shift.
(define (js/bitwise-shift-left_ . args)
  (js/op/apply << args))

;;; Bitwise right shift.
(define (js/bitwise-shift-right_ . args)
  (js/op/apply >> args))

;;; Bitwise unsigned right shift.
(define (js/unsigned-bitwise-shift-right_ . args)
  (js/op/apply >>> args))

;;; Immediately invoked function expression (IIFE).
(define (js/iife_ f args)
  (apply f args))

(provide
  js/abs_
  js/and_
  js/array?_
  js/bitwise-and_
  js/bitwise-not_
  js/bitwise-or_
  js/bitwise-shift-left_
  js/bitwise-shift-right_
  js/bitwise-xor_
  js/delete_
  js/dot_
  js/eval_
  js/find-index_
  js/function-object?_
  js/function-type?_
  js/function?_
  js/get_
  js/gt_
  js/gte_
  js/iife_
  js/in_
  js/instance-of?_
  js/keys_
  js/length_
  js/loosely-equal?_
  js/lt_
  js/lte_
  js/mod_
  js/nan?_
  js/new_
  js/not_
  js/null?_
  js/obj-append_
  js/obj-spread_
  js/obj?_
  js/obj_
  js/object-type?_
  js/optional-chaining_
  js/or_
  js/plus_
  js/reduce-right_
  js/reduce_
  js/regexp-match_
  js/regexp-replace_
  js/regexp?_
  js/regexp_
  js/return_
  js/same-value-zero?_
  js/same-value?_
  js/slice_
  js/strictly-equal?_
  js/string-concat_
  js/string-literal?_
  js/string-object?_
  js/string?_
  js/tagged-template_
  js/type-of_
  js/unsigned-bitwise-shift-right_
  js/yield_)
