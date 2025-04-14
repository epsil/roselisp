;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Procedures
;;;
;;; Various procedures.
;;;
;;; ## Description
;;;
;;; This file defines various procedures that are part of the language
;;; environment.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; Call `f` with `args`, using the last arg as a list of args.
;;; Returns the value `f` returns.
;;;
;;; Similar to [`apply` in Racket][rkt:apply] and
;;; [`apply` in Common Lisp][cl:apply].
;;;
;;; [rkt:apply]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._apply%29%29
;;; [cl:apply]: http://clhs.lisp.se/Body/f_apply.htm#apply
(define (apply_ f . args)
  (when (> (array-list-length args) 0)
    (set! args
          (append (drop-right args 1)
                  (array-list-last args))))
  (send f apply js/null args))

;;; Call `f` with `args`.
;;; Returns the value `f` returns.
;;;
;;; Similar to [`funcall` in Common Lisp][cl:funcall].
;;;
;;; [cl:funcall]: http://clhs.lisp.se/Body/f_funcal.htm#funcall
(define (funcall_ f . args)
  (send/apply f call js/null args))

;;; Whether `obj` is a procedure (i.e., a function).
;;;
;;; Similar to [`procedure?` in Racket][rkt:procedurep] and
;;; [`functionp` in Common Lisp][cl:functionp].
;;;
;;; [rkt:procedurep]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28quote._~23~25kernel%29._procedure~3f%29%29
;;; [cl:functionp]: http://clhs.lisp.se/Body/f_fnp.htm#functionp
(define (procedure?_ obj)
  (js/function? obj))

;;; Whether `obj` is a fexpr, that is, a procedure that
;;; does not evaluate its arguments.
(define (fexpr?_ obj)
  (and (procedure? obj)
       (get-field fexpr obj)))

;;; Logical negation.
;;;
;;; Similar to [`not` in Racket][rkt:not] and
;;; [`not` in Common Lisp][cl:not].
;;;
;;; [rkt:not]: https://docs.racket-lang.org/reference/booleans.html#%28def._%28%28quote._~23~25kernel%29._not%29%29
;;; [cl:not]: http://clhs.lisp.se/Body/f_not.htm
(define (not_ x)
  (not x))

;;; Map a procedure over a list.
;;;
;;; Similar to [`map` in Racket][rkt:map] and
;;; [`mapcar` in Common Lisp][cl:mapcar].
;;;
;;; [rkt:map]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._map%29%29
;;; [cl:mapcar]: http://clhs.lisp.se/Body/f_mapc_.htm#mapcar
(define (map_ f seq)
  (map f seq))

;;; Less than operator.
;;;
;;; Similar to [`<` in Racket][rkt:lt] and [`<` in Common Lisp][cl:lt].
;;;
;;; [rkt:lt]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._~3c%29%29
;;; [cl:lt]: http://clhs.lisp.se/Body/f_eq_sle.htm#LT
(define (lt_ . args)
  (cond
   ((< (array-list-length args) 2)
    #t)
   (else
    (for ((i (range 1 (array-list-length args))))
      ;; !(x < y) === (x >= y)
      (when (>= (array-list-nth (- i 1) args)
                (array-list-nth i args))
        (return #f)))
    #t)))

;;; Less than or equal operator.
;;;
;;; Similar to [`<=` in Racket][rkt:lte] and [`<=` in Common Lisp][cl:lte].
;;;
;;; [rkt:lte]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._~3c~3d%29%29
;;; [cl:lte]: http://clhs.lisp.se/Body/f_eq_sle.htm#LTEQ
(define (lte_ . args)
  (cond
   ((< (array-list-length args) 2)
    #t)
   (else
    (for ((i (range 1 (array-list-length args))))
      ;; !(x <= y) === (x > y)
      (when (> (array-list-nth (- i 1) args)
               (array-list-nth i args))
        (return #f)))
    #t)))

;;; Greater than operator.
;;;
;;; Similar to [`>` in Racket][rkt:gt] and [`>` in Common Lisp][cl:gt].
;;;
;;; [rkt:gt]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._~3e%29%29
;;; [cl:gt]: http://clhs.lisp.se/Body/f_eq_sle.htm#GT
(define (gt_ . args)
  (cond
   ((< (array-list-length args) 2)
    #t)
   (else
    (for ((i (range 1 (array-list-length args))))
      ;; !(x > y) === (x <= y)
      (when (<= (array-list-nth (- i 1) args)
                (array-list-nth i args))
        (return #f)))
    #t)))

;;; Greater than or equal operator.
;;;
;;; Similar to [`>=` in Racket][rkt:gte] and [`>=` in Common Lisp][cl:gte].
;;;
;;; [rkt:gte]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._~3e~3d%29%29
;;; [cl:gte]: http://clhs.lisp.se/Body/f_eq_sle.htm#GTEQ
(define (gte_ . args)
  (cond
   ((< (array-list-length args) 2)
    #t)
   (else
    (for ((i (range 1 (array-list-length args))))
      ;; !(x >= y) === (x < y)
      (when (< (array-list-nth (- i 1) args)
               (array-list-nth i args))
        (return #f)))
    #t)))

;;; Modulo operation.
;;;
;;; Similar to [`modulo` in Racket][rkt:modulo] and
;;; [`mod` in Common Lisp][cl:mod].
;;;
;;; [rkt:modulo]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._modulo%29%29
;;; [cl:mod]: http://clhs.lisp.se/Body/f_mod_r.htm
(define (modulo_ x y)
  (modulo x y))

;;; Addition.
;;;
;;; Similar to [`+` in Racket][rkt:add] and [`+` in Common Lisp][cl:add].
;;;
;;; [rkt:add]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._%2B%29%29
;;; [cl:add]: http://clhs.lisp.se/Body/f_pl.htm
(define (add_ . args)
  (let ((result 0))
    (for ((arg args))
      (set! result
            (+ result arg)))
    result))

;;; Return `(+ x 1)`.
;;;
;;; Similar to [`add1` in Racket][rkt:add1].
;;;
;;; [rkt:add1]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._add1%29%29
(define (add1_ x)
  (+ x 1))

;;; Subtraction.
;;;
;;; Similar to [`-` in Racket][rkt:sub] and [`-` in Common Lisp][cl:sub].
;;;
;;; [rkt:sub]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._-%29%29
;;; [cl:sub]: http://clhs.lisp.se/Body/f__.htm
(define (sub_ . args)
  (let ((len (array-list-length args)))
    (cond
     ((zero? len)
      0)
     ((one? len)
      (- (first args)))
     (else
      (let ((result (first args)))
        (for ((i (range 1 len)))
          (set! result
                (- result (array-list-nth i args))))
        result)))))

;;; Return `(- x 1)`.
;;;
;;; Similar to [`sub1` in Racket][rkt:sub1].
;;;
;;; [rkt:sub1]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._sub1%29%29
(define (sub1_ x)
  (- x 1))

;;; Multiplication.
;;;
;;; Similar to [`*` in Racket][rkt:mul] and [`*` in Common Lisp][cl:mul].
;;;
;;; [rkt:mul]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._%2A%29%29
;;; [cl:mul]: http://clhs.lisp.se/Body/f_st.htm
(define (mul_ . args)
  (let ((result 1))
    (for ((arg args))
      (set! result
            (* result arg)))
    result))

;;; Division.
;;;
;;; Similar to [`/` in Racket][rkt:div] and [`/` in Common Lisp][cl:div].
;;;
;;; [rkt:div]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._%2F%29%29
;;; [cl:div]: http://clhs.lisp.se/Body/f_sl.htm
(define (div_ . args)
  (cond
   ((= (array-list-length args) 1)
    (/ 1 (first args)))
   (else
    (let ((result (first args)))
      (for ((i (range 1 (array-list-length args))))
        (set! result
              (/ result (array-list-nth i args))))
      result))))

;;; Whether a value is the number zero.
;;;
;;; Similar to [`zerop` in Racket][rkt:zerop] and
;;; [`zerop` in Common Lisp][cl:zerop].
;;;
;;; [rkt:zerop]: https://docs.racket-lang.org/reference/number-types.html#%28def._%28%28quote._~23~25kernel%29._zero~3f%29%29
;;; [cl:zerop]: http://clhs.lisp.se/Body/f_zerop.htm#zerop
(define (zero?_ n)
  (= n 0))

;;; Whether a value is the number one.
(define (one?_ n)
  (= n 1))

;;; Whether a number is odd.
;;;
;;; Similar to [`odd?` in Racket][rkt:oddp] and
;;; [`oddp` in Common Lisp][rkt:oddp].
;;;
;;; [rkt:oddp]: https://docs.racket-lang.org/reference/number-types.html#%28def._%28%28quote._~23~25kernel%29._odd~3f%29%29
;;; [cl:oddp]: http://clhs.lisp.se/Body/f_evenpc.htm#oddp
(define (odd?_ n)
  (not (even? n)))

;;; Whether a number is even.
;;;
;;; Similar to [`even?` in Racket][rkt:evenp] and
;;; [`evenp` in Common Lisp][rkt:evenp].
;;;
;;; [rkt:oddp]: https://docs.racket-lang.org/reference/number-types.html#%28def._%28%28quote._~23~25kernel%29._even~3f%29%29
;;; [cl:oddp]: http://clhs.lisp.se/Body/f_evenpc.htm#evenp
(define (even?_ n)
  (zero? (modulo n 2)))

;;; Whether a value is truthy.
(define (true? x)
  ;; TODO: Remove.
  (and (not (undefined? x))
       x
       (not (and (array? x)
                 (= (array-list-length x) 0)))
       (not (and (= (array-list-length (js-keys x)) 0)
                 (eq? (get-field constructor x) Object)))
       #t))

;;; Whether a value is falsy.
(define (false? x)
  ;; TODO: Remove.
  (not (true? x)))

;;; The identity function.
;;;
;;; Similar to [`identity` in Racket][rkt:identity] and
;;; [`identity` in Common Lisp][cl:identity].
;;;
;;; [rkt:identity]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Ffunction..rkt%29._identity%29%29
;;; [cl:identity]: http://clhs.lisp.se/Body/f_identi.htm#identity
(define (identity_ x)
  x)

;;; Returns a procedure that accepts any arguments and returns `x`.
;;;
;;; Similar to [`const` in Racket][rkt:const] and
;;; [`constantly` in Common Lisp][cl:constantly].
;;;
;;; [rkt:const]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Ffunction..rkt%29._const%29%29
;;; [cl:constantly]: http://clhs.lisp.se/Body/f_cons_1.htm#constantly
(define (const_ (x undefined))
  (lambda args
    x))

;;; Return a tuple of multiple values.
;;;
;;; Similar to [`values` in Racket][rkt:values] and
;;; [`values` in Common Lisp][cl:values].
;;;
;;; [rkt:values]: https://docs.racket-lang.org/reference/values.html#%28def._%28%28quote._~23~25kernel%29._values%29%29
;;; [cl:values]: http://clhs.lisp.se/Body/f_values.htm
(define (values_ . args)
  ;; Implementation-wise, `values` does the same as `list`;
  ;; there is no separate data type for value tuples.
  args)

;;; Whether something is a keyword, i.e., a symbol
;;; whose first character is `:`.
;;;
;;; Similar to [`keyword?` in Racket][rkt:keywordp] and
;;; [`keywordp` in Common Lisp][cl:keywordp].
;;;
;;; [rkt:keywordp]: https://docs.racket-lang.org/reference/keywords.html#%28def._%28%28quote._~23~25kernel%29._keyword~3f%29%29
;;; [cl:keywordp]: http://clhs.lisp.se/Body/f_kwdp.htm#keywordp
(define (keyword?_ obj)
  (and (symbol? obj)
       (regexp-match (regexp "^:")
                     (symbol->string obj))))

;;; Whether something is a number.
;;;
;;; Similar to [`number?` in Racket][rkt:numberp] and
;;; [`numberp` in Common Lisp][cl:numberp].
;;;
;;; [rkt:numberp]: https://docs.racket-lang.org/reference/number-types.html#%28def._%28%28quote._~23~25kernel%29._number~3f%29%29
;;; [cl:numberp]: http://clhs.lisp.se/Body/f_nump.htm#numberp
(define (number?_ obj)
  (send Number isFinite obj))

;;; Whether something is a boolean value.
;;;
;;; Similar to [`boolean?` in Racket][rkt:booleanp] and
;;; [`booleanp` in Emacs Lisp][el:booleanp].
;;;
;;; [rkt:booleanp]: https://docs.racket-lang.org/reference/booleans.html#%28def._%28%28quote._~23~25kernel%29._boolean~3f%29%29
;;; [el:booleanp]: https://www.gnu.org/software/emacs/manual/html_node/elisp/nil-and-t.html#index-booleanp
(define (boolean?_ obj)
  (eq? (type-of obj) "boolean"))

;;; Whether something is the value `undefined`.
(define (undefined?_ obj)
  (eq? obj undefined))

;;; Fold up a list left to right.
;;;
;;; Similar to [`foldl` in Racket][rkt:foldl] and
;;; [`reduce` in Common Lisp][cl:reduce].
;;;
;;; [rkt:foldl]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._foldl%29%29
;;; [cl:reduce]: http://clhs.lisp.se/Body/f_reduce.htm#reduce
(define (foldl_ f v lst)
  (foldl f v lst))

;;; Fold up a list right to left.
;;;
;;; Similar to [`foldr` in Racket][rkt:foldr] and
;;; [`reduce` in Common Lisp][cl:reduce].
;;;
;;; [rkt:foldr]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._foldr%29%29
;;; [cl:reduce]: http://clhs.lisp.se/Body/f_reduce.htm#reduce
(define (foldr_ f v lst)
  (foldr f v lst))

;;; Whether a list contains a value.
;;; Returns a sublist if found, otherwise `#f`.
;;; By default, comparison is done with `equal?`.
;;;
;;; Similar to [`member` in Racket][rkt:member] and
;;; [`member` in Common Lisp][cl:member].
;;;
;;; [rkt:member]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._member%29%29
;;; [cl:member]: http://clhs.lisp.se/Body/f_mem_m.htm
(define (member_ v lst (is-equal undefined))
  (let ((idx (js/find-index
              (if is-equal
                  (lambda (x)
                    (is-equal v x))
                  (lambda (x)
                    (equal? v x)))
              lst)))
    (if (>= idx 0)
        (drop lst idx)
        #f)))

;;; Whether a list contains a value.
;;; Like `member`, but always returns a boolean value.
(define (member?_ v lst (is-equal undefined))
  (memf? (if is-equal
             (lambda (x)
               (is-equal v x))
             (lambda (x)
               (equal? v x)))
         lst))

;;; Whether a list contains a value.
;;; Like `member`, but comparison is done with `eq?`.
;;;
;;; Similar to [`meq` in Racket][rkt:memq] and
;;; [`memq` in Emacs Lisp][el:memq].
;;;
;;; [rkt:memq]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._memq%29%29
;;; [el:memq]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Sets-And-Lists.html#index-memq
(define (memq_ v lst)
  (let ((idx (js/find-index
              (lambda (x)
                (eq? v x))
              lst)))
    (if (>= idx 0)
        (drop lst idx)
        #f)))

;;; Whether a list contains a value,
;;; using `eq?` for comparisons. Like `memq`,
;;; but always returns a boolean value.
(define (memq?_ v lst)
  ;; This construct maps neatly onto
  ;; [`Array.prototype.includes()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes).
  (send lst includes v))

;;; Whether a list contains a value matching a predicate.
;;; Applies the predicate `proc` to elements in the list
;;; and returns a sublist if found, otherwise `#f`.
;;;
;;; Similar to [`memf` in Racket][rkt:memf] and
;;; [`member-if` in Common Lisp][cl:member-if].
;;;
;;; [rkt:memf]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._memf%29%29
;;; [cl:member-if]: http://clhs.lisp.se/Body/f_mem_m.htm#member-if
(define (memf_ proc lst (not-found #f))
  (let ((idx (js/find-index proc lst)))
    (if (>= idx 0)
        (drop lst idx)
        not-found)))

;;; Whether a list contains a value matching a predicate.
;;; Like `memf`, but always returns a boolean value.
(define (memf?_ proc lst)
  (>= (js/find-index proc lst) 0))

;;; Find a list element matching a predicate.
;;;
;;; Similar to [`findf` in Racket][rkt:findf] and
;;; [`find-if` in Common Lisp][cl:find-if].
;;;
;;; [rkt:findf]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._findf%29%29
;;; [cl:find-if]: http://clhs.lisp.se/Body/f_find_.htm#find-if
(define (findf_ proc lst (not-found #f))
  (let ((idx (js/find-index proc lst)))
    (if (>= idx 0)
        (array-list-nth idx lst)
        not-found)))

;;; Find the index of a list element matching a predicate.
;;;
;;; Similar to [`index-where` in Racket][rkt:index-where] and
;;; [`position-if` in Common Lisp][cl:position-if].
;;;
;;; [rkt:index-where]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._index-where%29%29
;;; [cl:position-if]: http://clhs.lisp.se/Body/f_pos_p.htm#position-if
(define (findf-index_ proc seq (not-found #f))
  (let ((idx (js/find-index proc seq)))
    (if (>= idx 0)
        idx
        not-found)))

;;; Find the index of a list element matching a predicate.
;;;
;;; Similar to [`index-where` in Racket][rkt:index-where] and
;;; [`position-if` in Common Lisp][cl:position-if].
;;;
;;; [rkt:index-where]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._index-where%29%29
;;; [cl:position-if]: http://clhs.lisp.se/Body/f_pos_p.htm#position-if
(define (index-where_ seq proc (not-found #f))
  (let ((idx (js/find-index proc seq)))
    (if (>= idx 0)
        idx
        not-found)))

;;; Find the index of a list element.
;;;
;;; Similar to [`index-of` in Racket][rkt:index-of] and
;;; [`position` in Common Lisp][cl:position].
;;;
;;; [rkt:index-of]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._index-of%29%29
;;; [cl:position]: http://clhs.lisp.se/Body/f_pos_p.htm#position
(define (index-of_ seq v (is-equal undefined))
  (index-where seq
               (if is-equal
                   (lambda (x)
                     (is-equal x v))
                   (lambda (x)
                     (equal? x v)))))

;;; Return the intersection of multiple lists.
;;;
;;; Similar to [`intersection` in Common Lisp][cl:intersection].
;;;
;;; [cl:intersection]: http://clhs.lisp.se/Body/f_isec_.htm#intersection
(define (intersection_ . args)
  (define (intersection2 arr1 arr2)
    (let ((result '()))
      (for ((element arr1))
        (when (and (memq? element arr2)
                   (not (memq? element result)))
          (push-right! result element)))
      result))
  (cond
   ((= (array-list-length args) 0)
    '())
   ((= (array-list-length args) 1)
    (first args))
   (else
    (foldl (lambda (x acc)
             (intersection2 acc x))
           (first args)
           (rest args)))))

;;; Return the union of multiple lists.
;;;
;;; Similar to [`union` in Common Lisp][cl:union].
;;;
;;; [cl:union]: http://clhs.lisp.se/Body/f_unionc.htm#union
(define (union_ . args)
  (define (union2 arr1 arr2)
    (let ((result '()))
      (for ((element arr1))
        (unless (memq? element result)
          (push-right! result element)))
      (for ((element arr2))
        (unless (memq? element result)
          (push-right! result element)))
      result))
  (foldl (lambda (x acc)
           (union2 acc x))
         '()
         args))

;;; Produce a list of numbers from `start`, inclusive,
;;; to `end`, exclusive.
;;;
;;; Similar to [`range` in Racket][rkt:range].
;;;
;;; [rkt:range]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._range%29%29
(define (range_ start (end undefined) (step undefined))
  (let* ((start-n (if (eq? end undefined) 0 start))
         (end-n (if (eq? end undefined) start end))
         (step-n (or step 1))
         (result '()))
    (for ((i (range start-n end-n step-n)))
      (push-right! result i))
    result))

;;; Right-to-left function composition.
;;;
;;; Similar to [`compose` in Racket][rkt:compose].
;;;
;;; [rkt:compose]: https://docs.racket-lang.org/reference/procedures.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._compose%29%29
(define (compose_ . args)
  (let ((functions (drop-right args 1))
        (last-function (array-list-last args)))
    (lambda args
      (let ((val (apply last-function args)))
        (foldr (lambda (f x)
                 (f x))
               val
               functions)))))

;;; Left-to-right function composition.
;;;
;;; Like `compose`, but in the other direction.
(define (pipe_ . args)
  (let ((functions (rest args))
        (first-function (first args)))
    (lambda args
      (let ((val (apply first-function args)))
        (foldl (lambda (f x)
                 (f x))
               val
               functions)))))

;;; Filter a list by a predicate.
;;;
;;; Similar to [`filter` in Racket][rkt:filter] and
;;; [`remove-if-not` in Common Lisp][cl:remove-if-not].
;;;
;;; [rkt:filter]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._filter%29%29
;;; [cl:remove-if-not]: http://clhs.lisp.se/Body/f_rm_rm.htm#remove-if-not
(define (filter_ pred lst)
  (send lst filter pred))

;;; Whether a value is self-evaluating.
(define (self-evaluating?_ x)
  (or (boolean? x)
      (number? x)
      (string? x)
      (keyword? x)
      (js/null? x)
      (undefined? x)))

;;; Write an error message to the console
;;; if the assertion is false.
;;;
;;; Similar to [`assert` in Clojure][clj:assert]
;;; and [`assert` in Racket][rkt:assert].
;;;
;;; [clj:assert]: https://clojuredocs.org/clojure.core/assert
;;; [rkt:assert]: https://docs.racket-lang.org/ts-reference/Utilities.html#(def._((lib._typed-racket/base-env/extra-procs..rkt)._assert))
(define (assert_ x . args)
  (send/apply console assert x args))

;;; Output a message to the console.
;;;
;;; Similar to [`display` in Racket][rkt:display] and
;;; [`print` in Common Lisp][cl:print].
;;;
;;; [rkt:display]: https://docs.racket-lang.org/reference/Writing.html#%28def._%28%28quote._~23~25kernel%29._display%29%29
;;; [cl:print]: http://clhs.lisp.se/Body/f_wr_pr.htm#print
(define (display_ . args)
  (send/apply console log args))

;;; Throw an error.
;;;
;;; Similar to [`error` in Racket][rkt:error] and
;;; [`error` in Common Lisp][cl:error].
;;;
;;; [rkt:error]: https://docs.racket-lang.org/reference/exns.html#%28def._%28%28quote._~23~25kernel%29._error%29%29
;;; [cl:error]: http://clhs.lisp.se/Body/f_error.htm
(define (error_ (arg undefined))
  (throw (new Error arg)))

;;; Get the type of a value.
;;;
;;; Similar to [`type-of` in Common Lisp][cl:type-of].
;;;
;;; [cl:type-of]: http://clhs.lisp.se/Body/f_tp_of.htm#type-of
(define (type-of_ x)
  (js/typeof x))

;;; Whether `obj` is an instance of `cls`.
;;;
;;; Similar to [`is-a?` in Racket][rkt:is-a-p].
;;;
;;; [rkt:is-a-p]: https://docs.racket-lang.org/reference/objectutils.html#%28def._%28%28lib._racket%2Fprivate%2Fclass-internal..rkt%29._is-a~3f%29%29
(define (is-a?_ obj cls)
  (js/instanceof? obj cls))

(provide
  (rename-out (add1_ add1))
  (rename-out (add_ _add))
  (rename-out (add_ add))
  (rename-out (add_ plus))
  (rename-out (apply_ apply))
  (rename-out (compose_ compose))
  (rename-out (display_ display))
  (rename-out (div_ _div))
  (rename-out (div_ div))
  (rename-out (error_ error))
  (rename-out (false? false?_))
  (rename-out (fexpr?_ fexprp))
  (rename-out (findf-index_ findf-index))
  (rename-out (findf_ findf))
  (rename-out (foldl_ foldl))
  (rename-out (foldr_ foldr))
  (rename-out (funcall_ funcall))
  (rename-out (gt_ gt))
  (rename-out (gte_ gte))
  (rename-out (intersection_ intersection))
  (rename-out (is-a?_ instance-of))
  (rename-out (is-a?_ instance-of?))
  (rename-out (is-a?_ instance-of?_))
  (rename-out (is-a?_ instance-of_))
  (rename-out (is-a?_ instanceof?))
  (rename-out (is-a?_ is-a?))
  (rename-out (keyword?_ keyword?))
  (rename-out (lt_ lt))
  (rename-out (lte_ lte))
  (rename-out (map_ map))
  (rename-out (map_ mapcar))
  (rename-out (member?_ member-p))
  (rename-out (member?_ member-p_))
  (rename-out (member?_ member?))
  (rename-out (member?_ memberp))
  (rename-out (member?_ memberp_))
  (rename-out (member_ member))
  (rename-out (member_ memq))
  (rename-out (memf_ memf))
  (rename-out (mul_ _mul))
  (rename-out (mul_ mul))
  (rename-out (not_ not))
  (rename-out (number?_ number?))
  (rename-out (pipe_ pipe))
  (rename-out (procedure?_ function?))
  (rename-out (procedure?_ functionp))
  (rename-out (procedure?_ procedure?))
  (rename-out (procedure?_ procedurep))
  (rename-out (range_ range))
  (rename-out (sub1_ sub1))
  (rename-out (sub_ _sub))
  (rename-out (sub_ minus))
  (rename-out (sub_ sub))
  (rename-out (sub_ subtract))
  (rename-out (true? true?_))
  (rename-out (type-of_ type-of))
  (rename-out (union_ union))
  (rename-out (values_ values))
  (rename-out (zero?_ zerop))
  ;; (rename-out (display_ print))
  ;; (rename-out (type-of_ type-of?))
  add1_
  add_
  apply_
  assert_
  boolean?_
  compose_
  const_
  display_
  div_
  error_
  even?_
  false?
  fexpr?_
  filter_
  findf-index_
  findf_
  foldl_
  foldr_
  funcall_
  gt_
  gte_
  identity_
  index-of_
  index-where_
  intersection_
  is-a?_
  keyword?_
  lt_
  lte_
  map_
  member?_
  member_
  memf?_
  memf_
  memq?_
  memq_
  modulo_
  mul_
  not_
  number?_
  odd?_
  one?_
  pipe_
  procedure?_
  range_
  self-evaluating?_
  sub1_
  sub_
  true?
  type-of_
  undefined?_
  union_
  values_
  zero?_)
