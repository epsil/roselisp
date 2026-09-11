;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Arrays
;;;
;;; Array functions.
;;;
;;; ## Description
;;;
;;; Functions for working with arrays.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; Whether something is an array.
(define-inline (array?_ x)
  (js/array? x))

;;; Return the length of an array.
(define-inline (array-length_ arr)
  (js/length arr))

;;; Copy an array.
(define-inline (array-copy_ arr)
  `(,@arr))

;;; Return the first element of an array.
(define-inline (array-first_ arr)
  (array-ref arr 0))

;;; Return the second element of an array.
(define-inline (array-second_ arr)
  (array-ref arr 1))

;;; Return the third element of an array.
(define-inline (array-third_ arr)
  (array-ref arr 2))

;;; Return the fourth element of an array.
(define-inline (array-fourth_ arr)
  (array-ref arr 3))

;;; Return the fifth element of an array.
(define-inline (array-fifth_ arr)
  (array-ref arr 4))

;;; Return the sixth element of an array.
(define-inline (array-sixth_ arr)
  (array-ref arr 5))

;;; Return the seventh element of an array.
(define-inline (array-seventh_ arr)
  (array-ref arr 6))

;;; Return the eight element of an array.
(define-inline (array-eighth_ arr)
  (array-ref arr 7))

;;; Return the ninth element of an array.
(define-inline (array-ninth_ arr)
  (array-ref arr 8))

;;; Return the tenth element of an array.
(define-inline (array-tenth_ arr)
  (array-ref arr 9))

;;; Return the last element of an array.
(define-inline (array-last_ arr)
  (array-nlast arr 1))

;;; Return the `n`-th element counting from
;;; the end of the array.
(define-inline (array-nlast_ arr n)
  ;; We could have called `array-at` with a negative index,
  ;; but this has better backwards compatibility.
  (array-ref arr (- (array-length arr) n)))

;;; Access the array element indicated by
;;; one or more `indices`.
;;;
;;; Similar to [`array-ref` in Racket][rkt:array-ref],
;;; [`aref` in Common Lisp][cl:aref] and
;;; [`aget` in ClojureScript][cljs:aget].
;;;
;;; [rkt:array-ref]: https://docs.racket-lang.org/array/index.html#%28def._%28%28lib._array%2Fmain..rkt%29._array-ref%29%29
;;; [cl:aref]: http://clhs.lisp.se/Body/f_aref.htm#aref
;;; [cljs:aget]: https://cljs.github.io/api/cljs.core/#aget
(define (array-ref_ arr . indices)
  (define result arr)
  (for ((i indices))
    (set! result (js/get arr i)))
  result)

;;; Compiler macro for `(array-ref ...)` expressions.
(define-compiler-macro (array-ref_ arr &rest indices)
  `(js/get ,arr ,@indices))

;;; Return the `i`-th element of the array.
;;; Accepts negative values, counting back
;;; from the end of the array.
(define-inline (array-at_ arr i)
  (send arr at i))

;;; Set an array position to a given value.
;;; Returns a new array.
(define (array-set_ arr . indices-and-value)
  (define result
    (array-copy arr))
  (cond
   ((> (length indices-and-value) 2)
    (define-values (i . indices-and-value-1)
      indices-and-value)
    (array-set! result
                i
                (apply array-set_
                       (array-ref result i)
                       indices-and-value-1)))
   (else
    (define-values (i val)
      indices-and-value)
    (array-set! result i val)))
  result)

;;; Set the array position indiciated by one or more indices
;;; to a given value.
;;;
;;; Similar to [`array-set!` in Racket][rkt:array-set] and
;;; [`aset` in ClojureScript][cljs:aset].
;;;
;;; [rkt:array-set]: https://docs.racket-lang.org/array/index.html#%28def._%28%28lib._array%2Fmain..rkt%29._array-set%21%29%29
;;; [cljs:aset]: https://cljs.github.io/api/cljs.core/#aset
(define (array-set!_ arr . indices-and-value)
  (define indices
    (drop-right indices-and-value 1))
  (define first-indices
    (drop-right indices 1))
  (define last-index
    (last indices))
  (define value
    (last indices-and-value))
  (define arr1 arr)
  (for ((i first-indices))
    (set! arr1 (array-ref arr1 i)))
  (js/= (array-ref arr1 last-index) value)
  value)

;;; Compiler macro for `(array-set! ...)` expressions.
(define-compiler-macro (array-set!_ arr &rest indices-and-value)
  (define indices
    (drop-right indices-and-value 1))
  (define value
    (last indices-and-value))
  `(js/= (js/get ,arr ,@indices) ,value))

;;; Take the `n` first elements from `arr`.
(define-inline (array-take_ arr n)
  (array-drop-right arr
                    (- (array-length arr) n)))

;;; Return the tail of an array.
(define-inline (array-rest_ arr)
  (array-drop arr 1))

;;; Slice a JavaScript array.
(define-inline (array-slice_ arr . args)
  (send/apply arr slice args))

;;; Return the array obtained by dropping
;;; the first `n` elements from `arr`.
(define (array-drop_ arr n)
  (array-slice arr n))

;;; Compiler macro for `(array-drop ...)` expressions.
(define-compiler-macro (array-drop_ arr n)
  (cond
   ((eq? n 0)
    arr)
   (else
    `(array-slice ,arr ,n))))

;;; Return the array obtained by dropping
;;; the last `n` elements from `arr`.
(define (array-drop-right_ arr n)
  ;; Edge case: `(array-slice arr 0 (- n))` works well most
  ;; of the time, but not when `n` is zero, in which case
  ;; `(array-slice arr 0 0)` returns an empty array (and
  ;; not the full array, as expected). Hence the `or`
  ;; expression.
  (array-slice arr 0 (or (- n) #u)))

;;; Compiler macro for `(array-drop-right ...)` expressions.
(define-compiler-macro (array-drop-right_ arr n)
  (cond
   ((number? n)
    (cond
     ((= n 0)
      arr)
     (else
      `(array-slice ,arr 0 (- ,n)))))
   (else
    `(array-slice ,arr 0 (or (- ,n) #u)))))

;;; Concatenate arrays.
(define (array-concat_ . args)
  (send/apply '() concat args))

;;; Compiler macro for `(array-concat ...)` expressions.
(define-compiler-macro (array-concat_ &rest args)
  (cond
   ((eq? (length args) 0)
    '())
   ((eq? (length args) 1)
    (first args))
   (else
    `(send ,(first args) concat ,@(rest args)))))

;;; Reverse the order of an array.
;;; Returns a new array.
(define-inline (array-reverse_ arr)
  (array-reverse! (array-copy arr)))

;;; Reverse the order of an array.
;;; Returns a new array.
(define-inline (array-reverse!_ arr)
  (send arr reverse))

;;; Pop an element off the beginning of an array.
(define-inline (array-pop-left!_ arr)
  (send arr shift))

;;; Pop an element off the end of an array.
(define-inline (array-pop-right!_ arr)
  (send arr pop))

;;; Push an element onto the beginning of an array.
(define (array-push-left!_ arr x)
  (send arr unshift x)
  arr)

;;; Compiler macro for `(array-push-left! ...)` expressions.
(define-compiler-macro (array-push-left!_ arr x)
  `(js/statement-or-expression
    :statement (send ,arr unshift ,x)
    :expression ,(once-only*
                  (arr)
                  `(begin
                     (send ,arr unshift ,x)
                     ,arr))))

;;; Push an element onto the end of an array.
(define (array-push-right!_ arr x)
  (send arr push x)
  arr)

;;; Compiler macro for `(array-push-right! ...)` expressions.
(define-compiler-macro (array-push-right!_ arr x)
  `(js/statement-or-expression
    :statement (send ,arr push ,x)
    :expression ,(once-only*
                  (arr)
                  `(begin
                     (send ,arr push ,x)
                     ,arr))))

(provide
  (rename-out (array-ref_ aget))
  (rename-out (array-ref_ aget_))
  (rename-out (array-ref_ aref))
  (rename-out (array-set_ array-set))
  (rename-out (array-set_ aset))
  (rename-out (array-set_ aset_))
  array-at_
  array-concat_
  array-copy_
  array-drop-right_
  array-drop_
  array-eighth_
  array-fifth_
  array-first_
  array-fourth_
  array-last_
  array-length_
  array-ninth_
  array-pop-left!_
  array-pop-right!_
  array-push-left!_
  array-push-right!_
  array-ref_
  array-rest_
  array-reverse!_
  array-reverse_
  array-second_
  array-set!_
  array-set_
  array-seventh_
  array-sixth_
  array-slice_
  array-take_
  array-tenth_
  array-nlast_
  array-third_
  array?_)
