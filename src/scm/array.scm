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
(define (array?_ obj)
  (send Array isArray obj))

;;; Return the last element of an array.
(define (array-last_ arr)
  (aget arr (- (array-length arr) 1)))

;;; Return the length of an array.
(define (array-length_ arr)
  (get-field length arr))

;;; Return the first element of an array.
(define (array-first_ lst)
  (aget lst 0))

;;; Return the second element of an array.
(define (array-second_ lst)
  (aget lst 1))

;;; Return the third element of an array.
(define (array-third_ lst)
  (aget lst 2))

;;; Return the fourth element of an array.
(define (array-fourth_ lst)
  (aget lst 3))

;;; Return the fifth element of an array.
(define (array-fifth_ lst)
  (aget lst 4))

;;; Return the sixth element of an array.
(define (array-sixth_ lst)
  (aget lst 5))

;;; Return the seventh element of an array.
(define (array-seventh_ lst)
  (aget lst 6))

;;; Return the eight element of an array.
(define (array-eighth_ lst)
  (aget lst 7))

;;; Return the ninth element of an array.
(define (array-ninth_ lst)
  (aget lst 8))

;;; Return the tenth element of an array.
(define (array-tenth_ lst)
  (aget lst 9))

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
  (cond
   ((= (array-length indices) 1)
    (aget arr (first indices)))
   (else
    (foldl (lambda (i arr)
             (aget arr i))
           arr
           indices))))

;;; Set the array position indiciated by one or more indices
;;; to a given value.
;;;
;;; Similar to [`array-set!` in Racket][rkt:array-set] and
;;; [`aset` in ClojureScript][cljs:aset].
;;;
;;; [rkt:array-set]: https://docs.racket-lang.org/array/index.html#%28def._%28%28lib._array%2Fmain..rkt%29._array-set%21%29%29
;;; [cljs:aset]: https://cljs.github.io/api/cljs.core/#aset
(define (array-set_ arr . indices-and-value)
  (define value
    (aget indices-and-value
          (- (array-length indices-and-value) 1)))
  (define idx
    (aget indices-and-value
          (- (array-length indices-and-value) 2)))
  (define indices
    (drop-right indices-and-value 2))
  (define arr1
    (apply array-ref_ arr indices))
  (aset! arr1 idx value)
  value)

;;; Return the array obtained by dropping
;;; the first `n` elements from `arr`.
(define (array-drop_ arr n)
  (cond
   ((= n 0)
    arr)
   (else
    (send arr slice n))))

;;; Return the array obtained by dropping
;;; the last `n` elements from `arr`.
(define (array-drop-right_ arr n)
  (cond
   ((= n 0)
    arr)
   (else
    (send arr slice 0 (- n)))))

;;; Return the tail of an array.
(define (array-rest_ arr)
  (array-drop arr 1))

;;; Reverse the order of an array.
;;; Returns a new array list.
(define (array-reverse_ arr)
  (send arr reverse))

;;; Take the `n` first elements from `arr`.
(define (array-take_ arr n)
  (array-drop-right arr
                    (- (array-length arr) n)))

(provide
  (rename-out (array-ref_ aget))
  (rename-out (array-ref_ aget_))
  (rename-out (array-ref_ aref))
  (rename-out (array-set_ array-set))
  (rename-out (array-set_ aset))
  (rename-out (array-set_ aset_))
  array-drop-right_
  array-drop_
  array-eighth_
  array-fifth_
  array-first_
  array-fourth_
  array-last_
  array-length_
  array-ninth_
  array-ref_
  array-rest_
  array-reverse_
  array-second_
  array-set_
  array-seventh_
  array-sixth_
  array-tenth_
  array-take_
  array-third_
  array?_)
