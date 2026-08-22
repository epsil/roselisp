;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Equality
;;;
;;; Equality algorithms.
;;;
;;; ## Description
;;;
;;; This file provides various functions for checking if two values
;;; are equal.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; Strict equality.
;;;
;;; Similar to [`eq?` in Racket][rkt:eqp] and
;;; [`eq` in Common Lisp][cl:eq].
;;;
;;; [rkt:eqp]: https://docs.racket-lang.org/reference/Equality.html#%28def._%28%28quote._~23~25kernel%29._eq~3f%29%29
;;; [cl:eq]: http://clhs.lisp.se/Body/f_eq.htm#eq
(define (eq?_ x y)
  (js/=== x y))

;;; Loose equality.
;;;
;;; Similar to [`eqv?` in Racket][rkt:eqvp] and
;;; [`eql` in Common Lisp][cl:eql].
;;;
;;; [rkt:eqvp]: https://docs.racket-lang.org/reference/Equality.html#%28def._%28%28quote._~23~25kernel%29._eqv~3f%29%29
;;; [cl:eql]: http://clhs.lisp.se/Body/f_eql.htm#eql
(define (eqv?_ x y)
  (js/same-value? x y))

;;; Structural equality.
;;;
;;; Similar to [`equal?` in Racket][rkt:equalp] and
;;; [`equal` in Common Lisp][cl:equal].
;;;
;;; [rkt:equalp]: https://docs.racket-lang.org/reference/Equality.html#%28def._%28%28quote._~23~25kernel%29._equal~3f%29%29
;;; [cl:equal]: http://clhs.lisp.se/Body/f_equal.htm#equal
(define (equal?_ x y)
  (cond
   ;; Compare equivalent values.
   ((eq? x y)
    #t)
   ;; Compare dotted lists.
   ((and (dotted-list? x)
         (pair-or-list? y))
    (define cdr-x
      (dotted-list-cdr x))
    (cond
     ((and (dotted-list? x)
           (= (length x) 3)
           (not (pair-or-list? cdr-x))
           (not (dotted-list? cdr-x)))
      #f)
     ((equal?_ (car x) (car y))
      (equal?_ cdr-x (cdr y)))
     (else
      #f)))
   ((and (pair-or-list? x)
         (dotted-list? y))
    (equal?_ y x))
   ;; Compare lists.
   ((and (pair-or-list? x)
         (pair-or-list? y))
    (unless (= (length x)
               (length y))
      (return #f))
    (for ((i (range 0 (length x))))
      (unless (equal?_ (aget x i)
                       (aget y i))
        (return #f)))
    #t)
   ;; Compare hash maps.
   ((and (hash? x)
         (hash? y))
    (unless (= (hash-size x)
               (hash-size y))
      (return #f))
    (for ((entry (send x entries)))
      (define-values (key1 value1)
        entry)
      (define value2
        (send y get key1))
      (unless (equal?_ value1 value2)
        (return #f)))
    #t)
   ;; Compare objects.
   ((and (object? x)
         (object? y))
    (unless (= (length (js/keys x))
               (length (js/keys y)))
      (return #f))
    (for ((key (js/keys x)))
      (unless (equal?_ (oget x key)
                       (oget y key))
        (return #f)))
    #t)
   (else
    #f)))

(provide
  (rename-out (eq?_ eq))
  (rename-out (eq?_ eq?))
  (rename-out (eq?_ eq_))
  (rename-out (equal?_ equal?))
  (rename-out (equal?_ equal_))
  (rename-out (eqv?_ eql))
  (rename-out (eqv?_ eql?))
  (rename-out (eqv?_ eql?_))
  (rename-out (eqv?_ eql_))
  (rename-out (eqv?_ eqv))
  (rename-out (eqv?_ eqv?))
  (rename-out (eqv?_ eqv_))
  eq?_
  equal?_
  eqv?_)
