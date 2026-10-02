;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Currying
;;;
;;; Currying and partial application.
;;;
;;; ## Description
;;;
;;; [Ramda][r:curry]-compatible implementation of currying and partial
;;; application of functions.
;;;
;;; This file defines a placeholder value, [`__`][r:dash], which is
;;; compatible with Ramda.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;;;
;;; [r:curry]: https://ramdajs.com/docs/#curry
;;; [r:dash]: https://ramdajs.com/docs/#__

;;; Ramda-compatible placeholder value.
;;;
;;; See [`R.__`][r:dash] in Ramda.
;;;
;;; [r:dash]: https://ramdajs.com/docs/#__
(define __
  (js/obj "@@functional/placeholder" #t))

;;; Whether a value is the placeholder value, `__`.
(define (placeholder? x (placeholder __))
  (eq? x placeholder))

;;; Creates a function that accepts arguments of `f` and either invokes `f`
;;; returning its result, if at least `arity` number of arguments have been
;;; provided, or returns a function that accepts the remaining `f`
;;; arguments, and so on. The arity of `f` may be specified if `f.length`
;;; is not sufficient.
;;;
;;; Loosely based on [`curry` from Ramda][r:curry].
;;;
;;; [r:curry]: https://ramdajs.com/docs/#curry
(define (curry_ f (ar (arity f)))
  (curry-n_ ar f))

;;; Make a curried function. `f` is the function to call, `arity` is
;;; the arity of the function, and `placeholder` is a placeholder value
;;; like {@link __ `R.__`}. `received` is used internally and is an
;;; array of the arguments received thus far.
;;;
;;; Loosely based on [`curryN` from Ramda][r:curryn].
;;;
;;; [r:curryn]: https://ramdajs.com/docs/#curryN
(define (curry-n_ arity f (received '()))
  (define (curried-f . args)
    (cond
     ((= (length args) 0)
      curried-f)
     (else
      (define args-idx 0)
      (define left arity)
      (define combined '())
      (define combined-idx 0)
      (define result)
      (while (or (< combined-idx (length received))
                 (< args-idx (length args)))
        (cond
         ((and (< combined-idx (length received))
               (or (not (eq? (list-ref received combined-idx)
                             __))
                   (>= args-idx (length args))))
          (set! result (list-ref received combined-idx)))
         (else
          (set! result (list-ref args args-idx))
          (set! args-idx (+ args-idx 1))))
        (list-set! combined combined-idx result)
        (unless (eq? result __)
          (set! left (- left 1)))
        (set! combined-idx (+ combined-idx 1)))
      (cond
       ((<= left 0)
        (apply f combined))
       (else
        (curry-n_ arity f combined))))))
  curried-f)

;;; Add support for partial application with
;;; a placeholder value like {@link __ `R.__`}.
(define (dashify_ f (placeholder __))
  ;; `g` is a wrapper around `f` that adds support for
  ;; the placeholder value.
  (define (g . args)
    (define indices '())
    (define complete-args
      (list-copy args))
    (define arg)
    (for ((i (range 0 (length args))))
      (set! arg (list-ref args i))
      (when (eq? arg placeholder)
        (push-right! indices i)))
    (cond
     ((= (length indices) 0)
      (apply f args))
     (else
      ;; `h` is a function that receives remaining arguments.
      ;; When all arguments have been received, it invokes `f`.
      (define (h . remaining-args)
        (for ((i (range 0 (length remaining-args))))
          (cond
           ((= (length indices) 0)
            (break))
           ((eq? (list-ref remaining-args i) placeholder)
            (continue))
           (else
            (define j
              (pop! indices))
            (list-set! complete-args
                       j
                       (list-ref remaining-args i)))))
        (cond
         ((= (length indices) 0)
          (apply f complete-args))
         (else
          h)))
      h)))
  g)

(provide
  (rename-out (__ _))
  (rename-out (__ placeholder))
  (rename-out (curry-n_ curry-n))
  (rename-out (curry_ curry))
  (rename-out (dashify_ dashify))
  (rename-out (placeholder? is-placeholder))
  __
  curry-n_
  curry_
  dashify_
  placeholder?)
