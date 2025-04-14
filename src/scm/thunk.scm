;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Thunks
;;;
;;; Thunk implementation.
;;;
;;; ## Description
;;;
;;; Defines a `Thunk` class for thunks, which can be forced by calling
;;; the `.force()` method. Also provides functions for creating and
;;; forcing thunks.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; Thunk class.
;;;
;;; This class is a wrapper around a function `f` that is called with
;;; zero arguments. The function `f` is passed to the constructor, and
;;; is called only once, when the `.force` method is invoked for the
;;; first time; subsequent invocations return a cached value.
(define-class Thunk ()
  ;;; Thunk function.
  (define f)
  ;;; Whether the thunk has been forced yet.
  (define forced #f)
  ;;; Cached value.
  (define value undefined)

  ;;; Create a new thunk.
  ;;; `f` should be a function of zero arguments.
  (define/public (constructor f)
    (set-field! f this f))

  ;;; Get the value of the thunk.
  ;;; Alias for `.force()`
  (define/public (get-value)
    (send this force))

  ;;; Force the thunk.
  (define/public (force)
    (cond
     ((get-field forced this)
      (get-field value this))
     (else
      (set-field! forced this #t)
      (define f
        (get-field f this))
      (define value
        (f))
      (set-field! value this value)
      value))))

;;; Make a thunk.
;;;
;;; `f` should be a function of zero arguments.
(define (thunk f)
  (new Thunk f))

;;; Whether something is a thunk.
(define (thunk? x)
  (is-a? x Thunk))

;;; Whether something appears to be a thunk.
(define (thunkish? x)
  (and (object? x)
       (procedure? (get-field force x))))

;;; Whether something is a thunk,
;;; or appears to be a thunk.
(define (thunkable? x)
  (or (thunk? x)
      (thunkish? x)))

;;; Force a thunk.
(define (force x)
  (send x force))

;;; Map for storing thunks in.
;;;
;;; Like [`Map`][js:Map], but stores thunked values transparently.
;;;
;;; [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
(define-class ThunkedMap (Map)
  (define/public (get x)
    (define val
      (send super get x))
    (cond
     ((thunk? val)
      (set! val (force val))
      (send super set x val)
      val)
     (else
      val))))

(provide
  (rename-out (thunk delay))
  Thunk
  ThunkedMap
  force
  thunk
  thunk?
  thunkable?
  thunkish?)
