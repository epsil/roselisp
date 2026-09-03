;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Thunks and promises
;;;
;;; Implementation of thunks and promises.
;;;
;;; ## Description
;;;
;;; A thunk is a function of zero arguments. A promise is like a thunk,
;;; but is only evaluated once. (A promise, in this context, is not to
;;; be confused with a JavaScript `Promise`, which is a different
;;; construct.)
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; Make a thunk.
(define-macro (thunk_ &rest body)
  `(lambda ()
     ,@body))

;;; Whether something is a thunk.
(define (thunk?_ x)
  (and (procedure? x)
       (zero? (arity x))))

;;; Make a promise.
(define-macro (delay_ &rest body)
  (let ((sym (gensym "promise-f")))
    `(begin
       (define ,sym
         (thunk
          (cond
           ((get-field forced ,sym)
            (get-field value ,sym))
           (else
            (set-field! forced ,sym #u)
            (set-field! value ,sym (begin ,@body))
            (set-field! forced ,sym #t)
            (get-field value ,sym)))))
       (set-field! value ,sym (ann #u Any))
       (set-field! forced ,sym (ann #f Any))
       (set-field! ftype ,sym "thunk")
       ,sym)))

;;; Make a composable promise.
(define-macro (lazy_ &rest body)
  `(delay
     (define result
       (begin ,@body))
     (when (promise? result)
       (set! result (force result)))
     result))

;;; Whether something is a promise.
(define (promise?_ x)
  (and (js/function-type? x)
       (eq? (get-field ftype (ann x Any))
            "thunk")))

;;; Force a promise.
(define (force_ x)
  ((ann x Any)))

;;; Whether a promise has been forced.
(define (promise-forced?_ x)
  (if (get-field forced x) #t #f))

;;; Whether a promise is running.
(define (promise-running?_ x)
  (undefined? (get-field forced x)))

;;; Map for storing promises in.
;;;
;;; Like [`Map`][js:Map], but stores promised values transparently.
;;;
;;; [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
(define-class PromiseMap (Map)
  (define/public (get x)
    (define val
      (send super get x))
    (cond
     ((promise? val)
      (set! val (force val))
      (send super set x val)
      val)
     (else
      val))))

;;; Promise wrapper, for use within the language implementation
;;; in a way that does not interfere with user-defined promises.
(define-class InternalPromise ()
  (define promise)

  (define/public (constructor promise)
    (set-field! promise this promise))

  (define/public (force)
    (force (get-field promise this))))

(provide
  (rename-out (delay_ delay))
  (rename-out (force_ force))
  (rename-out (lazy_ lazy))
  (rename-out (promise-forced?_ promise-forced?))
  (rename-out (promise-running?_ promise-running?))
  (rename-out (promise?_ promise?))
  (rename-out (thunk?_ thunk))
  (rename-out (thunk_ thunk))
  InternalPromise
  PromiseMap
  delay_
  force_
  lazy_
  promise-forced?_
  promise-running?_
  promise?_
  thunk?_
  thunk_)
