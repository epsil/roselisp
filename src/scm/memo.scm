;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Memoization
;;;
;;; Memoization implementation.
;;;
;;; ## Description
;;;
;;; Utilities for [memoizing][w:Memoization] functions.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;;;
;;; [w:Memoization]: https://en.wikipedia.org/wiki/Memoization

(require (only-in "./util"
                  map-get-tuple
                  map-set!))

;;; End-of-sequence marker. Special value used as a key
;;; in the cache map.
;;;
;;; It is implemented as a unique [`Symbol`][js:Symbol] to prevent
;;; collision with cached values. Avoid using this value in code;
;;; it is exported for testing purposes only.
;;;
;;; [js:Symbol]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol
(define eof
  (gensym "eof"))

;;; Make a [memoized][w:Memoization] version of the function `f`.
;;; Functions of any arity may be memoized.
;;;
;;; Returns a function wrapper around `f` that caches the values
;;; produced by `f`. The memoization cache is exposed as the
;;; `cache` property on the memoized function.
;;;
;;; [w:Memoization]: https://en.wikipedia.org/wiki/Memoization
(define (memoize f (cache (make-hash)))
  (define-values (memoized-f)
    (memoize2 f cache))
  memoized-f)

;;; Make a [memoized][w:Memoization] version of the function `f`.
;;;
;;; Returns a tuple `(memoized-f cache)`, where `memoized-f` is
;;; the memoized function and `cache` is the memoization cache.
;;; The memoization cache is also exposed as the `cache` property
;;; on the memoized function.
;;;
;;; It is possible to specify a custom memoization cache with the
;;; optional `cache` argument. This value must be an instance of
;;; [`Map`][js:Map], or an object that implements the same interface as
;;; `Map` (i.e., `.has()`, `.get()` and `.set()`). This is useful if
;;; one wants value equality to be based on something else than the
;;; [SameValueZero][js:SameValueZero] algorithm, which is what `Map`
;;; uses.
;;;
;;; [w:Memoization]: https://en.wikipedia.org/wiki/Memoization
;;; [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
;;; [js:SameValueZero]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value-zero_equality
(define (memoize2 f (cache (make-hash)))
  (define (memoized-f . args)
    (define map
      (get-cache memoized-f))
    (define path
      (make-path args))
    (define-values (value has-value)
      (map-get-tuple map path))
    (cond
     (has-value
      value)
     (else
      (set! value (apply f args))
      (map-set! map path value)
      value)))
  (set-cache! memoized-f cache)
  (values memoized-f cache))

;;; Get the memoization cache of `f`.
(define (get-cache f)
  (get-field cache f))

;;; Set the memoization cache of `f` to `cache`.
;;; Returns `f`.
(define (set-cache! f cache)
  (set-field! cache f cache)
  f)

;;; Return a [memoized][w:Memoization] version of the function `f`,
;;; where the memoization cache is passed as the last argument.
;;;
;;; [w:Memoization]: https://en.wikipedia.org/wiki/Memoization
(define (memoize-with-arg f (arity (get-field length f)))
  (define (memoized-f . args)
    (define map
      (array-list-last args))
    (cond
     ((and (= (array-list-length args) arity)
           (hash? map))
      (define args1
        (drop-right args 1))
      (define path
        (make-path args1))
      (define-values (value has-value)
        (map-get-tuple map path))
      (cond
       (has-value
        value)
       (else
        (set! value (apply f args))
        (map-set! map path value)
        value)))
     (else
      ;; If no memoization cache was passed, just perform
      ;; an unmemoized function call.
      (apply f args))))
  memoized-f)

;;; Make a cache path corresponding to the argument list `args`.
;;;
;;; The cache path is the given path terminated by a special
;;; symbol. This permits paths of any length to be stored in
;;; the same cache map; thus, the implementation can handle
;;; argument lists of any length.
(define (make-path args)
  `(,@args ,eof))

(provide
  eof
  memoize
  memoize2
  memoize-with-arg)
