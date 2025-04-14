;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Objects
;;;
;;; JavaScript objects.
;;;
;;; ## Description
;;;
;;; Functions for working with JavaScript objects.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; Make a JavaScript object.
;;;
;;; Similar to [`js-obj` in ClojureScript][cljs:js-obj].
;;;
;;; [cljs:js-obj]: https://cljs.github.io/api/cljs.core/#js-obj
(define (js-obj_ . args)
  (define entries '())
  (for ((i (range 0 (array-list-length args) 2)))
    (push-right! entries
                 (list (aget args i)
                       (aget args (+ i 1)))))
  (send Object fromEntries entries))

;;; Whether something is a JavaScript object.
(define (js-obj?_ x)
  ;; This function avoids regarding JavaScript's `null` value as an
  ;; object (even if JavaScript does), because it has no properties;
  ;; and unlike the empty object, attempting to access a property on
  ;; it causes an error to be thrown. This is more trouble than it is
  ;; worth, so only non-`null` object values are considered to be
  ;; proper objects here.
  (and (not (eq? x js/null))
       (js/object-type? x)))

;;; Whether something types as a JavaScript object.
;;;
;;; Note that this includes JavaScript's `null` value.
(define (js-object-type?_ x)
  (eq? (type-of x) "object"))

;;; Combine multiple JavaScript objects into a new JavaScript object.
;;;
;;; Like `append`, but for JavaScript objects.
(define (js-obj-append_ . args)
  (send/apply Object assign (js-obj) args))

;;; Return the keys for a JavaScript object.
;;;
;;; Similar to [`js-keys` in ClojureScript][cljs:js-keys].
;;; [cljs:js-keys]: https://cljs.github.io/api/cljs.core/#js-keys
(define (js-keys_ obj)
  (send Object keys obj))

;;; Look up the property `key` in `obj`.
;;;
;;; Similar to [`object-get` in Racket][rkt:object-get] and
;;; [`oget` in ClojureScript][cljs:oget].
;;;
;;; [rkt:object-get]: https://docs.racket-lang.org/javascript/runtime.html#%28def._%28%28lib._javascript%2Fruntime..rkt%29._object-get%29%29
;;; [cljs:oget]: https://github.com/binaryage/cljs-oops#object-operations
(define (object-ref_ obj key)
  (oget obj key))

;;; Set the property `key` in `obj` to `val`.
;;;
;;; Similar to [`object-set!` in Racket][rkt:object-set] and
;;; [`oset!` in ClojureScript][cljs:oset].
;;;
;;; [rkt:object-set]: https://docs.racket-lang.org/javascript/runtime.html#%28def._%28%28lib._javascript%2Fruntime..rkt%29._object-set%21%29%29
;;; [cljs:oset]: https://github.com/binaryage/cljs-oops#object-operations
(define (object-set!_ obj key val)
  (oset! obj key val))

;;; Return the keys for an object.
;;;
;;; Similar to [`field-names` in Racket][rkt:field-names].
;;;
;;; [rkt:field-names]: https://docs.racket-lang.org/reference/objectutils.html#%28def._%28%28lib._racket%2Fprivate%2Fclass-internal..rkt%29._field-names%29%29
(define (field-names_ obj)
  (js-keys obj))

(provide
  (rename-out (js-obj?_ js-obj-p_))
  (rename-out (js-object-type?_ js-object-type-p_))
  (rename-out (object-ref_ object-get_))
  (rename-out (object-ref_ oget_))
  (rename-out (object-set!_ object-set_))
  (rename-out (object-set!_ oset!_))
  (rename-out (object-set!_ oset_))
  field-names_
  js-keys_
  js-obj-append_
  js-obj?_
  js-obj_
  js-object-type?_
  object-ref_
  object-set!_)
