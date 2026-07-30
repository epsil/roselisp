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

(require (only-in "./javascript"
                  js/obj_
                  js/obj?_
                  js/object-type?_
                  js/obj-append_
                  js/keys_))

;;; Look up the property `key` in `obj`.
;;;
;;; Similar to [`object-get` in Racket][rkt:object-get] and
;;; [`oget` in ClojureScript][cljs:oget].
;;;
;;; [rkt:object-get]: https://docs.racket-lang.org/javascript/runtime.html#%28def._%28%28lib._javascript%2Fruntime..rkt%29._object-get%29%29
;;; [cljs:oget]: https://github.com/binaryage/cljs-oops#object-operations
(define (object-ref_ obj key)
  (js/get obj key))

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
  (js/keys obj))

(provide
  (rename-out (object-ref_ object-get_))
  (rename-out (object-ref_ oget_))
  (rename-out (object-set!_ object-set_))
  (rename-out (object-set!_ oset!_))
  (rename-out (object-set!_ oset_))
  field-names_
  js/keys_
  js/obj-append_
  js/obj?_
  js/obj_
  js/object-type?_
  object-ref_
  object-set!_)
