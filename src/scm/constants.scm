;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Constants
;;;
;;; Various constants.
;;;
;;; ## Description
;;;
;;; This file defines various constants used by Roselisp.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; Boolean true value, `#t`.
;;;
;;; Similar to [`#t` in Racket][rkt:t] and [`t` in Common Lisp][cl:t].
;;;
;;; [rkt:t]: https://docs.racket-lang.org/reference/reader.html#%28idx._%28gentag._18._%28lib._scribblings%2Freference%2Freference..scrbl%29%29%29
;;; [cl:t]: http://clhs.lisp.se/Body/v_t.htm
(define true_ #t)

;;; Boolean false value, `#f`.
;;;
;;; Similar to [`#f` in Racket][rkt:f] and [`nil` in Common Lisp][cl:nil].
;;;
;;; [rkt:f]: https://docs.racket-lang.org/reference/reader.html#%28idx._%28gentag._21._%28lib._scribblings%2Freference%2Freference..scrbl%29%29%29
;;; [cl:nil]: http://clhs.lisp.se/Body/v_nil.htm
(define false_ #f)

;;; The empty list, `'()`.
;;;
;;; Similar to [`null` in Racket][rkt:null] and
;;; [`nil` in Common Lisp][cl:nil]. However, note that this value
;;; is truthy like in Scheme, and not falsy like in Common Lisp.
;;;
;;; [rkt:null]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._null%29%29
;;; [cl:nil]: http://clhs.lisp.se/Body/v_nil.htm
(define null_ '())

;;; JavaScript's `null` value.
;;;
;;; The special value [`null` in JavaScript][js:null] is one
;;; of JavaScript's primitive values, representing the absence
;;; of a value. It is treated as falsy.
;;;
;;; [js:null]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/null
(define js-null_ js/null)

;;; JavaScript's `undefined` value.
;;;
;;; The special value [`undefined` in JavaScript][js:undefined] is one
;;; of JavaScript's primitive values, representing an undefined value.
;;; It is treated as falsy.
;;;
;;; [js:undefined]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/undefined
(define undefined_ undefined)

;;; Symbol for `'`.
(define quote-sym_
  (string->symbol "quote"))

;;; Symbol for `` ` ``.
(define quasiquote-sym_
  (string->symbol "quasiquote"))

;;; Symbol for `,`.
(define unquote-sym_
  (string->symbol "unquote"))

;;; Symbol for `,@`.
(define unquote-splicing-sym_
  (string->symbol "unquote-splicing"))

;;; Default language to compile to.
(define default-language "JavaScript")

;;; Roselisp package name.
(define package-name "roselisp")

;;; Roselisp version.
(define version "0.0.1")

;;; Roselisp license.
(define license
  'MPL-2.0)

(provide
  default-language
  false_
  js-null_
  license
  null_
  package-name
  quasiquote-sym_
  quote-sym_
  true_
  undefined_
  unquote-splicing-sym_
  unquote-sym_
  version)
