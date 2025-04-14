;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Symbols
;;;
;;; Symbol implementation.
;;;
;;; ## Description
;;;
;;; This file maps Roselisp's "symbol" concept onto JavaScript's
;;; [`Symbol`][js:Symbol] construct. It provides utilities for
;;; working with symbols.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;;;
;;; [js:Symbol]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol

;;; Whether something is a symbol.
;;;
;;; Similar to [`symbol?` in Racket][rkt:symbolp] and
;;; [`symbolp` in Common Lisp][cl:symbolp].
;;;
;;; [rkt:symbolp]: https://docs.racket-lang.org/reference/symbols.html#%28def._%28%28quote._~23~25kernel%29._symbol~3f%29%29
;;; [cl:symbolp]: http://clhs.lisp.se/Body/f_symbol.htm#symbolp
(define (symbol?_ obj)
  (eq? (type-of obj) "symbol"))

;;; Convert a symbol to a string.
;;;
;;; Similar to [`symbol->string` in Racket][rkt:symbol-to-string]
;;; and [`symbol-name` in Common Lisp][cl:symbol-name].
;;;
;;; [rkt:symbol-to-string]: https://docs.racket-lang.org/reference/symbols.html#%28def._%28%28quote._~23~25kernel%29._symbol-~3estring%29%29
;;; [cl:symbol-name]: http://clhs.lisp.se/Body/f_symb_2.htm#symbol-name
(define (symbol->string_ sym)
  (ann (get-field description sym)
       String))

;;; Convert a string to a symbol.
;;;
;;; Similar to [`string->symbol` in Racket][rkt:string-to-symbol]
;;; and [`intern` in Common Lisp][cl:intern].
;;;
;;; [rkt:string-to-symbol]: https://docs.racket-lang.org/reference/symbols.html#%28def._%28%28quote._~23~25kernel%29._string-~3esymbol%29%29
;;; [cl:intern]: http://clhs.lisp.se/Body/f_intern.htm#intern
(define (string->symbol_ str)
  ;; `Symbol.for()` returns the same symbol for a given string,
  ;; similar to `string->symbol`.
  (send Symbol for str))

;;; Create a unique symbol.
;;;
;;; Similar to [`gensym` in Racket][rkt:gensym] and
;;; [`gensym` in Common Lisp][cl:gensym].
;;;
;;; [rkt:gensym]: https://docs.racket-lang.org/reference/symbols.html#%28def._%28%28quote._~23~25kernel%29._gensym%29%29
;;; [cl:gensym]: http://clhs.lisp.se/Body/f_gensym.htm#gensym
(define (gensym_ str)
  ;; `Symbol()` returns a unique symbol for any string,
  ;; similar to `gensym`.
  (Symbol str))

;;; Whether something is a unique symbol.
(define (gensym?_ obj)
  ;; It is a unique symbol if it is a symbol that is different
  ;; from the one returned by `string->symbol`.
  (and (symbol? obj)
       (not (eq? obj
                 (string->symbol
                  (symbol->string obj))))))

(provide
  (rename-out (string->symbol_ intern_))
  gensym?_
  gensym_
  string->symbol_
  symbol->string_
  symbol?_)
