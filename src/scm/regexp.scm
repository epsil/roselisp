;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Regular expressions
;;;
;;; Regular expression utilities.
;;;
;;; ## Description
;;;
;;; For the sake of simplicity, all regexps are JavaScript regexps,
;;; which is to say that `(regexp ...)` behaves the same as
;;; `(js/regexp ...)`. (One may look into writing a `rkt/regexp` macro
;;; that translates Racket regexps to JavaScript regexps.)
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; Convert `input` to a [regular expression][mdn:Regular Expressions] object.
;;;
;;; [mdn:Regular Expressions]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
(define (regexp_ input (flags undefined))
  (if (string? input)
      (new RegExp input flags)
      input))

;;; Whether `obj` is a regular expression.
(define (regexp?_ obj)
  (is-a? obj RegExp))

;;; Make a regexp string suitable for matching the given string.
;;; Pass the regexp string to `regexp` to make a regular expression.
;;;
;;; Similar to [`regexp-quote` in Racket][rkt:regexp-quote].
;;;
;;; [rkt:regexp-quote]: https://docs.racket-lang.org/reference/regexp.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._regexp-quote%29%29
(define (regexp-quote_ str)
  ;; Based on `escapeRegExp()` from
  ;; <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#escaping>.
  (regexp-replace (regexp "[.*+?^${}()|[\\]\\\\]" "g")
                  str
                  "\\$&"))

;;; Match `pattern` against `input`.
;;;
;;; `pattern` is a [regular expression][mdn:Regular Expressions]
;;; object.
;;;
;;; This function is implemented in terms of
;;; [`String.prototype.match()`][mdn:String.prototype.match] and is
;;; similar to [`regexp-match`][rkt:regexp-match] in Racket.
;;;
;;; [mdn:Regular Expressions]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
;;; [mdn:String.prototype.match]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/match
;;; [rkt:regexp-match]: https://docs.racket-lang.org/reference/regexp.html#%28def._%28%28quote._~23~25kernel%29._regexp-match%29%29
(define (regexp-match_ pattern input)
  (send input match pattern))

(define (regexp-match?_ pattern input)
  (not (eq? (regexp-match pattern input)
            js/null)))

;;; Match `pattern` against `input` and replace with `insert`.
;;; `pattern` is a [regular expression][mdn:Regular Expressions]
;;; object. This function is implemented in terms of
;;; [`String.prototype.replace()`][mdn:String.prototype.replace] and is
;;; similar to [`regexp-replace`][rkt:regexp-replace] in Racket.
;;;
;;; [mdn:Regular Expressions]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
;;; [mdn:String.prototype.replace]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
;;; [rkt:regexp-replace]: https://docs.racket-lang.org/reference/regexp.html#%28def._%28%28quote._~23~25kernel%29._regexp-replace%29%29
(define (regexp-replace_ pattern input insert)
  (send input replace pattern insert))

(provide
  (rename-out (regexp-match?_ js-regexp-match-p_))
  (rename-out (regexp-match?_ regexp-match-p_))
  (rename-out (regexp-match_ js-regexp-match_))
  (rename-out (regexp-quote_ js-regexp-quote_))
  (rename-out (regexp-replace_ js-regexp-replace_))
  (rename-out (regexp?_ js-regexp-p_))
  (rename-out (regexp?_ regexp-p_))
  (rename-out (regexp_ js-regexp_))
  regexp-match?_
  regexp-match_
  regexp-quote_
  regexp-replace_
  regexp?_
  regexp_)
