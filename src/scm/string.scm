;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Strings
;;;
;;; String utilities.
;;;
;;; ## Description
;;;
;;; Various functions for working with strings.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(require (only-in "./env"
                  current-compilation-options))

;;; Whether something is a string.
;;;
;;; Similar to [`string?` in Racket][rkt:stringp] and
;;; [`stringp` in Common Lisp][cl:stringp].
;;;
;;; [rkt:stringp]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string~3f%29%29
;;; [cl:stringp]: http://clhs.lisp.se/Body/f_stgp.htm#stringp
(define (string?_ x)
  (js/string? x))

;;; Compiler macro for `(string? ...)` expressions.
(define-compiler-macro (string?_ x)
  (define-fields (fstringobjects)
    (current-compilation-options))
  (cond
   (fstringobjects
    `(funcall string? ,x))
   (else
    `(js/string-literal? ,x))))

;;; The length of a string.
(define-inline (string-length_ x)
  (js/length x))

;;; Concatenate one or more strings together.
;;;
;;; Similar to [`string-append` in Racket][rkt:string-append].
;;;
;;; [rkt:string-append]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-append%29%29
(define (string-append_ . args)
  (let ((result ""))
    (for ((x args))
      (set! result (js/string-concat result x)))
    result))

;;; Compiler macro for `(string-append ...)` expressions.
(define-compiler-macro (string-append_ &rest args)
  `(js/string-concat ,@args))

;;; Get the character at a particular position in a string.
;;;
;;; Similar to [`string-ref` in Racket][rkt:string-ref].
;;;
;;; [rkt:string-ref]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-ref%29%29
(define-inline (string-ref_ str n)
  (send str charAt n))

;;; Trim whitespace from the beginning and end of a string.
;;;
;;; Similar to [`string-trim` in Racket][rkt:string-trim].
;;;
;;; [rkt:string-trim]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-trim%29%29
(define (string-trim_ str (sep #u) . options)
  (cond
   (sep
    (define repeat-option
      (plist-get options :repeat?))
    (define pattern-str
      (string-append
       "(" (regexp-quote sep) ")"
       (if repeat-option "+" "")))
    (~> str
        (regexp-replace
         (regexp (string-append "^" pattern-str)) _ "")
        (regexp-replace
         (regexp (string-append pattern-str "$")) _ "")))
   (else
    (send str trim))))

;;; Compiler macro for `(string-trim ...)` expressions.
(define-compiler-macro (string-trim_ str &rest args)
  (cond
   ((null? args)
    `(send ,str trim))
   (else
    `(funcall string-trim ,str ,@args))))

;;; Repeat a string `n` times.
(define-inline (string-repeat_ str n)
  (send str repeat n))

;;; Join a list of strings, using `sep` as the separator.
;;;
;;; Similar to [`string-join` in Racket][rkt:string-join].
;;;
;;; [rkt:string-join]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-join%29%29
(define-inline (string-join_ lst (sep " "))
  (send lst join sep))

;;; Split a string into a list of strings.
;;;
;;; Similar to [`string-split` in Racket][rkt:string-split].
;;;
;;; [rkt:string-split]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-split%29%29
(define-inline (string-split_ str (sep (regexp "\\s+" "g")))
  (send str split sep))

;;; Return a copy of `str` where `from` is replaced with `to`.
;;;
;;; Similar to [`string-replace`][rkt:string-replace] in Racket.
;;;
;;; [rkt:string-replace]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-replace%29%29
(define (string-replace_ str from to)
  (send str replace from to))

;;; Convert string to upper case.
;;;
;;; Similar to [`string-upcase` in Racket][rkt:string-upcase].
;;;
;;; [rkt:string-upcase]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-upcase%29%29
(define-inline (string-upcase_ str)
  (send str toUpperCase))

;;; Convert string to lower case.
;;;
;;; Similar to [`string-downcase` in Racket][rkt:string-downcase].
;;;
;;; [rkt:string-downcase]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-downcase%29%29
(define-inline (string-downcase_ str)
  (send str toLowerCase))

;;; Return a substring of `str`, from `start` to `end`.
;;;
;;; Similar to [`substring` in Racket][rkt:substring].
;;;
;;; [rkt:substring]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._substring%29%29
(define (substring_ str start (end #u))
  (send str substring start end))

;;; Compiler macro for `(substring ...)` expressions.
(define-compiler-macro (substring_ str &rest args)
  `(send ,str substring ,@args))

;;; Convert a string to a number.
;;;
;;; Similar to [`string->number` in Racket][rkt:string-to-number].
;;;
;;; [rkt:string-to-number]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._string-~3enumber%29%29
(define-inline (string->number_ str)
  (js/parse-float str))

;;; Convert a number to a string.
;;;
;;; Similar to [`number->string` in Racket][rkt:number-to-string].
;;;
;;; [rkt:number-to-string]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._number-~3estring%29%29
(define-inline (number->string_ n)
  (send n toString))

(provide
  (rename-out (number->string_ number->string))
  (rename-out (string->number_ string->number))
  (rename-out (string-append_ string-append))
  (rename-out (string-replace_ string-replace))
  (rename-out (string?_ string?))
  (rename-out (substring_ substring))
  number->string_
  string-append_
  string-downcase_
  string-join_
  string-length_
  string-ref_
  string-repeat_
  string-replace_
  string-split_
  string-to-number_
  string-trim_
  string-upcase_
  string?_
  substring_)
