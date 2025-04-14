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

;;; Whether something is a string.
;;;
;;; Similar to [`string?` in Racket][rkt:stringp] and
;;; [`stringp` in Common Lisp][cl:stringp].
;;;
;;; [rkt:stringp]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string~3f%29%29
;;; [cl:stringp]: http://clhs.lisp.se/Body/f_stgp.htm#stringp
(define (string?_ obj)
  (or (string-primitive? obj)
      (string-object? obj)))

;;; Whether something is a string primitive.
(define (string-primitive?_ obj)
  (eq? (type-of obj) "string"))

;;; Whether something is a string object.
(define (string-object?_ obj)
  (is-a? obj String))

;;; Concatenate one or more strings together.
;;;
;;; Similar to [`string-append` in Racket][rkt:string-append].
;;;
;;; [rkt:string-append]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-append%29%29
(define (string-append_ . args)
  (foldl (lambda (x acc)
           (string-append acc x))
         ""
         args))

;;; Get the character at a particular position in a string.
;;;
;;; Similar to [`string-ref` in Racket][rkt:string-ref].
;;;
;;; [rkt:string-ref]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-ref%29%29
(define (string-ref_ str n)
  (send str charAt n))

;;; Trim whitespace from the beginning and end of a string.
;;;
;;; Similar to [`string-trim` in Racket][rkt:string-trim].
;;;
;;; [rkt:string-trim]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-trim%29%29
(define (string-trim_ str (sep undefined) . options)
  (cond
   (sep
    (define repeat-option
      (plist-get options ':repeat?))
    (define pattern-str
      (string-append
       "("
       (regexp-quote sep)
       ")"
       (if repeat-option
           "+"
           "")))
    (~> str
        (regexp-replace (regexp (string-append "^" pattern-str))
                        _
                        "")
        (regexp-replace (regexp (string-append pattern-str "$"))
                        _
                        "")))
   (else
    (send str trim))))

;;; Repeat a string `n` times.
(define (string-repeat_ str n)
  (send str repeat n))

;;; Join a list of strings, using `sep` as the separator.
;;;
;;; Similar to [`string-join` in Racket][rkt:string-join].
;;;
;;; [rkt:string-join]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-join%29%29
(define (string-join_ lst (sep " "))
  (send lst join sep))

;;; Split a string into a list of strings.
;;;
;;; Similar to [`string-split` in Racket][rkt:string-split].
;;;
;;; [rkt:string-split]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-split%29%29
(define (string-split_ str (sep (regexp "\\s+" "g")))
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
(define (string-upcase_ str)
  (send str toUpperCase))

;;; Convert string to lower case.
;;;
;;; Similar to [`string-downcase` in Racket][rkt:string-downcase].
;;;
;;; [rkt:string-downcase]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-downcase%29%29
(define (string-downcase_ str)
  (send str toLowerCase))

;;; Return a substring of `str`, from `start` to `end`.
;;;
;;; Similar to [`substring` in Racket][rkt:substring].
;;;
;;; [rkt:substring]: https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._substring%29%29
(define (substring_ str start (end undefined))
  (send str substring start end))

;;; Convert a string to a number.
;;;
;;; Similar to [`string->number` in Racket][rkt:string-to-number].
;;;
;;; [rkt:string-to-number]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._string-~3enumber%29%29
(define (string->number_ str)
  (parseFloat str))

;;; Convert a number to a string.
;;;
;;; Similar to [`number->string` in Racket][rkt:number-to-string].
;;;
;;; [rkt:number-to-string]: https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._number-~3estring%29%29
(define (number->string_ n)
  (string-append n ""))

;;; Indent a string by prepending each line with `n` spaces.
(define (indent-string str (n 2) (options (js-obj)))
  (define whitespace-option
    (oget options "whitespace"))
  (define whitespace
    (or whitespace-option " "))
  (define include-empty-lines-option
    (oget options "includeEmptyLines"))
  (define pattern
    (if include-empty-lines-option
        (regexp "^" "gm")
        (regexp "^(?!\s*$)" "gm")))
  (define indentation
    (string-repeat whitespace n))
  (regexp-replace pattern str indentation))

(provide
  (rename-out (number->string_ number-to-string_))
  (rename-out (string->number_ string-to-number_))
  (rename-out (string-append_ string-append))
  (rename-out (string-object?_ string-object-p_))
  (rename-out (string-primitive?_ string-primitive-p_))
  (rename-out (string-replace_ string-replace))
  (rename-out (string?_ string?))
  (rename-out (string?_ stringp))
  (rename-out (string?_ stringp_))
  (rename-out (substring_ substring))
  indent-string
  number->string_
  string-append_
  string-downcase_
  string-join_
  string-object?_
  string-primitive?_
  string-ref_
  string-repeat_
  string-replace_
  string-split_
  string-to-number_
  string-trim_
  string-upcase_
  string?_
  substring_)
