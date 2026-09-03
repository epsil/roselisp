;; SPDX-License-Identifier: MPL-2.0
;;; # Utilities.
;;;
;;; Various utilities.
;;;
;;; ## Description
;;;
;;; A "miscellaneous" category for various utility functions.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(require (only-in "./constants"
                  quote-sym_
                  quasiquote-sym_
                  unquote-sym_
                  unquote-splicing-sym_))
(require (only-in "./rose"
                  syntax?
                  syntax->datum))

;;; Get the value stored under `path` in the map `map`.
(define (map-get map path)
  (define-values (value)
    (map-get-tuple map path))
  value)

;;; Get the value stored under `path` in the map `map`.
;;; Returns a tuple `(value found)`, where `found`
;;; is `#f` if there is no value stored under that path.
(define (map-get-tuple map path)
  (define value map)
  (define found #t)
  (for ((key path))
    (cond
     ((and (map? value)
           (hash-has-key? value key))
      (set! value (hash-ref value key)))
     (else
      (set! value #u)
      (set! found #f)
      (break))))
  (values value found))

;;; Whether there is a value stored under `path` in the map `map`.
(define (map-has? map path)
  (define-values (_ found)
    (map-get-tuple map path))
  found)

;;; Store a value `value` under `path` in the map `map`.
(define (map-set! map path value)
  (define map-constructor
    (get-field constructor map))
  (define map-path
    (drop-right path 1))
  (define map-key
    (last path))
  (define current-map map)
  (for ((key map-path))
    (define current-value
      (hash-ref current-map key))
    (unless (is-a? current-value map-constructor)
      (set! current-value (new map-constructor))
      (hash-set! current-map key current-value))
    (set! current-map current-value))
  (hash-set! current-map map-key value)
  map)

;;; Whether an object is a map.
;;;
;;; An object is regarded as a map if it is an instance of
;;; [`Map`][js:Map], or if it defines `.has()`, `.get()` and `.set()`.
;;;
;;; [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
(define (map? x)
  (or (map-instance? x)
      (map-like? x)))

;;; Whether an object is a `Map` instance.
(define (map-instance? x)
  (hash? x))

;;; Whether an object implements a [`Map`][js:Map]-like
;;; interface of `.has()`, `.get()` and `.set()`.
;;;
;;; [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
(define (map-like? x)
  (and (method? "has" x)
       (method? "get" x)
       (method? "set" x)))

;;; Whether a method is defined on an object.
(define (method? method obj)
  (and (object? obj)
       (procedure? (oget obj method))))

;;; Make a symbol with a name different from the ones in `lst`.
;;; The prefix to use may be specified with `prefix`.
(define (make-unique-symbol (lst '()) (prefix 'x))
  (define result prefix)
  (define name
    (symbol->string result))
  (define i 1)
  (while (memq? result lst)
    (set! result
          (string->symbol
           (string-append
            name
            (number->string i))))
    (set! i (+ i 1)))
  result)

;;; Whether `casing-style` is a casing style that is
;;; appropriate for JavaScript identifiers. Camel case
;;; and snake case can be used in JavaScript, but
;;; kebab case cannot.
(define (valid-js-casing-style? casing-style)
  (memq? casing-style
         '("camelcase"
           "snakecase")))

;;; Transform a string to a valid JavaScript identifier
;;; string, provided an appropriate casing style
;;; (camel case or snake case) is specified in `options`.
;;; The input is assumed to be kebab case.
(define (make-identifier-string str (options (js/obj)))
  (define result str)
  (define case-option
    (or (oget options :case)
        "none"))
  (when (valid-js-casing-style? case-option)
    (set! result
          (make-identifier-string-helper result)))
  (cond
   ((eq? case-option "camelcase")
    (kebab-case->camel-case result))
   ((eq? case-option "snakecase")
    (kebab-case->snake-case result))
   (else
    result)))

;;; Helper function for `make-identifier-string`.
(define (make-identifier-string-helper str)
  (define result
    (~> str
        (regexp-replace (regexp "^\\+$" "g") _ "_add")
        (regexp-replace (regexp "^-$" "g") _ "_sub")
        (regexp-replace (regexp "^\\*$" "g") _ "_mul")
        (regexp-replace (regexp "^/$" "g") _ "_div")
        (regexp-replace (regexp "%" "g") _ "")
        (regexp-replace (regexp "/" "g") _ "-")
        (regexp-replace (regexp ":" "g") _ "-")
        (regexp-replace (regexp "->" "g") _ "-to-")
        (regexp-replace (regexp "\\+" "g") _ "_")
        (regexp-replace (regexp "\\*$" "g") _ "-star")
        (regexp-replace (regexp "\\*" "g") _ "star-")))
  (define contains-multiple-segments
    (regexp-match (regexp "-" "g") result))
  (cond
   (contains-multiple-segments
    (~> result
        (regexp-replace (regexp "\\?" "g") _ "-p")
        (regexp-replace (regexp "!" "g") _ "-x")))
   (else
    (~> result
        (regexp-replace (regexp "\\?" "g") _ "p")
        (regexp-replace (regexp "!" "g") _ "x")))))

;;; Convert an identifier string from kebab case
;;; to camel case.
;;;
;;;    > (camel-case "foo-bar")
;;;    "fooBar"
;;;
;;; See also `kebab-case->snake-case`.
(define (kebab-case->camel-case str)
  (define segments
    (~> str
        (string-split _ "-")
        (filter (lambda (x)
                  (not (eq? x "")))
                _)))
  (cond
   ((= (length segments) 0)
    "")
   ((= (length segments) 1)
    (first segments))
   (else
    (define-values (first-segment . rest-segments)
      segments)
    (string-append
     first-segment
     (~> rest-segments
         (map (lambda (x)
                (string-append
                 (string-upcase (string-ref x 0))
                 (substring x 1)))
              _)
         (string-join _ ""))))))

;;; Convert an identifier string from kebab case
;;; to snake case.
;;;
;;;    > (camel-case "foo-bar")
;;;    "foo_bar"
;;;
;;; See also `kebab-case->camel-case`.
(define (kebab-case->snake-case str)
  (regexp-replace (regexp "-" "g")
                  str
                  "_"))

;;; Whether `exp` is a list whose first element is `tag`.
;;; If `len` is specified, also checks whether the list is
;;; of that length.
;;;
;;; Similar to [`tagged-list?` in
;;; *Structure and Interpreation of Computer
;;; Programs*][sicp:tagged-list-p].
;;;
;;; [sicp:tagged-list-p]: https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/full-text/book/book-Z-H-26.html#%_idx_4290
(define (tagged-list? exp tag (len #u))
  (cond
   ((syntax? exp)
    (tagged-list? (syntax->datum exp) tag len))
   ((pair-or-list? tag)
    (for ((x tag))
      (when (tagged-list? exp x len)
        (return #t)))
    #f)
   ((number? len)
    (and (tagged-list? exp tag)
         (= (length exp) len)))
   (else
    (and (pair-or-list? exp)
         (symbol? (first exp))
         (eq? (first exp) tag)))))

;;; Unwrap a `(quote ...)` expression.
(define (text-of-quotation exp)
  (second exp))

;;; Whether `exp` is a form referencing `f` in `env`.
(define (form? exp f env)
  (cond
   ((syntax? exp)
    (form? (syntax->datum exp) f env))
   (else
    (cond
     ((and (pair-or-list? exp)
           (> (length exp) 0))
      (define op
        (first exp))
      (cond
       ((not (symbol? op))
        #f)
       ;; If the operator is bound to a thunk,
       ;; it is a user-defined binding.
       ((send env has-promise? op)
        #f)
       (else
        (define val
          (send env get op))
        (eq? f val))))
     (else
      #f)))))

;;; Whether `exp` is a `(: ...)` expression.
(define (colon-form? exp)
  (cond
   ((syntax? exp)
    (colon-form? (syntax->datum exp)))
   (else
    (and (pair-or-list? exp)
         (>= (length exp) 3)
         (eq? (second exp) ':)))))


;;; Whether `exp` is a `(quote ...)` expression.
(define (quote? exp)
  (tagged-list? exp quote-sym_))

;;; Whether `exp` is a `(quasiquote ...)` expression.
(define (quasiquote? exp)
  (tagged-list? exp quasiquote-sym_))

;;; Whether `exp` is an `(unquote ...)` expression.
(define (unquote? exp)
  (tagged-list? exp unquote-sym_))

;;; Whether `exp` is an `(unquote-splicing ...)` expression.
(define (unquote-splicing? obj)
  (tagged-list? obj unquote-splicing-sym_))

;;; Convert a `lambda` expression to a `let` expression.
(define (lambda->let lambda-exp args)
  (define params
    (second lambda-exp))
  (define body
    (drop lambda-exp 2))
  (define bindings '())
  (cond
   ((symbol? params)
    (push-right! bindings `(,params ',args)))
   (else
    (for ((i (range 0 (length params))))
      (define param
        (list-ref params i))
      (define name
        (if (pair-or-list? param)
            (first param)
            param))
      (define value
        (if (>= i (length args))
            (if (pair-or-list? param)
                (second param)
                #u)
            `(quote ,(list-ref args i))))
      (define binding
        `(,name ,value))
      (push-right! bindings binding))))
  `(let* ,bindings
     ,@body))

;;; Map a function over a tree.
(define (map-tree f x)
  (cond
   ((pair-or-list? x)
    (map (lambda (x1)
           (map-tree f x1))
         x))
   (else
    (f x))))

;;; Count the number of occurrences in a tree
;;; of elements matching the predicate `f`.
(define (count-tree f x)
  (let ((n 0))
    (map-tree (lambda (x)
                (when (f x)
                  (set! n (+ n 1)))
                x)
              x)
    n))

;;; Wrap a list of expressions in a `(begin ...)` expression.
(define (begin-wrap expressions)
  (cond
   ((not (list? expressions))
    expressions)
   (else
    `(begin ,@expressions))))

;;; Wrap a list of expressions in a `(begin ...)` expression,
;;; but do it smartly: in the case of a single expression,
;;; no wrapping is necessary.
(define (begin-wrap-smart expressions)
  (cond
   ((not (list? expressions))
    expressions)
   ((= (length expressions) 1)
    (first expressions))
   (else
    (begin-wrap expressions))))

;;; Define a generic function.
;;;
;;; Similar to [`defgeneric`][cl:defgeneric] in Common Lisp.
;;;
;;; [cl:defgeneric]: http://clhs.lisp.se/Body/m_defgen.htm
(define (define-generic f)
  (define methods '())
  (define (generic-function . args)
    (define methods
      (get-field methods generic-function))
    (for ((entry methods))
      (define-values (params function-definition)
        entry)
      (when (args-matches-params? args params)
        (return (apply function-definition args))))
    (if f
        (apply f args)
        #u))
  (set-field! methods generic-function methods)
  (set-field! defmethod
              generic-function
              (lambda (arg-list function-definition)
                (define entry
                  (list arg-list function-definition))
                (push! (get-field methods generic-function) entry)
                generic-function))
  generic-function)

;;; Define a method for a generic function.
;;;
;;; Similar to [`defmethod`][cl:defmethod] in Common Lisp.
;;;
;;; [cl:defmethod]: http://clhs.lisp.se/Body/m_defmet.htm
(define (define-method generic-function arglist function-definition)
  (send generic-function defmethod arglist function-definition))

;;; Helper function for `defGeneric`.
(define (args-matches-params? args params)
  (unless (= (length args)
             (length params))
    (return #f))
  (for ((i (range 0 (length params))))
    (define param
      (list-ref params i))
    (define arg
      (list-ref args i))
    (cond
     ((pair-or-list? param)
      (cond
       ((eq? (first param) "eql")
        (define value
          (second param))
        (unless (eq? arg value)
          (return #f)))
       ((eq? (first param) "pred")
        (define pred
          (second param))
        (unless (pred arg)
          (return #f)))))
     ((eq? (type-of param) "string")
      (cond
       ((eq? param "any")
        (continue))
       ((eq? param "array")
        (cond
         ((pair-or-list? arg)
          (continue))
         (else
          (return #f))))
       ((not (eq? (type-of arg) param))
        (return #f))))
     (else
      (unless (is-a? arg param)
        (return #f)))))
  (return #t))

;;; Convert a list expression like `(list ...)`
;;; or `(list* ...)` to a list expression pattern.
(define (list-expression->pattern exp)
  (cond
   ((pair-or-list? exp)
    (cond
     ((tagged-list? exp '(list values))
      (cond
       ((eq? (last exp) '...)
        (define head
          (~> (drop exp 1)
              (drop-right _ 2)))
        (define tail
          (list-ref exp (- (length exp) 2)))
        (list-expression->pattern
         `(list* ,@head ,tail)))
       (else
        (map list-expression->pattern (rest exp)))))
     ((tagged-list? exp 'list*)
      (define head
        (~> (drop exp 1)
            (drop-right _ 1)))
      (define tail
        (last exp))
      (if (= (length head) 0)
          (list-expression->pattern tail)
          `(,@(map list-expression->pattern head)
            .
            ,(list-expression->pattern tail))))
     (else
      #f)))
   (else
    exp)))

;;; Convert a number to a letter.
;;;
;;; Counting starts at zero. `0` corresponds to
;;; `a`, `1` to `b`, etc., unless a different
;;; starting letter is specified with `start`.
(define (number->letter n (start "a"))
  (~> (send start charCodeAt 0)
      (+ _ n)
      (send String fromCharCode _)))

(provide
  (rename-out (map-has? map-has))
  (rename-out (map-set! map-set))
  begin-wrap
  begin-wrap-smart
  colon-form?
  count-tree
  define-generic
  define-method
  form?
  kebab-case->camel-case
  kebab-case->snake-case
  lambda->let
  list-expression->pattern
  make-identifier-string
  make-unique-symbol
  map-get
  map-get-tuple
  map-has?
  map-set!
  map-tree
  number->letter
  quasiquote?
  quote?
  tagged-list?
  text-of-quotation
  unquote-splicing?
  unquote?
  valid-js-casing-style?)
