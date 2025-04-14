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
                  Rose))

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
      (set! value undefined)
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
    (array-list-last path))
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
   ((= (array-list-length segments) 0)
    "")
   ((= (array-list-length segments) 1)
    (array-list-first segments))
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
(define (tagged-list? exp tag (len undefined))
  (cond
   ((is-a? exp Rose)
    (tagged-list? (send exp get-value) tag len))
   ((number? len)
    (and (tagged-list? exp tag)
         (= (array-length exp) len)))
   (else
    (and (array? exp)
         (symbol? (first exp))
         (eq? (first exp) tag)))))

;;; Unwrap a `(quote ...)` expression.
(define (text-of-quotation exp)
  (second exp))

;;; Whether `exp` is a form referencing `f` in `env`.
(define (form? exp f env)
  (cond
   ((is-a? exp Rose)
    (form? (send exp get-value) f env))
   (else
    (and (array? exp)
         (> (array-length exp) 0)
         (symbol? (array-first exp))
         (eq? (send env get (array-first exp))
              f)))))

;;; Whether `exp` is a `(: ...)` expression.
(define (colon-form? exp)
  (cond
   ((is-a? exp Rose)
    (colon-form? (send exp get-value)))
   (else
    (and (array? exp)
         (>= (array-list-length exp) 3)
         (eq? (array-second exp) ':)))))


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
    (push-right! bindings
                 `(,params (quote ,args))))
   (else
    (for ((i (range 0 (array-list-length params))))
      (define param
        (aget params i))
      (define name
        (if (array? param)
            (first param)
            param))
      (define value
        (if (>= i (array-list-length args))
            (if (array? param)
                (second param)
                undefined)
            `(quote ,(aget args i))))
      (define binding
        `(,name ,value))
      (push-right! bindings binding))))
  `(let* ,bindings
     ,@body))

;;; Map a function over a tree.
(define (map-tree f x)
  (cond
   ((array? x)
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
   ((= (array-list-length expressions) 1)
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
      (when (args-matches-params args params)
        (return (apply function-definition args))))
    (if f
        (apply f args)
        undefined))
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
(define (args-matches-params args params)
  (unless (= (array-list-length args)
             (array-list-length params))
    (return #f))
  (for ((i (range 0 (array-list-length params))))
    (define param
      (aget params i))
    (define arg
      (aget args i))
    (cond
     ((array? param)
      (cond
       ((eq? (array-first param) "eql")
        (define value
          (array-second param))
        (unless (eq? arg value)
          (return #f)))
       ((eq? (array-first param) "pred")
        (define pred
          (array-second param))
        (unless (pred arg)
          (return #f)))))
     ((eq? (type-of param) "string")
      (cond
       ((eq? param "any")
        (continue))
       ((eq? param "array")
        (cond
         ((array? arg)
          (continue))
         (else
          (return #f))))
       ((not (eq? (type-of arg) param))
        (return #f))))
     (else
      (unless (is-a? arg param)
        (return #f)))))
  (return #t))

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
  make-unique-symbol
  map-get
  map-get-tuple
  map-has?
  map-set!
  map-tree
  quasiquote?
  quote?
  tagged-list?
  text-of-quotation
  unquote-splicing?
  unquote?)
