;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Macros
;;;
;;; Macro definitions.
;;;
;;; ## Description
;;;
;;; This file provides macro implementations of some special forms.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(require (only-in "./env"
                  current-environment
                  empty-environment))
(require (only-in "./eval"
                  eval_))
(require (only-in "./list"
                  flatten))
(require (only-in "./rose"
                  Rose
                  make-rose))
(require (only-in "./util"
                  count-tree
                  map-tree
                  tagged-list?))

;;; Expand a `(define/private ...)` expression.
(defmacro define-private_ (&rest body)
  `(define ,@body))

;;; Expand a `(define/public ...)` expression.
(defmacro define-public_ (&rest body)
  `(define ,@body))

;;; Expand a `(defclass ...)` expression.
(defmacro defclass_ (&rest body)
  `(define-class ,@body))

;;; Expand a `(defmacro ...)` expression.
(defmacro defmacro_ (name args &rest body)
  `(define-macro ,(cons name args)
     ,@body))

;;; Expand a `(defun ...)` expression.
(defmacro defun_ (name args &rest body)
  `(define (,name ,@args)
     ,@body))

;;; Expand a `(begin0 ...)` or `(prog1 ...)` expression.
(defmacro begin0_ (x &rest xs)
  (cond
   ((= (array-list-length xs) 0)
    x)
   (else
    (define result
      (gensym "begin0-result"))
    `(let ((,result ,x))
       ,@xs
       ,result))))

;;; Expand a `(multiple-values-bind ...)` expression.
(defmacro multiple-value-bind_ (bindings expression &rest body)
  `(let-values ((,bindings ,expression))
     ,@body))

;;; Expand a `(rkt/new ...)' expression.
(defmacro rkt-new_ (constructor &rest args)
  ;; We are not able to do much here other than to rewrite the
  ;; expression to a `(make-object ...)` expression. JavaScript lacks
  ;; support for creating a new object on the basis of by-name
  ;; initialization arguments; it only supports by-position
  ;; initialization arguments, which are passed to the constructor.
  `(make-object ,constructor ,@(map array-list-second args)))

;;; Expand an `(if ...)` expression.
(defmacro if_ (condition then-clause &rest else-clauses)
  `(cond
    (,condition
     ,then-clause)
    ,@(cond
       ((> (array-list-length else-clauses) 0)
        `((else ,@else-clauses)))
       (else
        '()))))

;;; Expand a `(when ...)` expression.
(defmacro when_ (condition &rest body)
  `(if ,condition
       (begin ,@body)))

;;; Expand an `(unless ...)` expression.
(defmacro unless_ (condition &rest body)
  `(if (not ,condition)
       (begin ,@body)))

;;; Expand an `(as~> ...)` expression.
;;;
;;; Similar to the [`as->` macro][clj:thread-as] in Clojure.
;;;
;;; [clj:thread-as]: https://clojuredocs.org/clojure.core/as-%3E
(defmacro thread-as_ (val sym &rest forms)
  ;; This macro goes to some lengths to avoid introducing a `let`
  ;; variable unless it is absolutely necessary. In many cases, the
  ;; forms can simply be chained together, using `sym` as the
  ;; insertion point. A variable is needed only if `sym` occurs more
  ;; than once in the same form.

  ;; Whether a `let` variable has been defined.
  (define is-let #f)
  ;; Reducer function.
  (define (f form exp)
    (cond
     ;; If we are in the process of creating a `let` expression,
     ;; simply append a `set!` expression to it.
     (is-let
      `(,@exp (set! ,sym ,form)))
     ;; Otherwise, count the occurrences of `sym` in the form
     ;; in order to determine what to do.
     (else
      (define n
        (count-tree (lambda (el)
                      (eq? el sym))
                    form))
      (cond
       ;; If `sym` occurs zero times in the form, create a
       ;; `begin` expression to chain things togethr.
       ((= n 0)
        (cond
         ((tagged-list? exp 'begin)
          ;; If chaining two `begin` expressions together,
          ;; simply append the latter to the former.
          `(,@exp ,form))
         (else
          `(begin
             ,exp
             ,form))))
       ;; If `sym` occurs exactly once in the form, chain it together
       ;; with the preceding expression, using `sym` as the insertion
       ;; point.
       ((= n 1)
        (map-tree (lambda (x)
                    (if (eq? x sym)
                        exp
                        x))
                  form))
       ;; If `sym` occurs more than once in the form, create a
       ;; `let` expression with `sym` as a variable.
       (else
        (set! is-let #t)
        `(let ((,sym ,exp))
           (set! ,sym ,form)))))))
  ;; Fold up `forms` left-to-right.
  (define result
    (foldl f val forms))
  ;; If a `let` expression was indeed created, add `sym` as
  ;; the final expression.
  (when is-let
    (set! result `(,@result ,sym)))
  result)

;;; Evaluate a `(~> ...)` expression. Based on the
;;; [`->` macro][clj:thread-first] in Clojure (also known as
;;; the "`thread-first` macro").
;;;
;;; [clj:thread-first]: https://clojuredocs.org/clojure.core/-%3E
(defmacro thread-first_ (x &rest forms)
  (define hole-marker '_)
  (when (and (> (array-list-length forms) 1)
             (eq? (array-list-first forms) ':hole-marker))
    (set! hole-marker (array-list-second forms))
    (set! forms (drop forms 2)))
  (define (f val acc)
    (cond
     ((symbol? val)
      `(,@acc (,val _)))
     ((= (count-tree (lambda (x)
                       (eq? x hole-marker))
                     val)
         0)
      `(,@acc (,(array-list-first val)
               ,hole-marker
               ,@(rest val))))
     (else
      `(,@acc ,val))))
  (define as-exp
    `(as~> ,x ,hole-marker))
  (foldl f as-exp forms))

;;; Evaluate a `(~>> ...)` expression. Based on the
;;; [`->>` macro][clj:thread-last] in Clojure (also known as
;;; the "`thread-last` macro").
;;;
;;; [clj:thread-last]: https://clojuredocs.org/clojure.core/-%3E%3E
(defmacro thread-last_ (x &rest forms)
  (define hole-marker '_)
  (when (and (> (array-list-length forms) 1)
             (eq? (array-list-first forms) ':hole-marker))
    (set! hole-marker (array-list-second forms))
    (set! forms (drop forms 2)))
  (define (f val acc)
    (cond
     ((symbol? val)
      `(,@acc (,val _)))
     ((= (count-tree (lambda (x)
                       (eq? x hole-marker))
                     val)
         0)
      `(,@acc (,@val ,hole-marker)))
     (else
      `(,@acc ,val))))
  (define as-exp
    `(as~> ,x ,hole-marker))
  (foldl f as-exp forms))

;;; Expand an `(unwind-protect ...)` expression.
(defmacro unwind-protect_ (body-form &rest unwind-forms)
  `(try
     ,body-form
     (finally
       ,@unwind-forms)))

;;; Expand a `(do ...)` expression.
(defmacro do_ (bindings tests &rest body)
  (cond
   ;; For expressions with no bindings, we wrap
   ;; the expansion in `begin`.
   ((= (array-list-length bindings) 0)
    (define result
      `(begin
         (js/while (not ,(array-list-first tests))
           ,@body)
         ,@(drop tests 1)))
    ;; If there is no finishing expression,
    ;; the code can be simplified further.
    (when (= (array-list-length result) 2)
      (set! result (array-list-second result)))
    result)
   ;; For expressions with bindings, we wrap
   ;; the expansion in `let`.
   (else
    (define let-bindings '())
    (define setters '())
    (for ((binding bindings))
      (push-right! let-bindings
                   (take binding 2))
      (when (= (array-list-length binding) 3)
        (push-right! setters
                     `(set! ,(array-list-first binding)
                            ,(array-list-third binding)))))
    (define result
      `(let ,let-bindings
         (js/while (not ,(array-list-first tests))
           ,@body
           ,@setters)
         ,@(drop tests 1)))
    result)))

;;; Expand a `(while ...)` expression.
(defmacro while_ (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

;;; Expand a `(js/for ...)` expression.
(defmacro js/for_ (args &rest body)
  (define inits '())
  (define tests '())
  (for ((arg args))
    (define init
      (array-list-first arg))
    (define test
      (array-list-second arg))
    (define update
      (array-list-third arg))
    (when (tagged-list? init 'define)
      (set! init (drop init 1)))
    (when (tagged-list? update 'set!)
      (set! update (array-list-third init)))
    (push-right! inits `(,@init ,update))
    (push-right! tests test))
  (define test-exp
    (if (= (array-list-length tests) 1)
        (array-list-first tests)
        `(and ,@tests)))
  `(do ,inits
       ((not ,test-exp))
     ,@body))

;;; Expand a `(js/for-in ...)` expression.
(defmacro js/for-in_ (args &rest body)
  (define bindings
    (map (lambda (x)
           (define left
             (array-list-first x))
           (define right
             (array-list-second x))
           (list left `(js-keys ,right)))
         args))
  `(js/for-of ,bindings
              ,@body))

;;; Expand a `(js/for-of ...)` expression.
(defmacro js/for-of_ (args &rest body)
  (define bindings
    (map (lambda (x)
           (define left
             (array-list-first x))
           (define right
             (array-list-second x))
           (when (tagged-list? left 'define)
             (set! left (array-list-second left)))
           (list left right))
         args))
  `(for ,bindings
     ,@body))

;;; Expand a `(field-bound? ...)` expression.
(defmacro field-bound?_ (id obj)
  (cond
   ((symbol? obj)
    `(and ,obj
          (js/in ,(symbol->string id) ,obj)))
   (else
    (define obj-sym
      (gensym "obj"))
    `(let ((,obj-sym ,obj))
       (and ,obj-sym
            (js/in ,(symbol->string id) ,obj-sym))))))

;;; Expand a `(case ...)` expression.
(defmacro case_ (val &rest clauses)
  (define has-complex-clauses #f)
  (define (is-simple-value x)
    (or (boolean? x)
        (number? x)
        (symbol? x)
        (string? x)))
  (define (is-complex-value x)
    (not (is-simple-value x)))
  (for ((x clauses))
    (when (and (not (eq? (array-list-first x) 'else))
               (memf? is-complex-value (array-list-first x)))
      (set! has-complex-clauses #t)
      (break)))
  (cond
   (has-complex-clauses
    ;; Complex case: there is one or more regular clauses that contain
    ;; patterns that must be matched against with `equal?`, not `eq? `.
    (define is-complex-val
      (not (symbol? val)))
    (define value-var
      (if is-complex-val
          (gensym "_value")
          val))
    (define cond-clauses
      (map (lambda (x)
             (cond
              ((eq? (array-list-first x) 'else)
               x)
              (else
               `((member? ,value-var
                          (quote ,(array-list-first x))
                          equal?)
                 ,@(rest x)))))
           clauses))
    (define result
      `(cond ,@cond-clauses))
    ;; If `val` is a complex expression, we get a
    ;; `(let ... (cond ...))` form.
    (when is-complex-val
      (set! result
            `(let ((,value-var ,val))
               ,result)))
    result)
   (else
    ;; Simple case: all patterns can be matched against with `eq?`,
    ;; so translate the entire form to a `case/eq` form.
    `(case/eq ,val ,@clauses))))

;;; Expand a `(case/eq ...)` expression.
(defmacro case-eq_ (val &rest clauses)
  (define has-complex-clauses #f)
  (for ((x clauses))
    (when (and (not (eq? (array-list-first x) 'else))
               (> (array-list-length (array-list-first x)) 1))
      (set! has-complex-clauses #t)
      (break)))
  (cond
   ;; Complex case: there is one or more regular clauses that contain
   ;; multiple patterns. This is translatable to a `(cond ...)` form
   ;; that performs pattern matching.
   (has-complex-clauses
    (define is-complex-val
      (not (symbol? val)))
    (define value-var
      (if is-complex-val
          (gensym "_value")
          val))
    (define cond-clauses
      (map (lambda (x)
             (cond
              ((eq? (array-list-first x) 'else)
               x)
              (else
               `((member? ,value-var (quote ,(array-list-first x)))
                 ,@(rest x)))))
           clauses))
    (define result
      `(cond ,@cond-clauses))
    ;; If `val` is a complex expression, we get a
    ;; `(let ... (cond ...))` form.
    (when is-complex-val
      (set! result
            `(let ((,value-var ,val))
               ,result)))
    result)
   (else
    ;; Simple case: each regular clause contains exactly one pattern.
    ;; This is translatable to a `(js/swith ...)` form.
    (define switch-clauses
      (map (lambda (x)
             (cond
              ((eq? (array-list-first x) 'else)
               `(default ,@(rest x)))
              (else
               `(case (quote ,(array-list-first (array-list-first x)))
                  ,@(rest x)
                  (break)))))
           clauses))
    `(js/switch ,val
                ,@switch-clauses))))

;;; Expand a `(let-env ...)` expression.
(defmacro let-env_ (x &rest body)
  `(scm/eval (quote (begin ,@body))
             (extend-environment
              ,x
              (current-environment))))

;;; Expand a `(set ...)` expression.
(defmacro set_ (sym val)
  `(set! ,(array-list-second sym) ,val))

;;; Expand a `(new/apply ...)` expression.
(defmacro new-apply_ (&rest args)
  `(apply new ,@args))

;;; Expand a `(clj/try ...)` expression.
;;;
;;; Similar to the [`try` special form][clj:try] in Clojure.
;;;
;;; [clj:try]: https://clojuredocs.org/clojure.core/try
(defmacro clj-try_ (&rest body)
  (define body-exps '())
  (define catch-clauses '())
  (define clj-catch-clauses '())
  (define finalizer-clauses '())
  (for ((x body))
    (cond
     ((tagged-list? x 'catch)
      (push-right! clj-catch-clauses x))
     ((tagged-list? x 'finally)
      (push-right! finalizer-clauses x))
     (else
      (push-right! body-exps x))))
  (when (> (array-list-length clj-catch-clauses) 0)
    (define exception
      (second (first clj-catch-clauses)))
    (define sym
      (third (first clj-catch-clauses)))
    (cond
     ((and (= (array-list-length clj-catch-clauses) 1)
           (memq? exception
                  '(_
                    js/Object
                    Object
                    object%)))
      (define clj-catch-clause
        (first clj-catch-clauses))
      (define catch-clause
        `(catch ,sym
             ,@(drop clj-catch-clause 3)))
      (set! catch-clauses
            (list catch-clause)))
     (else
      (define cond-exp
        `(cond
          ,@(map (lambda (x)
                   `((is-a? ,sym ,(second x))
                     ,@(drop x 3)))
                 clj-catch-clauses)
          (else
           (throw ,sym))))
      (define catch-clause
        `(catch ,sym
             ,cond-exp))
      (set! catch-clauses
            (list catch-clause)))))
  `(js/try
    ,@body-exps
    ,@catch-clauses
    ,@finalizer-clauses))

(provide
  (rename-out (field-bound?_ field-boundp_))
  begin0_
  case-eq_
  case_
  clj-try_
  defclass_
  define-private_
  define-public_
  defmacro_
  defun_
  do_
  field-bound?_
  if_
  js/for-in_
  js/for-of_
  js/for_
  let-env_
  multiple-value-bind_
  new-apply_
  rkt-new_
  set_
  thread-as_
  thread-first_
  thread-last_
  unless_
  unwind-protect_
  when_
  while_)
