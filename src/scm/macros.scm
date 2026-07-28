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
(require (only-in "./util"
                  count-tree
                  map-tree
                  tagged-list?))

;;; Expand a `(defun ...)` expression.
(define-macro (defun_ name args &rest body)
  `(define (,name ,@args)
     ,@body))

;;; Expand a `(define/private ...)` expression.
(define-macro (define-private_ &rest body)
  `(define ,@body))

;;; Expand a `(define/public ...)` expression.
(define-macro (define-public_ &rest body)
  `(define ,@body))

;;; Expand a `(defclass ...)` expression.
(define-macro (defclass_ &rest body)
  `(define-class ,@body))

;;; Expand a `(define-macro ...)` expression.
;;;
;;; Similar to [`define-macro` in Guile][guile:define-macro] and
;;; [`defmacro` in Common Lisp][cl:defmacro].
;;;
;;; [guile:define-macro]: https://www.gnu.org/software/guile/docs/docs-2.2/guile-ref/Defmacros.html
;;; [cl:defmacro]: http://clhs.lisp.se/Body/m_defmac.htm#defmacro
(define-macro (define-macro_ name-and-args &rest body)
  (define name
    (car name-and-args))
  (define macro-fn-form
    (define-macro->lambda-form
      `(define-macro ,name-and-args
         ,@body)))
  (define args
    (js/second macro-fn-form))
  (define macro-body
    (drop macro-fn-form 2))
  `(begin
     (define (,name ,@args)
       ,@macro-body)
     (declare-macro ,name)))

;;; Create a macro function on the basis of a
;;; `(define-macro ...)` expression.
(define (define-macro->function exp env)
  (define macro-fn
    (define-macro->lambda-form exp))
  (eval_ macro-fn env))

;;; Create a `(lambda ...)` form for a macro function
;;; on the basis of a `(define-macro ...)` expression.
(define (define-macro->lambda-form exp)
  (define name-and-args
    (second exp))
  (define name
    (car name-and-args))
  (define args
    (cdr name-and-args))
  (define body
    (drop exp 2))
  (define exp-arg 'exp)
  (define env-arg 'env)
  (define macro-args '())
  (define rest-arg #u)
  (cond
   ((list? args)
    (define i 0)
    (while (< i (js/length args))
      (define arg
        (aget args i))
      (cond
       ((eq? arg '&rest)
        (set! rest-arg (aget args (+ i 1)))
        (set! i (+ i 2)))
       ((eq? arg '&whole)
        (set! exp-arg (aget args (+ i 1)))
        (set! i (+ i 2)))
       ((eq? arg '&environment)
        (set! env-arg (aget args (+ i 1)))
        (set! i (+ i 2)))
       (else
        (push-right! macro-args arg)
        (set! i (+ i 1))))))
   (else
    (set! macro-args args)))
  (when rest-arg
    (cond
     ((null? macro-args)
      (set! macro-args rest-arg))
     (else
      (set! macro-args
            (apply list*
                   (append macro-args
                           (list rest-arg)))))))
  `(lambda (,exp-arg ,env-arg)
     ,@(if (null? macro-args)
           '()
           `((define-values ,macro-args
               (rest ,exp-arg))))
     ,@body))

;;; Expand a `(defmacro ...)` expression.
(define-macro (defmacro_ name args &rest body)
  `(define-macro ,(cons name args)
     ,@body))

;;; Expand a `(define-fexpr ...)` expression.
(define-macro (define-fexpr_ name-and-args &rest body)
  `(begin
     (define ,name-and-args
       ,@body)
     (declare-fexpr ,(car name-and-args))))

;;; Expand a `(declare ...)` expression.
(define-macro (declare_ name &rest specs)
  `(begin
     ,@(map (lambda (spec)
              `(set-field! ,(js/first spec)
                           ,name
                           ,(js/second spec)))
            specs)))

;;; Expand a `(declare-macro ...)` expression.
(define-macro (declare-macro_ name)
  `(declare ,name (ftype "macro")))

;;; Expand a `(declare-fexpr ...)` expression.
(define-macro (declare-fexpr_ name)
  `(declare ,name (ftype "fexpr")))

;;; Expand a `(begin0 ...)` or `(prog1 ...)` expression.
(define-macro (begin0_ x &rest xs)
  (cond
   ((= (js/length xs) 0)
    x)
   (else
    (define result
      (gensym "begin0-result"))
    `(let ((,result ,x))
       ,@xs
       ,result))))

;;; Expand a `(multiple-values-bind ...)` expression.
(define-macro (multiple-value-bind_ bindings expression &rest body)
  `(let-values ((,bindings ,expression))
     ,@body))

;;; Expand a `(rkt/new ...)' expression.
(define-macro (rkt-new_ constructor &rest args)
  ;; We are not able to do much here other than to rewrite the
  ;; expression to a `(make-object ...)` expression. JavaScript lacks
  ;; support for creating a new object on the basis of by-name
  ;; initialization arguments; it only supports by-position
  ;; initialization arguments, which are passed to the constructor.
  `(make-object ,constructor ,@(map js/second args)))

;;; Expand an `(if ...)` expression.
(define-macro (if_ condition then-clause &rest else-clauses)
  `(cond
    (,condition
     ,then-clause)
    ,@(cond
       ((> (js/length else-clauses) 0)
        `((else ,@else-clauses)))
       (else
        '()))))

;;; Expand a `(when ...)` expression.
(define-macro (when_ condition &rest body)
  `(if ,condition
       (begin ,@body)))

;;; Expand an `(unless ...)` expression.
(define-macro (unless_ condition &rest body)
  `(if (not ,condition)
       (begin ,@body)))

;;; Expand an `(as~> ...)` expression.
;;;
;;; Similar to the [`as->` macro][clj:thread-as] in Clojure.
;;;
;;; [clj:thread-as]: https://clojuredocs.org/clojure.core/as-%3E
(define-macro (thread-as_ val sym &rest forms)
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
(define-macro (thread-first_ x &rest forms)
  (define hole-marker '_)
  (when (and (> (js/length forms) 1)
             (eq? (js/first forms) ':hole-marker))
    (set! hole-marker (js/second forms))
    (set! forms (drop forms 2)))
  (define (f val acc)
    (cond
     ((symbol? val)
      `(,@acc (,val _)))
     ((= (count-tree (lambda (x)
                       (eq? x hole-marker))
                     val)
         0)
      `(,@acc (,(js/first val)
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
(define-macro (thread-last_ x &rest forms)
  (define hole-marker '_)
  (when (and (> (js/length forms) 1)
             (eq? (js/first forms) ':hole-marker))
    (set! hole-marker (js/second forms))
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
(define-macro (unwind-protect_ body-form &rest unwind-forms)
  `(try
     ,body-form
     (finally
       ,@unwind-forms)))

;;; Expand a `(do ...)` expression.
(define-macro (do_ bindings tests &rest body)
  (cond
   ;; For expressions with no bindings, we wrap
   ;; the expansion in `begin`.
   ((= (js/length bindings) 0)
    (define result
      `(begin
         (js/while (not ,(js/first tests))
           ,@body)
         ,@(drop tests 1)))
    ;; If there is no finishing expression,
    ;; the code can be simplified further.
    (when (= (js/length result) 2)
      (set! result (js/second result)))
    result)
   ;; For expressions with bindings, we wrap
   ;; the expansion in `let`.
   (else
    (define let-bindings '())
    (define setters '())
    (for ((binding bindings))
      (push-right! let-bindings
                   (take binding 2))
      (when (= (js/length binding) 3)
        (push-right! setters
                     `(set! ,(js/first binding)
                            ,(js/third binding)))))
    (define result
      `(let ,let-bindings
         (js/while (not ,(js/first tests))
           ,@body
           ,@setters)
         ,@(drop tests 1)))
    result)))

;;; Expand a `(while ...)` expression.
(define-macro (while_ test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

;;; Expand a `(js/for ...)` expression.
(define-macro (js/for_ args &rest body)
  (define inits '())
  (define tests '())
  (define-values (init test update)
    args)
  (when (tagged-list? init 'define)
    (set! init (drop init 1)))
  (when (tagged-list? update 'set!)
    (set! update (js/third init)))
  (push-right! inits `(,@init ,update))
  (push-right! tests test)
  (define test-exp
    (if (= (js/length tests) 1)
        (js/first tests)
        `(and ,@tests)))
  `(do ,inits
       ((not ,test-exp))
     ,@body))

(define-macro (js/for-2_ args &rest body)
  (define inits '())
  (define tests '())
  (for ((arg args))
    (define init
      (js/first arg))
    (define test
      (js/second arg))
    (define update
      (js/third arg))
    (when (tagged-list? init 'define)
      (set! init (drop init 1)))
    (when (tagged-list? update 'set!)
      (set! update (js/third init)))
    (push-right! inits `(,@init ,update))
    (push-right! tests test))
  (define test-exp
    (if (= (js/length tests) 1)
        (js/first tests)
        `(and ,@tests)))
  `(do ,inits
       ((not ,test-exp))
     ,@body))

;;; Expand a `(js/for-in ...)` expression.
(define-macro (js/for-in_ args &rest body)
  (define bindings
    (map (lambda (x)
           (define left
             (js/first x))
           (define right
             (js/second x))
           (list left `(js/keys ,right)))
         args))
  `(js/for-of ,bindings
              ,@body))

;;; Expand a `(js/for-of ...)` expression.
(define-macro (js/for-of_ args &rest body)
  (define bindings
    (map (lambda (x)
           (define left
             (js/first x))
           (define right
             (js/second x))
           (when (tagged-list? left 'define)
             (set! left (js/second left)))
           (list left right))
         args))
  `(for ,bindings
     ,@body))

;;; Expand a `(case ...)` expression.
(define-macro (case_ val &rest clauses)
  (define has-complex-clauses #f)
  (define (is-simple-value x)
    (or (boolean? x)
        (number? x)
        (symbol? x)
        (string? x)))
  (define (is-complex-value x)
    (not (is-simple-value x)))
  (for ((x clauses))
    (when (and (not (eq? (js/first x) 'else))
               (memf? is-complex-value (js/first x)))
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
              ((eq? (js/first x) 'else)
               x)
              (else
               `((member? ,value-var
                          ',(js/first x)
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
(define-macro (case-eq_ val &rest clauses)
  (define has-complex-clauses #f)
  (for ((x clauses))
    (when (and (not (eq? (js/first x) 'else))
               (> (js/length (js/first x)) 1))
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
              ((eq? (js/first x) 'else)
               x)
              (else
               `((member? ,value-var ',(js/first x))
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
              ((eq? (js/first x) 'else)
               `(default ,@(rest x)))
              (else
               `(case ',(js/first (js/first x))
                  ,@(rest x)
                  (break)))))
           clauses))
    `(js/switch ,val
                ,@switch-clauses))))

;;; Expand a `(let-env ...)` expression.
(define-macro (let-env_ x &rest body)
  `(scm/eval (quote (begin ,@body))
             (extend-environment
              ,x
              (current-environment))))

;;; Expand a `(set ...)` expression.
(define-macro (set_ sym val)
  `(set! ,(js/second sym) ,val))

;;; Expand a `(new/apply ...)` expression.
(define-macro (new-apply_ &rest args)
  `(apply new ,@args))

;;; Expand a `(clj/try ...)` expression.
;;;
;;; Similar to the [`try` special form][clj:try] in Clojure.
;;;
;;; [clj:try]: https://clojuredocs.org/clojure.core/try
(define-macro (clj-try_ &rest body)
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
  (when (> (js/length clj-catch-clauses) 0)
    (define exception
      (second (first clj-catch-clauses)))
    (define sym
      (third (first clj-catch-clauses)))
    (cond
     ((and (= (js/length clj-catch-clauses) 1)
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
  begin0_
  case-eq_
  case_
  clj-try_
  declare-fexpr_
  declare-macro_
  declare_
  defclass_
  define-fexpr_
  define-macro->function
  define-macro->lambda-form
  define-macro_
  define-private_
  define-public_
  defmacro_
  defun_
  do_
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
