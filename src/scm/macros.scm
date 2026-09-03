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

(require (only-in "./eval"
                  eval_))
(require (only-in "./plist"
                  plist-get_))
(require (only-in "./rose"
                  datum->syntax
                  syntax->datum
                  transfer-comments))
(require (only-in "./util"
                  count-tree
                  list-expression->pattern
                  map-tree
                  number->letter
                  tagged-list?))

;;; Expand a `(defun ...)` expression.
;;;
;;; Similar to [`defun` in Common Lisp][cl:defun] and
;;; [`defun` in Emacs Lisp][el:defun].
;;;
;;; [cl:defun]: http://clhs.lisp.se/Body/m_defun.htm
;;; [el:defun]: https://www.gnu.org/software/emacs/manual/html_node/eintr/defun.html
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
;;;
;;; Similar to [`defclass` in Common Lisp][cl:defclass].
;;;
;;; [cl:defclass]: http://clhs.lisp.se/Body/m_defcla.htm
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
    (second macro-fn-form))
  (define macro-body
    (drop macro-fn-form 2))
  `(begin
     (define (,name ,@args)
       ,@macro-body)
     (declare-macro ,name)))

;;; Expand a `(syntax ...)` expression.
(define-macro (syntax_ v)
  `(datum->syntax #f (quote ,v)))

;;; Expand a `(quasisyntax ...)` expression.
(define-macro (quasisyntax_ v)
  `(datum->syntax #f (quasiquote ,v)))

;;; Expand a `(define-syntax ...)` expression.
(define-macro (define-syntax_ name-and-args &rest body)
  (define name
    (if (symbol? name-and-args)
        name-and-args
        (car name-and-args)))
  `(begin
     (define ,name-and-args
       ,@body)
     (declare ,name (ftype (macro-> Syntax Syntax)))))

;;; Create a macro function on the basis of a
;;; `(define-macro ...)` expression.
(define (define-macro->function exp env)
  (define macro-fn
    (define-macro->lambda-form exp))
  (eval_ macro-fn env))

;;; Create a `(lambda ...)` form for a macro function
;;; on the basis of a `(define-macro ...)` expression.
(define (define-macro->lambda-form exp (options (js/obj)))
  (define name-and-args
    (second exp))
  (define name
    (car name-and-args))
  (define args
    (cdr name-and-args))
  (define body
    (drop exp 2))
  (define exp-arg
    (or (oget options :exp)
        (gensym "exp")))
  (define env-arg
    (or (oget options :env)
        (gensym "env")))
  (define macro-args '())
  (define rest-arg #u)
  (cond
   ((list? args)
    (define i 0)
    (while (< i (length args))
      (define arg
        (list-ref args i))
      (cond
       ((memq? arg '(&body &rest))
        (set! rest-arg (list-ref args (+ i 1)))
        (set! i (+ i 2)))
       ((eq? arg '&whole)
        (set! exp-arg (list-ref args (+ i 1)))
        (set! i (+ i 2)))
       ((eq? arg '&environment)
        (set! env-arg (list-ref args (+ i 1)))
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
;;;
;;; Similar to [`defmacro` in Common Lisp][cl:defmacro]
;;; and [`defmacro` in Emacs Lisp][el:defmacro].
;;;
;;; [cl:defmacro]: http://clhs.lisp.se/Body/m_defmac.htm
;;; [el:defmacro]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Macros.html#index-defmacro
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
;;;
;;; Similar to [`declare` in Common Lisp] and
;;; [`declare` in Emacs Lisp][el:declare].
;;;
;;; [cl:declare]: http://clhs.lisp.se/Body/s_declar.htm#declare
;;; [el:declare]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Declare-Form.html
(define-macro (declare_ name &rest specs)
  `(begin
     ,@(map (lambda (spec)
              `(set-field! ,(first spec)
                           ,name
                           (quote ,(second spec))))
            specs)))

;;; Expand a `(declare-macro ...)` expression.
(define-macro (declare-macro_ name)
  `(declare ,name (ftype "macro")))

;;; Expand a `(declare-fexpr ...)` expression.
(define-macro (declare-fexpr_ name)
  `(declare ,name (ftype "fexpr")))

;;; Expand a `(begin0 ...)` expression.
;;;
;;; Similar to [`begin0` in Racket] and
;;; [`prog1` in Common Lisp][cl:prog1].
;;;
;;; [rkt:begin0]: https://docs.racket-lang.org/reference/begin.html#%28form._%28%28quote._~23~25kernel%29._begin0%29%29
;;; [cl:prog1]: http://clhs.lisp.se/Body/m_prog1c.htm
(define-macro (begin0_ x &rest xs)
  (cond
   ((= (length xs) 0)
    x)
   (else
    (define result
      (gensym "begin0-result"))
    `(let ((,result ,x))
       ,@xs
       ,result))))

;;; Expand a `(multiple-value-bind ...)` expression.
;;;
;;; Similar to [`multiple-value-bind` in
;;; Common Lisp][cl:multiple-value-bind].
;;;
;;; [cl:multiple-value-bind]: http://clhs.lisp.se/Body/m_multip.htm
(define-macro (multiple-value-bind_ bindings expression &rest body)
  `(let-values ((,bindings ,expression))
     ,@body))

;;; Expand a `(rkt/new ...)' expression.
;;;
;;; Similar to [`new` in Racket][rkt:new].
;;;
;;; [rkt:new]: https://docs.racket-lang.org/reference/objcreation.html#%28form._%28%28lib._racket%2Fprivate%2Fclass-internal..rkt%29._new%29%29
(define-macro (rkt/new_ constructor &rest args)
  ;; We are not able to do much here other than to rewrite the
  ;; expression to a `(make-object ...)` expression. JavaScript lacks
  ;; support for creating a new object on the basis of by-name
  ;; initialization arguments; it only supports by-position
  ;; initialization arguments, which are passed to the constructor.
  `(make-object ,constructor ,@(map second args)))

;;; Expand an `(and ...)` expression.
;;;
;;; Similar to [`and` in Racket][rkt:and], [`and` in Guile][guile:and],
;;; [`and` in Common Lisp][cl:and] and [`and` in Emacs Lisp][el:and].
;;;
;;; [rkt:and]: https://docs.racket-lang.org/reference/if.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._and%29%29
;;; [guile:and]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/and-or.html#index-and
;;; [cl:and]: http://clhs.lisp.se/Body/m_and.htm
;;; [el:and]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Combining-Conditions.html#index-and
(define-syntax (and_ stx)
  (datum->syntax
   stx
   `(js/&& ,@(send stx drop 1))))

;; (define-macro (and-1_ &rest args)
;;   ;; TODO: Rewrite to use `define-syntax`.
;;   `(js/&& ,@args))

;;; Expand an `(or ...)` expression.
;;;
;;; Similar to [`or` in Racket][rkt:or], [`or` in Guile][guile:or],
;;; [`or` in Common Lisp][cl:or] and [`or` in Emacs Lisp][el:or].
;;;
;;; [rkt:or]: https://docs.racket-lang.org/reference/if.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._or%29%29
;;; [guile:or]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/and-or.html#index-or
;;; [cl:or]: http://clhs.lisp.se/Body/m_or.htm
;;; [el:or]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Combining-Conditions.html#index-or
(define-syntax (or_ stx)
  (datum->syntax
   stx
   `(js/\|\| ,@(send stx drop 1))))

;; (define-macro (or-1_ &rest args)
;;   ;; TODO: Rewrite to use `define-syntax`.
;;   `(js/\|\| ,@args))

;;; Expand a `(cond ...)` expression.
;;;
;;; Similar to [`cond` in Racket][rkt:cond] and
;;; [`cond` in Guile][guile:cond].
;;;
;;; [rkt:cond]: https://docs.racket-lang.org/reference/if.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._cond%29%29
;;; [guile:cond]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/Conditionals.html#index-cond-1
(define-syntax (cond_ stx)
  (define cond-var #u)
  (define clauses
    (~> (send stx drop 1)
        (drop-right _ 1)))
  (define last-clause
    (send stx last))
  (define (wrap-clause-body x)
    (if (= (send x size) 2)
        (transfer-comments
         x
         (send x get 1))
        (datum->syntax
         x
         `(begin ,@(send x drop 1)))))
  (define (transform-clause x (acc #u))
    (cond
     ((and (= (send x size) 3)
           (eq? (syntax->datum (send x get 1))
                '=>))
      (unless cond-var
        (set! cond-var (gensym "_cond-var")))
      (datum->syntax
       #f
       `(if (set! ,cond-var ,(send x get 0))
            (,(send x get 2) ,cond-var)
            ,@(if acc
                  (list acc)
                  '()))))
     (else
      (datum->syntax
       #f
       `(if ,(send x get 0)
            ,(wrap-clause-body x)
            ,@(if acc
                  (list acc)
                  '()))))))
  (define (transform-last-clause x)
    (if (tagged-list? x 'else)
        (wrap-clause-body x)
        (transform-clause x)))
  (define result
    (foldr transform-clause
           (transform-last-clause last-clause)
           clauses))
  (when cond-var
    (set! result
          (datum->syntax
           #f
           `(let (,cond-var)
              ,result))))
  (transfer-comments stx result))

;;; Expand a `(when ...)` expression.
;;;
;;; Similar to [`when` in Racket][rkt:when], [`when` in Guile][guile:when],
;;; [`when` in Common Lisp][cl:when] and [`when` in Emacs Lisp][el:when].
;;;
;;; [rkt:when]: https://docs.racket-lang.org/reference/when_unless.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._when%29%29
;;; [guile:when]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/Conditionals.html#index-when-1
;;; [cl:when]: http://clhs.lisp.se/Body/m_when_.htm
;;; [el:when]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html#index-when
(define-syntax (when_ stx)
  (datum->syntax
   stx
   `(if ,(send stx get 1)
        (begin ,@(send stx drop 2)))))

;; (define-macro (when-1_ condition &rest body)
;;   ;; TODO: Rewrite to use `define-syntax`.
;;   `(if ,condition
;;        (begin ,@body)))

;;; Expand an `(unless ...)` expression.
;;;
;;; Similar to [`unless` in Racket][rkt:unless], [`unless` in Guile][guile:unless],
;;; [`unless` in Common Lisp][cl:unless] and [`unless` in Emacs Lisp][el:unless].
;;;
;;; [rkt:unless]: https://docs.racket-lang.org/reference/when_unless.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._unless%29%29
;;; [guile:unless]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/Conditionals.html#index-unless-1
;;; [cl:unless]: http://clhs.lisp.se/Body/m_when_.htm
;;; [el:unless]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html#index-unless
(define-syntax (unless_ stx)
  (datum->syntax
   stx
   `(if (not ,(send stx get 1))
        (begin ,@(send stx drop 2)))))

;; (define-macro (unless-1_ condition &rest body)
;;   ;; TODO: Rewrite to use `define-syntax`.
;;   `(if (not ,condition)
;;        (begin ,@body)))

;;; Expand an `(el/if ...)` expression.
;;;
;;; Similar to [`if` in Emacs Lisp][el:if].
;;;
;;; [el:if]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html#index-if
(define-macro (el/if_ cond-exp then-exp &rest else-exps)
  ;; Emacs Lisp's `if` accepts more than three arguments.
  `(if ,cond-exp
       ,then-exp
       ,@(cond
          ((= (length else-exps) 0)
           '())
          ((= (length else-exps) 1)
           (list (first else-exps)))
          (else
           (list `(begin ,@else-exps))))))

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
  (when (and (> (length forms) 1)
             (eq? (first forms) ':hole-marker))
    (set! hole-marker (second forms))
    (set! forms (drop forms 2)))
  (define (f val acc)
    (cond
     ((symbol? val)
      `(,@acc (,val _)))
     ((= (count-tree (lambda (x)
                       (eq? x hole-marker))
                     val)
         0)
      `(,@acc (,(first val)
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
  (when (and (> (length forms) 1)
             (eq? (first forms) ':hole-marker))
    (set! hole-marker (second forms))
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
;;;
;;; Similar to [`unwind-protect` in Common Lisp][cl:unwind-protect]
;;; and [`unwind-protect` in Emacs Lisp][el:unwind-protect]
;;;
;;; [cl:unwind-protect]: http://clhs.lisp.se/Body/s_unwind.htm
;;; [el:unwind-protect]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Cleanups.html#index-unwind_002dprotect
(define-macro (unwind-protect_ body-form &rest unwind-forms)
  `(try
     ,body-form
     (finally
       ,@unwind-forms)))

;;; Expand a `(do ...)` expression.
;;;
;;; Similar to [`do` in Racket][rkt:do] and
;;; [`do` in Guile][guile:do].
;;;
;;; [rkt:do]: https://docs.racket-lang.org/reference/for.html#%28form._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._do%29%29
;;; [guile:do]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/while-do.html#index-do
(define-macro (do_ bindings tests &rest body)
  (cond
   ;; For expressions with no bindings, we wrap
   ;; the expansion in `begin`.
   ((= (length bindings) 0)
    (define result
      `(begin
         (while (not ,(first tests))
           ,@body)
         ,@(drop tests 1)))
    ;; If there is no finishing expression,
    ;; the code can be simplified further.
    (when (= (length result) 2)
      (set! result (second result)))
    result)
   ;; For expressions with bindings, we wrap
   ;; the expansion in `let`.
   (else
    (define let-bindings '())
    (define setters '())
    (for ((binding bindings))
      (push-right! let-bindings
                   (take binding 2))
      (when (= (length binding) 3)
        (push-right! setters
                     `(set! ,(first binding)
                            ,(third binding)))))
    (define result
      `(let ,let-bindings
         (while (not ,(first tests))
           ,@body
           ,@setters)
         ,@(drop tests 1)))
    result)))

;;; Expand a `(while ...)` expression.
;;;
;;; Similar to [`while` in Guile][guile:while] and
;;; [`while` in Emacs Lisp][el:while].
;;;
;;; [guile:while]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/while-do.html#index-while
;;; [el:while]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Iteration.html#index-while
(define-macro (while_ test &rest body)
  ;; TODO: Rewrite to use `define-syntax`.
  `(js/while ,test ,@body))

;;; Expand a `(for ...)` expression.
;;;
;;; Similar to [`for` in Racket][rkt:for].
;;;
;;; [rkt:for]: https://docs.racket-lang.org/reference/for.html#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._for%29%29
(define-macro (for_ args &rest body)
  (define (combine-inits init1 init2)
    (cond
     ((not init1)
      init2)
     (else
      `(js/let ,@(rest init1) ,@(rest init2)))))
  (define (combine-tests test1 test2)
    (cond
     ((not test1)
      test2)
     (else
      `(and ,test1 ,test2))))
  (define (combine-updates update1 update2)
    (cond
     ((not update1)
      update2)
     (else
      `(begin ,update1 ,update2))))
  (define let-bindings '())
  (define result #u)
  (define definitions '())
  (cond
   ;; If the loop can easily be expressed as a
   ;; `js/for-of` loop, do that.
   ((and (= (length args) 1)
         (not (tagged-list? (second (first args))
                            'range)))
    (set! result
          `(js/for-of ,args ,@body)))
   ;; Otherwise, create a `js/for` loop.
   (else
    (define init #u)
    (define test #u)
    (define update #u)
    (for ((i (range 0 (length args))))
      (define decl
        (list-ref args i))
      (define-values (sym val)
        decl)
      (unless (tagged-list? val 'range)
        (unless (symbol? val)
          (define val-var
            (gensym "_val"))
          (push-right! let-bindings
                       `(,val-var ,val))
          (set! val val-var))
        (define range-exp
          `(range 0 (length ,val)))
        (define index
          (gensym (number->letter i "i")))
        (define definition-exp
          `(define ,sym (list-ref ,val ,index)))
        (push-right! definitions definition-exp)
        (set! sym index)
        (set! val range-exp))
      (define start
        (second val))
      (define end
        (third val))
      (define step
        (or (fourth val) 1))
      ;; If `start`, `end` or `step` is a function call,
      ;; then rewrite the expression to a `let` expression
      ;; so that the function is called only once.
      (when (pair-or-list? start)
        (define start-var
          (gensym "_start"))
        (push-right! let-bindings
                     `(,start-var ,start))
        (set! start start-var))
      (when (pair-or-list? end)
        (define end-var
          (gensym "_end"))
        (push-right! let-bindings
                     `(,end-var ,end))
        (set! end end-var))
      (when (pair-or-list? step)
        (define step-var
          (gensym "_step"))
        (push-right! let-bindings
                     `(,step-var ,step))
        (set! step step-var))
      (set! init
            (combine-inits
             init
             `(js/let ,sym ,start)))
      (set! test
            (combine-tests
             test
             (cond
              ((number? step)
               (if (< step 0)
                   `(> ,sym ,end)
                   `(< ,sym ,end)))
              (else
               `(if (< ,step 0)
                    (> ,sym ,end)
                    (< ,sym ,end))))))
      (set! update
            (combine-updates
             update
             (cond
              ((number? step)
               (if (< step 0)
                   `(set! ,sym (- ,sym ,(abs step)))
                   `(set! ,sym (+ ,sym ,step))))
              (else
               `(set! ,sym (+ ,sym ,step)))))))
    (set! result
          `(js/for (,init ,test ,update)
                   ,@definitions
                   ,@body))))
  (when (> (length let-bindings) 0)
    (set! result
          `(let ,let-bindings ,result)))
  result)

;;; Expand a `(case ...)` expression.
;;;
;;; Similar to [`case` in Racket][rkt:case].
;;;
;;; [rkt:case]: https://docs.racket-lang.org/reference/case.html#%28form._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._case%29%29
(define-macro (case_ val &rest clauses)
  (define has-complex-clauses #f)
  (define (simple-value? x)
    (or (boolean? x)
        (number? x)
        (symbol? x)
        (string? x)))
  (define (complex-value? x)
    (not (simple-value? x)))
  (for ((x clauses))
    (when (and (not (eq? (first x) 'else))
               (memf? complex-value? (first x)))
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
              ((eq? (first x) 'else)
               x)
              (else
               `((member? ,value-var
                          ',(first x)
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
    (when (and (not (eq? (first x) 'else))
               (> (length (first x)) 1))
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
              ((eq? (first x) 'else)
               x)
              (else
               `((member? ,value-var ',(first x))
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
              ((eq? (first x) 'else)
               `(default ,@(rest x)))
              (else
               `(case ',(first (first x))
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
;;;
;;; Similar to [`set` in Common Lisp][cl:set] and
;;; [`set` in Emacs Lisp][el:set].
;;;
;;; [cl:set]: http://clhs.lisp.se/Body/f_set.htm
;;; [el:set]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Variables.html#index-set
(define-macro (set_ sym val)
  `(set! ,(second sym) ,val))

;;; Expand a `(setq ...)` expression.
;;;
;;; Similar to [`setq` in Common Lisp][cl:setq]
;;; and [`setq` in Emacs Lisp][el:setq].
;;;
;;; [cl:setq]: http://clhs.lisp.se/Body/s_setq.htm#setq
;;; [el:setq]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Variables.html#index-setq
(define-macro (setq_ &rest bindings)
  (define bindings1 '())
  (for ((i (range 0 (length bindings) 2)))
    (define sym
      (list-ref bindings i))
    (define val
      (list-ref bindings (+ i 1)))
    (push-right! bindings1 `(set! ,sym ,val)))
  (if (= (length bindings1) 1)
      (first bindings1)
      `(begin ,@bindings1)))

;;; Expand a `(new/apply ...)` expression.
(define-macro (new/apply_ &rest args)
  `(apply new ,@args))

;;; Expand a `(try ...)` expression.
;;;
;;; Similar to the [`try` special form][clj:try] in Clojure.
;;;
;;; [clj:try]: https://clojuredocs.org/clojure.core/try
(define-macro (try_ &rest body)
  `(clj/try ,@body))

;;; Expand a `(clj/try ...)` expression.
;;;
;;; Similar to the [`try` special form][clj:try] in Clojure.
;;;
;;; [clj:try]: https://clojuredocs.org/clojure.core/try
(define-macro (clj/try_ &rest body)
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
  (when (> (length clj-catch-clauses) 0)
    (define exception
      (second (first clj-catch-clauses)))
    (define sym
      (third (first clj-catch-clauses)))
    (cond
     ((and (= (length clj-catch-clauses) 1)
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

;;; Expand a `(match ...)` expression.
;;;
;;; Similar to [`match` in Racket] and, to a lesser extent,
;;; [`match` in Guile][guile:match].
;;;
;;; [rkt:match]: https://docs.racket-lang.org/reference/match.html#%28form._%28%28lib._racket%2Fmatch..rkt%29._match%29%29
;;; [guile:match]: https://doc.guix.gnu.org/guile/latest/en/html_node/Pattern-Matching.html#index-match
(define-macro (match_ exp &rest clauses)
  (define (pattern-bind pat exp)
    (cond
     ((eq? pat '_)
      '())
     ((symbol? pat)
      (list
       `(define ,pat ,exp)))
     ((pair-or-list? pat)
      (cond
       ((null? pat)
        '())
       ((tagged-list? pat 'quote)
        '())
       ((tagged-list? pat 'var)
        (list
         `(define ,(second pat)
            ,exp)))
       ((tagged-list? pat 'cons)
        (pattern-bind `(list* ,@(rest pat)) exp))
       ((tagged-list? pat '(list list*))
        (list
         `(define-values ,(list-expression->pattern pat)
            ,exp)))
       (else
        '())))
     (else
      '())))
  (define (pattern-match pat exp (make-let #t))
    (cond
     ((and make-let
           (pair-or-list? exp))
      (let ((pattern-match-val (gensym "pattern-match-val")))
        `(let ((,pattern-match-val ,exp))
           ,(pattern-match pat pattern-match-val))))
     ((symbol? pat)
      #t)
     ((pair-or-list? pat)
      (cond
       ((null? pat)
        `(null? ,exp))
       ((tagged-list? pat 'quote)
        `(,(if (pair-or-list? (second pat))
               'equal?
               'eq?)
          ,exp
          ,pat))
       ((tagged-list? pat 'var)
        #t)
       ((tagged-list? pat 'not)
        `(not ,(pattern-match (second pat) exp #f)))
       ((tagged-list? pat 'and)
        (apply combine-expressions
               '(and)
               (map (lambda (x)
                      (pattern-match x exp #f))
                    (rest pat))))
       ((tagged-list? pat 'or)
        (apply combine-expressions
               '(or)
               (map (lambda (x)
                      (pattern-match x exp #f))
                    (rest pat))))
       ((tagged-list? pat 'cons)
        (pattern-match `(list* ,@(rest pat)) exp #f))
       ((tagged-list? pat 'list)
        (cond
         ((eq? (last pat) '...)
          (define head
            (~> (drop pat 1)
                (drop-right _ 2)))
          (define tail
            (list-ref pat (- (length pat) 2)))
          (define pat1
            `(list* ,@head ,tail))
          (pattern-match pat1 exp #f))
         (else
          (define len
            (- (length pat) 1))
          (define result
            `(and (pair-or-list? ,exp)
                  (= (length ,exp)
                     ,len)))
          (for ((i (range 1 (length pat))))
            (define pat1
              (list-ref pat i))
            (define exp1
              `(list-ref ,exp ,(- i 1)))
            (define result1
              (pattern-match pat1 exp1 #f))
            (set! result (combine-expressions result result1)))
          result)))
       ((tagged-list? pat 'list*)
        (define head
          (~> (drop pat 1)
              (drop-right _ 1)))
        (define tail
          (last pat))
        (define len
          (length head))
        (define result
          `(and (pair-or-list? ,exp)
                (>= (length ,exp)
                    ,(length head))))
        (for ((i (range 0 (length head))))
          (define pat1
            (list-ref head i))
          (define exp1
            `(list-ref ,exp ,i))
          (define result1
            (pattern-match pat1 exp1 #f))
          (set! result (combine-expressions result result1)))
        (define exp2
          `(drop ,exp ,len))
        (define result2
          (pattern-match tail exp2 #f))
        (set! result (combine-expressions result result2))
        result)
       ((tagged-list? pat 'regexp)
        `(regexp-match ,pat ,exp))
       ((tagged-list? pat '?)
        (apply combine-expressions
               '(and)
               `(,(second pat) ,exp)
               (map (lambda (x)
                      (pattern-match x exp #f))
                    (drop pat 2))))
       ((tagged-list? pat 'app)
        (define pats
          (drop pat 2))
        (define exp1
          `(,(second pat) ,exp))
        (cond
         ((= (length pats) 1)
          (pattern-match (first pats) exp1 #f))
         (else
          (pattern-match `(and ,@pats) exp1))))
       (else
        #f)))
     (else
      `(eq? ,exp ,pat))))
  (define (combine-expressions . exps)
    (foldl (lambda (x acc)
             (cond
              ((not (pair-or-list? acc))
               acc)
              ((tagged-list? x 'and)
               (for ((x1 (rest x)))
                 (push-right! acc x1))
               acc)
              ((pair-or-list? x)
               (push-right! acc x)
               acc)
              ((and (eq? x #t)
                    (tagged-list? acc 'or))
               #t)
              ((and (eq? x #f)
                    (tagged-list? acc 'and))
               #f)
              (else
               acc)))
           (first exps)
           (rest exps)))
  (cond
   ((pair-or-list? exp)
    (let ((match-val (gensym "match-val")))
      `(let ((,match-val ,exp))
         (match ,match-val
           ,@clauses))))
   (else
    (define cond-clauses
      (map (lambda (x)
             (define pat
               (first x))
             (define body
               (rest x))
             `(,(pattern-match pat exp)
               ,@(pattern-bind pat exp)
               ,@body))
           clauses))
    (define last-cond-clause
      (last cond-clauses))
    (when (eq? (first last-cond-clause) #t)
      (set-car! last-cond-clause 'else))
    `(cond
      ,@cond-clauses))))

;;; Expand a `(cl/loop ...)` expression.
;;;
;;; Similar to [`loop` in Common Lisp][cl:loop].
;;;
;;; [cl:loop]: http://clhs.lisp.se/Body/m_loop.htm#loop
(define-macro (cl/loop_ &rest body)
  ;; Currently just a very simple implementation of a
  ;; tiny subset of Common Lisp's `loop` macro. Useful
  ;; for testing purposes.
  (define plists '())
  (define plist '())
  (for ((x body))
    (when (memq? x '(for collect))
      (unless (null? plist)
        (push-right! plists plist))
      (set! plist '()))
    (push-right! plist x))
  (unless (null? plist)
    (push-right! plists plist))
  (define for-plists '())
  (define accumulation-plist '())
  (for ((plist plists))
    (cond
     ((tagged-list? plist 'for)
      (push-right! for-plists plist))
     ((tagged-list? plist 'collect)
      (set! accumulation-plist plist))))
  (define result-var #u)
  (define let-bindings '())
  (define body1 '())
  (unless (null? accumulation-plist)
    (set! result-var (gensym "result"))
    (push-right! let-bindings `(,result-var '()))
    (define result-exp
      `(push-right ,result-var
                   ,(plist-get_ accumulation-plist
                                'collect)))
    (push-right! body1 result-exp))
  (define result
    `(for ,(map (lambda (x)
                  `(,(plist-get_ x 'for)
                    ,(plist-get_ x 'in)))
                for-plists)
       ,@body1))
  (unless (null? let-bindings)
    (set! result
          `(let ,let-bindings
             ,result
             ,result-var)))
  result)

;;; `with-gensyms` macro as defined in
;;; Peter Seibel's [*Practical Common Lisp*][book:pcl].
;;;
;;; [book:pcl]: https://gigamonkeys.com/book/macros-defining-your-own#macro-writing-macros
(define-macro (with-gensyms_ names &rest body)
  `(let ,(cl/loop for n in names collect `(,n (gensym)))
     ,@body))

;;; `once-only` macro as defined in
;;; Peter Seibel's [*Practical Common Lisp*][book:pcl].
;;;
;;; [book:pcl]: https://gigamonkeys.com/book/macros-defining-your-own#macro-writing-macros
(define-macro (once-only_ names &rest body)
  (let ((gensyms (cl/loop for n in names collect (gensym))))
    `(let (,@(cl/loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(cl/loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(cl/loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))

(provide
  and_
  begin0_
  case-eq_
  case_
  cl/loop_
  clj/try_
  cond_
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
  define-syntax_
  defmacro_
  defun_
  do_
  el/if_
  for_
  let-env_
  match_
  multiple-value-bind_
  new/apply_
  once-only_
  or_
  quasisyntax_
  rkt/new_
  set_
  setq_
  syntax_
  thread-as_
  thread-first_
  thread-last_
  try_
  unless_
  unwind-protect_
  when_
  with-gensyms_
  while_)
