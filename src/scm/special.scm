;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Special forms
;;;
;;; Interpreter procedures for special forms.
;;;
;;; ## Description
;;;
;;; The default approach for the interpreter is to compile the Lisp
;;; code to JavaScript code and then evaluate the JavaScript code.
;;; That is to say that the S-expression is compiled to an ESTree
;;; tree, which is then evaluated.
;;;
;;; However, it is possible to make the intepretation more efficient
;;; by skipping the compilation step and interpreting the S-expression
;;; directly. This file defines interpreter procedures which do just
;;; that.
;;;
;;; The functions defined in this file are *special forms* and must be
;;; typed as such in the language environment. A special form receives
;;; its arguments unevaluated and returns a value that is used
;;; directly. It is similar to a macro, except that the value returned
;;; by a macro is re-evaluated, while the value returned by a special
;;; form is used as-is.
;;;
;;; Care must be taken to implement these functions correctly, as
;;; their behavior should be identical to the standard behavior of
;;; compiling the code to an ESTree tree and then evaluating. In other
;;; words, the only purpose of the code in this file is to make
;;; interpretation faster.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(require (only-in "./array"
                  aget_
                  aset_))
(require (only-in "./env"
                  EnvironmentStack
                  LispEnvironment
                  current-environment))
(require (only-in "./eval"
                  (eval_ eval-t)
                  ;; eval-t
                  eval_))
(require (only-in "./exception"
                  BreakException
                  ContinueException
                  ReturnException))
(require (only-in "./procedures"
                  (funcall tcall)))
(require (only-in "./rose"
                  Rose
                  make-rose
                  rose?
                  transfer-comments))
;; (require (only-in "./trampoline"
;;                   tcall))
(require (only-in "./util"
                  begin-wrap
                  form?
                  lambda->let
                  quasiquote?
                  quote?
                  tagged-list?
                  text-of-quotation
                  unquote-splicing?
                  unquote?))

;;; Evaluate a `(quote ...)` form.
(define (quote-special_ exp env)
  (text-of-quotation exp))

;;; Evaluate a `(quasiquote ...)` form.
(define (quasiquote-special_ exp env)
  (quasiquote-helper (second exp) env))

;;; Helper function for `quasiquote-special_`.
(define (quasiquote-helper exp env)
  (cond
   ((not (list? exp))
    exp)
   (else
    (define result '())
    (for ((x exp))
      (cond
       ((unquote? x)
        (define val
          (eval_ (second x) env))
        (push-right! result val))
       ((unquote-splicing? x)
        (define val
          (eval_ (second x) env))
        (unless (list? val)
          (error "Wrong type of argument: expected list"))
        (set! result (append result val)))
       ((quasiquote? x)
        (push-right! result x))
       ((list? x)
        (define val
          (quasiquote-helper x env))
        (push-right! result val))
       (else
        (push-right! result x))))
    result)))

;;; Evaluate a `(setq ...)` form.
(define (setq-special_ exp env)
  (define assignments '())
  (for ((i (range 1 (- (array-list-length exp) 1) 2)))
    (define sym
      (aget exp i))
    (define val
      (aget exp (+ i 1)))
    (define assignment
      `(set (quote ,sym) ,val))
    (push-right! assignments assignment))
  (define set-exp '())
  (if (> (array-list-length assignments) 1)
      (set! set-exp `(begin ,@assignments))
      (set! set-exp (first assignments)))
  (eval_ set-exp env))

;;; Evaluate a `(set ...)` form.
(define (set-special_ exp env)
  (define params
    (rest exp))
  (define sym
    (first params))
  (set! sym (eval_ sym env))
  (cond
   ((form? sym env aget_)
    (eval_ `(,aset_ ,@(rest sym) ,(second params)) env))
   (else
    (define val
      (eval_ (second params) env))
    (cond
     ((and (list? sym)
           (= (array-list-length sym) 2))
      (define prop
        (first sym))
      (define obj
        (eval_ (second sym) env))
      (cond
       ((symbol? prop)
        (define match)
        (when (set! match
                    (regexp-match (regexp "^\\.-(.*)$")
                                  (symbol->string prop)))
          (set! prop (second match))
          (set! (oget obj prop) val)))))
     (else
      (send env set sym val "variable")))
    val)))

;;; Evaluate a `(fset ...)` form.
(define (fset-special_ exp env)
  (define params
    (rest exp))
  (define sym
    (eval_ (first params) env))
  (define val
    (eval_ (second params) env))
  (send env set sym val "function")
  val)

;;; Evaluate a `(module ...)` form.
(define (module-special_ exp env)
  (tcall eval-t `(begin ,@(drop exp 3)) env))

;;; Evaluate a `(begin ...)` form.
(define (begin-special_ exp env)
  (begin-helper (rest exp) env undefined))

;;; Helper function for `begin-special_`.
(define (begin-helper expressions env val)
  (if (= (array-list-length expressions) 0)
      val
      (tcall begin-helper
             (rest expressions)
             env
             (tcall eval-t (first expressions) env))))

;;; Evaluate a `(let* ...)` form.
(define (let-star-special_ exp env)
  (define params
    (rest exp))
  (define var-exps
    (first params))
  (define body
    (rest params))
  (define bindings '())
  (define init-exps '())
  (for ((var-exp var-exps))
    (cond
     ((symbol? var-exp)
      (push-right! bindings
                   (list var-exp undefined "variable")))

     (else
      (push-right! bindings
                   (list (first var-exp) undefined "variable"))
      (define init-exp
        `(setq ,@var-exp))
      (push-right! init-exps init-exp))))
  (define let-env
    (new LispEnvironment bindings))
  (define combined-env
    (new EnvironmentStack let-env env))
  (define begin-exp
    `(begin ,@init-exps ,@body))
  (tcall eval-t begin-exp combined-env))

;;; Evaluate a `(let-values ...)` form.
(define (let-values-special_ exp env)
  (define bindings
    (second exp))
  (define body
    (drop exp 2))
  (define let-bindings '())
  (for ((i (range 0 (array-list-length bindings))))
    (define result
      (gensym
       (string-append
        "let-values-result-"
        (number->string
         (+ i 1)))))
    (define binding
      (aget bindings i))
    (define binding-vars
      (first binding))
    (define binding-exp
      (second binding))
    (define regular-bindings '())
    (define rest-binding undefined)
    (cond
     ((symbol? binding-vars)
      (set! rest-binding binding-vars))
     ((dotted-list? binding-vars)
      (define binding-list
        (flatten binding-vars))
      (set! regular-bindings
            (drop-right binding-list 1))
      (set! rest-binding
            (array-list-last binding-list)))
     (else
      (set! regular-bindings binding-vars)))
    (push-right! let-bindings (list result binding-exp))
    (for ((j (range 0 (array-list-length regular-bindings))))
      (push-right! let-bindings
                   (list (aget regular-bindings j)
                         `(aget ,result ,j))))
    (when rest-binding
      (push-right! let-bindings
                   (list rest-binding
                         `(nthcdr ,(array-list-length
                                    regular-bindings)
                                  ,result)))))
  (eval_ `(let* ,let-bindings
            ,@body)
         env))

;;; Evaluate a `(define-values ...)` form.
(define (define-values-special_ exp env)
  (define ids
    (second exp))
  (define val
    (third exp))
  (define regular-bindings '())
  (define rest-binding undefined)
  (define result)
  (cond
   ((symbol? ids)
    (set! rest-binding ids))
   ((dotted-list? ids)
    (define binding-list
      (flatten ids))
    (set! regular-bindings
          (drop-right binding-list 1))
    (set! rest-binding
          (array-list-last binding-list)))
   (else
    (set! regular-bindings ids)))
  (set! result (eval_ val env))
  (for ((i (range 0 (array-list-length regular-bindings))))
    (eval_ `(define ,(aget regular-bindings i)
              (quote ,(aget result i)))
           env))
  (when rest-binding
    (eval_ `(define ,rest-binding
              (quote ,(nthcdr (array-list-length
                               regular-bindings)
                              result)))
           env))
  undefined)

;;; Evaluate a `(set!-values ...)` form.
(define (set-values-special_ exp env)
  (define ids
    (second exp))
  (define val
    (third exp))
  (define regular-bindings '())
  (define rest-binding undefined)
  (define result)
  (cond
   ((symbol? ids)
    (set! rest-binding ids))
   ((dotted-list? ids)
    (define binding-list
      (flatten ids))
    (set! regular-bindings
          (drop-right binding-list 1))
    (set! rest-binding
          (array-list-last binding-list)))
   (else
    (set! regular-bindings ids)))
  (set! result (eval_ val env))
  (for ((i (range 0 (array-list-length regular-bindings))))
    (eval_ `(set! ,(aget regular-bindings i)
                  (quote ,(aget result i)))
           env))
  (when rest-binding
    (eval_ `(set! ,rest-binding
                  (quote ,(nthcdr (array-list-length
                                   regular-bindings)
                                  result)))
           env))
  undefined)

;;; Evaluate a `(define ...)` form.
(define (define-special_ exp env)
  (define name
    (second exp))
  (define body
    (drop exp 2))
  (cond
   ;; Function definition.
   ((array? name)
    (define name-and-params name)
    (define f-name
      (first name-and-params))
    (define params
      (rest name-and-params))
    (cond
     ;; Curried function definition.
     ((array? f-name)
      ;; Curry the function. (The following is more similar to
      ;; currying in Haskell than currying in Racket because the
      ;; whole function is curried, not just some arguments.)
      (define curried-name-and-params
        (send name-and-params flat Infinity))
      (define curried-name
        (first curried-name-and-params))
      (define curried-params
        (rest curried-name-and-params))
      (define curried-arity
        (array-list-length curried-params))
      (define curried-function-exp
        `(curry
          (lambda ,curried-params
            ,@body)
          ,curried-arity))
      (define val
        (eval_ curried-function-exp env))
      (send env set-local curried-name val)
      val)
     ;; Uncurried function definition.
     (else
      (define lambda-exp
        `(lambda ,params
           ,@body))
      (define val
        (eval_ lambda-exp env))
      (send env set-local f-name val)
      val)))
   ;; Class definition.
   ((and (= (array-list-length exp) 3)
         ;; (form? (third exp) env define-class_)
         (tagged-list? (third exp) 'define-class))
    (eval_ (define->define-class exp) env))
   ;; Variable definition.
   (else
    (define val-exp
      (third exp))
    (define val
      (eval_ val-exp env))
    (send env set-local name val)
    val)))

;;; Convert a `(define ... (class ...))` form to
;;; a `(define-class ...)` form.
(define (define->define-class node)
  (cond
   ((is-a? node Rose)
    (define superclass
      (send (send node get 2) get 1))
    (define superclass-exp
      (send superclass get-value))
    (define superclass-list
      (if (or (eq? superclass-exp 'object%)
              (eq? superclass-exp 'object)
              (eq? superclass-exp 'Object))
          '()
          (list superclass)))
    (transfer-comments
     node
     (make-rose
      `(define-class ,(send node get 1)
         ,(make-rose superclass-list)
         ,@(send (send node get 2) drop 2)))))
   (else
    (~> (define->define-class (make-rose node))
        (send _ get-value)))))

;;; Evaluate a `(define/public ...)` form.
(define (define-public-special_ exp env)
  (define-special_ exp env))

;;; Evaluate a `(define/generator ...)` form.
(define (define-generator-special_ exp env)
  (define-special_ exp env))

;;; Evaluate a `(define/async ...)` form.
(define (define-async-special_ exp env)
  (define-special_ exp env))

;;; Evaluate a `(defmacro ...)` form.
(define (defmacro-special_ exp env)
  (define name
    (second exp))
  (when (list? name)
    (set! name (first name)))
  (define macro-fn
    (defmacro->fn exp env))
  (send env set name macro-fn "macro")
  ;; name
  macro-fn)

;;; Create a macro function on the basis of a
;;; `(defmacro ...)` form.
(define (defmacro->fn exp env)
  (define macro-fn
    (defmacro->lambda-form exp))
  (eval_ macro-fn env))

;;; Create a `(lambda ...)` form for a macro function
;;; on the basis of a `(defmacro ...)` form.
(define (defmacro->lambda-form exp)
  (define name
    (second exp))
  (define args
    (third exp))
  (define body
    (drop exp 3))
  (define env 'env)
  (define macro-args '())
  (when (list? name)
    (set! args (rest name))
    (set! name (first name))
    (set! body (drop exp 2)))
  (cond
   ((list? args)
    (for ((i (range 0 (array-list-length args))))
      (define arg
        (aget args i))
      (cond
       ((eq arg '&environment)
        (set! env (aget args (+ i 1)))
        (set! i (+ i 2)))
       (else
        (push-right! macro-args arg)))))
   (else
    (set! macro-args args)))
  (cond
   ((null? macro-args)
    `(lambda (exp ,env)
       ,@body))
   (else
    `(lambda (exp ,env)
       (let-values ((,macro-args (rest exp)))
         ,@body)))))

;;; Evaluate a `(define-macro ...)` form.
(define (define-macro-special_ exp env)
  (define name
    (car (second exp)))
  (define macro-fn
    (define-macro->fn exp env))
  (send env set name macro-fn "macro")
  name)

;;; Create a macro function on the basis of a
;;; `(define-macro ...)` form.
(define (define-macro->fn exp env)
  (define macro-fn
    (define-macro->lambda-form exp))
  (eval_ macro-fn env))

;;; Create a `(lambda ...)` form for a macro function
;;; on the basis of a `(define-macro ...)` form.
(define (define-macro->lambda-form exp)
  (define name-and-args
    (second exp))
  (define name
    (car name-and-args))
  (define args
    (cdr name-and-args))
  (define body
    (drop exp 2))
  (define env 'env)
  (define macro-args '())
  (cond
   ((list? args)
    (for ((i (range 0 (array-list-length args))))
      (define arg
        (aget args i))
      (cond
       ((eq arg '&environment)
        (set! env (aget args (+ i 1)))
        (set! i (+ i 2)))
       (else
        (push-right! macro-args arg)))))
   (else
    (set! macro-args args)))
  (cond
   ((null? macro-args)
    `(lambda (exp ,env)
       ,@body))
   (else
    `(lambda (exp ,env)
       (let-values ((,macro-args (rest exp)))
         ,@body)))))

;;; Evaluate a `(for ...)` form.
(define (for-special_ exp env)
  (define decls
    (second exp))
  (define body
    (drop exp 2))
  (define-values (decl1)
    decls)
  (define-values (sym values-expr)
    decl1)
  (define values
    (eval_ values-expr env))
  (define result undefined)
  (try
    (for ((value values))
      (try
        (set! result
              (eval_ `(let ((,sym ,value))
                        ,@body)
                     env))
        (catch ContinueException e)))
    (catch BreakException e
      (set! result undefined)))
  result)

;;; Evaluate a `(js/while ...)` form.
(define (js-while-special_ exp env)
  (define test
    `(truep ,(second exp)))
  (define body
    (begin-wrap (drop exp 2)))
  (define result undefined)
  (try
    (while (eval_ test env)
      (try
        (set! result (eval_ body env))
        (catch ContinueException e)))
    (catch BreakException e
      (set! result undefined)))
  result)

;;; Evaluate a `(js/do-while ...)` form.
(define (js-do-while-special_ exp env)
  (define body (second exp))
  (define test (third exp))
  (define begin-exp
    `(begin
       ,body
       (while ,test
         ,body)))
  (eval_ begin-exp env))

;;; Evaluate a `(break)` form.
(define (break-special_ exp env)
  (throw (new BreakException)))

;;; Evaluate a `(continue)` form.
(define (continue-special_ exp env)
  (throw (new ContinueException)))

;;; Evaluate a `(yield ...)` form.
(define (yield-special_ exp env)
  (define val
    (eval_ (second exp) env))
  val)

;;; Evaluate a `(return ...)` form.
(define (return-special_ exp env)
  (define val
    (eval_ (second exp) env))
  (throw (new ReturnException val)))

;;; Evaluate a `(throw ...)` form.
(define (throw-special_ exp env)
  (define val
    (eval_ (second exp) env))
  (throw val))

;;; Evaluate an `(async ...)` form.
(define (async-special_ exp env)
  (async
   (lambda args
     (apply (eval_ (second exp) env)
            args))))

;;; Evaluate an `(await ...)` form.
(define await-special_
  (async
   (lambda (exp env)
     (await (eval_ (second exp) env)))))

;;; Evaluate a `(lambda ...)` form.
(define (lambda-special_ exp env)
  (js-function-special_ exp env))

;;; Evaluate a `(js/function ...)` form.
(define (js-function-special_ exp env)
  (define f
    (js/function
        args
      (define let-exp
        (lambda->let exp args))
      (define result)
      (try
        (set! result (eval_ let-exp env))
        (catch ReturnException e
          (set! result (get-field value e))))
      result))
  (set-field! lisp-info f (list exp env))
  f)

;;; Evaluate a `(js/arrow ...)` form.
(define (js-arrow-special_ exp env)
  (define f
    (js/arrow
        args
      (define let-exp
        (lambda->let exp args))
      (define result)
      (try
        (set! result (eval_ let-exp env))
        (catch ReturnException e
          (set! result (get-field value e))))
      result))
  (set-field! lisp-info f (list exp env))
  f)

;;; Evaluate a `(cond ...)` form.
(define (cond-special_ exp env)
  (cond
   ((<= (array-list-length exp) 1)
    #f)
   (else
    (define clause
      (second exp))
    (define clauses
      (drop exp 2))
    (define condition
      (first clause))
    (define then-expr
      (begin-wrap
       (rest clause)))
    (tcall cond-helper
           (or (eq? condition 'else)
               (tcall eval-t `(truep ,condition) env))
           then-expr
           clauses
           env))))

;;; Helper function for `cond-special_`.
(define (cond-helper condition then-expr clauses env)
  (cond
   (condition
    (tcall eval-t then-expr env))
   ((= (array-list-length clauses) 0)
    undefined)
   (else
    (define clause1
      (first clauses))
    (define clauses1
      (rest clauses))
    (define condition1
      (first clause1))
    (define then-expr-1
      (begin-wrap (rest clause1)))
    (tcall cond-helper
           (or (eq? condition1 'else)
               (tcall eval-t `(truep ,condition1) env))
           then-expr-1
           clauses1
           env))))

;;; Evaluate an `(and ...)` form.
(define (and-special_ exp env)
  (define params
    (rest exp))
  (define result #t)
  (for ((operand params))
    (set! result (eval_ operand env))
    (unless (eval_ `(truep (quote ,result)) env)
      (return #f)))
  result)

;;; Evaluate an `(or ...)` form.
(define (or-special_ exp env)
  (define params
    (rest exp))
  (define result #f)
  (for ((operand params))
    (set! result (eval_ operand env))
    (when (eval_ `(truep (quote ,result)) env)
      (return result)))
  result)

;;; Evaluate a `(send ...)` form.
(define (send-special_ exp env)
  (define obj
    (second exp))
  (set! obj (eval_ obj env))
  (define method
    (third exp))
  (when (quote? method)
    (set! method (eval_ method env)))
  (define args
    (drop exp 3))
  (set! args
        (map (lambda (x)
               (eval_ x env))
             args))
  (apply send-method obj method args))

;;; Evaluate a `(send/apply ...)` form.
(define (send-apply-special_ exp env)
  (define obj
    (second exp))
  (set! obj (eval_ obj env))
  (define method
    (third exp))
  (when (quote? method)
    (set! method (eval_ method env)))
  (define args
    (drop exp 3))
  (set! args
        (map (lambda (x)
               (eval_ x env))
             args))
  (when (> (array-list-length args) 0)
    (set! args (append (drop-right args 1)
                       (array-list-last args))))
  (apply send-method obj method args))

;;; Call a method on an object.
;;;
;;; Helper function for `send-special_` and `send-apply-special_`.
(define (send-method . args)
  (define-values (obj method . rest-args)
    args)
  (cond
   ((eq? (type-of method) "symbol")
    (apply send-method obj (symbol->string method) rest-args))
   ((eq? (type-of method) "string")
    (apply send-method obj (oget obj method) rest-args))
   ((is-a? method Function)
    (send/apply method call obj rest-args))
   (else
    (throw (new Error
                (string-append "Not a method: "
                               method))))))

;;; Evaluate a `(. ...)` form.
(define (dot-special_ exp env)
  (define obj
    (eval_ (second exp) env))
  (define method
    (third exp))
  (define field)
  (define match)
  (cond
   ((and (symbol? method)
         (set! match
               (regexp-match (regexp "^-(.*)$")
                             (symbol->string method))))
    (set! field (second match))
    (eval_ `(get-field ,(string->symbol field)
                       ,obj)
           env))
   (else
    (send-special_ exp env))))

;;; Evaluate a `(get-field ...)` form.
(define (get-field-special_ exp env)
  (define field
    (second exp))
  (define field-name
    (symbol->string field))
  (define obj
    (third exp))
  (~> (eval_ obj env)
      (oget _ field-name)))

;;; Evaluate a `(js/optional-chaining ...)` form.
(define (js-optional-chaining-special_ exp env)
  (define obj
    (second exp))
  (define field
    (third exp))
  (define field-name
    (symbol->string field))
  (~> (eval_ obj env)
      (oget _ field-name)))

;;; Evaluate a `(set-field! ...)` form.
(define (set-field-special_ exp env)
  (define field
    (second exp))
  (define obj
    (third exp))
  (define val
    (fourth exp))
  (oset! (eval_ obj env)
         (symbol->string field)
         (eval_ val env)))

;;; Evaluate a `(define-class ...)` form.
(define (define-class-special_ exp env)
  (define fields '())
  (define methods '())
  (define constructors
    (make-hash))
  (define constructor
    (lambda (this . args)
      ;; Initialize fields.
      (for ((field fields))
        (define name
          (symbol->string (second field)))
        (define exp
          (third field))
        (set! (oget this name)
              (eval_ `(let ((this (quote ,this)))
                        ,exp)
                     env)))
      (define arity
        (array-list-length args))
      (define constructor-fn
        (send constructors get arity))
      (when (procedure? constructor-fn)
        (send/apply constructor-fn call this args))))
  (define params
    (rest exp))
  (define definitions params)
  ;; Get name of the class and, if specified, the base class it extends.
  ;; If no class name is specified, an anonymous class is created.
  (define class-name-symbol
    (first params))
  (define class-name
    (if (symbol? class-name-symbol)
        (symbol->string class-name-symbol)
        ""))
  (define base-class undefined)
  (unless (eq? class-name "")
    (set! definitions (rest definitions)))
  (define super-classes
    (first definitions))
  (when (and (list? super-classes)
             (not (tagged-list? super-classes 'define)))
    ;; The first form is a list of superclasses.
    (set! definitions (rest definitions))
    ;; JavaScript supports single inheritance only,
    ;; so only the first class is used.
    (when (> (array-list-length super-classes) 0)
      (set! base-class (eval_ (first super-classes) env))))
  (when base-class
    ;; <https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/Inheritance#setting_teachers_prototype_and_constructor_reference>
    (set! (oget constructor "prototype")
          (send Object create (oget base-class "prototype"))))
  ;; Sort field definitions from method definitions.
  (for ((definition definitions))
    (if (symbol? (second definition))
        (push-right! fields definition)
        (push-right! methods definition)))
  (for ((method methods))
    (define def-params
      (rest method))
    (define def-name-and-args
      (first def-params))
    (define def-name
      (symbol->string
       (first def-name-and-args)))
    (define def-args
      (rest def-name-and-args))
    (define def-body
      (rest def-params))
    (define arity
      (array-list-length def-args))
    ;; Create a method function that binds JavaScript's
    ;; `this` value to the Lisp symbol `this`.
    (define method-fn
      (lambda (this . args)
        (define var-exps '())
        (for ((i (range 0 arity)))
          (define arg-exp
            (aget def-args i))
          (define name
            (if (list? arg-exp)
                (first arg-exp)
                arg-exp))
          (define value
            (if (>= i (array-list-length args))
                undefined
                (aget args i)))
          (define var-exp
            `(,name (quote ,value)))
          (push-right! var-exps var-exp))
        (define this-exp
          `(this (quote ,this)))
        (push-right! var-exps this-exp)
        (define let-exp
          `(let* ,var-exps
             ,@def-body))
        (eval_ let-exp env)))
    ;; A form on the form `(define (constructor ...) ...)`
    ;; or `(define/public (constructor ...) ...)` is
    ;; understood to define a constructor.
    ;; Cf. the constructor syntax of TypeScript:
    ;; <https://www.typescriptlang.org/docs/handbook/2/classes.html#constructors>
    (if (or (eq? def-name "constructor")
            (eq? def-name class-name))
        (send constructors set arity method-fn)
        (set! (oget (get-field prototype constructor) def-name)
              method-fn)))
  (unless (eq? class-name "")
    (send env set class-name-symbol constructor "variable"))
  constructor)

;;; Evaluate a `(try ...)` form.
(define (try-special_ exp env)
  (define body-clauses '())
  (define catch-clauses '())
  (define finally-clauses '())
  (define result undefined)
  (define body)
  (for ((x (drop exp 1)))
    (cond
     ((tagged-list? x 'catch)
      (push-right! catch-clauses x))
     ((tagged-list? x 'finally)
      (push-right! finally-clauses x))
     (else
      (push-right! body-clauses x))))
  (set! body
        (if (= (array-list-length body-clauses) 1)
            (first body-clauses)
            `(begin ,@body-clauses)))
  (try
    (set! result (eval_ body env))
    (catch Object err
      (for ((clause catch-clauses))
        (when (is-a? err (eval_ (second clause) env))
          (set! result
                (eval_ `(let ((,(third clause) (quote ,err)))
                          ,@(drop clause 3))
                       env))
          (break))))
    (finally
      (when (> (array-list-length finally-clauses) 0)
        (eval_ `(begin
                  ,@(drop (first finally-clauses) 1))
               env))))
  result)

;;; Evaluate a `(provide ...)` form.
(define (provide-special_ exp env)
  undefined)

;;; Evaluate a `(require ...)` form.
(define (require-special_ exp env)
  undefined)

;;; Evaluate an `(ann ...)` form.
(define (ann-special_ exp env)
  (eval_ (second exp) env))

;;; Evaluate a `(colon ...)` form.
(define (colon-special_ exp env)
  undefined)

;;; Evaluate a `(define-type ...)` form.
(define (define-type-special_ exp env)
  undefined)

;;; Evaluate a `(let-js-obj ...)` form.
(define (let-js-obj-special_ exp env)
  undefined)

;;; Evaluate a `(define-js-obj ...)` form.
(define (define-js-obj-special_ exp env)
  undefined)

;;; Evaluate a `(set!-js-obj ...)` form.
(define (set-js-obj-special_ exp env)
  undefined)

;;; Evaluate a `(let-env ...)` form.
(define (let-env-special_ exp env)
  ;; Legacy function.
  (define params
    (rest exp))
  (define body
    (rest params))
  (define let-env
    (eval_ (first params) env))
  (define combined-env
    (new EnvironmentStack let-env env))
  (eval_ `(begin ,@body) combined-env))

;;; Evaluate a `(macrop ...)` form.
;;;
;;; Whether `macro` is a macro in the environment `env`.
;;; `macro` may be a symbol, a string, or a macro function.
(define (macrop-special_ val)
  (eval_ `(macrop ,val) (current-environment)))

;;; Evaluate an `(nlambda ...)` form.
(define (nlambda-special_ exp env)
  (define f
    (lambda-special_ exp env))
  (set-field! fexpr f #t)
  f)

(provide
  and-special_
  ann-special_
  async-special_
  await-special_
  begin-special_
  break-special_
  colon-special_
  cond-special_
  continue-special_
  define-async-special_
  define-class-special_
  define-generator-special_
  define-js-obj-special_
  define-macro-special_
  define-public-special_
  define-special_
  define-type-special_
  define-values-special_
  defmacro-special_
  dot-special_
  for-special_
  fset-special_
  get-field-special_
  js-arrow-special_
  js-do-while-special_
  js-function-special_
  js-optional-chaining-special_
  js-while-special_
  lambda-special_
  let-env-special_
  let-js-obj-special_
  let-star-special_
  let-values-special_
  macrop-special_
  module-special_
  nlambda-special_
  or-special_
  provide-special_
  quasiquote-special_
  quote-special_
  require-special_
  return-special_
  send-apply-special_
  send-special_
  set-field-special_
  set-js-obj-special_
  set-special_
  set-values-special_
  setq-special_
  throw-special_
  try-special_
  yield-special_)
