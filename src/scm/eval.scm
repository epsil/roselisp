;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Evaluator
;;;
;;; S-expression evaluator and ESTree evaluator.
;;;
;;; ## Description
;;;
;;; Implements two evaluators: one for S-expressions (raw or wrapped
;;; in rose trees) and one for ESTree nodes.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(require (only-in "./curry"
                  dashify))
(require (only-in "./estree"
                  ArrayPattern
                  AssignmentExpression
                  BinaryExpression
                  BlockStatement
                  Literal
                  Program
                  VariableDeclaration
                  VariableDeclarator
                  estree-quote
                  estree-type
                  estree-type?
                  estree?
                  get-estree-field))
(require (only-in "./env"
                  Environment
                  EnvironmentStack
                  LispEnvironment
                  TypedEnvironment
                  current-environment
                  default-environment
                  empty-environment
                  environment-frames
                  extend-environment
                  link-environment-frames
                  make-environment
                  with-environment
                  with-environment-f))
(require (only-in "./exception"
                  BreakException
                  ContinueException
                  YieldException
                  ReturnException))
(require (only-in "./javascript"
                  js/eval_))
(require (only-in "./printer"
                  print-estree
                  write-to-string))
(require (only-in "./procedures"
                  fexpr-type?
                  fexpr?
                  macro-type?
                  macro?
                  procedure-type?
                  special-type?
                  undefined-type?
                  variable-type?))
(require (only-in "./rose"
                  syntax->datum
                  syntax?))
(require (only-in "./thunk"
                  InternalPromise))
(require (only-in "./util"
                  tagged-list?))

;;; The default evaluator.
(define default-evaluator eval1)

;;; Evaluate a S-expression `exp` in the Lisp environment `env`.
;;;
;;; `env` may be an {@link Environment}, or it may be an
;;; {@link Evaluator}. In the latter case, evaluation takes place
;;; by calling its `.eval()` method.
;;;
;;; This function is more fundamental and basic than `interpret`, which
;;; is probably what you want. `interpret` stacks the given environment
;;; on top of a Lisp environment, while `eval_` is a low-level function
;;; that performs no such stacking.
(define eval_
  (dashify
   (lambda (exp (env #u) (options (js/obj)))
     (define evaluator
       (or (oget options :evaluator)
           default-evaluator))
     (call-evaluator evaluator exp env options))))

;;; Call an evaluator on an expression.
(define (call-evaluator evaluator
                        exp
                        (env #u)
                        (options (js/obj)))
  (cond
   ((is-a? evaluator Evaluator)
    (send evaluator eval exp env options))
   ((procedure? evaluator)
    (evaluator exp env options))
   (else
    #u)))

;;; Whether something is an evaluator.
(define (evaluator? obj)
  (or (procedure? obj)
      (is-a? obj Evaluator)))

;;; Evaluator class.
(define-class Evaluator ()
  ;;; The simplest possible evaluator is the
  ;;; identity function.
  (define/public (eval exp (env #u) (options (js/obj)))
    exp))

;;; Lisp-1 evaluator function.
(define (eval1 exp env (options (js/obj)))
  (cond
   ((syntax? exp)
    (eval-syntax exp env options))
   (else
    (eval-sexp exp env options))))

;;; Evaluate an S-expression.
;;;
;;; This is a case-by-case function.
(define (eval-sexp exp env (options (js/obj)))
  (with-environment
   env
   (cond
    ((is-a? exp InternalPromise)
     (eval-sexp (send exp force) env options))
    ((null? exp)
     exp)
    ((list? exp)
     (define-values (op . args)
       exp)
     (cond
      ((symbol? op)
       (define name
         (symbol->string op))
       (define match)
       (cond
        ((set! match
               (regexp-match (regexp "^\\.(.+)$") name))
         ;; Method call expression
         (define method
           (second match))
         (define-values
             (obj . fargs) args)
         (define dot-exp
           `(,(string->symbol ".")
             ,obj
             ,(string->symbol method)
             ,@fargs))
         (eval-sexp dot-exp env options))
        (else
         (define-values (f binding-type)
           (send env get-typed-value op))
         (cond
          ((macro-type? binding-type)
           ;; Macros are implemented with a macro function that
           ;; has the signature `(exp, env) => value`. The arguments
           ;; to the macro are *not* evaluated, but the macro's
           ;; return value---the macro expansion---*is* evaluated:
           ;; that is, it is pushed back on the expressions stack
           ;; for further evaluation.
           (define expansion
             (f exp env))
           (eval-sexp expansion env options))
          ((fexpr-type? binding-type)
           ;; A fexpr is a function that receives its arguments
           ;; unevaluated, like a macro. However, unlike a macro,
           ;; the return value is not re-evaluated---it is simply
           ;; returned.
           (apply f args))
          ((special-type? binding-type)
           ;; Special form
           (f exp env))
          ((or (procedure-type? binding-type)
               (and (variable-type? binding-type)
                    (procedure? f)))
           ;; Function call
           (cond
            ((fexpr? f)
             (apply f args))
            ;; Macro function
            ((macro? f)
             ;; (eq? (get-field ftype f)
             ;;      "macro")
             (define expansion
               (f exp env))
             (eval-sexp expansion env options))
            (else
             ;; Apply `f` to evaluated arguments
             (apply f
                    (map (lambda (arg)
                           (eval-sexp arg env options))
                         args)))))))))
      ((not op)
       #u)
      ((procedure? op)
       ;; `(<fn> ...)` call. The first element is a
       ;; function object. If it is a fexpr call, the function
       ;; is called with its arguments unevaluated. Otherwise,
       ;; the arguments have to be evaluated first.
       (define f op)
       (cond
        ((or (= (length args) 0)
             (fexpr? f))
         ;; Fexpr call. The function is called with its
         ;; arguments unevaluated.
         (apply f args))
        (else
         ;; Regular call. The arguments are evaluated,
         ;; and the values are passed to the function.
         (apply f
                (map (lambda (arg)
                       (eval-sexp arg env options))
                     args)))))
      (else
       ;; `((...) ...)` call. The first element is a expression
       ;; that has to be evaluated before function application
       ;; can proceed.
       (eval-sexp (cons (eval-sexp op env options)
                        args)
                  env
                  options))))
    ((keyword? exp)
     ;; Keyword
     exp)
    ((symbol? exp)
     ;; Variable
     (define name
       (symbol->string exp))
     (define binding
       (send env get-typed-value exp))
     (cond
      (binding
       (define-values (value)
         binding)
       value)
      (else
       (error
        (string-append
         "Could not find symbol: "
         (symbol->string exp))))))
    ((string? exp)
     ;; String
     exp)
    ((estree? exp)
     ;; ESTree
     (eval-estree exp env))
    (else
     ;; Self-evaluating value
     exp))))

;;; Evaluate a syntax object.
(define (eval-syntax node env (options (js/obj)))
  (~> node
      (syntax->datum _)
      (eval-sexp _ env options)))

;;; Evaluate an [ESTree][github:estree] node
;;; (i.e., a JavaScript [AST][w:Abstract syntax tree]).
;;;
;;; [github:estree]: https://github.com/estree/estree
;;; [w:Abstract syntax tree]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
(define (eval-estree node env (options (js/obj)))
  (cond
   ((not node)
    #u)
   ((is-a? node InternalPromise)
    (eval-estree (send node force) env options))
   (else
    (define type_
      (estree-type node))
    (define evaluator
      (send eval-estree-map get type_))
    (cond
     (evaluator
      (with-environment
       env
       (evaluator node env options)))
     (else
      #u)))))

;;; Evaluate an ESTree [`Program`][estree:program] node
;;; (i.e., a JavaScript program).
;;;
;;; [estree:program]: https://github.com/estree/estree/blob/master/es5.md#programs
(define (eval-estree-program node env (options (js/obj)))
  (define body
    (get-estree-field "body" node))
  (define result #u)
  (for ((statement body))
    (set! result
          (eval-estree statement env options)))
  result)

;;; Evaluate an ESTree [`BlockStatement`][estree:blockstatement] node.
;;;
;;; [estree:blockstatement]: https://github.com/estree/estree/blob/master/es5.md#blockstatement
(define (eval-estree-block-statement node env (options (js/obj)))
  (define env1
    (extend-environment (new LispEnvironment)
                        env))
  (with-environment
   env1
   (try
     (catch Error e))
   (eval-estree-program node env1 options)))

;;; Evaluate an ESTree [`SequenceExpression`][estree:sequenceexpression] node.
;;;
;;; [estree:sequenceexpression]: https://github.com/estree/estree/blob/master/es5.md#sequenceexpression
(define (eval-estree-sequence-expression node env (options (js/obj)))
  (define expressions
    (get-estree-field "expressions" node))
  (define program
    (new Program expressions))
  (eval-estree-program program env options))

;;; Evaluate an ESTree [`Literal`][estree:literal] node.
;;;
;;; [estree:literal]: https://github.com/estree/estree/blob/master/es5.md#literal
(define (eval-estree-literal node env (options (js/obj)))
  (get-estree-field "value" node))

;;; Evaluate an ESTree [`Identifier`][estree:identifier] node.
;;;
;;; [estree:identifier]: https://github.com/estree/estree/blob/master/es5.md#identifier
(define (eval-estree-identifier node env (options (js/obj)))
  (define name
    (get-estree-field "name" node))
  (cond
   ;; JavaScript's `undefined` is parsed as an `Identifier`
   ;; (and not as a `Literal`, as one might expect), so it
   ;; has to be handled here.
   ((eq? name "undefined")
    #u)
   (else
    (define sym
      (string->symbol name))
    (send env get sym))))

;;; Evaluate an ESTree [`MemberExpression`][estree:memberexpression] node.
;;;
;;; [estree:memberexpression]: https://github.com/estree/estree/blob/master/es5.md#memberexpression
(define (eval-estree-member-expression node env (options (js/obj)))
  (define object
    (get-estree-field "object" node))
  (define property
    (get-estree-field "property" node))
  (define computed
    (get-estree-field "computed" node))
  (define optional
    (get-estree-field "optional" node))
  (define object-val
    (eval-estree object env options))
  (define property-val
    (cond
     (computed
      (eval-estree property env options))
     (else
      (get-estree-field "name" property))))
  (cond
   ((and optional
         (undefined? object-val))
    #u)
   (else
    (oget object-val property-val))))

;;; Evaluate an ESTree [`CallExpression`][estree:callexpression] node.
;;;
;;; [estree:callexpression]: https://github.com/estree/estree/blob/master/es5.md#callexpression
(define (eval-estree-call-expression node env (options (js/obj)))
  ;; TODO: Handle `js/eval` calls directly?
  (define callee
    (get-estree-field "callee" node))
  (define args
    (get-estree-field "arguments" node))
  (define args-vals
    (eval-estree-array-expression-helper
     args env options))
  (cond
   ((estree-type? callee "MemberExpression")
    (define method-val
      (eval-estree callee env options))
    (cond
     ((and (undefined? method-val)
           (get-estree-field "optional" callee))
      #u)
     (else
      (define obj
        (get-estree-field "object" callee))
      (define obj-val
        (eval-estree obj env options))
      (send method-val apply obj-val args-vals))))
   (else
    (define f callee)
    (define f-val
      (eval-estree f env options))
    (cond
     ((and (undefined? f-val)
           (get-estree-field "optional" node))
      #u)
     (else
      (apply f-val args-vals))))))

;;; Evaluate an ESTree [`BreakStatement`][estree:breakstatement] node.
;;;
;;; [estree:breakstatement]: https://github.com/estree/estree/blob/master/es5.md#breakstatement
(define (eval-estree-break-statement node env (options (js/obj)))
  (throw (new BreakException)))

;;; Evaluate an ESTree [`ContinueStatement`][estree:continuestatement] node.
;;;
;;; [estree:continuestatement]: https://github.com/estree/estree/blob/master/es5.md#continuestatement
(define (eval-estree-continue-statement node env (options (js/obj)))
  (throw (new ContinueException)))

;;; Evaluate an ESTree [`YieldExpression`][estree:yieldexpression] node.
;;;
;;; [estree:yieldexpression]: https://github.com/estree/estree/blob/master/es2015.md#yieldexpression
(define (eval-estree-yield-expression node env (options (js/obj)))
  (define argument
    (get-estree-field "argument" node))
  (define argument-val
    (eval-estree argument env options))
  (throw (new YieldException argument-val)))

;;; Evaluate an ESTree [`ReturnStatement`][estree:returnstatement] node.
;;;
;;; [estree:returnstatement]: https://github.com/estree/estree/blob/master/es5.md#returnstatement
(define (eval-estree-return-statement node env (options (js/obj)))
  (define argument
    (get-estree-field "argument" node))
  (define argument-val
    (eval-estree argument env options))
  (throw (new ReturnException argument-val)))

;;; Evaluate an ESTree [`ThrowStatement`][estree:throwstatement] node.
;;;
;;; [estree:throwstatement]: https://github.com/estree/estree/blob/master/es5.md#throwstatement
(define (eval-estree-throw-statement node env (options (js/obj)))
  (define argument
    (get-estree-field "argument" node))
  (define argument-val
    (eval-estree argument env options))
  (throw argument-val))

;;; Evaluate an ESTree [`ThisExpression`][estree:thisexpression] node.
;;;
;;; [estree:thisexpression]: https://github.com/estree/estree/blob/master/es5.md#thisexpression
(define (eval-estree-this-expression node env (options (js/obj)))
  current-this-value)

;;; Evaluate an ESTree [`NewExpression`][estree:newexpression] node.
;;;
;;; [estree:newexpression]: https://github.com/estree/estree/blob/master/es2015.md#expressions
(define (eval-estree-new-expression node env (options (js/obj)))
  (define callee
    (get-estree-field "callee" node))
  (define args
    (get-estree-field "arguments" node))
  (define callee-val
    (eval-estree callee env options))
  (define args-vals
    (map (lambda (x)
           (eval-estree x env options))
         args))
  (apply new callee-val args-vals))

;;; Evaluate an ESTree [`ObjectExpression`][estree:objectexpression] node.
;;;
;;; [estree:objectexpression]: https://github.com/estree/estree/blob/master/es5.md#objectexpression
(define (eval-estree-object-expression node env (options (js/obj)))
  (define result
    (js/obj))
  (define properties
    (get-estree-field "properties" node))
  (for ((prop properties))
    (cond
     ((estree-type? prop "SpreadElement")
      (define argument
        (get-estree-field "argument" prop))
      (define argument-val
        (eval-estree argument env options))
      (set! result (js/obj-append result argument-val)))
     (else
      (define key
        (get-estree-field "key" prop))
      (define value
        (get-estree-field "value" prop))
      (define key-val
        (cond
         ((estree-type? key "Identifier")
          (get-estree-field "name" key))
         (else
          (get-estree-field "value" key))))
      (define value-val
        (eval-estree value env options))
      (oset! result key-val value-val))))
  result)

;;; Evaluate an ESTree [`VariableDeclaration`][estree:variabledeclaration] node.
;;;
;;; [estree:variabledeclaration]: https://github.com/estree/estree/blob/master/es5.md#variabledeclaration
(define (eval-estree-variable-declaration node env (options (js/obj)))
  (define declarations
    (get-estree-field "declarations" node))
  (for ((x declarations))
    (eval-estree x env options))
  #u)

;;; Evaluate an ESTree [`VariableDeclarator`][estree:variabledeclarator] node.
;;;
;;; [estree:variabledeclarator]: https://github.com/estree/estree/blob/master/es5.md#variabledeclarator
(define (eval-estree-variable-declarator node env (options (js/obj)))
  (define id
    (get-estree-field "id" node))
  (define init
    (get-estree-field "init" node))
  (define assignment
    (new AssignmentExpression "=" id init))
  (eval-estree-assignment-expression-helper
   assignment
   env
   options
   (js/obj :local #t)))

;;; Evaluate an ESTree [`AssignmentExpression`][estree:assignmentexpression] node.
;;;
;;; [estree:assignmentexpression]: https://github.com/estree/estree/blob/master/es5.md#assignmentexpression
(define (eval-estree-assignment-expression node env (options (js/obj)))
  (eval-estree-assignment-expression-helper node env options))

;;; Evaluate an ESTree [`ArrayExpression`][estree:arrayexpression] node.
;;;
;;; [estree:arrayexpression]: https://github.com/estree/estree/blob/master/es5.md#arrayexpression
(define (eval-estree-array-expression node env (options (js/obj)))
  (define elements
    (get-estree-field "elements" node))
  (eval-estree-array-expression-helper
   elements env options))

;;; Evaluate an ESTree [`ArrayPattern`][estree:arraypattern] node.
;;;
;;; [estree:arraypattern]: https://github.com/estree/estree/blob/master/es2015.md#arraypattern
(define (eval-estree-array-pattern node env (options (js/obj)))
  (eval-estree-array-expression node env options))

;;; Evaluate an ESTree [`RestElement`][estree:restelement] node.
;;;
;;; [estree:restelement]: https://github.com/estree/estree/blob/master/es2015.md#restelement
(define (eval-estree-rest-element node env (options (js/obj)))
  (define argument
    (get-estree-field "argument" node))
  (eval-estree argument env options))

;;; Evaluate an ESTree [`SpreadElement`][estree:spreadelement] node.
;;;
;;; [estree:spreadelement]: https://github.com/estree/estree/blob/master/es2015.md#expressions
(define (eval-estree-spread-element node env (options (js/obj)))
  (define argument
    (get-estree-field "argument" node))
  (eval-estree argument env options))

;;; Evaluate an ESTree [`ExpressionStatement`][estree:expressionstatement] node.
;;;
;;; [estree:expressionstatement]: https://github.com/estree/estree/blob/master/es5.md#expressionstatement
(define (eval-estree-expression-statement node env (options (js/obj)))
  (define expression
    (get-estree-field "expression" node))
  (eval-estree expression env options))

;;; Evaluate an ESTree [`FunctionDeclaration`][estree:functiondeclaration] node.
;;;
;;; [estree:functiondeclaration]: https://github.com/estree/estree/blob/master/es5.md#functiondeclaration
(define (eval-estree-function-declaration node env (options (js/obj)))
  (define id
    (get-estree-field "id" node))
  (define name
    (string->symbol (get-estree-field "name" id)))
  (define f
    (eval-estree-function-expression node env options))
  (send env set-local! name f '(->* :rest Any Any))
  #u)

;;; Evaluate an ESTree [`FunctionExpression`][estree:functionexpression] node.
;;;
;;; <https://docs.esprima.org/en/latest/syntax-tree-format.html#function-expression>
;;;
;;; [estree:functionexpression]: https://github.com/estree/estree/blob/master/es5.md#functionexpression
(define (eval-estree-function-expression node env (options (js/obj)))
  (eval-estree-function-expression-helper
   node
   env
   options))

;;; Evaluate an ESTree [`ArrowFunctionExpression`][estree:arrowfunctionexpression] node.
;;;
;;; [estree:arrowfunctionexpression]: https://github.com/estree/estree/blob/master/es2015.md#arrowfunctionexpression
(define (eval-estree-arrow-function-expression node env (options (js/obj)))
  (eval-estree-function-expression-helper
   node
   env
   options
   (js/obj :arrow #t)))

;;; Evaluate an ESTree [`UnaryExpression`][estree:unaryexpression] node.
;;;
;;; [estree:unaryexpression]: https://github.com/estree/estree/blob/master/es5.md#unaryexpression
(define (eval-estree-unary-expression node env (options (js/obj)))
  (define operator
    (get-estree-field "operator" node))
  (define prefix
    (get-estree-field "prefix" node))
  (define argument
    (get-estree-field "argument" node))
  (cond
   ((eq? operator "!")
    (not (eval-estree argument env options)))
   ((eq? operator "+")
    (eval-estree argument env options))
   ((eq? operator "-")
    (- (eval-estree argument env options)))
   ((or (eq? operator "++")
        (eq? operator "--"))
    (define is-add
      (eq? operator "++"))
    (define assignment
      (new AssignmentExpression
           "="
           argument
           (new BinaryExpression
                (if is-add
                    "+"
                    "-")
                argument
                (new Literal 1))))
    (cond
     (prefix
      (eval-estree assignment env options))
     (else
      (define val
        (eval-estree assignment env options))
      (define val-orig
        (cond
         (is-add
          (- val 1))
         (else
          (+ val 1))))
      val-orig)))
   ((eq? operator "typeof")
    (type-of (eval-estree argument env options)))
   (else
    #u)))

;;; Evaluate an ESTree [`UpdateExpression`][estree:updateexpression] node.
;;;
;;; [estree:updateexpression]: https://github.com/estree/estree/blob/master/es5.md#updateexpression
(define (eval-estree-update-expression node env (options (js/obj)))
  (eval-estree-unary-expression node env options))

;;; Evaluate an ESTree [`BinaryExpression`][estree:binaryexpression] node.
;;;
;;; [estree:binaryexpression]: https://github.com/estree/estree/blob/master/es5.md#binaryexpression
(define (eval-estree-binary-expression node env (options (js/obj)))
  (define operator
    (get-estree-field "operator" node))
  (define left
    (get-estree-field "left" node))
  (define left-val
    (eval-estree left env options))
  (define right
    (get-estree-field "right" node))
  (define right-val
    (eval-estree right env options))
  (cond
   ((eq? operator "+")
    (+ left-val right-val))
   ((eq? operator "-")
    (- left-val right-val))
   ((eq? operator "*")
    (* left-val right-val))
   ((eq? operator "/")
    (/ left-val right-val))
   ((eq? operator "<")
    (< left-val right-val))
   ((eq? operator "<=")
    (<= left-val right-val))
   ((eq? operator ">")
    (> left-val right-val))
   ((eq? operator ">=")
    (>= left-val right-val))
   ((eq? operator "==")
    (js/== left-val right-val))
   ((eq? operator "===")
    (js/=== left-val right-val))
   ((eq? operator "!=")
    (not (js/== left-val right-val)))
   ((eq? operator "!==")
    (not (js/=== left-val right-val)))
   ((eq? operator "in")
    (js/in left-val right-val))
   ((eq? operator "instanceof")
    (is-a? left-val right-val))
   (else
    (js/eval
     (string-append
      "("
      (print-estree (estree-quote left-val))
      ") "
      operator
      " ("
      (print-estree (estree-quote right-val))
      ")")))))

;;; Evaluate an ESTree [`LogicalExpression`][estree:logicalexpression] node.
;;;
;;; [estree:logicalexpression]: https://github.com/estree/estree/blob/master/es5.md#logicalexpression
(define (eval-estree-logical-expression node env (options (js/obj)))
  (define operator
    (get-estree-field "operator" node))
  (define left
    (get-estree-field "left" node))
  (define right
    (get-estree-field "right" node))
  (cond
   ((eq? operator "&&")
    (define left-val
      (eval-estree left env options))
    (cond
     (left-val
      (eval-estree right env options))
     (else
      #f)))
   ((eq? operator "||")
    (define left-val
      (eval-estree left env options))
    (cond
     (left-val
      left-val)
     (else
      (eval-estree right env options))))
   (else
    #u)))

;;; Evaluate an ESTree [`IfStatement`][estree:ifstatement] node.
;;;
;;; [estree:ifstatement]: https://github.com/estree/estree/blob/master/es5.md#ifstatement
(define (eval-estree-if-statement node env (options (js/obj)))
  (eval-estree-conditional-expression node env options))

;;; Evaluate an ESTree [`ConditionalExpression`][estree:conditionalexpression] node.
;;;
;;; [estree:conditionalexpression]: https://github.com/estree/estree/blob/master/es5.md#conditionalexpression
(define (eval-estree-conditional-expression node env (options (js/obj)))
  (define test
    (get-estree-field "test" node))
  (define consequent
    (get-estree-field "consequent" node))
  (define alternate
    (get-estree-field "alternate" node))
  (cond
   ((eval-estree test env options)
    (eval-estree consequent env options))
   (alternate
    (eval-estree alternate env options))
   (else
    #u)))

;;; Evaluate an ESTree [`WhileStatement`][estree:whilestatement] node.
;;;
;;; [estree:whilestatement]: https://github.com/estree/estree/blob/master/es5.md#whilestatement
(define (eval-estree-while-statement node env (options (js/obj)))
  ;; TODO: Convert `BlockStatement` to `Program` fragment
  ;; and extend the environment manually, only once.
  (define test
    (get-estree-field "test" node))
  (define body
    (get-estree-field "body" node))
  (try
    (while (eval-estree test env options)
      (try
        (eval-estree body env options)
        (catch ContinueException e)))
    (catch BreakException e))
  #u)

;;; Evaluate an ESTree [`ForStatement`][estree:forstatement] node.
;;;
;;; [estree:forstatement]: https://github.com/estree/estree/blob/master/es5.md#forstatement
(define (eval-estree-for-statement node env (options (js/obj)))
  (define init
    (get-estree-field "init" node))
  (define test
    (get-estree-field "test" node))
  (define update
    (get-estree-field "update" node))
  (define body
    (get-estree-field "body" node))
  (define for-env
    (extend-environment (new LispEnvironment)
                        env))
  (with-environment
   for-env
   (eval-estree init for-env options)
   (try
     (while (if test
                (eval-estree test for-env options)
                #t)
       (try
         (when body
           (eval-estree body for-env options))
         (catch ContinueException e))
       (when update
         (eval-estree update for-env options)))
     (catch BreakException e))
   #u))

;;; Evaluate an ESTree [`ForOfStatement`][estree:forofstatement] node.
;;;
;;; [estree:forofstatement]: https://github.com/estree/estree/blob/master/es2015.md#forofstatement
(define (eval-estree-for-of-statement node env (options (js/obj)))
  (define left
    (get-estree-field "left" node))
  (define right
    (get-estree-field "right" node))
  (define body
    (get-estree-field "body" node))
  (define for-of-env
    (extend-environment (new LispEnvironment)
                        env))
  (with-environment
   for-of-env
   (define identifier
     (cond
      ((estree-type? left "VariableDeclaration")
       (~> left
           (get-estree-field "declarations" _)
           (first _)
           (get-estree-field "id" _)))
      (else
       (get-estree-field "left" left))))
   (define right-val
     (eval-estree right for-of-env options))
   (try
     (for ((x right-val))
       (define declaration
         (new VariableDeclaration
              (list
               (new VariableDeclarator
                    identifier
                    (new Literal x)))
              "let"))
       (try
         (eval-estree declaration for-of-env options)
         (eval-estree body for-of-env options)
         (catch ContinueException e)))
     (catch BreakException e))
   #u))

;;; Evaluate an ESTree [`TryStatement`][estree:trystatement] node.
;;;
;;; [estree:trystatement]: https://github.com/estree/estree/blob/master/es5.md#trystatement
(define (eval-estree-try-statement node env (options (js/obj)))
  (define block
    (get-estree-field "block" node))
  (define handler
    (get-estree-field "handler" node))
  (define finalizer
    (get-estree-field "finalizer" node))
  (define result #u)
  (try
    (set! result
          (eval-estree block env options))
    (catch Object err
      (cond
       (handler
        (define handler-param
          (get-estree-field "param" handler))
        (define handler-param-sym
          (string->symbol (get-estree-field "name" handler-param)))
        (define handler-body
          (get-estree-field "body" handler))
        (define handler-env
          (extend-environment (new LispEnvironment)
                              env))
        (with-environment
         handler-env
         (send handler-env
               set-local!
               handler-param-sym
               err)
         (set! result
               (eval-estree handler-body
                            handler-env
                            options))))
       (else
        (throw err))))
    (finally
      (when finalizer
        (eval-estree finalizer env options))))
  result)

;;; Evaluate an ESTree [`ClassDeclaration`][estree:classdeclaration] node.
;;;
;;; [estree:classdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#classdeclaration
(define (eval-estree-class-declaration node env (options (js/obj)))
  (define id
    (get-estree-field "id" node))
  (define sym
    (string->symbol (get-estree-field "name" id)))
  (define class-expression
    (eval-estree-class-expression node env options))
  (send env set! sym class-expression)
  #u)

;;; Evaluate an ESTree [`ClassExpression`][estree:classexpression] node.
;;;
;;; [estree:classexpression]: https://github.com/estree/estree/blob/master/es2015.md#classexpression
(define (eval-estree-class-expression node env (options (js/obj)))
  (define super-class
    (get-estree-field "superClass" node))
  (define class-body
    (get-estree-field "body" node))
  (define class-body-statements
    (get-estree-field "body" class-body))
  (define constructor-f
    (lambda (this . args)
      (define constructor-inner-f #u)
      (for ((x class-body-statements))
        ;; TODO: Move evaluation outside---no reason to do it each
        ;; time we are instantiating.
        (define key
          (get-estree-field "key" x))
        (define key-str
          (get-estree-field "name" key))
        (define value
          (get-estree-field "value" x))
        (define value-val
          (cond
           (value
            (eval-estree value env options))
           (else
            #u)))
        (oset! this key-str value-val)
        (when (eq? key-str "constructor")
          (set! constructor-inner-f value-val)))
      (when constructor-inner-f
        (send constructor-inner-f apply this args))
      #u))
  (when super-class
    (define super-class-val
      (eval-estree super-class env options))
    ;; <https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/Inheritance#setting_teachers_prototype_and_constructor_reference>
    (oset! constructor-f
           :prototype
           (send Object
                 create
                 (oget super-class-val :prototype))))
  constructor-f)

;;; Evaluate an ESTree [`SwitchStatement`][estree:switchstatement] node.
;;;
;;; [estree:switchstatement]: https://github.com/estree/estree/blob/master/es5.md#switchstatement
(define (eval-estree-switch-statement node env (options (js/obj)))
  (define discriminant
    (get-estree-field "discriminant" node))
  (define discriminant-val
    (eval-estree discriminant env options))
  (define cases
    (get-estree-field "cases" node))
  (define result #u)
  (try
    (for ((x cases))
      (define test
        (get-estree-field "test" x))
      (when (or (not test)
                (eq? discriminant-val
                     (eval-estree test env options)))
        (set! result (eval-estree x env options))))
    (catch BreakException e
      (set! result (get-estree-field "value" e))))
  result)

;;; Evaluate an ESTree [`SwitchCase`][estree:switchcase] node.
;;;
;;; [estree:switchcase]: https://github.com/estree/estree/blob/master/es5.md#switchcase
(define (eval-estree-switch-case node env (options (js/obj)))
  (define consequent
    (get-estree-field "consequent" node))
  (when (and (= (length consequent) 1)
             (estree-type? (first consequent)
                           "BlockStatement"))
    (set! consequent
          (get-estree-field "body" (first consequent))))
  (define result #u)
  (try
    (for ((x consequent))
      (set! result (eval-estree x env options)))
    (catch BreakException e
      (throw (new BreakException result))))
  result)

;;; Evaluate a TSESTree `TSAsExpression` node.
(define (eval-estree-ts-as-expression node env (options (js/obj)))
  (define expression
    (get-estree-field "expression" node))
  (eval-estree expression env options))

;;; Evaluate an ESTree `XRawJavaScript` node.
;;; This is an unofficial ESTree extension.
(define (eval-estree-x-raw-javascript node env (options (js/obj)))
  (define str
    (get-estree-field "js" node))
  (set! str (string-append "(" str ")"))
  (js/eval str))

;;; Global variable used for storing the value of `this`.
;;; Used for evaluating `ThisExpression`.
(define current-this-value #u)

;;; Temporarily set `current-this-value` to `val`,
;;; call `f`, and restore the original value.
;;; Returns the result of calling `f`.
(define (with-this-value val f)
  (define result #u)
  (define tmp current-this-value)
  (try
    (set! current-this-value val)
    (set! result (f))
    (finally
      (set! current-this-value tmp)))
  result)

;;; Helper function for `eval-estree-assignment-expression`.
(define (eval-estree-assignment-expression-helper node env (options (js/obj)) (settings (js/obj)))
  (define local-setting
    (oget settings :local))
  (define left
    (get-estree-field "left" node))
  (define right
    (get-estree-field "right" node))
  (define right-val
    (if right
        (eval-estree right env options)
        #u))
  (define (eval-pattern pattern val)
    (cond
     ((estree-type? pattern "Identifier")
      (define sym
        (string->symbol (get-estree-field "name" pattern)))
      (cond
       (local-setting
        (send env set-local! sym val))
       (else
        (send env set! sym val)))
      val)
     ((estree-type? pattern "MemberExpression")
      (define obj
        (get-estree-field "object" pattern))
      (define obj-val
        (eval-estree obj env options))
      (define computed
        (get-estree-field "computed" pattern))
      (define prop
        (get-estree-field "property" pattern))
      (define prop-val
        (cond
         (computed
          (eval-estree prop env options))
         ((estree-type? prop "Identifier")
          (get-estree-field "name" prop))
         (else
          (get-estree-field "value" prop))))
      (oset! obj-val prop-val val)
      val)
     ((estree-type? pattern "ObjectPattern")
      (define properties
        (get-estree-field "properties" pattern))
      (for ((prop properties))
        (define key
          (get-estree-field "key" prop))
        (define value
          (get-estree-field "value" prop))
        (define sym
          (string->symbol (get-estree-field "name" value)))
        (define val1
          (oget val
                (get-estree-field "name" key)))
        (cond
         (local-setting
          (send env set-local! sym val1))
         (else
          (send env set! sym val1))))
      val)
     ((estree-type? pattern "ArrayPattern")
      (define elements
        (get-estree-field "elements" pattern))
      (for ((i (range 0 (length elements))))
        (define x
          (list-ref elements i))
        (define x1
          (if (is-a? x InternalPromise)
              (send x force)
              x))
        (cond
         ((not x1)
          (continue))
         ((estree-type? x1 "RestElement")
          (eval-pattern (get-estree-field "argument" x1)
                        (drop val i)))
         (else
          (eval-pattern x1 (list-ref val i)))))
      val)
     ((estree-type? pattern "AssignmentPattern")
      (define left
        (get-estree-field "left" pattern))
      (define right
        (get-estree-field "right" pattern))
      (define val1
        (if (undefined? val)
            (eval-estree right env options)
            val))
      (eval-pattern left val1))
     ;; TODO: Chain expressions
     (else
      #u)))
  (eval-pattern left right-val))

;;; Helper function for `eval-estree-assignment-expression-helper`.

;;; Helper function for `eval-estree-array-expression`.
(define (eval-estree-array-expression-helper elements env (options (js/obj)))
  (define result '())
  (for ((x elements))
    (cond
     ((estree-type? x "SpreadElement")
      (for ((y (eval-estree x env options)))
        (push-right! result y)))
     (else
      (push-right! result
                   (eval-estree x env options)))))
  result)

;;; Helper function for `eval-estree-function-expression`.
(define (eval-estree-function-expression-helper node env (options (js/obj)) (settings (js/obj)))
  (define arrow-setting
    (oget settings :arrow))
  (define params
    (get-estree-field "params" node))
  (define rest-param
    (if (and (> (length params) 0)
             (estree-type? (last params) "RestElement"))
        (last params)
        #u))
  (define body
    (get-estree-field "body" node))
  (cond
   (arrow-setting
    (make-arity-function
     (js/arrow args
       (define result #u)
       (try
         (set! result
               (eval-estree
                (if (= (length params) 0)
                    body
                    (new BlockStatement
                         `(,(new VariableDeclaration
                                 (list
                                  (new VariableDeclarator
                                       (new ArrayPattern params)
                                       (estree-quote args)))
                                 "let")
                           ,@(get-estree-field "body" body))))
                env
                options))
         (catch ReturnException e
           (set! result
                 (get-estree-field "value" e))))
       result)
     (if rest-param
         #u
         (length params))
     #t))
   (else
    (make-arity-function
     (lambda (this . args)
       (with-this-value
        this
        (lambda ()
          (define result #u)
          (try
            (set! result
                  (eval-estree
                   (if (= (length params) 0)
                       body
                       (new BlockStatement
                            `(,(new VariableDeclaration
                                    (list
                                     (new VariableDeclarator
                                          (new ArrayPattern params)
                                          (estree-quote args)))
                                    "let")
                              ,@(get-estree-field "body" body))))
                   env
                   options))
            (catch ReturnException e
              (set! result
                    (get-estree-field "value" e))))
          result)))
     (if rest-param
         #u
         (length params))))))

;;; Make a function of the specified arity.
(define (make-arity-function fun (n #u) (arrow #f))
  (cond
   (arrow
    (case n
      ((0)
       (js/arrow ()
         (fun)))
      ((1)
       (js/arrow (a)
         (fun a)))
      ((2)
       (js/arrow (a b)
         (fun a b)))
      ((3)
       (js/arrow (a b c)
         (fun a b c)))
      ((4)
       (js/arrow (a b c d)
         (fun a b c d)))
      ((5)
       (js/arrow (a b c d e)
         (fun a b c d e)))
      ((6)
       (js/arrow (a b c d e f)
         (fun a b c d e f)))
      ((7)
       (js/arrow (a b c d e f g)
         (fun a b c d e f g)))
      ((8)
       (js/arrow (a b c d e f g h)
         (fun a b c d e f g h)))
      ((9)
       (js/arrow (a b c d e f g h i)
         (fun a b c d e f g h i)))
      ((10)
       (js/arrow (a b c d e f g h i j)
         (fun a b c d e f g h i j)))
      (else
       fun)))
   (else
    (case n
      ((0)
       (lambda (this)
         (send fun apply this arguments)))
      ((1)
       (lambda (this a)
         (send fun apply this arguments)))
      ((2)
       (lambda (this a b)
         (send fun apply this arguments)))
      ((3)
       (lambda (this a b c)
         (send fun apply this arguments)))
      ((4)
       (lambda (this a b c d)
         (send fun apply this arguments)))
      ((5)
       (lambda (this a b c d e)
         (send fun apply this arguments)))
      ((6)
       (lambda (this a b c d e fun)
         (send fun apply this arguments)))
      ((7)
       (lambda (this a b c d e f g)
         (send fun apply this arguments)))
      ((8)
       (lambda (this a b c d e f g h)
         (send fun apply this arguments)))
      ((9)
       (lambda (this a b c d e f g h i)
         (send fun apply this arguments)))
      ((10)
       (lambda (this a b c d e f g h i j)
         (send fun apply this arguments)))
      (else
       fun)))))

;;; Mapping from ESTree node types to evaluator functions.
(define eval-estree-map
  (make-hash
   `(("ArrayExpression" . ,eval-estree-array-expression)
     ("ArrayPattern" . ,eval-estree-array-pattern)
     ("ArrowFunctionExpression" . ,eval-estree-arrow-function-expression)
     ("AssignmentExpression" . ,eval-estree-assignment-expression)
     ("BinaryExpression" . ,eval-estree-binary-expression)
     ("BlockStatement" . ,eval-estree-block-statement)
     ("BreakStatement" . ,eval-estree-break-statement)
     ("CallExpression" . ,eval-estree-call-expression)
     ("ClassDeclaration" . ,eval-estree-class-declaration)
     ("ClassExpression" . ,eval-estree-class-expression)
     ("ConditionalExpression" . ,eval-estree-conditional-expression)
     ("ContinueStatement" . ,eval-estree-continue-statement)
     ("ExpressionStatement" . ,eval-estree-expression-statement)
     ("ForOfStatement" . ,eval-estree-for-of-statement)
     ("ForStatement" . ,eval-estree-for-statement)
     ("FunctionDeclaration" . ,eval-estree-function-declaration)
     ("FunctionExpression" . ,eval-estree-function-expression)
     ("Identifier" . ,eval-estree-identifier)
     ("IfStatement" . ,eval-estree-if-statement)
     ("Literal" . ,eval-estree-literal)
     ("LogicalExpression" . ,eval-estree-logical-expression)
     ("MemberExpression" . ,eval-estree-member-expression)
     ("NewExpression" . ,eval-estree-new-expression)
     ("ObjectExpression" . ,eval-estree-object-expression)
     ("Program" . ,eval-estree-program)
     ("RestElement" . ,eval-estree-rest-element)
     ("ReturnStatement" . ,eval-estree-return-statement)
     ("SequenceExpression" . ,eval-estree-sequence-expression)
     ("SpreadElement" . ,eval-estree-spread-element)
     ("SwitchCase" . ,eval-estree-switch-case)
     ("SwitchStatement" . ,eval-estree-switch-statement)
     ("TSAsExpression" . ,eval-estree-ts-as-expression)
     ("ThisExpression" . ,eval-estree-this-expression)
     ("ThrowStatement" . ,eval-estree-throw-statement)
     ("TryStatement" . ,eval-estree-try-statement)
     ("UnaryExpression" . ,eval-estree-unary-expression)
     ("UpdateExpression" . ,eval-estree-update-expression)
     ("VariableDeclaration" . ,eval-estree-variable-declaration)
     ("VariableDeclarator" . ,eval-estree-variable-declarator)
     ("WhileStatement" . ,eval-estree-while-statement)
     ("XRawJavaScript" . ,eval-estree-x-raw-javascript)
     ("YieldExpression" . ,eval-estree-yield-expression))))

(provide
  (rename-out (eval-syntax eval-rose))
  (rename-out (eval_ seval))
  Evaluator
  call-evaluator
  default-evaluator
  eval-estree
  eval-sexp
  eval-syntax
  eval1
  eval_
  evaluator?
  js/eval_)
