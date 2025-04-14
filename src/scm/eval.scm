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
                  wrap-in-estree
                  estree?
                  estree-type?
                  estree-type))
(require (only-in "./env"
                  Environment
                  TypedEnvironment
                  LispEnvironment
                  EnvironmentStack
                  current-environment
                  empty-environment
                  default-environment
                  with-environment
                  make-environment
                  extend-environment
                  environment-frames
                  link-environment-frames))
(require (only-in "./exception"
                  BreakException
                  ContinueException
                  YieldException
                  ReturnException))
(require (only-in "./javascript"
                  js-eval_))
(require (only-in "./printer"
                  print-estree
                  write-to-string))
(require (only-in "./rose"
                  Rose))

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
   (lambda (exp (env undefined) (options (js-obj)))
     (define evaluator
       (or (oget options "evaluator")
           default-evaluator))
     (call-evaluator evaluator exp env options))))

;;; Call an evaluator on an expression.
(define (call-evaluator evaluator
                        exp
                        (env undefined)
                        (options (js-obj)))
  (cond
   ((is-a? evaluator Evaluator)
    (send evaluator eval exp env options))
   ((procedure? evaluator)
    (evaluator exp env options))
   (else
    undefined)))

;;; Whether something is an evaluator.
(define (evaluator? obj)
  (or (procedure? obj)
      (is-a? obj Evaluator)))

;;; Evaluator class.
(define-class Evaluator ()
  ;;; The simplest possible evaluator is the
  ;;; identity function.
  (define/public (eval exp (env undefined) (options (js-obj)))
    exp))

;;; Lisp-1 evaluator function.
(define (eval1 exp env (options (js-obj)))
  (cond
   ((is-a? exp Rose)
    (eval-rose exp env options))
   (else
    (eval-sexp exp env options))))

;;; Evaluate an S-expression.
;;;
;;; This is a case-by-case function.
(define (eval-sexp exp env (options (js-obj)))
  (with-environment
   env
   (lambda ()
     (cond
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
            ((eq? binding-type "macro")
             ;; Macros are implemented with a macro function that
             ;; has the signature `(exp, env) => value`. The arguments
             ;; to the macro are *not* evaluated, but the macro's
             ;; return value---the macro expansion---*is* evaluated:
             ;; that is, it is pushed back on the expressions stack
             ;; for further evaluation.
             (define expansion
               (f exp env))
             (eval-sexp expansion env options))
            ((eq? binding-type "fexpr")
             ;; A fexpr is a function that receives its arguments
             ;; unevaluated, like a macro. However, unlike a macro,
             ;; the return value is not re-evaluated---it is simply
             ;; returned.
             (apply f args))
            ((eq? binding-type "special")
             ;; Special form
             (f exp env))
            ((or (memq? binding-type
                        '("function"
                          "procedure"))
                 (and (eq? binding-type "variable")
                      (procedure? f)))
             ;; Function call
             (cond
              ((fexpr? f)
               (apply f args))
              ;; Macro function
              ((get-field lispMacro f)
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
         undefined)
        ((procedure? op)
         ;; `(<fn> ...)` call. The first element is a
         ;; function object. If it is a fexpr call, the function
         ;; is called with its arguments unevaluated. Otherwise,
         ;; the arguments have to be evaluated first.
         (define f op)
         (cond
          ((or (= (array-list-length args) 0)
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
       exp)))))

;;; Evaluate an S-expression wrapped in a rose tree.
(define (eval-rose node env (options (js-obj)))
  (eval-sexp (send node get-value)
             env
             options))

;;; Evaluate an [ESTree][github:estree] node
;;; (i.e., a JavaScript [AST][w:Abstract syntax tree]).
;;;
;;; [github:estree]: https://github.com/estree/estree
;;; [w:Abstract syntax tree]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
(define (eval-estree node env (options (js-obj)))
  (define type_
    (estree-type node))
  (define evaluator
    (send eval-estree-map get type_))
  (cond
   (evaluator
    (with-environment
     env
     (lambda ()
       (evaluator node env options))))
   (else
    undefined)))

;;; Evaluate an ESTree [`Program`][estree:program] node
;;; (i.e., a JavaScript program).
;;;
;;; [estree:program]: https://github.com/estree/estree/blob/master/es5.md#programs
(define (eval-estree-program node env (options (js-obj)))
  (define body
    (get-field body node))
  (define result undefined)
  (for ((statement body))
    (set! result
          (eval-estree statement env options)))
  result)

;;; Evaluate an ESTree [`BlockStatement`][estree:blockstatement] node.
;;;
;;; [estree:blockstatement]: https://github.com/estree/estree/blob/master/es5.md#blockstatement
(define (eval-estree-block-statement node env (options (js-obj)))
  (define env1
    (extend-environment (new LispEnvironment)
                        env))
  (with-environment
   env1
   (js/arrow ()
     (try
       (catch Error e))
     (eval-estree-program node env1 options))))

;;; Evaluate an ESTree [`SequenceExpression`][estree:sequenceexpression] node.
;;;
;;; [estree:sequenceexpression]: https://github.com/estree/estree/blob/master/es5.md#sequenceexpression
(define (eval-estree-sequence-expression node env (options (js-obj)))
  (define expressions
    (get-field expressions node))
  (define program
    (new Program expressions))
  (eval-estree-program program env options))

;;; Evaluate an ESTree [`Literal`][estree:literal] node.
;;;
;;; [estree:literal]: https://github.com/estree/estree/blob/master/es5.md#literal
(define (eval-estree-literal node env (options (js-obj)))
  (get-field value node))

;;; Evaluate an ESTree [`Identifier`][estree:identifier] node.
;;;
;;; [estree:identifier]: https://github.com/estree/estree/blob/master/es5.md#identifier
(define (eval-estree-identifier node env (options (js-obj)))
  (define name
    (get-field name node))
  (send env get (string->symbol name)))

;;; Evaluate an ESTree [`MemberExpression`][estree:memberexpression] node.
;;;
;;; [estree:memberexpression]: https://github.com/estree/estree/blob/master/es5.md#memberexpression
(define (eval-estree-member-expression node env (options (js-obj)))
  (define object
    (get-field object node))
  (define property
    (get-field property node))
  (define object-val
    (eval-estree object env options))
  (define property-val
    (cond
     ((estree-type? property "Identifier")
      (get-field name property))
     (else
      (eval-estree property env options))))
  (oget object-val property-val))

;;; Evaluate an ESTree [`CallExpression`][estree:callexpression] node.
;;;
;;; [estree:callexpression]: https://github.com/estree/estree/blob/master/es5.md#callexpression
(define (eval-estree-call-expression node env (options (js-obj)))
  ;; TODO: Handle `js/eval` calls directly?
  (define callee
    (get-field callee node))
  (define args
    (get-field arguments node))
  (define args-vals
    (eval-estree-array-expression-helper
     args env options))
  (cond
   ((estree-type? callee "MemberExpression")
    (define obj
      (get-field object callee))
    (define obj-val
      (eval-estree obj env options))
    (define method-val
      (eval-estree callee env options))
    (send method-val apply obj-val args-vals))
   (else
    (define f callee)
    (define f-val
      (eval-estree f env options))
    (define result
      (apply f-val args-vals))
    result)))

;;; Evaluate an ESTree [`BreakStatement`][estree:breakstatement] node.
;;;
;;; [estree:breakstatement]: https://github.com/estree/estree/blob/master/es5.md#breakstatement
(define (eval-estree-break-statement node env (options (js-obj)))
  (throw (new BreakException)))

;;; Evaluate an ESTree [`ContinueStatement`][estree:continuestatement] node.
;;;
;;; [estree:continuestatement]: https://github.com/estree/estree/blob/master/es5.md#continuestatement
(define (eval-estree-continue-statement node env (options (js-obj)))
  (throw (new ContinueException)))

;;; Evaluate an ESTree [`YieldExpression`][estree:yieldexpression] node.
;;;
;;; [estree:yieldexpression]: https://github.com/estree/estree/blob/master/es2015.md#yieldexpression
(define (eval-estree-yield-expression node env (options (js-obj)))
  (define argument
    (get-field argument node))
  (define argument-val
    (eval-estree argument env options))
  (throw (new YieldException argument-val)))

;;; Evaluate an ESTree [`ReturnStatement`][estree:returnstatement] node.
;;;
;;; [estree:returnstatement]: https://github.com/estree/estree/blob/master/es5.md#returnstatement
(define (eval-estree-return-statement node env (options (js-obj)))
  (define argument
    (get-field argument node))
  (define argument-val
    (eval-estree argument env options))
  (throw (new ReturnException argument-val)))

;;; Evaluate an ESTree [`ThrowStatement`][estree:throwstatement] node.
;;;
;;; [estree:throwstatement]: https://github.com/estree/estree/blob/master/es5.md#throwstatement
(define (eval-estree-throw-statement node env (options (js-obj)))
  (define argument
    (get-field argument node))
  (define argument-val
    (eval-estree argument env options))
  (throw argument-val))

;;; Evaluate an ESTree [`ThisExpression`][estree:thisexpression] node.
;;;
;;; [estree:thisexpression]: https://github.com/estree/estree/blob/master/es5.md#thisexpression
(define (eval-estree-this-expression node env (options (js-obj)))
  current-this-value)

;;; Evaluate an ESTree [`NewExpression`][estree:newexpression] node.
;;;
;;; [estree:newexpression]: https://github.com/estree/estree/blob/master/es2015.md#expressions
(define (eval-estree-new-expression node env (options (js-obj)))
  (define callee
    (get-field callee node))
  (define args
    (get-field arguments node))
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
(define (eval-estree-object-expression node env (options (js-obj)))
  (define result
    (js-obj))
  (define properties
    (get-field properties node))
  (for ((prop properties))
    (cond
     ((estree-type? prop "SpreadElement")
      (define argument
        (get-field argument prop))
      (define argument-val
        (eval-estree argument env options))
      (set! result (js-obj-append result argument-val)))
     (else
      (define key
        (get-field key prop))
      (define value
        (get-field value prop))
      (define key-val
        (cond
         ((estree-type? key "Identifier")
          (get-field name key))
         (else
          (get-field value key))))
      (define value-val
        (eval-estree value env options))
      (oset! result key-val value-val))))
  result)

;;; Evaluate an ESTree [`VariableDeclaration`][estree:variabledeclaration] node.
;;;
;;; [estree:variabledeclaration]: https://github.com/estree/estree/blob/master/es5.md#variabledeclaration
(define (eval-estree-variable-declaration node env (options (js-obj)))
  (define declarations
    (get-field declarations node))
  (for ((x declarations))
    (eval-estree x env options))
  undefined)

;;; Evaluate an ESTree [`VariableDeclarator`][estree:variabledeclarator] node.
;;;
;;; [estree:variabledeclarator]: https://github.com/estree/estree/blob/master/es5.md#variabledeclarator
(define (eval-estree-variable-declarator node env (options (js-obj)))
  (define id
    (get-field id node))
  (define init
    (get-field init node))
  (define assignment
    (new AssignmentExpression "=" id init))
  (eval-estree-assignment-expression-helper
   assignment
   env
   options
   (js-obj "local" #t)))

;;; Evaluate an ESTree [`AssignmentExpression`][estree:assignmentexpression] node.
;;;
;;; [estree:assignmentexpression]: https://github.com/estree/estree/blob/master/es5.md#assignmentexpression
(define (eval-estree-assignment-expression node env (options (js-obj)))
  (eval-estree-assignment-expression-helper node env options))

;;; Evaluate an ESTree [`ArrayExpression`][estree:arrayexpression] node.
;;;
;;; [estree:arrayexpression]: https://github.com/estree/estree/blob/master/es5.md#arrayexpression
(define (eval-estree-array-expression node env (options (js-obj)))
  (define elements
    (get-field elements node))
  (eval-estree-array-expression-helper
   elements env options))

;;; Evaluate an ESTree [`ArrayPattern`][estree:arraypattern] node.
;;;
;;; [estree:arraypattern]: https://github.com/estree/estree/blob/master/es2015.md#arraypattern
(define (eval-estree-array-pattern node env (options (js-obj)))
  (eval-estree-array-expression node env options))

;;; Evaluate an ESTree [`RestElement`][estree:restelement] node.
;;;
;;; [estree:restelement]: https://github.com/estree/estree/blob/master/es2015.md#restelement
(define (eval-estree-rest-element node env (options (js-obj)))
  (define argument
    (get-field argument node))
  (eval-estree argument env options))

;;; Evaluate an ESTree [`ExpressionStatement`][estree:expressionstatement] node.
;;;
;;; [estree:expressionstatement]: https://github.com/estree/estree/blob/master/es5.md#expressionstatement
(define (eval-estree-expression-statement node env (options (js-obj)))
  (define expression
    (get-field expression node))
  (eval-estree expression env options))

;;; Evaluate an ESTree [`FunctionDeclaration`][estree:functiondeclaration] node.
;;;
;;; [estree:functiondeclaration]: https://github.com/estree/estree/blob/master/es5.md#functiondeclaration
(define (eval-estree-function-declaration node env (options (js-obj)))
  (define id
    (get-field id node))
  (define name
    (string->symbol (get-field name id)))
  (define f
    (eval-estree-function-expression node env options))
  (send env set-local name f "function")
  undefined)

;;; Evaluate an ESTree [`FunctionExpression`][estree:functionexpression] node.
;;;
;;; <https://docs.esprima.org/en/latest/syntax-tree-format.html#function-expression>
;;;
;;; [estree:functionexpression]: https://github.com/estree/estree/blob/master/es5.md#functionexpression
(define (eval-estree-function-expression node env (options (js-obj)))
  (eval-estree-function-expression-helper
   node
   env
   options))

;;; Evaluate an ESTree [`ArrowFunctionExpression`][estree:arrowfunctionexpression] node.
;;;
;;; [estree:arrowfunctionexpression]: https://github.com/estree/estree/blob/master/es2015.md#arrowfunctionexpression
(define (eval-estree-arrow-function-expression node env (options (js-obj)))
  (eval-estree-function-expression-helper
   node
   env
   options
   (js-obj "arrow" #t)))

;;; Evaluate an ESTree [`UnaryExpression`][estree:unaryexpression] node.
;;;
;;; [estree:unaryexpression]: https://github.com/estree/estree/blob/master/es5.md#unaryexpression
(define (eval-estree-unary-expression node env (options (js-obj)))
  (define operator
    (get-field operator node))
  (define prefix
    (get-field prefix node))
  (define argument
    (get-field argument node))
  (cond
   ((eq? operator "!")
    (not (eval-estree argument env options)))
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
    undefined)))

;;; Evaluate an ESTree [`UpdateExpression`][estree:updateexpression] node.
;;;
;;; [estree:updateexpression]: https://github.com/estree/estree/blob/master/es5.md#updateexpression
(define (eval-estree-update-expression node env (options (js-obj)))
  (eval-estree-unary-expression node env options))

;;; Evaluate an ESTree [`BinaryExpression`][estree:binaryexpression] node.
;;;
;;; [estree:binaryexpression]: https://github.com/estree/estree/blob/master/es5.md#binaryexpression
(define (eval-estree-binary-expression node env (options (js-obj)))
  (define operator
    (get-field operator node))
  (define left
    (get-field left node))
  (define left-val
    (eval-estree left env options))
  (define right
    (get-field right node))
  (define right-val
    (eval-estree right env options))
  ;; TODO: Replace with `case`.
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
   ((eq? operator "in")
    (js/in left-val right-val))
   ((eq? operator "instanceof")
    (is-a? left-val right-val))
   (else
    undefined)))

;;; Evaluate an ESTree [`LogicalExpression`][estree:logicalexpression] node.
;;;
;;; [estree:logicalexpression]: https://github.com/estree/estree/blob/master/es5.md#logicalexpression
(define (eval-estree-logical-expression node env (options (js-obj)))
  (define operator
    (get-field operator node))
  (define left
    (get-field left node))
  (define right
    (get-field right node))
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
    undefined)))

;;; Evaluate an ESTree [`IfStatement`][estree:ifstatement] node.
;;;
;;; [estree:ifstatement]: https://github.com/estree/estree/blob/master/es5.md#ifstatement
(define (eval-estree-if-statement node env (options (js-obj)))
  (eval-estree-conditional-expression node env options))

;;; Evaluate an ESTree [`ConditionalExpression`][estree:conditionalexpression] node.
;;;
;;; [estree:conditionalexpression]: https://github.com/estree/estree/blob/master/es5.md#conditionalexpression
(define (eval-estree-conditional-expression node env (options (js-obj)))
  (define test
    (get-field test node))
  (define consequent
    (get-field consequent node))
  (define alternate
    (get-field alternate node))
  (cond
   ((eval-estree test env options)
    (eval-estree consequent env options))
   (alternate
    (eval-estree alternate env options))
   (else
    undefined)))

;;; Evaluate an ESTree [`WhileStatement`][estree:whilestatement] node.
;;;
;;; [estree:whilestatement]: https://github.com/estree/estree/blob/master/es5.md#whilestatement
(define (eval-estree-while-statement node env (options (js-obj)))
  ;; TODO: Convert `BlockStatement` to `Program` fragment
  ;; and extend the environment manually, only once.
  (define test
    (get-field test node))
  (define body
    (get-field body node))
  (try
    (while (eval-estree test env options)
      (try
        (eval-estree body env options)
        (catch ContinueException e)))
    (catch BreakException e))
  undefined)

;;; Evaluate an ESTree [`ForStatement`][estree:forstatement] node.
;;;
;;; [estree:forstatement]: https://github.com/estree/estree/blob/master/es5.md#forstatement
(define (eval-estree-for-statement node env (options (js-obj)))
  (define init
    (get-field init node))
  (define test
    (get-field test node))
  (define update
    (get-field update node))
  (define body
    (get-field body node))
  (define for-env
    (extend-environment (new LispEnvironment)
                        env))
  (with-environment
   for-env
   (js/arrow ()
     (eval-estree init for-env options)
     (try
       (while (eval-estree test for-env options)
         (try
           (eval-estree body for-env options)
           (catch ContinueException e))
         (eval-estree update for-env options))
       (catch BreakException e))
     undefined)))

;;; Evaluate an ESTree [`ForOfStatement`][estree:forofstatement] node.
;;;
;;; [estree:forofstatement]: https://github.com/estree/estree/blob/master/es2015.md#forofstatement
(define (eval-estree-for-of-statement node env (options (js-obj)))
  (define left
    (get-field left node))
  (define right
    (get-field right node))
  (define body
    (get-field body node))
  (define for-of-env
    (extend-environment (new LispEnvironment)
                        env))
  (with-environment
   for-of-env
   (js/arrow ()
     (define identifier
       (cond
        ((estree-type? left "VariableDeclaration")
         (~> left
             (get-field declarations _)
             (first _)
             (get-field id _)))
        (else
         (get-field left left))))
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
     undefined)))

;;; Evaluate an ESTree [`TryStatement`][estree:trystatement] node.
;;;
;;; [estree:trystatement]: https://github.com/estree/estree/blob/master/es5.md#trystatement
(define (eval-estree-try-statement node env (options (js-obj)))
  (define block
    (get-field block node))
  (define handler
    (get-field handler node))
  (define finalizer
    (get-field finalizer node))
  (define result undefined)
  (try
    (set! result
          (eval-estree block env options))
    (catch Object err
      (cond
       (handler
        (define handler-param
          (get-field param handler))
        (define handler-param-sym
          (string->symbol (get-field name handler-param)))
        (define handler-body
          (get-field body handler))
        (define handler-env
          (extend-environment (new LispEnvironment)
                              env))
        (with-environment
         handler-env
         (js/arrow ()
           (send handler-env
                 set-local
                 handler-param-sym
                 err)
           (eval-estree handler-body handler-env options))))
       (else
        (throw err))))
    (finally
      (when finalizer
        (eval-estree finalizer env options))))
  result)

;;; Evaluate an ESTree [`ClassDeclaration`][estree:classdeclaration] node.
;;;
;;; [estree:classdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#classdeclaration
(define (eval-estree-class-declaration node env (options (js-obj)))
  (define id
    (get-field id node))
  (define sym
    (string->symbol (get-field name id)))
  (define class-expression
    (eval-estree-class-expression node env options))
  (send env set sym class-expression)
  undefined)

;;; Evaluate an ESTree [`ClassExpression`][estree:classexpression] node.
;;;
;;; [estree:classexpression]: https://github.com/estree/estree/blob/master/es2015.md#classexpression
(define (eval-estree-class-expression node env (options (js-obj)))
  (define super-class
    (get-field superClass node))
  (define class-body
    (get-field body node))
  (define class-body-statements
    (get-field body class-body))
  (define constructor-f
    (lambda (this . args)
      (define constructor-inner-f undefined)
      (for ((x class-body-statements))
        ;; TODO: Move evaluation outside---no reason to do it each
        ;; time we are instantiating.
        (define key
          (get-field key x))
        (define key-str
          (get-field name key))
        (define value
          (get-field value x))
        (define value-val
          (cond
           (value
            (eval-estree value env options))
           (else
            undefined)))
        (oset! this key-str value-val)
        (when (eq? key-str "constructor")
          (set! constructor-inner-f value-val)))
      (when constructor-inner-f
        (send constructor-inner-f apply this args))
      undefined))
  (when super-class
    (define super-class-val
      (eval-estree super-class env options))
    ;; <https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/Inheritance#setting_teachers_prototype_and_constructor_reference>
    (oset! constructor-f
           "prototype"
           (send Object
                 create
                 (oget super-class-val "prototype"))))
  constructor-f)

;;; Evaluate an ESTree [`SwitchStatement`][estree:switchstatement] node.
;;;
;;; [estree:switchstatement]: https://github.com/estree/estree/blob/master/es5.md#switchstatement
(define (eval-estree-switch-statement node env (options (js-obj)))
  (define discriminant
    (get-field discriminant node))
  (define discriminant-val
    (eval-estree discriminant env options))
  (define cases
    (get-field cases node))
  (try
    (for ((x cases))
      (define test
        (get-field test x))
      (when (or (not test)
                (eq? discriminant-val
                     (eval-estree test env options)))
        (eval-estree x env options)))
    (catch BreakException e))
  undefined)

;;; Evaluate an ESTree [`SwitchCase`][estree:switchcase] node.
;;;
;;; [estree:switchcase]: https://github.com/estree/estree/blob/master/es5.md#switchcase
(define (eval-estree-switch-case node env (options (js-obj)))
  (define consequent
    (get-field consequent node))
  (for ((x consequent))
    (eval-estree x env options))
  undefined)

;;; Evaluate an ESTree `XRawJavaScript` node.
;;; This is an unofficial ESTree extension.
(define (eval-estree-x-raw-javascript node env (options (js-obj)))
  (define str
    (get-field js node))
  (set! str (string-append "(" str ")"))
  (js/eval str))

;;; Global variable used for storing the value of `this`.
;;; Used for evaluating `ThisExpression`.
(define current-this-value undefined)

;;; Temporarily set `current-this-value` to `val`,
;;; call `f`, and restore the original value.
;;; Returns the result of calling `f`.
(define (with-this-value val f)
  (define result undefined)
  (define tmp current-this-value)
  (try
    (set! current-this-value val)
    (set! result (f))
    (finally
      (set! current-this-value tmp)))
  result)

;;; Helper function for `eval-estree-assignment-expression`.
(define (eval-estree-assignment-expression-helper node env (options (js-obj)) (settings (js-obj)))
  (define local-setting
    (oget settings "local"))
  (define left
    (get-field left node))
  (define left-type
    (estree-type left))
  (define right
    (get-field right node))
  (define right-val
    (cond
     (right
      (eval-estree right env options))
     (else
      undefined)))
  (cond
   ((eq? left-type "ArrayPattern")
    (define elements
      (get-field elements left))
    (for ((i (range 0 (array-list-length elements))))
      (define x
        (aget elements i))
      (define x-type
        (estree-type x))
      (cond
       ((eq? x-type "RestElement")
        (define name
          (~> x
              (get-field argument _)
              (get-field name _)))
        (define sym
          (string->symbol name))
        (define val
          (drop right-val i))
        (cond
         (local-setting
          (send env set-local sym val))
         (else
          (send env set sym val))))
       (else
        (define name
          (get-field name x))
        (define sym
          (string->symbol name))
        (define val
          (aget right-val i))
        (cond
         (local-setting
          (send env set-local sym val))
         (else
          (send env set sym val))))))
    right-val)
   ((eq? left-type "ObjectPattern")
    (define properties
      (get-field properties left))
    (for ((prop properties))
      (define key
        (get-field key prop))
      (define value
        (get-field value prop))
      (define sym
        (string->symbol (get-field name value)))
      (define val
        (oget right-val
              (get-field name key)))
      (cond
       (local-setting
        (send env set-local sym val))
       (else
        (send env set sym val))))
    right-val)
   ((eq? left-type "Identifier")
    (define sym
      (string->symbol (get-field name left)))
    (cond
     (local-setting
      (send env set-local sym right-val))
     (else
      (send env set sym right-val)))
    right-val)
   ((eq? left-type "MemberExpression")
    (define obj
      (get-field object left))
    (define obj-val
      (eval-estree obj env options))
    (define computed
      (get-field computed left))
    (define prop
      (get-field property left))
    (define prop-val
      (cond
       (computed
        (eval-estree prop env options))
       ((estree-type? prop "Identifier")
        (get-field name prop))
       (else
        (get-field value prop))))
    (oset! obj-val prop-val right-val)
    obj-val)
   ;; TODO: Chain expressions
   (else
    undefined)))

;;; Helper function for `eval-estree-array-expression`.
(define (eval-estree-array-expression-helper elements env (options (js-obj)))
  (define result '())
  (for ((x elements))
    (cond
     ((estree-type? x "RestElement")
      (set! result
            (append result
                    (eval-estree x env options))))
     (else
      (push-right! result
                   (eval-estree x env options)))))
  result)

;;; Helper function for `eval-estree-function-expression`.
(define (eval-estree-function-expression-helper node env (options (js-obj)) (settings (js-obj)))
  (define arrow-setting
    (oget settings "arrow"))
  (define params
    (get-field params node))
  (define body
    (get-field body node))
  (cond
   (arrow-setting
    (lambda args
      (define result undefined)
      (try
        (set! result
              (eval-estree
               (if (= (array-list-length params) 0)
                   body
                   (new BlockStatement
                        `(,(new VariableDeclaration
                                (list
                                 (new VariableDeclarator
                                      (new ArrayPattern params)
                                      (wrap-in-estree args)))
                                "let")
                          ,@(get-field body body))))
               env
               options))
        (catch ReturnException e
          (set! result
                (get-field value e))))
      result))
   (else
    (lambda (this . args)
      (with-this-value
       this
       (lambda ()
         (define result undefined)
         (try
           (set! result
                 (eval-estree
                  (if (= (array-list-length params) 0)
                      body
                      (new BlockStatement
                           `(,(new VariableDeclaration
                                   (list
                                    (new VariableDeclarator
                                         (new ArrayPattern params)
                                         (wrap-in-estree args)))
                                   "let")
                             ,@(get-field body body))))
                  env
                  options))
           (catch ReturnException e
             (set! result
                   (get-field value e))))
         result))))))

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
     ("SwitchStatement" . ,eval-estree-switch-statement)
     ("SwitchCase" . ,eval-estree-switch-case)
     ("ThisExpression" . ,eval-estree-this-expression)
     ("ThrowStatement" . ,eval-estree-throw-statement)
     ("TryStatement" . ,eval-estree-try-statement)
     ("UnaryExpression" . ,eval-estree-unary-expression)
     ("UpdateExpression" . ,eval-estree-update-expression)
     ("VariableDeclaration" . ,eval-estree-variable-declaration)
     ("VariableDeclarator" . ,eval-estree-variable-declarator)
     ("WhileStatement" . ,eval-estree-while-statement)
     ("YieldExpression" . ,eval-estree-yield-expression)
     ("XRawJavaScript" . ,eval-estree-x-raw-javascript))))

(provide
  (rename-out (eval_ seval))
  Evaluator
  call-evaluator
  default-evaluator
  eval-estree
  eval-rose
  eval-sexp
  eval1
  eval_
  evaluator?
  js-eval_)
