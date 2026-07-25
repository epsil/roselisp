;; SPDX-License-Identifier: MPL-2.0
;;; # Decompiler
;;;
;;; Decompile from JavaScript/TypeScript to Lisp.
;;;
;;; ## Description
;;;
;;; Simple decompiler implementation translating JavaScript or
;;; TypeScript code to Lisp code.
;;;
;;; ### Comments
;;;
;;; Note that currently, there is no support for comments as the
;;; parser we are using, `@typescript-eslint/parser`, throws them
;;; away.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(require (only-in "fs"
                  mkdirSync
                  readFileSync
                  writeFileSync))
(require (only-in "path"
                  basename
                  dirname
                  join
                  extname))
(require (only-in "@typescript-eslint/typescript-estree"
                  (parse parse-ts)))
(require (only-in "./estree"
                  estree-type?
                  estree-type))
(require (only-in "./rose"
                  RoseSplice
                  rose->sexp
                  sexp->rose))
(require (only-in "./printer"
                  write-to-string))
(require (only-in "./util"
                  make-unique-symbol
                  tagged-list?))

;;; Decompile a JavaScript or TypeScript program.
(define (decompile x (options (js-obj)))
  (define language
    (oget options "language"))
  (cond
   ((eq? language "TypeScript")
    (decompile-ts x options))
   (else
    (decompile-js x options))))

;;; Read a JavaScript or TypeScript program from disk
;;; and decompile it. The result is written to disk.
(define (decompile-file! file (options (js-obj)))
  ;; TODO: Refactor to `(compile-file in-file out-file options)`?
  (define extension
    (extname file))
  (define stem
    (basename file extension))
  (define language
    (oget options "language"))
  (define in-dir
    (dirname file))
  (define out-dir
    (or (oget options "outDir")
        in-dir))
  (define out-extension ".scm")
  (define out-file
    (join out-dir
          (string-append
           stem
           out-extension)))
  (define code)
  (define data)
  (set! language
        (if (or (regexp-match (regexp "^TypeScript$" "i")
                              language)
                (eq? extension ".ts"))
            "TypeScript"
            "JavaScript"))
  (set! options
        (js-obj-append
         options
         (js-obj "language" language
                 "module" #t
                 "noModuleForm" #t
                 "pretty" #t)))
  (set! data
        (readFileSync file
                      (js-obj "encoding" "utf8")))
  (set! code (decompile data options))
  (mkdirSync out-dir (js-obj "recursive" #t))
  (writeFileSync out-file
                 code
                 (js-obj "encoding" "utf8"))
  (display
   (string-append "Decompiled " file " to " out-file))
  file)

;;; Read JavaScript or TypeScript programs from disk
;;; and decompile them. The results are written to disk.
(define (decompile-files! files (options (js-obj)))
  (for ((file files))
    (decompile-file! file options))
  files)

;;; Decompile a JavaScript or TypeScript module.
(define (decompile-module m (options (js-obj)))
  ;; TODO
  m)

;;; Decompile a JavaScript program
;;; (i.e., an [ESTree][github:estree]
;;; [AST][w:Abstract syntax tree]).
;;;
;;; [github:estree]: https://github.com/estree/estree
;;; [w:Abstract syntax tree]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
(define (decompile-js x (options (js-obj)))
  (decompile-ts x options))

;;; Decompile a TypeScript program
;;; (i.e., a [TSESTree][npm:typescript-estree]
;;; [AST][w:Abstract syntax tree]).
;;;
;;; [npm:typescript-estree] https://www.npmjs.com/package/@typescript-eslint/typescript-estree
;;; [w:Abstract syntax tree]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
(define (decompile-ts x (options (js-obj)))
  (define ast
    (parse-ts x))
  (define result-node
    (decompile-estree ast options))
  (define result
    (rose->sexp result-node))
  (unless (oget options "sexp")
    (set! result (write-to-string result options)))
  result)

;;; Decompile an [ESTree][github:estree] node
;;; (i.e., a JavaScript [AST][w:Abstract syntax tree]).
;;;
;;; [github:estree]: https://github.com/estree/estree
;;; [w:Abstract syntax tree]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
(define (decompile-estree node (options (js-obj)))
  (define type
    (and node (estree-type node)))
  (define decompiler
    (or (hash-ref decompiler-map type)
        default-decompiler))
  (decompiler node options))

;;; Decompile an ESTree [`Program`][estree:program] node
;;; (i.e., a JavaScript program).
;;;
;;; [estree:program]: https://github.com/estree/estree/blob/master/es5.md#programs
(define (decompile-program node (options (js-obj)))
  (define module-option
    (oget options "module"))
  (define result
    (sexp->rose
     `(module
          m
          scheme
        ,@(map (lambda (x)
                 (decompile-estree x options))
               (get-field body node)))))
  (unless module-option
    (set! result
          (sexp->rose
           `(begin ,@(send result drop 3))))
    (when (= (js/length (rose->sexp result)) 2)
      (set! result (send result get 1))))
  result)

;;; Decompile an ESTree [`ExpressionStatement`][estree:expressionstatement] node.
;;;
;;; [estree:expressionstatement]: https://github.com/estree/estree/blob/master/es5.md#expressionstatement
(define (decompile-expression-statement node (options (js-obj)))
  (decompile-estree (get-field expression node)
                    options))

;;; Decompile an ESTree [`CallExpression`][estree:callexpression] node.
;;;
;;; [estree:callexpression]: https://github.com/estree/estree/blob/master/es5.md#callexpression
(define (decompile-call-expression node (options (js-obj)))
  (define (is-spread-element x)
    (and x
         (estree-type? x "SpreadElement")))
  (define callee
    (get-field callee node))
  (define callee-decompiled
    (decompile-estree callee options))
  (define callee-decompiled-exp
    (rose->sexp callee-decompiled))
  (define args
    (get-field arguments node))
  (define spread-idx
    (js/find-index is-spread-element args))
  (define is-spread
    (and (number? spread-idx)
         (>= spread-idx 0)))
  (define is-spread-last
    (and (number? spread-idx)
         (= spread-idx
            (- (js/length args) 1))))
  (define args-decompiled
    (cond
     ((and is-spread
           (not is-spread-last)
           (> (js/length args) 1))
      `((append
         ,@(map (lambda (x)
                  (define result
                    (decompile-estree x options))
                  (cond
                   ((is-spread-element x)
                    result)
                   (else
                    `(list ,result))))
                args))))
     (else
      (map (lambda (x)
             (decompile-estree x options))
           args))))
  (cond
   ((tagged-list? callee-decompiled-exp 'get-field)
    (sexp->rose
     `(,(if is-spread
            'send/apply
            'send)
       ,(send callee-decompiled get 2)
       ,(send callee-decompiled get 1)
       ,@args-decompiled)))
   (else
    (sexp->rose
     `(,@(if is-spread
             '(apply)
             '())
       ,callee-decompiled
       ,@args-decompiled)))))

;;; Decompile an ESTree [`AssignmentExpression`][estree:assignmentexpression] node.
;;;
;;; [estree:assignmentexpression]: https://github.com/estree/estree/blob/master/es5.md#assignmentexpression
(define (decompile-assignment-expression node (options (js-obj)))
  (define op
    (get-field operator node))
  (define left
    (get-field left node))
  (define left-decompiled
    (decompile-estree left))
  (define left-exp
    (rose->sexp left-decompiled))
  (define right
    (get-field right node))
  (define right-decompiled
    (decompile-estree right))
  (when (eq? op "+=")
    (set! right-decompiled
          (sexp->rose
           `(+ ,left-decompiled ,right-decompiled))))
  (cond
   ((tagged-list? left-exp 'get-field)
    (sexp->rose
     `(set-field! ,(send left-decompiled get 1)
                  ,(send left-decompiled get 2)
                  ,right-decompiled)))
   ((tagged-list? left-exp 'aget)
    (sexp->rose
     `(aset! ,@(send left-decompiled drop 1)
             ,right-decompiled)))
   ((tagged-list? left-exp 'oget)
    (sexp->rose
     `(oset! ,@(send left-decompiled drop 1)
             ,right-decompiled)))
   ((estree-type? left "ArrayPattern")
    (sexp->rose
     `(set!-values ,left-decompiled ,right-decompiled)))
   ((estree-type? left "ObjectPattern")
    (sexp->rose
     `(set!-fields ,left-decompiled ,right-decompiled)))
   (else
    (sexp->rose
     `(set! ,left-decompiled ,right-decompiled)))))

;;; Decompile an ESTree [`AssignmentPattern`][estree:assignmentpattern] node.
;;;
;;; [estree:assignmentpattern]: https://github.com/estree/estree/blob/master/es2015.md#assignmentpattern
(define (decompile-assignment-pattern node (options (js-obj)))
  (define assignment
    (decompile-assignment-expression node options))
  (define left
    (get-field left node))
  (define typ
    (get-field typeAnnotation left))
  (cond
   (typ
    (sexp->rose
     `(,(send assignment get 1)
       :
       ,(decompile-estree typ options)
       ,(send assignment get 2))))
   (else
    (sexp->rose
     (send assignment drop 1)))))

;;; Decompile an ESTree [`UnaryExpression`][estree:unaryexpression] node.
;;;
;;; [estree:unaryexpression]: https://github.com/estree/estree/blob/master/es5.md#unaryexpression
(define (decompile-unary-expression node (options (js-obj)))
  (define op
    (get-field operator node))
  (define op-decompiled
    (cond
     ((eq? op "typeof")
      'type-of)
     ((eq? op "delete")
      'js/delete)
     (else
      (string->symbol op))))
  (define argument
    (get-field argument node))
  (define argument-decompiled
    (decompile-estree argument options))
  (define argument-decompiled-exp
    (rose->sexp argument-decompiled))
  (cond
   ((eq? op "!")
    (sexp->rose
     `(not ,argument-decompiled)))
   ((and (eq? op "-")
         (number? argument-decompiled-exp))
    (sexp->rose
     (- argument-decompiled-exp)))
   (else
    (sexp->rose
     `(,op-decompiled
       ,argument-decompiled)))))

;;; Decompile an ESTree [`UpdateExpression`][estree:updateexpression] node.
;;;
;;; [estree:updateexpression]: https://github.com/estree/estree/blob/master/es5.md#updateexpression
(define (decompile-update-expression node (options (js-obj)))
  (define op
    (get-field operator node))
  (define argument
    (decompile-estree (get-field argument node)
                      options))
  (define prefix
    (get-field prefix node))
  (define result #f)
  (cond
   ((eq? op "++")
    (set! result
          `(set! ,argument (+ ,argument 1))))
   ((eq? op "--")
    (set! result
          `(set! ,argument (- ,argument 1)))))
  (unless prefix
    (set! result `(begin0 ,argument ,result)))
  (sexp->rose result))

;;; Decompile an ESTree [`BinaryExpression`][estree:binaryexpression] node.
;;;
;;; [estree:binaryexpression]: https://github.com/estree/estree/blob/master/es5.md#binaryexpression
(define (decompile-binary-expression node (options (js-obj)))
  (define (is-string-expression exp)
    (or (string? exp)
        (tagged-list? exp 'string-append)))
  (define operator
    (get-field operator node))
  (define op
    (cond
     ((or (eq? operator "===")
          (eq? operator "!=="))
      'eq?)
     ((or (eq? operator "==")
          (eq? operator "!="))
      'equal?)
     ((eq? operator "&&")
      'and)
     ((eq? operator "||")
      'or)
     ((eq? operator "in")
      'js/in)
     ((eq? operator "instanceof")
      'is-a?)
     (else
      (string->symbol operator))))
  (define left
    (get-field left node))
  (define left-decompiled
    (decompile-estree left))
  (define left-decompiled-exp
    (rose->sexp left-decompiled))
  (define right
    (get-field right node))
  (define right-decompiled
    (decompile-estree right))
  (define right-decompiled-exp
    (rose->sexp right-decompiled))
  (when (and (eq? op '+)
             (or (is-string-expression left-decompiled-exp)
                 (is-string-expression right-decompiled-exp)))
    (set! op 'string-append))
  (define left-operands
    (if (or (tagged-list? left-decompiled-exp op)
            (and (eq? op 'string-append)
                 (tagged-list? left-decompiled-exp '+)))
        (send left-decompiled drop 1)
        (list left-decompiled)))
  (define right-operands
    (if (or (and (tagged-list? right-decompiled-exp op)
                 (not (eq? op '-))
                 (not (eq? op '/)))
            (and (eq? op 'string-append)
                 (tagged-list? right-decompiled-exp '+)))
        (send right-decompiled drop 1)
        (list right-decompiled)))
  (cond
   ((or (eq? operator "!==")
        (eq? operator "!="))
    (sexp->rose
     `(not (,op ,@left-operands ,@right-operands))))
   (else
    (sexp->rose
     `(,op ,@left-operands ,@right-operands)))))

;;; Decompile an ESTree [`LogicalExpression`][estree:logicalexpression] node.
;;;
;;; [estree:logicalexpression]: https://github.com/estree/estree/blob/master/es5.md#logicalexpression
(define (decompile-logical-expression node (options (js-obj)))
  (decompile-binary-expression node options))

;;; Decompile an ESTree [`VariableDeclaration`][estree:variabledeclaration] node.
;;;
;;; [estree:variabledeclaration]: https://github.com/estree/estree/blob/master/es5.md#variabledeclaration
(define (decompile-variable-declaration node (options (js-obj)))
  (define decls
    (map (lambda (x)
           (decompile-estree x options))
         (get-field declarations node)))
  (define result
    (~> (sexp->rose decls)
        (send set-value (new RoseSplice))))
  (when (= (js/length decls) 1)
    (set! result (send result get 0)))
  result)

;;; Decompile an ESTree [`VariableDeclarator`][estree:variabledeclarator] node.
;;;
;;; [estree:variabledeclarator]: https://github.com/estree/estree/blob/master/es5.md#variabledeclarator
(define (decompile-variable-declarator node (options (js-obj)))
  (define id
    (get-field id node))
  (define id-type
    (estree-type id))
  (define init
    (get-field init node))
  (define define-sym
    (cond
     ((eq? id-type "ArrayPattern")
      'define-values)
     ((eq? id-type "ObjectPattern")
      'define-fields)
     (else
      'define)))
  (sexp->rose
   `(,define-sym
      ,(decompile-estree id options)
      ,@(if (or (eq? init js/undefined)
                (eq? init js/null))
            '()
            (list (decompile-estree init options))))))

;;; Decompile an ESTree [`Identifier`][estree:identifier] node.
;;;
;;; [estree:identifier]: https://github.com/estree/estree/blob/master/es5.md#identifier
(define (decompile-identifier node (options (js-obj)))
  (define name
    (string->symbol (get-field name node)))
  (sexp->rose name))

;;; Decompile an ESTree [`Literal`][estree:literal] node.
;;;
;;; [estree:literal]: https://github.com/estree/estree/blob/master/es5.md#literal
(define (decompile-literal node (options (js-obj)))
  (define value
    (get-field value node))
  (cond
   ((js/null? value)
    (sexp->rose 'js/null))
   ((boolean? value)
    (sexp->rose
     (if value
         #t
         #f)))
   ((regexp? value)
    (define pattern
      (~> node
          (get-field regex _)
          (get-field pattern _)))
    (define flags
      (~> node
          (get-field regex _)
          (get-field flags _)))
    ;; TODO: We can emit `regexp` instead of `js/regexp`
    ;; provided it is not the name of a local variable.
    ;; To do that, we need an environment in which to
    ;; keep track of bindings.
    (sexp->rose
     `(js/regexp
       ,pattern
       ,@(if flags
             (list flags)
             '()))))
   (else
    (sexp->rose value))))

;;; Decompile an ESTree [`MemberExpression`][estree:memberexpression] node.
;;;
;;; [estree:memberexpression]: https://github.com/estree/estree/blob/master/es5.md#memberexpression
(define (decompile-member-expression node (options (js-obj)))
  (define property
    (decompile-estree (get-field property node)
                      options))
  (define property-exp
    (rose->sexp property))
  (define object
    (decompile-estree (get-field object node) options))
  (define object-exp
    (rose->sexp object))
  (define computed
    (get-field computed node))
  (define optional
    (get-field optional node))
  (cond
   (computed
    (when optional
      (set! object `(js/?. ,object)))
    (cond
     ((number? property-exp)
      (sexp->rose
       `(aget
         ,@(if (tagged-list? object-exp 'aget)
               (send object drop 1)
               (list object))
         ,property)))
     (else
      (sexp->rose
       `(oget ,object ,property)))))
   (optional
    (sexp->rose
     `(js/?. ,object ,property)))
   ((eq? property-exp 'length)
    (sexp->rose
     `(js/length ,object)))
   (else
    (sexp->rose
     `(get-field ,property ,object)))))

;;; Decompile an ESTree [`ChainExpression`][estree:chainexpression] node.
;;;
;;; [estree:chainexpression]: https://github.com/estree/estree/blob/master/es2020.md#chainexpression
(define (decompile-chain-expression node (options (js-obj)))
  (define expression
    (get-field expression node))
  (define expression-decompiled
    (decompile-estree expression options))
  (define expression-decompiled-exp
    (rose->sexp expression-decompiled))
  (cond
   ((tagged-list? expression-decompiled-exp 'get-field)
    (cond
     ((symbol? (aget expression-decompiled-exp 2))
      (sexp->rose
       `(and (field-bound?
              ,(send expression-decompiled get 1)
              ,(send expression-decompiled get 2))
             ,expression-decompiled)))
     (else
      (sexp->rose
       `(~> ,(send expression-decompiled get 2)
            (and (field-bound?
                  ,(send expression-decompiled get 1)
                  _)
                 (get-field
                  ,(send expression-decompiled get 1)
                  _)))))))
   ((tagged-list? expression-decompiled-exp 'send)
    (cond
     ((symbol? (aget expression-decompiled-exp 1))
      (sexp->rose
       `(and (field-bound?
              ,(send expression-decompiled get 2)
              ,(send expression-decompiled get 1))
             ,expression-decompiled)))
     (else
      (sexp->rose
       `(~> ,(send expression-decompiled get 1)
            (and (field-bound?
                  ,(send expression-decompiled get 2)
                  _)
                 (send
                  _
                  ,@(send expression-decompiled drop 2))))))))
   (else
    expression-decompiled)))

;;; Decompile an ESTree [`FunctionDeclaration`][estree:functiondeclaration] node.
;;;
;;; [estree:functiondeclaration]: https://github.com/estree/estree/blob/master/es5.md#functiondeclaration
(define (decompile-function-declaration node (options (js-obj)))
  (decompile-function node options))

;;; Decompile an ESTree [`FunctionExpression`][estree:functionexpression] node.
;;;
;;; [estree:functionexpression]: https://github.com/estree/estree/blob/master/es5.md#functionexpression
(define (decompile-function-expression node (options (js-obj)))
  (decompile-function node options))

;;; Decompile an ESTree [`ArrowFunctionExpression`][estree:arrowfunctionexpression] node.
;;;
;;; [estree:arrowfunctionexpression]: https://github.com/estree/estree/blob/master/es2015.md#arrowfunctionexpression
(define (decompile-arrow-function-expression node (options (js-obj)))
  (decompile-function node options))

;;; Decompile an ESTree [`RestElement`][estree:restelement] node.
;;;
;;; [estree:restelement]: https://github.com/estree/estree/blob/master/es2015.md#restelement
(define (decompile-rest-element node (options (js-obj)))
  (decompile-estree (get-field argument node) options))

;;; Decompile an ESTree [`BlockStatement`][estree:blockstatement] node.
;;;
;;; [estree:blockstatement]: https://github.com/estree/estree/blob/master/es5.md#blockstatement
(define (decompile-block-statement node (options (js-obj)))
  (sexp->rose
   `(js/block
      ,@(map (lambda (x)
               (decompile-estree x options))
             (get-field body node)))))

;;; Decompile an ESTree [`SequenceExpression`][estree:sequenceexpression] node.
;;;
;;; [estree:sequenceexpression]: https://github.com/estree/estree/blob/master/es5.md#sequenceexpression
(define (decompile-sequence-expression node (options (js-obj)))
  (sexp->rose
   `(begin
      ,@(map (lambda (x)
               (decompile-estree x options))
             (get-field expressions node)))))

;;; Decompile an ESTree [`ReturnStatement`][estree:returnstatement] node.
;;;
;;; [estree:returnstatement]: https://github.com/estree/estree/blob/master/es5.md#returnstatement
(define (decompile-return-statement node (options (js-obj)))
  (define argument
    (get-field argument node))
  (cond
   (argument
    (sexp->rose
     `(return ,(decompile-estree argument options))))
   (else
    (sexp->rose
     `(return)))))

;;; Decompile an ESTree [`IfStatement`][estree:ifstatement] node.
;;;
;;; [estree:ifstatement]: https://github.com/estree/estree/blob/master/es5.md#ifstatement
(define (decompile-if-statement node (options (js-obj)))
  (define test
    (decompile-estree (get-field test node) options))
  (define test-exp
    (rose->sexp test))
  (define consequent
    (decompile-estree (get-field consequent node) options))
  (define consequent-exp
    (rose->sexp consequent))
  (define alternate
    (if (get-field alternate node)
        (decompile-estree (get-field alternate node) options)
        #f))
  (define alternate-exp
    (and alternate (rose->sexp alternate)))
  (when (and (tagged-list? consequent-exp 'js/block)
             (= (js/length consequent-exp) 2))
    (set! consequent (send consequent get 1))
    (set! consequent-exp (rose->sexp consequent)))
  (when (and (tagged-list? alternate-exp 'js/block)
             (= (js/length alternate-exp) 2))
    (set! alternate (send alternate get 1))
    (set! alternate-exp (rose->sexp alternate)))
  (cond
   ((tagged-list? alternate-exp 'when)
    (sexp->rose
     `(cond
       (,test
        ,@(if (tagged-list? consequent-exp 'js/block)
              (send consequent drop 1)
              (list consequent)))
       (,@(send alternate drop 1)))))
   ((tagged-list? alternate-exp 'unless)
    (sexp->rose
     `(cond
       (,test
        ,@(if (tagged-list? consequent-exp 'js/block)
              (send consequent drop 1)
              (list consequent)))
       ((not ,(send alternate get 1))
        ,@(send alternate drop 2)))))
   ((tagged-list? alternate-exp 'if)
    (define alternate-test
      (send alternate get 1))
    (define alternate-consequent
      (send alternate get 2))
    (define alternate-consequent-exp
      (rose->sexp alternate-consequent))
    (sexp->rose
     `(cond
       (,test
        ,@(if (tagged-list? consequent-exp 'js/block)
              (send consequent drop 1)
              (list consequent)))
       (,alternate-test
        ,@(if (tagged-list? alternate-consequent-exp 'js/block)
              (send alternate-consequent drop 1)
              (list alternate-consequent)))
       ,@(if (> (js/length alternate-exp) 3)
             (list `(else ,@(send alternate drop 3)))
             '()))))
   ((tagged-list? alternate-exp 'cond)
    (sexp->rose
     `(cond
       (,test
        ,@(if (tagged-list? consequent-exp 'js/block)
              (send consequent drop 1)
              (list consequent)))
       ,@(send alternate drop 1))))
   ((not alternate)
    (cond
     ((tagged-list? test-exp 'not)
      (sexp->rose
       `(unless ,(send test get 1)
          ,@(if (tagged-list? consequent-exp 'js/block)
                (send consequent drop 1)
                (list consequent)))))
     (else
      (sexp->rose
       `(when ,test
          ,@(if (tagged-list? consequent-exp 'js/block)
                (send consequent drop 1)
                (list consequent)))))))
   ((or (tagged-list? consequent-exp 'js/block)
        (tagged-list? alternate-exp 'js/block))
    (sexp->rose
     `(cond
       (,test
        ,@(if (tagged-list? consequent-exp 'js/block)
              (send consequent drop 1)
              (list consequent)))
       (else
        ,@(if (tagged-list? alternate-exp 'js/block)
              (send alternate drop 1)
              (list alternate))))))
   (else
    (sexp->rose
     `(if ,test
          ,consequent
          ,alternate)))))

;;; Decompile an ESTree [`WhileStatement`][estree:whilestatement] node.
;;;
;;; [estree:whilestatement]: https://github.com/estree/estree/blob/master/es5.md#whilestatement
(define (decompile-while-statement node (options (js-obj)))
  (define test
    (decompile-estree (get-field test node) options))
  (define body
    (decompile-estree (get-field body node) options))
  (define body-exp
    (rose->sexp body))
  (define result
    (sexp->rose
     `(do ()
          ((not ,test))
        ,@(if (tagged-list? body-exp 'js/block)
              (send body drop 1)
              (list body)))))
  result)

;;; Decompile an ESTree [`DoWhileStatement`][estree:dowhilestatement] node.
;;;
;;; [estree:dowhilestatement]: https://github.com/estree/estree/blob/master/es5.md#dowhilestatement
(define (decompile-do-while-statement node (options (js-obj)))
  (define test
    (decompile-estree (get-field test node) options))
  (define body
    (decompile-estree (get-field body node) options))
  (define result
    (sexp->rose
     `(js/do-while
       ,(send body drop 1)
       ,test)))
  result)

;;; Decompile an ESTree [`ForStatement`][estree:forstatement] node.
;;;
;;; [estree:forstatement]: https://github.com/estree/estree/blob/master/es5.md#forstatement
(define (decompile-for-statement node (options (js-obj)))
  (define init
    (decompile-estree (get-field init node) options))
  (define inits
    (if (is-a? (rose->sexp init) RoseSplice)
        (send init drop 0)
        (list init)))
  (define test
    (decompile-estree (get-field test node) options))
  (define update
    (decompile-estree (get-field update node) options))
  (define updates
    (if (tagged-list? (rose->sexp update) 'begin)
        (send update drop 1)
        (list update)))
  (define bindings '())
  (for ((i (range 0 (js/length inits))))
    (define current-init
      (aget inits i))
    (define current-init-exp
      (rose->sexp current-init))
    (when (or (tagged-list? current-init-exp 'define)
              (tagged-list? current-init-exp 'set!))
      (set! current-init
            (sexp->rose
             `(,@(send current-init drop 1))
             current-init))
      (set! current-init-exp
            (rose->sexp current-init)))
    (define current-update
      (aget updates i))
    (define current-update-exp
      (rose->sexp current-update))
    (when (tagged-list? current-update-exp 'begin0)
      (set! current-update
            (send current-update last))
      (set! current-update-exp
            (rose->sexp current-update)))
    (when (tagged-list? current-update-exp 'set!)
      (set! current-update
            (send current-update third))
      (set! current-update-exp
            (rose->sexp current-update)))
    (push-right! bindings
                 `(,@(send current-init drop 0)
                   ,current-update)))
  (define body
    (decompile-estree (get-field body node) options))
  (cond
   ((and (= (js/length bindings) 1)
         (or (tagged-list? test '<)
             (tagged-list? test '>)))
    (define binding
      (first bindings))
    (define i
      (first binding))
    (define start
      (second binding))
    (define end
      (send test get 2))
    (define update
      (third binding))
    (define step
      (~> update
          (send _ get 2)
          (rose->sexp _)))
    (when (tagged-list? update '-)
      (set! step (- step)))
    (sexp->rose
     `(for ((,i (range
                 ,start
                 ,end
                 ,@(if (= step 1)
                       '()
                       (list step)))))
        ,@(send body drop 1))))
   (else
    (sexp->rose
     `(do ,bindings
          ((not ,test))
        ,@(send body drop 1))))))

;;; Decompile an ESTree [`ForOfStatement`][estree:forofstatement] node.
;;;
;;; [estree:forofstatement]: https://github.com/estree/estree/blob/master/es2015.md#forofstatement
(define (decompile-for-of-statement node (options (js-obj)))
  (define left
    (decompile-estree (get-field left node) options))
  (define left-exp
    (rose->sexp left))
  (when (tagged-list? left-exp 'define)
    (set! left (send left get 1))
    (set! left-exp (rose->sexp left)))
  (define right
    (decompile-estree (get-field right node) options))
  (define right-exp
    (rose->sexp right))
  (define body
    (decompile-estree (get-field body node) options))
  (define body-nodes
    (send body drop 1))
  (when (tagged-list? left-exp 'define-values)
    (define sym
      (make-unique-symbol
       (cons right-exp (second left-exp))))
    (push-left! body-nodes
                (sexp->rose
                 `(,(send left get 0)
                   ,(send left get 1)
                   ,sym)))
    (set! left (sexp->rose sym)))
  (sexp->rose
   `(for ((,left ,right))
      ,@body-nodes)))

;;; Decompile an ESTree [`ForInStatement`][estree:forinstatement] node.
;;;
;;; [estree:forinstatement]: https://github.com/estree/estree/blob/master/es5.md#forinstatement
(define (decompile-for-in-statement node (options (js-obj)))
  (define left
    (decompile-estree (get-field left node) options))
  (define left-exp
    (rose->sexp left))
  (when (tagged-list? left-exp 'define)
    (set! left (send left get 1))
    (set! left-exp (rose->sexp left)))
  (define right
    (decompile-estree (get-field right node) options))
  (define body
    (decompile-estree (get-field body node) options))
  (sexp->rose
   `(for ((,left (js-keys ,right)))
      ,@(send body drop 1))))

;;; Decompile an ESTree [`BreakStatement`][estree:breakstatement] node.
;;;
;;; [estree:breakstatement]: https://github.com/estree/estree/blob/master/es5.md#breakstatement
(define (decompile-break-statement node (options (js-obj)))
  (sexp->rose
   `(break
     ,@(if (get-field label node)
           (list (decompile-estree (get-field label node)
                                   options))
           '()))))

;;; Decompile an ESTree [`ContinueStatement`][estree:continuestatement] node.
;;;
;;; [estree:continuestatement]: https://github.com/estree/estree/blob/master/es5.md#continuestatement
(define (decompile-continue-statement node (options (js-obj)))
  (sexp->rose
   `(continue
     ,@(if (get-field label node)
           (list (decompile-estree (get-field label node)
                                   options))
           '()))))

;;; Decompile an ESTree [`ThrowStatement`][estree:throwstatement] node.
;;;
;;; [estree:throwstatement]: https://github.com/estree/estree/blob/master/es5.md#throwstatement
(define (decompile-throw-statement node (options (js-obj)))
  (sexp->rose
   `(throw
     ,(decompile-estree (get-field argument node)
                        options))))

;;; Decompile an ESTree [`TryStatement`][estree:trystatement] node.
;;;
;;; [estree:trystatement]: https://github.com/estree/estree/blob/master/es5.md#trystatement
(define (decompile-try-statement node (options (js-obj)))
  (define block
    (get-field block node))
  (define handler
    (get-field handler node))
  (define finalizer
    (get-field finalizer node))
  (define result
    (sexp->rose
     `(try
        ,@(send (decompile-estree block options) drop 1)
        ,@(if handler
              `((catch Object
                    ,(if (get-field param handler)
                         (decompile-estree
                          (get-field param handler)
                          options)
                         '_)
                  ,@(send (decompile-estree
                           (get-field body handler)
                           options)
                          drop 1)))
              '())
        ,@(if finalizer
              `((finally ,@(send (decompile-estree
                                  finalizer options)
                                 drop 1)))
              '()))))
  result)

;;; Decompile an ESTree [`YieldExpression`][estree:yieldexpression] node.
;;;
;;; [estree:yieldexpression]: https://github.com/estree/estree/blob/master/es2015.md#yieldexpression
(define (decompile-yield-expression node (options (js-obj)))
  (sexp->rose
   `(yield
     ,(decompile-estree (get-field argument node)
                        options))))

;;; Decompile an ESTree [`NewExpression`][estree:newexpression] node.
;;;
;;; [estree:newexpression]: https://github.com/estree/estree/blob/master/es2015.md#expressions
(define (decompile-new-expression node (options (js-obj)))
  (define arguments_
    (get-field arguments node))
  (define is-spread
    (and (> (js/length arguments_) 0)
         (estree-type? (js/last arguments_) "SpreadElement")))
  (sexp->rose
   `(,@(if is-spread
           '(apply)
           '())
     new
     ,(decompile-estree (get-field callee node)
                        options)
     ,@(map (lambda (x)
              (decompile-estree x options))
            arguments_))))

;;; Decompile an ESTree [`ConditionalExpression`][estree:conditionalexpression] node.
;;;
;;; [estree:conditionalexpression]: https://github.com/estree/estree/blob/master/es5.md#conditionalexpression
(define (decompile-conditional-expression node (options (js-obj)))
  (decompile-if-statement node options))

;;; Decompile an ESTree [`ImportDeclaration`][estree:importdeclaration] node.
;;;
;;; [estree:importdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#importdeclaration
(define (decompile-import-declaration node (options (js-obj)))
  (define source
    (get-field source node))
  (define source-decompiled
    (decompile-estree source options))
  (define specifiers
    (get-field specifiers node))
  (cond
   ((= (js/length specifiers) 0)
    (sexp->rose
     `(require ,source-decompiled)))
   ((and (= (js/length specifiers) 1)
         (estree-type? (first specifiers)
                       "ImportNamespaceSpecifier"))
    (sexp->rose
     `(require
       ,(decompile-estree
         (get-field local (first specifiers))
         options)
       ,source-decompiled)))
   (else
    (define specifiers-decompiled
      (map (lambda (x)
             (define imported
               (decompile-estree
                (get-field imported x) options))
             (define local
               (decompile-estree
                (get-field local x) options))
             (if (eq? (rose->sexp imported)
                      (rose->sexp local))
                 imported
                 `(,imported ,local)))
           specifiers))
    (sexp->rose
     `(require
       (only-in ,source-decompiled
                ,@specifiers-decompiled))))))

;;; Decompile an ESTree [`ExportNamedDeclaration`][estree:exportnameddeclaration] node.
;;;
;;; [estree:exportnameddeclaration]: https://github.com/estree/estree/blob/master/es2015.md#exportnameddeclaration
(define (decompile-export-named-declaration node (options (js-obj)))
  (define specifiers
    (get-field specifiers node))
  (define specifiers-decompiled
    (map (lambda (x)
           (define exported
             (decompile-estree
              (get-field exported x) options))
           (define local
             (decompile-estree
              (get-field local x) options))
           (if (eq? (rose->sexp exported)
                    (rose->sexp local))
               exported
               `(rename-out (,local ,exported))))
         specifiers))
  (sexp->rose
   `(provide ,@specifiers-decompiled)))

;;; Decompile an ESTree [`ExportAllDeclaration`][estree:exportalldeclaration] node.
;;;
;;; [estree:exportalldeclaration]: https://github.com/estree/estree/blob/master/es2015.md#exportalldeclaration
(define (decompile-export-all-declaration node (options (js-obj)))
  (define source
    (get-field source node))
  (define source-decompiled
    (decompile-estree source options))
  (sexp->rose
   `(provide (all-from-out ,source-decompiled))))

;;; Decompile an ESTree [`ObjectExpression`][estree:objectexpression] node.
;;;
;;; [estree:objectexpression]: https://github.com/estree/estree/blob/master/es5.md#objectexpression
(define (decompile-object-expression node (options (js-obj)))
  (define spreads '())
  (define properties '())
  (for ((prop (get-field properties node)))
    (cond
     ((estree-type? prop "SpreadElement")
      (push-right! spreads
                   (decompile-estree (get-field argument prop)
                                     options)))
     (else
      (push-right! properties
                   (~> (get-field key prop)
                       (decompile-estree _ options)
                       (rose->sexp _)
                       (symbol->string _)))
      (push-right! properties
                   (decompile-estree (get-field value prop)
                                     options)))))
  (cond
   ((= (js/length spreads) 0)
    (sexp->rose
     `(js-obj ,@properties)))
   (else
    (sexp->rose
     `(js-obj-append
       ,@spreads
       ,@(if (> (js/length properties) 0)
             (list `(js-obj ,@properties))
             '()))))))

;;; Decompile an ESTree [`ObjectPattern`][estree:objectpattern] node.
;;;
;;; [estree:objectpattern]: https://github.com/estree/estree/blob/master/es2015.md#objectpattern
(define (decompile-object-pattern node (options (js-obj)))
  (define properties '())
  (for ((prop (get-field properties node)))
    (define key
      (decompile-estree (get-field key prop)
                        options))
    (define value
      (decompile-estree (get-field value prop)
                        options))
    (if (eq? (rose->sexp key)
             (rose->sexp value))
        (push-right! properties key)
        (push-right! properties (list key value))))
  (sexp->rose properties))

;;; Decompile an ESTree [`TemplateLiteral`][estree:templateliteral] node.
;;;
;;; [estree:templateliteral]: https://github.com/estree/estree/blob/master/es2015.md#templateliteral
(define (decompile-template-literal node (options (js-obj)))
  (define str "")
  (define quasis
    (get-field quasis node))
  (when (> (js/length quasis) 0)
    (set! str
          (~> (first quasis)
              (get-field value _)
              (get-field cooked _))))
  (sexp->rose str))

;;; Decompile an ESTree [`TaggedTemplateExpression`][estree:taggedtemplateexpression] node.
;;;
;;; [estree:taggedtemplateexpression]: https://github.com/estree/estree/blob/master/es2015.md#taggedtemplateexpression
(define (decompile-tagged-template-expression node (options (js-obj)))
  (define tag
    (get-field tag node))
  (define tag-decompiled
    (decompile-estree tag options))
  (define quasi
    (get-field quasi node))
  (define quasi-decompiled
    (decompile-estree quasi options))
  (sexp->rose
   `(js/tag ,tag-decompiled ,quasi-decompiled)))

;;; Decompile an ESTree [`ArrayExpression`][estree:arrayexpression] node.
;;;
;;; [estree:arrayexpression]: https://github.com/estree/estree/blob/master/es5.md#arrayexpression
(define (decompile-array-expression node (options (js-obj)))
  (define elements
    (get-field elements node))
  (define (decompile-element x)
    (if x
        (decompile-estree x options)
        '_))
  (cond
   ((and (> (js/length elements) 0)
         (js/last elements)
         (estree-type? (js/last elements) "RestElement"))
    (define regular-elements
      (map decompile-element
           (drop-right elements 1)))
    (define rest-element
      (decompile-element (js/last elements)))
    (sexp->rose
     (apply list*
            `(,@regular-elements ,rest-element))))
   ((findf (lambda (x)
             (and x
                  (estree-type? x "SpreadElement")))
           elements)
    (define elements-decompiled
      (map (lambda (x)
             (define result
               (decompile-element x))
             (if (and x
                      (estree-type? x "SpreadElement"))
                 result
                 (sexp->rose
                  `(list ,result))))
           elements))
    (sexp->rose
     `(append ,@elements-decompiled)))
   (else
    (define elements-decompiled
      (map decompile-element elements))
    (sexp->rose
     `(list ,@elements-decompiled)))))

;;; Decompile an ESTree [`ArrayPattern`][estree:arraypattern] node.
;;;
;;; [estree:arraypattern]: https://github.com/estree/estree/blob/master/es2015.md#arraypattern
(define (decompile-array-pattern node (options (js-obj)))
  (define array-expression
    (decompile-array-expression node options))
  (sexp->rose
   (if (tagged-list? (rose->sexp array-expression)
                     'list)
       (send array-expression drop 1)
       array-expression)))

;;; Decompile an ESTree [`SpreadElement`][estree:spreadelement] node.
;;;
;;; [estree:spreadelement]: https://github.com/estree/estree/blob/master/es2015.md#expressions
(define (decompile-spread-element node (options (js-obj)))
  (define argument
    (get-field argument node))
  (decompile-estree argument options))

;;; Decompile an ESTree [`Super`][estree:super] node.
;;;
;;; [estree:super]: https://github.com/estree/estree/blob/master/es2015.md#expressions
(define (decompile-super node (options (js-obj)))
  (sexp->rose 'super))

;;; Decompile an ESTree [`ThisExpression`][estree:thisexpression] node.
;;;
;;; [estree:thisexpression]: https://github.com/estree/estree/blob/master/es5.md#thisexpression
(define (decompile-this-expression node (options (js-obj)))
  (sexp->rose 'this))

;;; Decompile an ESTree [`ClassDeclaration`][estree:classdeclaration] node.
;;;
;;; [estree:classdeclaration]: https://github.com/estree/estree/blob/master/es2015.md#classdeclaration
(define (decompile-class-declaration node (options (js-obj)))
  (define id
    (get-field id node))
  (define id-decompiled
    (decompile-estree id options))
  (define super-class
    (get-field superClass node))
  (define super-class-decompiled
    (if super-class
        (decompile-estree super-class options)
        'object%))
  (define super-class-decompiled-exp
    (if (memq? super-class-decompiled
               '(object%
                 object
                 Object))
        '()
        (list super-class-decompiled)))
  (define body-decompiled '())
  (define body
    (get-field body node))
  (for ((x (get-field body body)))
    (push-right! body-decompiled
                 (decompile-estree x options)))
  (sexp->rose
   `(define-class ,id-decompiled ,super-class-decompiled-exp
      ,@body-decompiled)))

;;; Decompile an ESTree [`PropertyDefinition`][estree:propertydefinition] node.
;;;
;;; [estree:propertydefinition]: https://github.com/estree/estree/blob/master/es2022.md#propertydefinition
(define (decompile-property-definition node (options (js-obj)))
  (define key
    (get-field key node))
  (define key-decompiled
    (decompile-estree key options))
  (define value
    (get-field value node))
  (define value-decompiled
    (decompile-estree value options))
  (define define-symbol
    (if (eq? (get-field accessibility node)
             "private")
        'define
        'define/public))
  (sexp->rose
   `(,define-symbol
      ,key-decompiled
      ,@(if (eq? value #n)
            '()
            (list value-decompiled)))))

;;; Decompile an ESTree [`MethodDefinition`][estree:methoddefinition] node.
;;;
;;; [estree:methoddefinition]: https://github.com/estree/estree/blob/master/es2015.md#methoddefinition
(define (decompile-method-definition node (options (js-obj)))
  (define key
    (get-field key node))
  (define key-decompiled
    (decompile-estree key options))
  (define key-decompiled-exp
    (rose->sexp key-decompiled))
  (define value
    (get-field value node))
  (define value-decompiled
    (decompile-estree value options))
  (define define-symbol
    (cond
     ((get-field generator value)
      'define/generator)
     ((or (eq? (get-field accessibility node)
               "private")
          (eq? key-decompiled-exp
               'constructor))
      'define)
     (else
      'define/public)))
  (sexp->rose
   `(,define-symbol
      ,(cons key-decompiled-exp
             (aget (rose->sexp value-decompiled) 1))
      ,@(send value-decompiled drop 2))))

;;; Decompile a TSESTree `TSAsExpression` node.
(define (decompile-ts-as-expression node (options (js-obj)))
  (define expression
    (get-field expression node))
  (define expression-decompiled
    (decompile-estree expression options))
  (define type-annotation
    (get-field typeAnnotation node))
  (define type-annotation-decompiled
    (decompile-estree type-annotation options))
  (sexp->rose
   `(ann ,expression-decompiled
         ,type-annotation-decompiled)))

;;; Decompile a TSESTree `TSAnyKeyword` node.
(define (decompile-ts-any-keyword node (options (js-obj)))
  (sexp->rose 'Any))

;;; Decompile a TSESTree `TSBooleanKeyword` node.
(define (decompile-ts-boolean-keyword node (options (js-obj)))
  (sexp->rose 'Boolean))

;;; Decompile a TSESTree `TSNumberKeyword` node.
(define (decompile-ts-number-keyword node (options (js-obj)))
  (sexp->rose 'Number))

;;; Decompile a TSESTree `TSStringKeyword` node.
(define (decompile-ts-string-keyword node (options (js-obj)))
  (sexp->rose 'String))

;;; Decompile a TSESTree `TSUndefinedKeyword` node.
(define (decompile-ts-undefined-keyword node (options (js-obj)))
  (sexp->rose 'Undefined))

;;; Decompile a TSESTree `TSVoidKeyword` node.
(define (decompile-ts-void-keyword node (options (js-obj)))
  (sexp->rose 'Void))

;;; Decompile a TSESTree `TSLiteralType` node.
(define (decompile-ts-literal-type node (options (js-obj)))
  (define literal
    (get-field literal node))
  (define literal-decompiled
    (if literal
        'True
        'False))
  (sexp->rose literal-decompiled))

;;; Decompile a TSESTree `TSArrayType` node.
(define (decompile-ts-array-type node (options (js-obj)))
  (define element-type
    (get-field elementType node))
  (define element-type-decompiled
    (decompile-estree element-type options))
  (sexp->rose
   `(Listof ,element-type-decompiled)))

;;; Decompile a TSESTree `TSTupleType` node.
(define (decompile-ts-tuple-type node (options (js-obj)))
  (define element-types
    (get-field elementTypes node))
  (define element-types-decompiled
    (map (lambda (x)
           (decompile-estree x options))
         element-types))
  (sexp->rose
   `(List ,@element-types-decompiled)))

;;; Decompile a TSESTree `TSNamedTupleMember` node.
(define (decompile-ts-named-tuple-member node (options (js-obj)))
  ;; FIXME: Better decompilation of `TSNamedTupleMember`.
  ;; The following strips away the identifier.
  (define element-type
    (get-field elementType node))
  (decompile-estree element-type options))

;;; Decompile a TSESTree `TSUnionType` node.
(define (decompile-ts-union-type node (options (js-obj)))
  (define types
    (get-field types node))
  (define types-decompiled
    (map (lambda (x)
           (decompile-estree x options))
         types))
  (sexp->rose
   `(U ,@types-decompiled)))

;;; Decompile a TSESTree `TSFunctionType` node.
(define (decompile-ts-function-type node (options (js-obj)))
  (define params
    (get-field params node))
  (define params-decompiled
    (map (lambda (x)
           (decompile-estree
            (get-field typeAnnotation x)
            options))
         params))
  (define return-type
    (get-field returnType node))
  (define return-type-decompiled
    (decompile-estree return-type options))
  (sexp->rose
   `(-> ,@params-decompiled
        ,return-type-decompiled)))

;;; Decompile a TSESTree `TSTypeReference` node.
(define (decompile-ts-type-reference node (options (js-obj)))
  (define name
    (get-field typeName node))
  (define params
    (get-field typeParameters node))
  (define name-decompiled
    (decompile-estree name options))
  (define params-decompiled
    (if params
        (decompile-estree params options)
        '()))
  (cond
   ((= (js/length params-decompiled) 0)
    (sexp->rose name-decompiled))
   (else
    (sexp->rose
     `(,name-decompiled
       ,@params-decompiled)))))

;;; Decompile a TSESTree `TSTypeParameterInstantiation` node.
(define (decompile-ts-type-parameter-instantiation node (options (js-obj)))
  (define params
    (get-field params node))
  (map (lambda (x)
         (decompile-estree x options))
       params))

;;; Decompile a TSESTree `TSTypeAliasDeclaration` node.
(define (decompile-ts-type-alias-declaration node (options (js-obj)))
  (define id
    (get-field id node))
  (define id-decompiled
    (decompile-estree id options))
  (define type-annotation
    (get-field typeAnnotation node))
  (define type-annotation-decompiled
    (decompile-estree type-annotation options))
  (sexp->rose
   `(define-type ,id-decompiled
      ,type-annotation-decompiled)))

;;; Decompile a TSESTree `TSTypeAnnotation` node.
(define (decompile-ts-type-annotation node (options (js-obj)))
  (decompile-estree
   (get-field typeAnnotation node)
   options))

;;; Decompile a function expression or declaration.
(define (decompile-function node (options (js-obj)))
  (define type_
    (estree-type node))
  (define id
    (if (eq? type_ "FunctionDeclaration")
        (decompile-estree (get-field id node) options)
        #f))
  (define lambda-sym
    (if (eq? type_ "ArrowFunctionExpression")
        'js/arrow
        'lambda))
  (define params
    (map (lambda (x)
           (decompile-parameter x options))
         (get-field params node)))
  (when (and (> (js/length params) 0)
             (estree-type? (js/last (get-field params node))
                           "RestElement"))
    (if (= (js/length params) 1)
        (set! params (js/last params))
        (set! params (apply list* params))))
  (define body
    (remove-return-tail-call
     (decompile-estree (get-field body node) options)))
  (define body-exp (rose->sexp body))
  (define body-forms
    (if (tagged-list? body-exp 'js/block)
        (send body drop 1)
        (list body)))
  (define async-field
    (get-field async node))
  (cond
   (id
    (cond
     (async-field
      (sexp->rose
       `(define ,id
          (async
           (,lambda-sym ,params
                        ,@body-forms)))))
     (else
      (sexp->rose
       `(define ,(cons id params)
          ,@body-forms)))))
   (else
    (cond
     (async-field
      (sexp->rose
       `(async
         (,lambda-sym ,params
                      ,@body-forms))))
     (else
      (sexp->rose
       `(,lambda-sym ,params
                     ,@body-forms)))))))

;;; Decompile a function parameter.
;;; Helper function for `decompile-function`.
(define (decompile-parameter node (options (js-obj)))
  (cond
   ((estree-type? node "Identifier")
    (define type-annotation
      (get-field typeAnnotation node))
    (define optional
      (get-field optional node))
    (define name
      (string->symbol (get-field name node)))
    (cond
     (optional
      (sexp->rose
       `(,name undefined)))
     (type-annotation
      (sexp->rose
       `(,name
         :
         ,(decompile-ts-type-annotation
           type-annotation options))))
     (else
      (sexp->rose name))))
   (else
    (decompile-estree node options))))

;;; Remove superfluous `(return ...)` forms from
;;; a form that occurs in tail call position.
(define (remove-return-tail-call node)
  (define exp
    (rose->sexp node))
  (cond
   ((and (tagged-list? exp 'return)
         (= (js/length exp) 2))
    (send node get 1))
   ((tagged-list? exp 'js/block)
    (sexp->rose
     `(,@(send node drop-right 1)
       ,(remove-return-tail-call
         (send node get (- (js/length exp) 1))))
     node))
   ((tagged-list? exp 'if)
    (sexp->rose
     `(,(send node get 0)
       ,(send node get 1)
       ,@(map remove-return-tail-call
              (send node drop 2)))
     node))
   ((tagged-list? exp 'cond)
    (sexp->rose
     `(,(send node get 0)
       ,@(map (lambda (x)
                (sexp->rose
                 `(,@(send x drop-right 1)
                   ,(remove-return-tail-call
                     (send x
                           get
                           (- (js/length (rose->sexp x))
                              1))))
                 node))
              (send node drop 1)))
     node))
   (else
    node)))

;;; Default decompiler function.
(define (default-decompiler node (options (js-obj)))
  (sexp->rose
   (string-append
    (and node (estree-type node))
    " not supported yet")))

;;; Mapping from ESTree node types to decompiler functions.
(define decompiler-map
  (make-hash
   `(("ArrayExpression" . ,decompile-array-expression)
     ("ArrayPattern" . ,decompile-array-pattern)
     ("ArrowFunctionExpression" . ,decompile-arrow-function-expression)
     ("AssignmentExpression" . ,decompile-assignment-expression)
     ("AssignmentPattern" . ,decompile-assignment-pattern)
     ("BinaryExpression" . ,decompile-binary-expression)
     ("BlockStatement" . ,decompile-block-statement)
     ("BreakStatement" . ,decompile-break-statement)
     ("CallExpression" . ,decompile-call-expression)
     ("ChainExpression" . ,decompile-chain-expression)
     ("ClassDeclaration" . ,decompile-class-declaration)
     ("ConditionalExpression" . ,decompile-conditional-expression)
     ("ContinueStatement" . ,decompile-continue-statement)
     ("DoWhileStatement" . ,decompile-do-while-statement)
     ("ExportAllDeclaration" . ,decompile-export-all-declaration)
     ("ExportNamedDeclaration" . ,decompile-export-named-declaration)
     ("ExpressionStatement" . ,decompile-expression-statement)
     ("ForInStatement" . ,decompile-for-in-statement)
     ("ForOfStatement" . ,decompile-for-of-statement)
     ("ForStatement" . ,decompile-for-statement)
     ("FunctionDeclaration" . ,decompile-function-declaration)
     ("FunctionExpression" . ,decompile-function-expression)
     ("Identifier" . ,decompile-identifier)
     ("IfStatement" . ,decompile-if-statement)
     ("ImportDeclaration" . ,decompile-import-declaration)
     ("Literal" . ,decompile-literal)
     ("LogicalExpression" . ,decompile-logical-expression)
     ("MemberExpression" . ,decompile-member-expression)
     ("MethodDefinition" . ,decompile-method-definition)
     ("NewExpression" . ,decompile-new-expression)
     ("ObjectExpression" . ,decompile-object-expression)
     ("ObjectPattern" . ,decompile-object-pattern)
     ("Program" . ,decompile-program)
     ("PropertyDefinition" . ,decompile-property-definition)
     ("RestElement" . ,decompile-rest-element)
     ("ReturnStatement" . ,decompile-return-statement)
     ("SequenceExpression" . ,decompile-sequence-expression)
     ("SpreadElement" . ,decompile-spread-element)
     ("Super" . ,decompile-super)
     ("TSAnyKeyword" . ,decompile-ts-any-keyword)
     ("TSArrayType" . ,decompile-ts-array-type)
     ("TSAsExpression" . ,decompile-ts-as-expression)
     ("TSBooleanKeyword" . ,decompile-ts-boolean-keyword)
     ("TSFunctionType" . ,decompile-ts-function-type)
     ("TSLiteralType" . ,decompile-ts-literal-type)
     ("TSNumberKeyword" . ,decompile-ts-number-keyword)
     ("TSStringKeyword" . ,decompile-ts-string-keyword)
     ("TSTupleType" . ,decompile-ts-tuple-type)
     ("TSNamedTupleMember" . ,decompile-ts-named-tuple-member)
     ("TSTypeAliasDeclaration" . ,decompile-ts-type-alias-declaration)
     ("TSTypeAnnotation" . ,decompile-ts-type-annotation)
     ("TSTypeParameterInstantiation" . ,decompile-ts-type-parameter-instantiation)
     ("TSTypeReference" . ,decompile-ts-type-reference)
     ("TSUndefinedKeyword" . ,decompile-ts-undefined-keyword)
     ("TSUnionType" . ,decompile-ts-union-type)
     ("TSVoidKeyword" . ,decompile-ts-void-keyword)
     ("TaggedTemplateExpression" . ,decompile-tagged-template-expression)
     ("TemplateLiteral" . ,decompile-template-literal)
     ("ThisExpression" . ,decompile-this-expression)
     ("ThrowStatement" . ,decompile-throw-statement)
     ("TryStatement" . ,decompile-try-statement)
     ("UnaryExpression" . ,decompile-unary-expression)
     ("UpdateExpression" . ,decompile-update-expression)
     ("VariableDeclaration" . ,decompile-variable-declaration)
     ("VariableDeclarator" . ,decompile-variable-declarator)
     ("WhileStatement" . ,decompile-while-statement)
     ("YieldExpression" . ,decompile-yield-expression))))

(provide
  decompile
  decompile-file!
  decompile-files!
  decompile-module)
