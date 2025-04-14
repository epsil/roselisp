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
                  make-rose))
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
    (send result-node get-value))
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
    (make-rose
     `(module
          m
          scheme
        ,@(map (lambda (x)
                 (decompile-estree x options))
               (get-field body node)))))
  (unless module-option
    (set! result
          (make-rose
           `(begin ,@(send result drop 3))))
    (when (= (array-list-length (send result get-value)) 2)
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
    (send callee-decompiled get-value))
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
            (- (array-list-length args) 1))))
  (define args-decompiled
    (cond
     ((and is-spread
           (not is-spread-last)
           (> (array-list-length args) 1))
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
    (make-rose
     `(,(if is-spread
            'send/apply
             'send)
       ,(send callee-decompiled get 2)
       ,(send callee-decompiled get 1)
       ,@args-decompiled)))
   (else
    (make-rose
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
    (send left-decompiled get-value))
  (define right
    (get-field right node))
  (define right-decompiled
    (decompile-estree right))
  (when (eq? op "+=")
    (set! right-decompiled
          (make-rose
           `(+ ,left-decompiled ,right-decompiled))))
  (cond
   ((tagged-list? left-exp 'get-field)
    (make-rose
     `(set-field! ,(send left-decompiled get 1)
                  ,(send left-decompiled get 2)
                  ,right-decompiled)))
   ((tagged-list? left-exp 'aget)
    (make-rose
     `(aset! ,@(send left-decompiled drop 1)
             ,right-decompiled)))
   ((tagged-list? left-exp 'oget)
    (make-rose
     `(oset! ,@(send left-decompiled drop 1)
             ,right-decompiled)))
   ((estree-type? left "ArrayPattern")
    (make-rose
     `(set!-values ,left-decompiled ,right-decompiled)))
   ((estree-type? left "ObjectPattern")
    (make-rose
     `(set!-js-obj ,left-decompiled ,right-decompiled)))
   (else
    (make-rose
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
    (make-rose
     `(,(send assignment get 1)
       :
       ,(decompile-estree typ options)
       ,(send assignment get 2))))
   (else
    (make-rose
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
    (send argument-decompiled get-value))
  (cond
   ((eq? op "!")
    (make-rose
     `(not ,argument-decompiled)))
   ((and (eq? op "-")
         (number? argument-decompiled-exp))
    (make-rose
     (- argument-decompiled-exp)))
   (else
    (make-rose
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
  (make-rose result))

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
    (send left-decompiled get-value))
  (define right
    (get-field right node))
  (define right-decompiled
    (decompile-estree right))
  (define right-decompiled-exp
    (send right-decompiled get-value))
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
    (make-rose
     `(not (,op ,@left-operands ,@right-operands))))
   (else
    (make-rose
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
    (~> (make-rose decls)
        (send set-value (new RoseSplice))))
  (when (= (array-list-length decls) 1)
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
      'define-js-obj)
     (else
      'define)))
  (make-rose
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
  (make-rose name))

;;; Decompile an ESTree [`Literal`][estree:literal] node.
;;;
;;; [estree:literal]: https://github.com/estree/estree/blob/master/es5.md#literal
(define (decompile-literal node (options (js-obj)))
  (define value
    (get-field value node))
  (cond
   ((eq? value js/null)
    (make-rose 'js/null))
   ((boolean? value)
    (make-rose
     (if value
         (string->symbol "#t")
         (string->symbol "#f"))))
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
    (make-rose
     `(js/regexp
       ,pattern
       ,@(if flags
             (list flags)
             '()))))
   (else
    (make-rose value))))

;;; Decompile an ESTree [`MemberExpression`][estree:memberexpression] node.
;;;
;;; [estree:memberexpression]: https://github.com/estree/estree/blob/master/es5.md#memberexpression
(define (decompile-member-expression node (options (js-obj)))
  (define property
    (decompile-estree (get-field property node)
                      options))
  (define property-exp
    (send property get-value))
  (define object
    (decompile-estree (get-field object node) options))
  (define object-exp
    (send object get-value))
  (define computed
    (get-field computed node))
  (cond
   (computed
    (cond
     ((number? property-exp)
      (make-rose
       `(aget
         ,@(if (tagged-list? object-exp 'aget)
               (send object drop 1)
               (list object))
         ,property)))
     (else
      (make-rose
       `(oget ,object ,property)))))
   (else
    (make-rose
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
    (send expression-decompiled get-value))
  (cond
   ((tagged-list? expression-decompiled-exp 'get-field)
    (cond
     ((symbol? (aget expression-decompiled-exp 2))
      (make-rose
       `(and (field-bound?
              ,(send expression-decompiled get 1)
              ,(send expression-decompiled get 2))
             ,expression-decompiled)))
     (else
      (make-rose
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
      (make-rose
       `(and (field-bound?
              ,(send expression-decompiled get 2)
              ,(send expression-decompiled get 1))
             ,expression-decompiled)))
     (else
      (make-rose
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
  (make-rose
   `(begin
      ,@(map (lambda (x)
               (decompile-estree x options))
             (get-field body node)))))

;;; Decompile an ESTree [`SequenceExpression`][estree:sequenceexpression] node.
;;;
;;; [estree:sequenceexpression]: https://github.com/estree/estree/blob/master/es5.md#sequenceexpression
(define (decompile-sequence-expression node (options (js-obj)))
  (make-rose
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
    (make-rose
     `(return ,(decompile-estree argument options))))
   (else
    (make-rose
     `(return)))))

;;; Decompile an ESTree [`IfStatement`][estree:ifstatement] node.
;;;
;;; [estree:ifstatement]: https://github.com/estree/estree/blob/master/es5.md#ifstatement
(define (decompile-if-statement node (options (js-obj)))
  (define test
    (decompile-estree (get-field test node) options))
  (define test-exp
    (send test get-value))
  (define consequent
    (decompile-estree (get-field consequent node) options))
  (define consequent-exp
    (send consequent get-value))
  (define alternate
    (if (get-field alternate node)
        (decompile-estree (get-field alternate node) options)
        #f))
  (define alternate-exp
    (and alternate (send alternate get-value)))
  (when (and (tagged-list? consequent-exp 'begin)
             (= (array-list-length consequent-exp) 2))
    (set! consequent (send consequent get 1))
    (set! consequent-exp (send consequent get-value)))
  (when (and (tagged-list? alternate-exp 'begin)
             (= (array-list-length alternate-exp) 2))
    (set! alternate (send alternate get 1))
    (set! alternate-exp (send alternate get-value)))
  (cond
   ((tagged-list? alternate-exp 'when)
    (make-rose
     `(cond
       (,test
        ,@(if (tagged-list? consequent-exp 'begin)
              (send consequent drop 1)
              (list consequent)))
       (,@(send alternate drop 1)))))
   ((tagged-list? alternate-exp 'unless)
    (make-rose
     `(cond
       (,test
        ,@(if (tagged-list? consequent-exp 'begin)
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
      (send alternate-consequent get-value))
    (make-rose
     `(cond
       (,test
        ,@(if (tagged-list? consequent-exp 'begin)
              (send consequent drop 1)
              (list consequent)))
       (,alternate-test
        ,@(if (tagged-list? alternate-consequent-exp 'begin)
              (send alternate-consequent drop 1)
              (list alternate-consequent)))
       ,@(if (> (array-list-length alternate-exp) 3)
             (list `(else ,@(send alternate drop 3)))
             '()))))
   ((tagged-list? alternate-exp 'cond)
    (make-rose
     `(cond
       (,test
        ,@(if (tagged-list? consequent-exp 'begin)
              (send consequent drop 1)
              (list consequent)))
       ,@(send alternate drop 1))))
   ((not alternate)
    (cond
     ((tagged-list? test-exp 'not)
      (make-rose
       `(unless ,(send test get 1)
          ,@(if (tagged-list? consequent-exp 'begin)
                (send consequent drop 1)
                (list consequent)))))
     (else
      (make-rose
       `(when ,test
          ,@(if (tagged-list? consequent-exp 'begin)
                (send consequent drop 1)
                (list consequent)))))))
   ((or (tagged-list? consequent-exp 'begin)
        (tagged-list? alternate-exp 'begin))
    (make-rose
     `(cond
       (,test
        ,@(if (tagged-list? consequent-exp 'begin)
              (send consequent drop 1)
              (list consequent)))
       (else
        ,@(if (tagged-list? alternate-exp 'begin)
              (send alternate drop 1)
              (list alternate))))))
   (else
    (make-rose
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
    (send body get-value))
  (define result
    (make-rose
     `(do ()
          ((not ,test))
        ,@(if (tagged-list? body-exp 'begin)
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
  (define body-exp
    (send body get-value))
  (when (and (tagged-list? body-exp 'begin)
             (= (array-list-length body-exp) 2))
    (set! body (send body get 1)))
  (define result
    (make-rose
     `(js/do-while
       ,body
       ,test)))
  result)

;;; Decompile an ESTree [`ForStatement`][estree:forstatement] node.
;;;
;;; [estree:forstatement]: https://github.com/estree/estree/blob/master/es5.md#forstatement
(define (decompile-for-statement node (options (js-obj)))
  (define init
    (decompile-estree (get-field init node) options))
  (define inits
    (if (is-a? (send init get-value) RoseSplice)
        (send init drop 0)
        (list init)))
  (define test
    (decompile-estree (get-field test node) options))
  (define update
    (decompile-estree (get-field update node) options))
  (define updates
    (if (tagged-list? (send update get-value) 'begin)
        (send update drop 1)
        (list update)))
  (define bindings '())
  (for ((i (range 0 (array-list-length inits))))
    (define current-init
      (aget inits i))
    (define current-init-exp
      (send current-init get-value))
    (when (or (tagged-list? current-init-exp 'define)
              (tagged-list? current-init-exp 'set!))
      (set! current-init
            (make-rose
             `(,@(send current-init drop 1))
             current-init))
      (set! current-init-exp
            (send current-init get-value)))
    (define current-update
      (aget updates i))
    (define current-update-exp
      (send current-update get-value))
    (when (tagged-list? current-update-exp 'begin0)
      (set! current-update
            (send current-update last))
      (set! current-update-exp
            (send current-update get-value)))
    (when (tagged-list? current-update-exp 'set!)
      (set! current-update
            (send current-update third))
      (set! current-update-exp
            (send current-update get-value)))
    (push-right! bindings
                 `(,@(send current-init drop 0)
                   ,current-update)))
  (define body
    (decompile-estree (get-field body node) options))
  (cond
   ((and (= (array-list-length bindings) 1)
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
          (send get 2)
          (send get-value)))
    (when (tagged-list? update '-)
      (set! step (- step)))
    (make-rose
     `(for ((,i (range
                 ,start
                 ,end
                 ,@(if (= step 1)
                       '()
                        (list step)))))
        ,@(send body drop 1))))
   (else
    (make-rose
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
    (send left get-value))
  (when (tagged-list? left-exp 'define)
    (set! left (send left get 1))
    (set! left-exp (send left get-value)))
  (define right
    (decompile-estree (get-field right node) options))
  (define right-exp
    (send right get-value))
  (define body
    (decompile-estree (get-field body node) options))
  (define body-nodes
    (send body drop 1))
  (when (tagged-list? left-exp 'define-values)
    (define sym
      (make-unique-symbol
       (cons right-exp (second left-exp))))
    (push-left! body-nodes
                (make-rose
                 `(,(send left get 0)
                   ,(send left get 1)
                   ,sym)))
    (set! left (make-rose sym)))
  (make-rose
   `(for ((,left ,right))
      ,@body-nodes)))

;;; Decompile an ESTree [`ForInStatement`][estree:forinstatement] node.
;;;
;;; [estree:forinstatement]: https://github.com/estree/estree/blob/master/es5.md#forinstatement
(define (decompile-for-in-statement node (options (js-obj)))
  (define left
    (decompile-estree (get-field left node) options))
  (define left-exp
    (send left get-value))
  (when (tagged-list? left-exp 'define)
    (set! left (send left get 1))
    (set! left-exp (send left get-value)))
  (define right
    (decompile-estree (get-field right node) options))
  (define body
    (decompile-estree (get-field body node) options))
  (make-rose
   `(for ((,left (js-keys ,right)))
      ,@(send body drop 1))))

;;; Decompile an ESTree [`BreakStatement`][estree:breakstatement] node.
;;;
;;; [estree:breakstatement]: https://github.com/estree/estree/blob/master/es5.md#breakstatement
(define (decompile-break-statement node (options (js-obj)))
  (make-rose
   `(break
     ,@(if (get-field label node)
           (list (decompile-estree (get-field label node)
                                   options))
           '()))))

;;; Decompile an ESTree [`ContinueStatement`][estree:continuestatement] node.
;;;
;;; [estree:continuestatement]: https://github.com/estree/estree/blob/master/es5.md#continuestatement
(define (decompile-continue-statement node (options (js-obj)))
  (make-rose
   `(continue
     ,@(if (get-field label node)
           (list (decompile-estree (get-field label node)
                                   options))
           '()))))

;;; Decompile an ESTree [`ThrowStatement`][estree:throwstatement] node.
;;;
;;; [estree:throwstatement]: https://github.com/estree/estree/blob/master/es5.md#throwstatement
(define (decompile-throw-statement node (options (js-obj)))
  (make-rose
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
    (make-rose
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
  (make-rose
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
    (and (> (array-list-length arguments_) 0)
         (estree-type? (array-list-last arguments_) "SpreadElement")))
  (make-rose
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
   ((= (array-list-length specifiers) 0)
    (make-rose
     `(require ,source-decompiled)))
   ((and (= (array-list-length specifiers) 1)
         (estree-type? (first specifiers)
                       "ImportNamespaceSpecifier"))
    (make-rose
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
             (if (eq? (send imported get-value)
                      (send local get-value))
                 imported
                 `(,imported ,local)))
           specifiers))
    (make-rose
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
           (if (eq? (send exported get-value)
                    (send local get-value))
               exported
               `(rename-out (,local ,exported))))
         specifiers))
  (make-rose
   `(provide ,@specifiers-decompiled)))

;;; Decompile an ESTree [`ExportAllDeclaration`][estree:exportalldeclaration] node.
;;;
;;; [estree:exportalldeclaration]: https://github.com/estree/estree/blob/master/es2015.md#exportalldeclaration
(define (decompile-export-all-declaration node (options (js-obj)))
  (define source
    (get-field source node))
  (define source-decompiled
    (decompile-estree source options))
  (make-rose
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
                   (symbol->string
                    (send (decompile-estree
                           (get-field key prop) options)
                          get-value)))
      (push-right! properties
                   (decompile-estree (get-field value prop)
                                     options)))))
  (cond
   ((= (array-list-length spreads) 0)
    (make-rose
     `(js-obj ,@properties)))
   (else
    (make-rose
     `(js-obj-append
       ,@spreads
       ,@(if (> (array-list-length properties) 0)
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
    (if (eq? (send key get-value)
             (send value get-value))
        (push-right! properties key)
        (push-right! properties (list key value))))
  (make-rose properties))

;;; Decompile an ESTree [`TemplateLiteral`][estree:templateliteral] node.
;;;
;;; [estree:templateliteral]: https://github.com/estree/estree/blob/master/es2015.md#templateliteral
(define (decompile-template-literal node (options (js-obj)))
  (define str "")
  (define quasis
    (get-field quasis node))
  (when (> (array-list-length quasis) 0)
    (set! str
          (~> (first quasis)
              (get-field value _)
              (get-field cooked _))))
  (make-rose str))

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
  (make-rose
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
   ((and (> (array-list-length elements) 0)
         (array-list-last elements)
         (estree-type? (array-list-last elements) "RestElement"))
    (define regular-elements
      (map decompile-element
           (drop-right elements 1)))
    (define rest-element
      (decompile-element (array-list-last elements)))
    (make-rose
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
                 (make-rose
                  `(list ,result))))
           elements))
    (make-rose
     `(append ,@elements-decompiled)))
   (else
    (define elements-decompiled
      (map decompile-element elements))
    (make-rose
     `(list ,@elements-decompiled)))))

;;; Decompile an ESTree [`ArrayPattern`][estree:arraypattern] node.
;;;
;;; [estree:arraypattern]: https://github.com/estree/estree/blob/master/es2015.md#arraypattern
(define (decompile-array-pattern node (options (js-obj)))
  (define array-expression
    (decompile-array-expression node options))
  (make-rose
   (if (tagged-list? (send array-expression get-value)
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
  (make-rose 'super))

;;; Decompile an ESTree [`ThisExpression`][estree:thisexpression] node.
;;;
;;; [estree:thisexpression]: https://github.com/estree/estree/blob/master/es5.md#thisexpression
(define (decompile-this-expression node (options (js-obj)))
  (make-rose 'this))

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
  (make-rose
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
  (make-rose
   `(,define-symbol
      ,key-decompiled
      ,@(if (eq? value js/null)
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
    (send key-decompiled get-value))
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
  (make-rose
   `(,define-symbol
      ,(cons key-decompiled-exp
             (aget (send value-decompiled get-value) 1))
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
  (make-rose
   `(ann ,expression-decompiled
         ,type-annotation-decompiled)))

;;; Decompile a TSESTree `TSAnyKeyword` node.
(define (decompile-ts-any-keyword node (options (js-obj)))
  (make-rose 'Any))

;;; Decompile a TSESTree `TSBooleanKeyword` node.
(define (decompile-ts-boolean-keyword node (options (js-obj)))
  (make-rose 'Boolean))

;;; Decompile a TSESTree `TSNumberKeyword` node.
(define (decompile-ts-number-keyword node (options (js-obj)))
  (make-rose 'Number))

;;; Decompile a TSESTree `TSStringKeyword` node.
(define (decompile-ts-string-keyword node (options (js-obj)))
  (make-rose 'String))

;;; Decompile a TSESTree `TSUndefinedKeyword` node.
(define (decompile-ts-undefined-keyword node (options (js-obj)))
  (make-rose 'Undefined))

;;; Decompile a TSESTree `TSVoidKeyword` node.
(define (decompile-ts-void-keyword node (options (js-obj)))
  (make-rose 'Void))

;;; Decompile a TSESTree `TSLiteralType` node.
(define (decompile-ts-literal-type node (options (js-obj)))
  (define literal
    (get-field literal node))
  (define literal-decompiled
    (if literal
        'True
         'False))
  (make-rose literal-decompiled))

;;; Decompile a TSESTree `TSArrayType` node.
(define (decompile-ts-array-type node (options (js-obj)))
  (define element-type
    (get-field elementType node))
  (define element-type-decompiled
    (decompile-estree element-type options))
  (make-rose
   `(Listof ,element-type-decompiled)))

;;; Decompile a TSESTree `TSTupleType` node.
(define (decompile-ts-tuple-type node (options (js-obj)))
  (define element-types
    (get-field elementTypes node))
  (define element-types-decompiled
    (map (lambda (x)
           (decompile-estree x options))
         element-types))
  (make-rose
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
  (make-rose
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
  (make-rose
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
   ((= (array-list-length params-decompiled) 0)
    (make-rose name-decompiled))
   (else
    (make-rose
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
  (make-rose
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
  (when (and (> (array-list-length params) 0)
             (estree-type? (array-list-last (get-field params node))
                           "RestElement"))
    (if (= (array-list-length params) 1)
        (set! params (array-list-last params))
        (set! params (apply list* params))))
  (define body
    (remove-return-tail-call
     (decompile-estree (get-field body node) options)))
  (define body-exp (send body get-value))
  (define body-forms
    (if (tagged-list? body-exp 'begin)
        (send body drop 1)
        (list body)))
  (define async-field
    (get-field async node))
  (cond
   (id
    (cond
     (async-field
      (make-rose
       `(define ,id
          (async
           (,lambda-sym ,params
                        ,@body-forms)))))
     (else
      (make-rose
       `(define ,(cons id params)
          ,@body-forms)))))
   (else
    (cond
     (async-field
      (make-rose
       `(async
         (,lambda-sym ,params
                      ,@body-forms))))
     (else
      (make-rose
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
      (make-rose
       `(,name undefined)))
     (type-annotation
      (make-rose
       `(,name
         :
         ,(decompile-ts-type-annotation
           type-annotation options))))
     (else
      (make-rose name))))
   (else
    (decompile-estree node options))))

;;; Remove superfluous `(return ...)` forms from
;;; a form that occurs in tail call position.
(define (remove-return-tail-call node)
  (define exp (send node get-value))
  (cond
   ((and (tagged-list? exp 'return)
         (= (array-list-length exp) 2))
    (send node get 1))
   ((tagged-list? exp 'begin)
    (make-rose
     `(,@(send node drop-right 1)
       ,(remove-return-tail-call
         (send node get (- (array-list-length exp) 1))))
     node))
   ((tagged-list? exp 'if)
    (make-rose
     `(,(send node get 0)
       ,(send node get 1)
       ,@(map remove-return-tail-call
              (send node drop 2)))
     node))
   ((tagged-list? exp 'cond)
    (make-rose
     `(,(send node get 0)
       ,@(map (lambda (x)
                (make-rose
                 `(,@(send x drop-right 1)
                   ,(remove-return-tail-call
                     (send x
                           get
                           (- (array-list-length (send x get-value))
                              1))))
                 node))
              (send node drop 1)))
     node))
   (else
    node)))

;;; Default decompiler function.
(define (default-decompiler node (options (js-obj)))
  (make-rose
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
