;; SPDX-License-Identifier: MPL-2.0
;; inline-lisp-sources: true
;;; # Language
;;;
;;; Language environment and compiler implementation.
;;;
;;; ## Description
;;;
;;; This file defines the language environment. It also contains most
;;; of the compiler code.
;;;
;;; ## License
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(require (only-in "fs"
                  fstatSync
                  mkdirSync
                  openSync
                  readFileSync
                  writeFileSync))
(require (only-in "path"
                  basename
                  extname
                  join))
(require (only-in "./array"
                  array-drop-right_
                  array-drop_
                  array-eighth_
                  array-fifth_
                  array-first_
                  array-fourth_
                  array-last_
                  array-length_
                  array-ninth_
                  array-ref_
                  array-rest_
                  array-reverse_
                  array-second_
                  array-set_
                  array-seventh_
                  array-sixth_
                  array-take_
                  array-tenth_
                  array-third_
                  array?_))
(require (only-in "./constants"
                  default-language
                  false_
                  js-null_
                  null_
                  package-name
                  quasiquote-sym_
                  quote-sym_
                  true_
                  undefined_))
(require (only-in "./curry"
                  __
                  curry
                  curry-n
                  dashify))
(require (only-in "./env"
                  Environment
                  EnvironmentPipe
                  EnvironmentStack
                  JavaScriptEnvironment
                  LispEnvironment
                  TypedEnvironment
                  current-environment_
                  default-environment
                  empty-environment
                  extend-environment
                  make-environment
                  with-environment))
(require (only-in "./equal"
                  eq?_
                  equal?_
                  eqv?_))
(require (only-in "./estree"
                  ArrayExpression
                  ArrayPattern
                  ArrowFunctionExpression
                  AssignmentExpression
                  AssignmentPattern
                  AwaitExpression
                  BinaryExpression
                  BlockComment
                  BlockStatement
                  BreakStatement
                  CallExpression
                  CatchClause
                  ClassBody
                  ClassDeclaration
                  ClassExpression
                  ConditionalExpression
                  ContinueStatement
                  DoWhileStatement
                  ESTreeNode
                  ExportAllDeclaration
                  ExportNamedDeclaration
                  ExportSpecifier
                  Expression
                  ExpressionStatement
                  ForOfStatement
                  ForStatement
                  FunctionDeclaration
                  FunctionExpression
                  Identifier
                  IfStatement
                  ImportDeclaration
                  ImportDefaultSpecifier
                  ImportNamespaceSpecifier
                  ImportSpecifier
                  LeadingComment
                  Literal
                  LogicalExpression
                  MemberExpression
                  MethodDefinition
                  NewExpression
                  Node
                  ObjectExpression
                  ObjectPattern
                  Program
                  Property
                  PropertyDefinition
                  RestElement
                  ReturnStatement
                  SequenceExpression
                  SpreadElement
                  SwitchCase
                  SwitchStatement
                  TSAnyKeyword
                  TSArrayType
                  TSAsExpression
                  TSBooleanKeyword
                  TSFunctionType
                  TSLiteralType
                  TSNumberKeyword
                  TSStringKeyword
                  TSTupleType
                  TSTypeAliasDeclaration
                  TSTypeParameterInstantiation
                  TSTypeReference
                  TSUndefinedKeyword
                  TSUnionType
                  TSVoidKeyword
                  TaggedTemplateExpression
                  TemplateElement
                  TemplateLiteral
                  ThisExpression
                  ThrowStatement
                  TrailingComment
                  TryStatement
                  UnaryExpression
                  UpdateExpression
                  VariableDeclaration
                  VariableDeclarator
                  WhileStatement
                  XRawJavaScript
                  YieldExpression
                  estree-type?
                  estree?))
(require (only-in "./eval"
                  call-evaluator
                  default-evaluator
                  eval_))
(require (only-in "./hash"
                  hash->list_
                  hash-clear!_
                  hash-clear_
                  hash-copy_
                  hash-entries_
                  hash-has-key?_
                  hash-keys_
                  hash-ref_
                  hash-remove!_
                  hash-remove_
                  hash-set!_
                  hash-size_
                  hash-values_
                  hash?_
                  make-hash_))
(require (only-in "./javascript"
                  js-delete_
                  js-eval_
                  js-find-index_
                  js-function-object?_
                  js-function-type?_
                  js-function?_
                  js-in_
                  js-instanceof_
                  js-is-loosely-equal?_
                  js-is-strictly-equal?_
                  js-null?_
                  js-plus_
                  js-same-value-zero?_
                  js-same-value?_
                  js-tagged-template_
                  js-typeof_))
(require (only-in "./list"
                  append_
                  array-list-cdr_
                  array-list-drop-right_
                  array-list-drop_
                  array-list-eighth_
                  array-list-fifth_
                  array-list-first_
                  array-list-fourth_
                  array-list-last_
                  array-list-length_
                  array-list-ninth_
                  array-list-nth_
                  array-list-nthcdr_
                  array-list-rest_
                  array-list-reverse_
                  array-list-second_
                  array-list-seventh_
                  array-list-sixth_
                  array-list-take_
                  array-list-tenth_
                  array-list-third_
                  array-list->linked-list_
                  array-list?_
                  build-list_
                  cadr_
                  car_
                  cdr_
                  circular-list?_
                  cons-dot-compiled_
                  cons-dot-f_
                  cons-dot?_
                  cons-dot_
                  cons?_
                  cons_
                  dotted-list?_
                  dotted-pair-p_
                  drop-right_
                  drop_
                  eighth_
                  fifth_
                  first_
                  flatten_
                  fourth_
                  improper-list?_
                  last-cdr_
                  last-pair_
                  last_
                  length_
                  linked-list-car_
                  linked-list-cdr_
                  linked-list-drop-right_
                  linked-list-eighth_
                  linked-list-fifth_
                  linked-list-first_
                  linked-list-fourth_
                  linked-list-head_
                  linked-list-last-cdr_
                  linked-list-last_
                  linked-list-length_
                  linked-list-link-car_
                  linked-list-link-cdr_
                  linked-list-link?_
                  linked-list-ninth_
                  linked-list-nth_
                  linked-list-nthcdr_
                  linked-list-second_
                  linked-list-seventh_
                  linked-list-sixth_
                  linked-list-tail_
                  linked-list-tenth_
                  linked-list-third_
                  linked-list->array-list_
                  linked-list?_
                  linked-pair-car_
                  linked-pair-cdr_
                  linked-pair-cdr_
                  linked-pair?_
                  list-star_
                  list-tail_
                  list?_
                  list_
                  make-list_
                  ninth_
                  nth_
                  nthcdr_
                  null?_
                  pop-left!_
                  pop-right!_
                  proper-list?_
                  push-left!_
                  push-right!_
                  rest_
                  reverse_
                  second_
                  set-car!_
                  set-cdr!_
                  seventh_
                  sixth_
                  take_
                  tenth_
                  third_))
(require (only-in "./macros"
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
                  while_))
(require (only-in "./object"
                  field-names_
                  js-keys_
                  js-obj-append_
                  js-obj-p_
                  js-obj_
                  js-object-type?_
                  object-ref_
                  object-set!_))
(require (only-in "./parser"
                  LeadingCommentToken
                  TrailingCommentToken
                  get-comment-level
                  read
                  read-rose
                  read-sexp
                  tokenize))
(require (only-in "./plist"
                  plist->alist_
                  plist-copy_
                  plist-get_
                  plist-has?_
                  plist-set!_
                  plist?_))
(require (only-in "./printer"
                  print
                  print-estree
                  print-sexp
                  print-sexp-as-expression
                  write-to-string))
(require (only-in "./procedures"
                  add1_
                  add_
                  apply_
                  assert_
                  boolean?_
                  const_
                  display_
                  div_
                  error_
                  even?_
                  false?_
                  fexpr?_
                  filter_
                  findf-index_
                  findf_
                  foldl_
                  foldr_
                  funcall_
                  gt_
                  gte_
                  identity_
                  index-of_
                  index-where_
                  is-a?_
                  intersection_
                  keyword?_
                  lt_
                  lte_
                  map_
                  member?_
                  member_
                  memf?_
                  memf_
                  memq?_
                  memq_
                  modulo_
                  mul_
                  not_
                  number?_
                  odd?_
                  one?_
                  procedure?_
                  range_
                  self-evaluating?_
                  sub1_
                  sub_
                  true?_
                  type-of_
                  undefined?_
                  union_
                  values_
                  zero?_))
(require (only-in "./regexp"
                  regexp-match?_
                  regexp-match_
                  regexp-quote_
                  regexp-replace_
                  regexp?_
                  regexp_))
(require (only-in "./rose"
                  Rose
                  begin-wrap-rose
                  begin-wrap-rose-smart
                  begin-wrap-rose-smart-1
                  insert-sexp-into-rose
                  make-rose
                  rose?
                  slice-rose
                  transfer-comments))
(require (only-in "./sexp"
                  s
                  sexp))
(require (only-in "./string"
                  number->string_
                  string->number_
                  string-append_
                  string-downcase_
                  string-join_
                  string-object?_
                  string-primitive?_
                  string-ref_
                  string-repeat_
                  string-replace_
                  string-split_
                  string-trim_
                  string-upcase_
                  string?_
                  substring_))
(require (only-in "./symbol"
                  gensym_
                  gensym?_
                  string->symbol_
                  symbol->string_
                  symbol?_))
(require (only-in "./thunk"
                  thunk
                  ThunkedMap))
(require (only-in "./util"
                  begin-wrap
                  colon-form?
                  form?
                  kebab-case->camel-case
                  kebab-case->snake-case
                  lambda->let
                  map-tree
                  quote?
                  tagged-list?
                  text-of-quotation))
(require (only-in "./visitor"
                  make-visitor
                  visit))

;;; Default options for interpretation and compilation.
;;; See also `default-compilation-options`.
(define default-options
  (js-obj "comments" #t
          "expressionType" "expression"
          "eval" #f
          "shouldInline" #t
          "inlineFunctions" #f
          "compileEnvironment" #t
          "gensymMap" (make-hash)))

;;; Add `default-options` to an options object.
;;; If `modify` is `#t`, the original object
;;; is modified, otherwise a new object is returned.
(define (add-default-options options (modify #f))
  (define result
    (if modify
        options
        (js-obj-append options)))
  (for ((key (js-keys default-options)))
    (when (eq? (oget result key) undefined)
      (oset! result key (oget default-options key))))
  result)

;;; Compilation environment class.
;;;
;;; A compilation environment is a typed environment mapping
;;; Lisp functions to compiled values, compiler procedures
;;; or compiler macros.
(define-class CompilationEnvironment (TypedEnvironment))

;;; Compilation variable environment.
;;;
;;; An environment mapping various Lisp values to their
;;; JavaScript equivalents.
(define compilation-variables-env
  (new CompilationEnvironment
       `((,(string->symbol "#f") ,(new Literal #f) "variable")
         (,(string->symbol "#t") ,(new Literal #t) "variable")
         (,(string->symbol "js-null") ,(new Literal js/null) "variable")
         (,(string->symbol "js-undefined") ,(new Literal undefined) "variable")
         (,(string->symbol "js/arguments") ,(new Identifier "arguments") "variable")
         (,(string->symbol "js/null") ,(new Literal js/null) "variable")
         (,(string->symbol "js/require") ,(new Identifier "require") "variable")
         (,(string->symbol "js/undefined") ,(new Literal undefined) "variable")
         (,(string->symbol "*cons-dot*") ,cons-dot-compiled_ "variable")
         (,(string->symbol "nil") ,(new Literal js/null) "variable")
         (,(string->symbol "null") ,(new ArrayExpression) "variable")
         (,(string->symbol "t") ,(new Literal #t) "variable")
         (,(string->symbol "undefined") ,(new Literal undefined) "variable"))))

;;; Compiler procedures mapping environment.
(define compilation-compiler-mapping-env
  (new CompilationEnvironment
       `((,add_ ,compile-add "compiler")
         (,and_ ,compile-and "compiler")
         (,ann_ ,compile-ann "compiler")
         (,append_ ,compile-append "compiler")
         (,apply_ ,compile-apply "compiler")
         (,array-ref_ ,compile-array-ref "compiler")
         (,array-set_ ,compile-array-set "compiler")
         (,begin_ ,compile-begin "compiler")
         (,block_ ,compile-block "compiler")
         (,break_ ,compile-break "compiler")
         (,class_ ,compile-class "compiler")
         (,colon_ ,compile-colon "compiler")
         (,cond_ ,compile-cond "compiler")
         (,continue_ ,compile-continue "compiler")
         (,define-async_ ,compile-define-async "compiler")
         (,define-class_ ,compile-define-class "compiler")
         (,define-generator_ ,compile-define-generator "compiler")
         (,define-js-obj_ ,compile-define-js-obj "compiler")
         (,define-macro_ ,compile-define-macro "compiler")
         (,define-type_ ,compile-define-type "compiler")
         (,define-values_ ,compile-define-values "compiler")
         (,define_ ,compile-define "compiler")
         (,div_ ,compile-div "compiler")
         (,dot_ ,compile-send "compiler")
         (,for_ ,compile-for "compiler")
         (,funcall_ ,compile-funcall "compiler")
         (,get-field_ ,compile-get-field "compiler")
         (,gt_ ,compile-greater-than "compiler")
         (,gte_ ,compile-greater-than-or-equal "compiler")
         (,js-arrow_ ,compile-js-arrow "compiler")
         (,js-async_ ,compile-js-async "compiler")
         (,js-await_ ,compile-js-await "compiler")
         (,js-delete_ ,compile-js-delete "compiler")
         (,js-do-while_ ,compile-js-do-while "compiler")
         (,js-eval_ ,compile-js-eval "compiler")
         (,js-function_ ,compile-js-function "compiler")
         (,js-in_ ,compile-js-in "compiler")
         (,js-instanceof_ ,compile-js-instanceof "compiler")
         (,js-is-loosely-equal?_ ,compile-js-is-loosely-equal "compiler")
         (,js-is-strictly-equal?_ ,compile-js-is-strictly-equal "compiler")
         (,js-obj-append_ ,compile-js-obj-append "compiler")
         (,js-obj_ ,compile-js-obj "compiler")
         (,js-optional-chaining_ ,compile-js-optional-chaining "compiler")
         (,js-plus_ ,compile-add "compiler")
         (,js-switch_ ,compile-js-switch "compiler")
         (,js-tagged-template_ ,compile-js-tagged-template "compiler")
         (,js-try_ ,compile-js-try "compiler")
         (,js-typeof_ ,compile-js-typeof "compiler")
         (,js-while_ ,compile-js-while "compiler")
         (,js_ ,compile-js "compiler")
         (,lambda_ ,compile-lambda "compiler")
         (,let-js-obj_ ,compile-let-js-obj "compiler")
         (,let-star_ ,compile-let "compiler")
         (,let-values_ ,compile-let-values "compiler")
         (,list_ ,compile-list "compiler")
         (,lt_ ,compile-less-than "compiler")
         (,lte_ ,compile-less-than-or-equal "compiler")
         (,module_ ,compile-module "compiler")
         (,modulo_ ,compile-modulo "compiler")
         (,mul_ ,compile-mul "compiler")
         (,new_ ,compile-new "compiler")
         (,not_ ,compile-not "compiler")
         (,object-ref_ ,compile-object-ref "compiler")
         (,object-set!_ ,compile-object-set "compiler")
         (,or_ ,compile-or "compiler")
         (,provide_ ,compile-provide "compiler")
         (,push-left!_ ,compile-push-left "compiler")
         (,push-right!_ ,compile-push-right "compiler")
         (,quasiquote_ ,compile-quasiquote "compiler")
         (,quote_ ,compile-quote "compiler")
         (,require_ ,compile-require "compiler")
         (,return_ ,compile-return "compiler")
         (,send-apply_ ,compile-send-apply "compiler")
         (,send_ ,compile-send "compiler")
         (,set!_ ,compile-set "compiler")
         (,set-field_ ,compile-set-field "compiler")
         (,set-js-obj_ ,compile-set-js-obj "compiler")
         (,set-values_ ,compile-set-values "compiler")
         (,string-append_ ,compile-string-append "compiler")
         (,sub_ ,compile-sub "compiler")
         (,throw_ ,compile-throw "compiler")
         (,yield_ ,compile-yield "compiler"))))

;;; Compiler macros mapping environment.
(define compilation-macro-mapping-env
  (new CompilationEnvironment
       `((,array-drop-right_ ,compile-array-drop-right-macro "macro")
         (,array-drop_ ,compile-array-drop-macro "macro")
         (,array-list-drop-right_ ,compile-array-list-drop-right-macro "macro")
         (,array-list-drop_ ,compile-array-list-drop-macro "macro")
         (,assert_ ,compile-assert-macro "macro")
         (,display_ ,compile-display-macro "macro")
         (,drop-right_ ,compile-drop-right-macro "macro")
         (,drop_ ,compile-drop-macro "macro")
         (,foldl_ ,compile-foldl-macro "macro")
         (,foldr_ ,compile-foldr-macro "macro")
         (,hash-clear_ ,compile-hash-clear-macro "macro")
         (,hash-ref_ ,compile-hash-ref-macro "macro")
         (,hash-remove!_ ,compile-hash-remove-macro "macro")
         (,hash-remove_ ,compile-hash-remove-macro "macro")
         (,make-hash_ ,compile-make-hash-macro "macro")
         (,map_ ,compile-map-macro "macro")
         (,member?_ ,compile-member-p-macro "macro")
         (,print ,compile-display-macro "macro")
         (,regexp_ ,compile-regexp-macro "macro")
         (,string-trim_ ,compile-string-trim-macro "macro")
         (,string?_ ,compile-stringp-macro "macro")
         (,substring_ ,compile-substring-macro "macro")
         (,values_ ,compile-values-macro "macro"))))

;;; Compilation mapping environment.
;;;
;;; An environment mapping Lisp functions to compiler procedures
;;; or compiler macros.
(define compilation-mapping-env
  (new EnvironmentStack
       compilation-macro-mapping-env
       compilation-compiler-mapping-env))

;;; Inlined functions.
;;;
;;; A list of functions whose definition is so simple
;;; that it might be inlined directly into the call site.
(define inlined-functions
  (list
   add1_
   array-eighth_
   array-fifth_
   array-first_
   array-fourth_
   array-last_
   array-length_
   array-list-cdr_
   array-list-eighth_
   array-list-fifth_
   array-list-first_
   array-list-fourth_
   array-list-last_
   array-list-length_
   array-list-ninth_
   array-list-nth_
   array-list-nthcdr_
   array-list-rest_
   array-list-reverse_
   array-list-second_
   array-list-seventh_
   array-list-sixth_
   array-list-take_
   array-list-tenth_
   array-list-third_
   array-list?_
   array-ninth_
   array-rest_
   array-reverse_
   array-second_
   array-seventh_
   array-sixth_
   array-take_
   array-tenth_
   array-third_
   array?_
   boolean?_
   cons-dot-f_
   cons-dot?_
   cons?_
   const_
   dotted-list?_
   dotted-pair-p_
   eighth_
   eq?_
   eqv?_
   error_
   even?_
   field-names_
   fifth_
   filter_
   findf-index_
   first_
   fourth_
   gensym?_
   gensym_
   hash->list_
   hash-clear!_
   hash-copy_
   hash-entries_
   hash-has-key?_
   hash-keys_
   hash-remove!_
   hash-set!_
   hash-size_
   hash-values_
   hash?_
   index-where_
   is-a?_
   js-find-index_
   js-function-object?_
   js-function-type?_
   js-function?_
   js-keys_
   js-null?_
   js-obj-p_
   js-object-type?_
   js-same-value?_
   linked-list-car_
   linked-list-cdr_
   linked-list-eighth_
   linked-list-fifth_
   linked-list-first_
   linked-list-fourth_
   linked-list-head_
   linked-list-link-car_
   linked-list-link-cdr_
   linked-list-link?_
   linked-list-ninth_
   linked-list-nth_
   linked-list-nthcdr_
   linked-list-second_
   linked-list-seventh_
   linked-list-sixth_
   linked-list-tail_
   linked-list-tenth_
   linked-list-third_
   linked-list?_
   linked-pair-car_
   linked-pair-cdr_
   linked-pair?_
   list?_
   memf?_
   memq?_
   ninth_
   nth_
   null?_
   number->string_
   number?_
   odd?_
   one?_
   plist-copy_
   plist?_
   pop-left!_
   pop-right!_
   procedure?_
   regexp-match?_
   regexp-match_
   regexp-quote_
   regexp-replace_
   regexp?_
   rest_
   reverse_
   second_
   seventh_
   sixth_
   string->number_
   string->symbol_
   string-downcase_
   string-join_
   string-object?_
   string-primitive?_
   string-ref_
   string-repeat_
   string-split_
   string-upcase_
   sub1_
   symbol->string_
   symbol?_
   tenth_
   third_
   type-of_
   undefined?_
   zero?_))

;;; Compilation map.
;;;
;;; Map from languages to compilation mapping environments.
(define compilation-map
  (make-hash
   `(("JavaScript" . ,compilation-mapping-env)
     ("TypeScript" . ,compilation-mapping-env))))

;;; Compile a Lisp expression to JavaScript or TypeScript.
;;; Returns a string of JavaScript or TypeScript code.
;;;
;;; `exp` may be an S-expression, an S-expression wrapped in a rose
;;; tree, or a module object.
(define (compile exp
                 (env (new LispEnvironment))
                 (options (js-obj)))
  (define language-option
    (or (oget options "language")
        default-language))
  (define lang-env
    (if (extends-lisp-environment? env)
        env
        (new EnvironmentStack
             env
             lang-environment)))
  (define mapping-env
    (or (hash-ref compilation-map language-option)
        compilation-mapping-env))
  (define compilation-options
    (add-default-options options #t))
  (define compiled-env
    (new LispEnvironment))
  (define bindings-env
    (new LispEnvironment))
  (define continuation-env
    (new EnvironmentStack bindings-env env))
  (oset! compilation-options "lispEnvironment" lang-env)
  (oset! compilation-options
         "compilationMappingEnvironment"
         mapping-env)
  (oset! compilation-options "bindings" bindings-env)
  (oset! compilation-options "continuationEnv" continuation-env)
  (oset! compilation-options "compiledEnv" compiled-env)
  (define ast
    (cond
     ((is-a? exp Module)
      (compile-module exp lang-env compilation-options))
     ((is-a? exp Rose)
      (compile-rose exp lang-env compilation-options))
     (else
      (compile-sexp exp lang-env compilation-options))))
  (set! ast (optimize-estree ast))
  (print-estree ast compilation-options))

;;; Compile a set of modules together.
;;; The modules may reference one another.
(define (compile-modules modules env (options (js-obj)))
  (define module-map
    (make-hash))
  (define compiled-module-map)
  (define module-name)
  (for ((module modules))
    (unless (is-a? module Rose)
      (set! module (make-rose module)))
    (set! module-name
          (~> (send module get 1)
              (send _ get-value)))
    (when (symbol? module-name)
      (set! module-name
            (symbol->string module-name)))
    (set! module-name
          (regexp-replace (regexp "^\\./") module-name ""))
    (hash-set! module-map module-name module))
  (set! compiled-module-map
        (compile-module-map module-map env options))
  ;; `(,@(send compiled-module-map values))
  (append (send compiled-module-map values)))

;;; Compile a module map.
;;; Returns a new map containing compiled modules.
(define (compile-module-map module-map env (options (js-obj)))
  (define result
    (make-hash))
  (define module-object-map
    (make-module-map module-map env))
  (define compiled-module)
  (define module)
  (for ((key (send module-object-map keys)))
    (set! module (send module-object-map get key))
    (set! compiled-module
          (compile module env options))
    (hash-set! result key compiled-module))
  result)

;;; Compile a module expression or object.
(define (compile-module obj env (options (js-obj)))
  (cond
   ((is-a? obj Module)
    (compile-module-object obj env options))
   (else
    (compile-module-expression obj env options))))

;;; Compile a `(module ...)` expression.
(define (compile-module-expression node env (options (js-obj)))
  (define module
    (module-expression-to-module-object node env))
  (define compilation-options
    (js-obj-append
     options
     (js-obj "currentModule" module)))
  (compile-module-object module env compilation-options))

;;; Compile a `Module` object.
(define (compile-module-object module env (options (js-obj)))
  (define expressions
    (send module get-expressions))
  (define bindings
    (or (oget options "bindings")
        (new LispEnvironment)))
  (define module-environment
    (send module get-environment))
  (define module-options
    (js-obj-append
     (js-obj "bindings"
             bindings
             "currentModule"
             module
             "referencedSymbols"
             '()
             "inlineLispSources"
             (send module get-inline-lisp-sources-flag))
     options))
  (define header-statements
    (compile-statement
     (begin-wrap-rose
      (get-field header-nodes module))
     module-environment module-options))
  (define require-statements
    (compile-statement
     (begin-wrap-rose
      (get-field require-nodes module))
     module-environment module-options))
  (define main-statements
    (compile-statement
     (begin-wrap-rose
      (get-field main-nodes module))
     module-environment module-options))
  (define provide-statements
    (compile-statement
     (begin-wrap-rose
      (get-field provide-nodes module))
     module-environment module-options))
  (define global-environment
    (build-global-environment
     (oget module-options "referencedSymbols")
     module-environment options))
  (define program
    (make-program
     (append (get-field body header-statements)
             (get-field body require-statements)
             (get-field body global-environment)
             (get-field body main-statements)
             (get-field body provide-statements))))
  program)

;;; Compile a set of files.
;;; This function writes to disk.
(define (compile-files! files (options (js-obj)))
  (define module-expression-map
    (new ThunkedMap))
  (define filename-map
    (new ThunkedMap))
  (define indent-option
    (oget options "indent"))
  (define language-option
    (if (regexp-match (regexp "^TypeScript$" "i")
                      (or (oget options "language")
                          ""))
        "TypeScript"
        "JavaScript"))
  (define out-dir-option
    (or (oget options "outDir") ""))
  (define comments-option
    (oget options "comments"))
  (define quick-option
    (oget options "quick"))
  (define compilation-options
    (js-obj-append
     options
     (js-obj "expressionType" "statement"
             "language" language-option)))
  (define extension
    (if (eq? language-option "TypeScript")
        ".ts"
        ".js"))
  (define code)
  (define data)
  (define module)
  (define module-name)
  (define module-names '())
  (define module-map)
  (define node)
  (define out-file)
  (for ((file files))
    (set! module-name
          (basename file (extname file)))
    (hash-set! filename-map
               module-name
               file)
    (hash-set! module-expression-map
               module-name
               (thunk
                (lambda ()
                  (define data
                    (~> file
                        (readFileSync _ (js-obj "encoding" "utf8"))
                        (regexp-replace (regexp "^#!.*") _ "")
                        (string-append
                         "(module m scheme\n"
                         _
                         "\n)")))
                  (define node
                    (read-rose data
                               (js-obj "comments"
                                       comments-option)))
                  node)))
    (cond
     (quick-option
      (define should-compile #f)
      (try
        (define in-file file)
        (define in-stats
          (fstatSync (openSync in-file "r")))
        (define out-file
          (join out-dir-option
                (string-append module-name
                               extension)))
        (define out-stats
          (fstatSync (openSync out-file "r")))
        (when (> (get-field mtimeMs in-stats)
                 (get-field mtimeMs out-stats))
          (set! should-compile #t))
        (catch Error err
          (set! should-compile #t)))
      (when should-compile
        (push-right! module-names module-name)))
     (else
      (push-right! module-names module-name))))
  (set! module-map
        (make-module-map module-expression-map
                         compilation-environment))
  (for ((module-name module-names))
    (set! module
          (send module-map get module-name))
    (set! code
          (compile module
                   compilation-environment
                   compilation-options))
    (set! out-file
          (join out-dir-option
                (string-append module-name
                               extension)))
    (mkdirSync out-dir-option
               (js-obj "recursive" #t))
    (writeFileSync out-file
                   code
                   (js-obj "encoding" "utf8"))
    (display
     (string-append "Compiled "
                    (hash-ref filename-map module-name)
                    " to "
                    out-file)))
  module-map)

;;; Compile a file.
;;; This function writes to disk.
(define (compile-file! infile outfile (options (js-obj)))
  ;; TODO: `outfile`. Maybe by adding an
  ;; `outFileMap` option to `compile-files!`?
  (compile-files! (list infile) options))

;;; Compile a S-expression wrapped in a rose tree.
(define (compile-rose node env (options (js-obj)))
  (define inherited-options
    (if (oget options "bindings")
        options
        (js-obj-append
         options
         (js-obj "bindings"
                 (new LispEnvironment)))))
  (define bindings
    (oget inherited-options "bindings"))
  (define comments-option
    (oget options "comments"))
  (define node1
    (optimize-rose node env))
  (define exp
    (send node1 get-value))
  (define result)
  (cond
   ((array? exp)
    (cond
     ((= (array-list-length exp) 0)
      (set! result
            (compile-list
             node1 env inherited-options)))
     (else
      (define op
        (first exp))
      (cond
       ((and (symbol? op)
             (send bindings has op)
             (not (eq? (send bindings get-type op)
                       "macro")))
        (set! result
              (compile-function-call
               node1 env inherited-options)))
       ((and (symbol? op)
             (regexp-match (regexp "^\\.")
                           (symbol->string op)))
        (set! result
              (compile-dot
               node1 env inherited-options)))
       (else
        (define-values (f op-type)
          (send env get-typed-value op))
        (cond
         ((eq? op-type "undefined")
          (set! result
                (compile-function-call
                 node1 env inherited-options)))
         ((memq? f inlined-functions)
          (define inlined-exp
            (definition->macro (source f) (rest exp)))
          (define inlined-node
            (make-rose inlined-exp node))
          (set! result
                (compile-rose inlined-node env options)))
         (else
          (define compilation-mapping-environment
            (oget inherited-options "compilationMappingEnvironment"))
          (define-values (compilation-f compilation-type)
            (send compilation-mapping-environment get-typed-value f))
          (cond
           ;; Compiler function.
           ((eq? compilation-type "compiler")
            (set! result
                  (compilation-f
                   node1 env
                   inherited-options)))
           ;; Compilation macro.
           ((eq? compilation-type "macro")
            (set! result
                  (compile-rose
                   (insert-sexp-into-rose
                    (compilation-f exp env)
                    node1
                    node1)
                   env
                   inherited-options)))
           ;; Macro call.
           ((eq? op-type "macro")
            (set! result
                  (compile-macro-call
                   node1 env
                   inherited-options)))
           (else
            (set! result
                  (compile-function-call
                   node1 env
                   inherited-options)))))))))))
   ((string? exp)
    (set! result
          (compile-string
           node1 env inherited-options)))
   ((symbol? exp)
    (set! result
          (compile-variable
           node1 env inherited-options)))
   ((estree? exp)
    (set! result exp))
   (else
    (set! result
          (compile-atom
           node1 env inherited-options))))
  (when (and comments-option
             (send node1 has-property "comments"))
    (define comments
      (send node1 get-property "comments"))
    (when (> (array-list-length comments) 0)
      (set-field! comments
                  result
                  (compile-comments comments))))
  result)

;;; Compile a S-expression.
(define (compile-sexp exp env (options (js-obj)))
  (~> (make-rose exp)
      (compile-rose _ env options)))

;;; Compile `node` as an expression.
(define (compile-expression node env (options (js-obj)))
  (compile-rose node env (make-expression-options options)))

;;; Compile `node` as a regular statement.
(define (compile-statement node env (options (js-obj)))
  (compile-rose node env (make-statement-options options)))

;;; Compile `node` as a return statement.
(define (compile-return-statement node env (options (js-obj)))
  (compile-rose node env (make-return-statement-options options)))

;;; Compile `node` as a regular statement or as a return statement,
;;; depending on the value of the `expressionType` option.
(define (compile-statement-or-return-statement node env (options (js-obj)))
  (cond
   ((eq? (oget options "expressionType") "return")
    (compile-return-statement node env options))
   (else
    (compile-statement node env options))))

;;; Helper function for compiling a list of statements.
;;; The last statement is compiled as a `return` statement
;;; if the `expressionType` option is `"return"`.
(define (compile-statements statements env options)
  (define expression-type
    (oget options "expressionType"))
  (define result '())
  (define return-idx -1)
  (when (eq? expression-type "return")
    (for ((i (range (- (array-list-length statements) 1) -1 -1)))
      (define statement
        (aget statements i))
      (unless (or (form? statement break_ env)
                  (form? statement continue_ env)
                  (form? statement yield_ env))
        (set! return-idx i)
        (break))))
  (for ((i (range 0 (array-list-length statements))))
    (define statement
      (aget statements i))
    (cond
     ((= i return-idx)
      (push-right! result
                   (compile-return-statement
                    statement env options)))
     (else
      (push-right! result
                   (compile-statement
                    statement env options)))))
  ;; TODO: If the last statement is a `break`/`yield` statement and
  ;; the penultimate statement is a `return` statement, we can drop
  ;; the last statement. (Might want a setting to make this behavior
  ;; toggleable, though.)
  result)

;;; Evaluate a Lisp expression `exp` with environment `env`.
;;;
;;; `env`, if specified, must be a Lisp environment as returned
;;; by {@link Environment}. The expression is evaluated in
;;; context of a basic Lisp environment defining such constructs
;;; as `(if ...)`, `(cond ...)`, and so on.
(define interpret
  (dashify
   (lambda (exp (env (default-environment)) (options (js-obj)))
     (define evaluator
       (or (oget options "evaluator")
           eval_
           default-evaluator))
     (define environment
       (make-interpretation-environment env options))
     (call-evaluator evaluator
                     exp
                     environment
                     options))))

;;; Interpret a string of Lisp code.
(define (interpret-string str (env undefined) (options (js-obj)))
  (interpret (read-sexp str) env options))

;;; Interpret a list of files.
(define (interpret-files files (env undefined) (options (js-obj)))
  (map (lambda (file)
         (define str
           (~> file
               (readFileSync _ (js-obj "encoding" "utf8"))
               (regexp-replace (regexp "^#!.*") _ "")
               (string-append "(begin\n" _ "\n)")))
         (define result
           (interpret-string str env options))
         result)
       files))

;;; Interpret a string of Lisp code.
;;; Alias for `interpret-string`.
(define (lisp str (env undefined))
  (interpret-string str env))

;;; Make a Lisp environment.
(define (make-lisp (variables '())
                   (is-lisp-2 #f))
  (new LispEnvironment
       variables
       lisp-environment))

;;; Make a Lisp interpretation environment.
(define (make-interpretation-environment env (options (js-obj)))
  (define eval-option
    (oget options "eval"))
  ;; TODO: Make `#f` the default.
  (when (eq? eval-option undefined)
    (set! eval-option #t))
  (cond
   ((or (eq? env lang-environment)
        (and (is-a? env EnvironmentStack)
             (send env has-environment lang-environment)))
    env)
   (else
    (new EnvironmentStack
         env
         (if eval-option
             interpretation-environment
             interpretation-environment-no-eval)))))

;;; Make an environment suitable for expanding macros
;;; and compiler macros.
(define (make-macro-environment env)
  (new EnvironmentStack
       (new EnvironmentPipe
            env
            compilation-macro-mapping-env)
       env))

;;; Make compilation options for compiling a form as
;;; an expression.
(define (make-expression-options options)
  (js-obj-append
   options
   (js-obj "expressionType" "expression")))

;;; Make compilation options for compiling a form as
;;; a statement.
(define (make-statement-options options)
  (js-obj-append
   options
   (js-obj "expressionType" "statement")))

;;; Make compilation options for compiling a form as
;;; a return statement.
(define (make-return-statement-options options)
  (js-obj-append
   options
   (js-obj "expressionType" "return")))

;;; Make an expression or statement ESTree node,
;;; conditional on options.
(define (make-expression-or-statement node (options (js-obj)))
  (define expression-type
    (oget options "expressionType"))
  (cond
   ((or (eq? expression-type "statement")
        (eq? expression-type "return"))
    (wrap-expression-in-statement node options))
   (else
    node)))

;;; Wrap an expression in a statement. An `ExpressionStatement`
;;; or `ReturnStatement` node is returned, conditional on options.
(define (wrap-expression-in-statement node (options (js-obj)))
  (define expression-type
    (oget options "expressionType"))
  (cond
   ((not (is-a? node Expression))
    node)
   ((eq? expression-type "return")
    (new ReturnStatement node))
   (else
    (new ExpressionStatement node))))

;;; Wraps `node` in a `BlockStatement`.
(define (wrap-in-block-statement obj)
  (make-block-statement (list obj)))

;;; Wraps `node` in a `BlockStatement` unless `node` already is
;;; a `BlockStatement`. In other words, avoids double wrapping.
(define (wrap-in-block-statement-smart node)
  (cond
   ((estree-type? node "BlockStatement")
    node)
   (else
    (make-block-statement
     (list node)))))

;;; Wrap `exp` in a `lambda` call.
(define (wrap-in-lambda-call exp)
  (make-rose
   `((lambda () ,exp))))

;;; Wrap `exp` in a `js/arrow` call.
(define (wrap-in-arrow-call exp)
  (make-rose
   `((js/arrow () ,exp))))

;;; Make a `BlockStatement`.
;;; Handles `Program` fragments.
(define (make-block-statement body)
  (cond
   ((array? body)
    (new BlockStatement
         (make-block-statement-helper body)))
   (else
    (make-block-statement (list body)))))

;;; Helper function for `make-block-statement`.
(define (make-block-statement-helper body)
  (define statements '())
  (for ((statement body))
    (cond
     ((estree-type? statement "Program")
      ;; Program fragments are represented with `Program`.
      ;; Their contents are spliced into the block statement.
      (define fragment statement)
      (define fragment-statements
        (get-field body fragment))
      (define fragment-comments
        (get-field comments fragment))
      (cond
       ((> (array-list-length fragment-statements) 0)
        (transfer-comments fragment (first fragment-statements))
        (set! statements
              (append statements fragment-statements)))
       ((> (array-list-length fragment-comments) 0)
        (push-right! statements statement))))
     (else
      (push-right! statements statement))))
  statements)

;;; Make a `Program`.
(define (make-program body)
  (new Program (make-block-statement-helper body)))

;;; Make a `Program` fragment (i.e., a program that
;;; is to be spliced into the containing program).
(define (make-program-fragment (body '()))
  ;; `Program` is used to represent programs
  ;; and program fragments.
  (make-program body))

;;; Make an empty program fragment.
(define (empty-program)
  (make-program-fragment))

;;; Unwrap a `BlockStatement`, i.e., return the expression it
;;; contains. The statement is assumed to contain a single
;;; expression.
(define (unwrap-block-statement exp)
  (unless (estree-type? exp "BlockStatement")
    (return exp))
  (define unwrapped-exp exp)
  (while (and (= (array-list-length (get-field body unwrapped-exp))
                 1)
              (estree-type? (first (get-field body unwrapped-exp))
                            "BlockStatement"))
    (set! unwrapped-exp
          (first (get-field body unwrapped-exp))))
  unwrapped-exp)

;;; Remove the comment prefix (`; `, `;; `, `;;; `, etc.)
;;; from a comment string.
(define (remove-comment-prefix comment)
  (regexp-replace (regexp "^[^\\S\\r\\n]*[;]+[^\\S\\r\\n]?" "gm")
                  comment
                  ""))

;;; Transfer the `comments` property from ESTree `node1` to ESTree `node2`,
;;; compiling them in the process.
(define (transfer-and-compile-comments node1 node2 (options (js-obj)))
  (define comments-option
    (oget options "comments"))
  (define comments
    (if (is-a? node1 Rose)
        (send node1 get-property "comments")
        (get-field comments node1)))
  (when (and comments-option comments)
    (if (is-a? node2 Rose)
        (send node2
              set-property
              "comments"
              (append comments
                      (or (send node2
                                get-property
                                "comments")
                          '())))
        (set! comments
              (compile-comments comments))
        (set-field! comments
                    node2
                    (append comments
                            (or (get-field comments node2)
                                '())))))
  node2)

;;; Compile comments.
(define (compile-comments comments)
  (define comments-compiled '())
  (for ((comment comments))
    (cond
     ((is-a? comment LeadingCommentToken)
      (define subcomments
        (split-comments (get-field value comment)))
      (for ((subcomment subcomments))
        (cond
         ((>= (get-comment-level subcomment) 3)
          (push-right! comments-compiled
                       (new BlockComment
                            (remove-comment-prefix
                             subcomment))))
         (else
          (push-right! comments-compiled
                       (new LeadingComment
                            (remove-comment-prefix
                             subcomment)))))))
     ((is-a? comment TrailingCommentToken)
      (push-right! comments-compiled
                   (new TrailingComment
                        (remove-comment-prefix
                         (get-field value comment)))))))
  comments-compiled)

;;; Split up a string containing multiple comments.
(define (split-comments str)
  (define comments '())
  (define comment "")
  (define current-level -1)
  (define lines
    (string-split str "\n"))
  (when (regexp-match (regexp "\\n$") str)
    (set! lines (drop-right lines 1)))
  (for ((x lines))
    (cond
     ((eq? x "")
      (cond
       ((regexp-match (regexp "\\n$") comment)
        (set! comment
              (string-append comment "\n"))
        (push-right! comments comment)
        (set! comment ""))
       (else
        (set! comment
              (string-append comment "\n")))))
     (else
      (define level
        (get-comment-level x))
      (unless (= level current-level)
        (unless (or (eq? comment "")
                    (eq? comment "\n"))
          (push-right! comments comment)
          (set! comment ""))
        (set! current-level level))
      (set! comment
            (string-append comment x "\n")))))
  (unless (or (eq? comment "")
              (eq? comment "\n"))
    (push-right! comments comment))
  comments)

;;; Whether `exp` is a function call, given `env`.
(define (function-call? exp env)
  (cond
   ((is-a? exp Rose)
    (macro-call? (send exp get-value) env))
   (else
    (and (array? exp)
         (> (array-list-length exp) 1)
         (symbol? (first exp))
         (memq? (send env get-type (first exp))
                '("function"
                  "procedure"))))))

;;; Whether `exp` is a macro call, given `env`.
(define (macro-call? exp env)
  (cond
   ((is-a? exp Rose)
    (macro-call? (send exp get-value) env))
   (else
    (and (array? exp)
         (> (array-list-length exp) 1)
         (symbol? (first exp))
         (eq? (send env get-type (first exp))
              "macro")))))

;;; Whether `exp` is a special form, given `env`.
(define (special-form? exp env)
  (cond
   ((is-a? exp Rose)
    (macro-call? (send exp get-value) env))
   (else
    (and (array? exp)
         (> (array-list-length exp) 1)
         (symbol? (first exp))
         (eq? (send env get-type (first exp))
              "special")))))

;;; Convert a `(define (...) ...)` form to
;;; a `(lambda (...) ...)` form.
(define (define->lambda node (options (js-obj)))
  (define curried-option
    (oget options "curried"))
  (define exp
    (send node get-value))
  (define name-and-params
    (second exp))
  (define name
    (car name-and-params))
  (define params
    (cdr name-and-params))
  (define should-curry
    (or curried-option
        (and (eq? curried-option undefined)
             (array? name))))
  (when should-curry
    (set! params (rest (flatten name-and-params)))
    (when (and (dotted-list? name-and-params)
               (= (array-list-length params) 1))
      (set! params (first params))))
  (make-rose
   `(lambda ,params
      ,@(send node drop 2))))

;;; Convert a function to a macro on the basis
;;; of its `(define ...)` form.
(define (definition->macro exp args)
  ;; TODO: Rest arguments.
  ;;
  ;; FIXME: When a complex argument is referenced inside of a `lambda`
  ;; expression, we should store the value in a local variable.
  (define params-list
    (rest (second exp)))
  (define params
    (map (lambda (x)
           (if (array? x)
               (first x)
               x))
         params-list))
  (define body
    (drop exp 2))
  (cond
   ((= (array-list-length params) 0)
    (cond
     ((= (array-list-length body) 1)
      (first body))
     (else
      `(begin ,@body))))
   (else
    (define counts
      (build-list (array-list-length args)
                  (const 0)))
    (define should-make-lambda #f)
    (define should-make-let #f)
    (define result
      (map (lambda (x)
             (map-tree
              (lambda (y)
                (define idx
                  (js/find-index
                   (lambda (z)
                     (eq? z y))
                   params))
                (cond
                 ((>= idx 0)
                  (list-set! counts
                             idx
                             (+ (aget counts idx)
                                1))
                  (cond
                   ((< idx (array-list-length args))
                    (aget args idx))
                   (else
                    (define current-param
                      (aget params-list idx))
                    (cond
                     ((array? current-param)
                      (second current-param))
                     (else
                      undefined)))))
                 (else
                  y)))
              x))
           body))
    ;; Determine whether a complex argument is referenced
    ;; more than once. If so, we need to make a `lambda`
    ;; expression instead.
    (for ((i (range 0 (array-list-length args))))
      (define count
        (aget counts i))
      (define arg
        (aget args i))
      (when (and (> count 1)
                 (not (or (symbol? arg)
                          (boolean? arg)
                          (string? arg)
                          (number? arg))))
        ;; (set! should-make-lambda #t)
        (set! should-make-let #t)
        (break)))
    (cond
     (should-make-let
      (define let-bindings '())
      (define gensym-map
        (make-hash))
      (for ((i (range 0 (array-list-length params))))
        (define arg-exp
          (cond
           ((< i (array-list-length args))
            (aget args i))
           (else
            (define current-param
              (aget params-list i))
            (cond
             ((array? current-param)
              (second current-param))
             (else
              undefined)))))
        (define param-exp
          (aget params i))
        (define param
          (if (array? param-exp)
              (first param-exp)
              param-exp))
        (cond
         ((symbol? arg-exp)
          (hash-set! gensym-map param arg-exp))
         (else
          (define param-gensym
            (gensym (symbol->string param)))
          (hash-set! gensym-map param param-gensym)
          (push-right! let-bindings
                       (list param-gensym arg-exp)))))
      (define let-body
        (map-tree (lambda (x)
                    (cond
                     ((hash-has-key? gensym-map x)
                      (hash-ref gensym-map x))
                     (else
                      x)))
                  body))
      `(let* ,let-bindings
         ,@let-body))
     (should-make-lambda
      `((lambda ,params
          ,@body)
        ,@args))
     (else
      (cond
       ((= (array-list-length result) 1)
        (first result))
       (else
        `(begin ,@result))))))))

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
  (define rest-arg undefined)
  (cond
   ((list? args)
    (define i 0)
    (while (< i (array-list-length args))
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

;;; Create a macro function on the basis of a
;;; `(define-macro ...)` expression.
(define (define-macro->function exp env)
  (define macro-fn
    (define-macro->lambda-form exp))
  (eval_ macro-fn env))

;;; Convert a `(define ... (class ...))` expression to
;;; a `(define-class ...)` expression.
(define (define->define-class node)
  (cond
   ((is-a? node Rose)
    (define superclass
      (send (send node get 2) get 1))
    (define superclass-exp
      (send superclass get-value))
    (define superclass-list
      (if (memq? superclass-exp
                 '(object%
                   object
                   Object))
          '()
          (list superclass)))
    (transfer-comments
     node
     (make-rose
      `(define-class ,(send node get 1)
         ,(make-rose superclass-list)
         ,@(send (send node get 2) drop 2)))))
   (else
    (send (define->define-class
            (make-rose node))
          get-value))))

;;; Wrap `f-exp` in a unary function wrapper.
(define (compile-map-macro-helper f-exp env)
  (cond
   ;; If `f-exp` is a symbolic expression, then wrap it in a
   ;; `lambda` expression.
   ((symbol? f-exp)
    `(lambda (x)
       (,f-exp x)))
   ;; If `f-exp` is an anonymous unary function, then there is
   ;; no need to wrap it.
   ((and (or (form? f-exp lambda_ env)
             (form? f-exp js-function_ env)
             (form? f-exp js-arrow_ env))
         (array? (second f-exp))
         (= (array-list-length (second f-exp)) 1))
    f-exp)
   (else
    ;; Curried function application, i.e., the **A** combinator
    ;; defined as a curried function. Calling this function with
    ;; a single argument produces a unary function wrapper that
    ;; calls a function with a single argument and disregards any
    ;; additional arguments.
    (define A-exp
      '(lambda (f)
         (lambda (x)
           (f x))))
    `(,A-exp ,f-exp))))

;;; Compile an `(and ...)` expression.
(define (compile-and node env (options (js-obj)))
  (compile-logical-expression
   node env
   options
   (js-obj "identity" #t
           "operator" "&&")))

;;; Compile an `(ann ...)` expression.
(define (compile-ann node env (options (js-obj)))
  (define language
    (oget options "language"))
  (define e_
    (send node get 1))
  (cond
   ((eq? language "TypeScript")
    (define t_
      (send node get 2))
    (make-expression-or-statement
     (new TSAsExpression
          (compile-expression e_ env options)
          (compile-type t_ env options))
     options))
   (else
    (compile-rose e_ env options))))

;;; Compile a `(define-type ...)` expression.
(define (compile-define-type node env (options (js-obj)))
  (define language
    (oget options "language"))
  (cond
   ((eq? language "TypeScript")
    (define id
      (compile-expression
       (send node get 1) env options))
    (define type_
      (compile-type
       (send node get 2) env options))
    (transfer-and-compile-comments
     node
     (new TSTypeAliasDeclaration id type_)
     options))
   (else
    (empty-program))))

;;; Compile a type expression.
(define (compile-type node env (options (js-obj)))
  (define exp
    (if (is-a? node Rose)
        (send node get-value)
        node))
  (compile-type-exp exp env options))

;;; Helper function for `compile-type`.
(define (compile-type-exp exp env (options (js-obj)))
  (cond
   ((symbol? exp)
    (cond
     ((eq? exp 'Any)
      (new TSAnyKeyword))
     ((eq? exp 'Void)
      (new TSVoidKeyword))
     ((eq? exp 'Undefined)
      (new TSUndefinedKeyword))
     ((eq? exp 'Boolean)
      (new TSBooleanKeyword))
     ((eq? exp 'True)
      (new TSLiteralType
           (new Literal #t)))
     ((eq? exp 'False)
      (new TSLiteralType
           (new Literal #f)))
     ((eq? exp 'Number)
      (new TSNumberKeyword))
     ((eq? exp 'Integer)
      (new TSNumberKeyword))
     ((eq? exp 'Natural)
      (new TSNumberKeyword))
     ((eq? exp 'Real)
      (new TSNumberKeyword))
     ((eq? exp 'String)
      (new TSStringKeyword))
     (else
      (new TSTypeReference
           (new Identifier
                (symbol->string exp))))))
   ((tagged-list? exp 'List)
    (new TSTupleType
         (map (lambda (x)
                (compile-type-exp
                 x env options))
              (rest exp))))
   ((tagged-list? exp 'Listof)
    (new TSArrayType
         (compile-type-exp
          (second exp)
          env
          options)))
   ((tagged-list? exp 'Pairof)
    (compile-type-exp
     `(Listof (U ,(second exp)
                 Symbol))
     env
     options))
   ((tagged-list? exp 'U)
    (new TSUnionType
         (map (lambda (x)
                (compile-type-exp
                 x env options))
              (rest exp))))
   ((or (tagged-list? exp '->)
        (tagged-list? exp '->*))
    (define mandatory-params
      (if (tagged-list? exp '->*)
          (second exp)
          (drop (drop-right exp 1) 1)))
    (define optional-params
      (if (and (tagged-list? exp '->*)
               (> (array-list-length exp) 3))
          (third exp)
          '()))
    (define return-value
      (array-list-last exp))
    (define i 0)
    (define (compile-param param optional)
      (define var-name
        (number->letter i))
      (set! i (+ i 1))
      (~> (new Identifier var-name optional)
          (send set-type
                (compile-type-exp param env options))))
    (new
     TSFunctionType
     (append (map (lambda (param)
                    (compile-param param #f))
                  mandatory-params)
             (map (lambda (param)
                    (compile-param param #t))
                  optional-params))
     (compile-type-exp return-value env options)))
   ((and (array? exp)
         (> (array-list-length exp) 0))
    (define name
      (new Identifier
           (symbol->string (first exp))))
    (define params
      (map symbol->string (rest exp)))
    (cond
     ((> (array-list-length params) 0)
      (new TSTypeReference
           name
           (new TSTypeParameterInstantiation
                params)))
     (else
      (new TSTypeReference name))))
   (else
    (new TSAnyKeyword))))

;;; Convert a number to a letter.
;;; `0` corresponds to `a`, `1` to `b`, etc.
(define (number->letter n)
  (~> (send "a" charCodeAt 0)
      (+ _ n)
      (send String fromCharCode _)))

;;; "NO-OP" operation.
(define (nop_ exp env)
  undefined)

;;; Compile a `(+ ...)` expression.
(define (compile-add node env (options (js-obj)))
  (compile-binary-expression
   node env
   options
   (js-obj "identity" 0
           "operator" "+")))

;;; Compile an `(apply ...)` expression.
(define (compile-apply node env (options (js-obj)))
  (define exp
    (send node get-value))
  (define f
    (second exp))
  (define is-make-object
    (eq? (send env get f) new_))
  (define callee
    (if is-make-object
        (third exp)
        f))
  (define args
    (if is-make-object
        (drop exp 3)
        (drop exp 2)))
  (define callee-compiled
    (compile-expression
     (make-rose callee)
     env options))
  (define args-compiled '())
  (when (> (array-list-length args) 0)
    (define regular-args
      (drop-right args 1))
    (define rest-arg
      (array-list-last args))
    (for ((arg regular-args))
      (push-right! args-compiled
                   (compile-expression
                    (make-rose arg)
                    env options)))
    (push-right! args-compiled
                 (new RestElement
                      (compile-expression
                       (make-rose rest-arg)
                       env options))))
  (cond
   (is-make-object
    (make-expression-or-statement
     (new NewExpression
          callee-compiled
          args-compiled)
     options))
   (else
    (make-expression-or-statement
     (new CallExpression
          callee-compiled
          args-compiled)
     options))))

;;; Compile an `(array-ref ...)` expression.
(define (compile-array-ref node env (options (js-obj)))
  (define variable
    (send node get 1))
  (define variable-compiled
    (compile-expression variable env options))
  (define indices
    (send node drop 2))
  (define indices-compiled
    (map (lambda (x)
           (compile-expression x env options))
         indices))
  (define result
    (foldl (lambda (x acc)
             (new MemberExpression acc x #t))
           variable-compiled
           indices-compiled))
  (make-expression-or-statement result options))

;;; Compile an `(array-set! ...)` expression.
(define (compile-array-set node env (options (js-obj)))
  (define exp
    (send node get-value))
  (define arr
    (second exp))
  (define indices
    (drop-right (drop exp 2) 1))
  (define value
    (aget exp (- (array-list-length exp) 1)))
  (compile-rose
   (insert-sexp-into-rose
    `(set! (array-ref ,arr ,@indices) ,value)
    node)
   env options))

;;; Compile an `(object-ref ...)` expression.
(define (compile-object-ref node env (options (js-obj)))
  (compile-array-ref node env options))

;;; Compile an `(object-set! ...)` expression.
(define (compile-object-set node env (options (js-obj)))
  (compile-array-set node env options))

;;; Compile an atomic expression, such as `foo`.
(define (compile-atom node env (options (js-obj)))
  (make-expression-or-statement
   (new Literal (send node get-value))
   options))

;;; Compile a `(: ...)` expression.
(define (compile-colon node env (options (js-obj)))
  (define bindings
    (oget options "bindings"))
  (define sym
    (send node get 1))
  (define sym-exp
    (send sym get-value))
  (define type_
    (send node get 2))
  (define type-exp
    (send type_ get-value))
  (define binding-type "variable")
  (when bindings
    (when (send bindings has sym-exp)
      (set! binding-type
            (send bindings get-type sym-exp)))
    ;; FIXME: This is a kludge. We need a better way
    ;; of storing types---either a separate environment,
    ;; or a typed environment, perhaps.
    (send bindings set-local sym-exp type-exp binding-type))
  (compile-nop node env options))

;;; Compile a `(cond ...)` expression.
(define (compile-cond node env (options (js-obj)))
  (define expression-type
    (oget options "expressionType"))
  (define cond-clauses
    (send node drop 1))
  (cond
   ((= (array-list-length cond-clauses) 0)
    (make-expression-or-statement
     (new Literal #f)
     options))
   (else
    (define (reducing-f cond-clause compiled-exp)
      (define condition
        (send cond-clause get 0))
      (define then-clauses
        (begin-wrap-rose-smart
         (send cond-clause drop 1)))
      (cond
       ((or (eq? expression-type "statement")
            (eq? expression-type "return"))
        (new IfStatement
             (compile-expression
              condition
              env options)
             (transfer-and-compile-comments
              cond-clause
              (unwrap-block-statement
               (wrap-in-block-statement-smart
                (compile-statement-or-return-statement
                 then-clauses
                 env options)))
              options)
             compiled-exp))
       (else
        (new ConditionalExpression
             (compile-expression
              condition
              env options)
             (transfer-and-compile-comments
              cond-clause
              (compile-expression
               then-clauses
               env options)
              options)
             compiled-exp))))
    (cond
     ((eq? (~> (array-list-last cond-clauses)
               (send get 0)
               (send get-value))
           'else)
      (define final-clause
        (cond
         ((or (eq? expression-type "statement")
              (eq? expression-type "return"))
          (transfer-and-compile-comments
           (array-list-last cond-clauses)
           (wrap-in-block-statement-smart
            (compile-statement-or-return-statement
             (begin-wrap-rose-smart-1
              (send (array-list-last cond-clauses)
                    drop 1))
             env options))
           options))
         (else
          (transfer-and-compile-comments
           (array-list-last cond-clauses)
           (compile-expression
            (begin-wrap-rose
             (send (array-list-last cond-clauses)
                   drop 1))
            env options)
           options))))
      (unless (or (not (eq? expression-type
                            "statement"))
                  (estree-type? final-clause
                                "BlockStatement"))
        (set! final-clause
              (make-block-statement
               (list final-clause))))
      (foldr reducing-f
             final-clause
             (drop-right cond-clauses 1)))
     (else
      (foldr reducing-f
             (if (or (eq? expression-type "statement")
                     (eq? expression-type "return"))
                 js/null
                 (new Identifier "undefined"))
             cond-clauses))))))

;;; Compile a `(define ...)` expression.
(define (compile-define node env (options (js-obj)))
  (define bindings
    (oget options "bindings"))
  (define language
    (oget options "language"))
  (define inline-lisp-source-option
    (oget options "inlineLispSources"))
  (define exp
    (send node get-value))
  (define type_ 'Any)
  (cond
   ;; Function definition.
   ((array? (second exp))
    (define sym
      (first (second exp)))
    (define should-curry
      (array? sym))
    (define name-sym
      (if should-curry
          (first (flatten sym))
          sym))
    (define function-name
      (print-estree
       (compile-symbol
        (make-rose name-sym)
        env
        (make-expression-options
         options))
       options))
    (define lambda-exp
      (define->lambda node))
    (define return-type
      (cond
       ((eq? (~> (send node get 2)
                 (send get-value))
             ':)
        (~> (send node get 3)
            (send get-value)))
       (else
        'Any)))
    (define params
      (~> (send lambda-exp get 1)
          (send get-value)))
    (define declared-type
      (send bindings get sym))
    (cond
     ((or (eq? declared-type undefined)
          (eq? declared-type #t)
          (eq? declared-type 'Any))
      (set! type_
            `(->
              ,@(cond
                 ((symbol? params)
                  (list '(Listof Any)))
                 ((dotted-list? params)
                  (append
                   (make-list (- (array-list-length params) 2) 'Any)
                   (list '(Listof Any))))
                 (else
                  (make-list (array-list-length params) 'Any)))
              ,return-type))
      (send bindings set-local (second exp) type_ "procedure"))
     (else
      (set! type_ declared-type)))
    (define compiled-type
      (compile-type-exp type_ env options))
    (define result)
    (cond
     (should-curry
      (set! result
            (compile-define
             (make-rose
              `(define ,name-sym
                 ;; (curry ,lambda-exp)
                 ,lambda-exp)
              node)
             env
             options)))
     (else
      (define return-type
        (if (and (is-a? compiled-type
                        TSFunctionType)
                 (is-a? (get-field returnType
                                   compiled-type)
                        TSVoidKeyword))
            "void"
            undefined))
      (set! result
            (compile-js-function
             lambda-exp
             env
             (make-expression-options
              options)
             (js-obj "functionName" function-name
                     "returnType" return-type)))
      (when (is-a? compiled-type TSFunctionType)
        (for ((i (range 0 (array-list-length (get-field params result)))))
          (define param
            (aget (get-field params result) i))
          (define type-param
            (aget (get-field params compiled-type) i))
          (define type-param-annotation
            (if type-param
                (get-field typeAnnotation type-param)
                (new TSAnyKeyword)))
          (unless (send param has-type)
            (send param set-type type-param-annotation)))
        (set-field! returnType
                    result
                    (get-field returnType
                               compiledType)))))
    (cond
     (inline-lisp-source-option
      (define lisp-code-exp
        (compile-sexp
         `(set-field! lispSource
                      ,(string->symbol function-name)
                      (quote ,exp))
         env options))
      (new Program (list result lisp-code-exp)))
     (else
      result)))
   ;; Uninitialized variable.
   ((= (array-list-length exp) 2)
    (send bindings set-local (second exp) #t "variable")
    (new VariableDeclaration
         (list (new VariableDeclarator
                    (compile-expression
                     (send node get 1)
                     env
                     options)))
         "let"))
   ;; Asynchronous function definition.
   ((and (form? (third exp) js-async_ env)
         (form? (second (third exp)) lambda_ env))
    (define lambda-node
      (send (send node get 2) get 1))
    (define name
      (send node get 1))
    (define args
      (send (send lambda-node get 1) drop 0))
    (define da-form
      (transfer-comments
       node
       (make-rose
        `(define/async
           (,name ,@args)
           ,@(send lambda-node drop 2)))))
    (compile-define-async da-form env options))
   ;; Class definition.
   ((form? (third exp) class_ env)
    (compile-define-class
     (make-rose
      (define->define-class exp)
      node)
     env options))
   ;; Initialized variable.
   (else
    (define sym
      (second exp))
    (define sym-compiled
      (compile-symbol
       (send node get 1)
       env
       options))
    (cond
     ((send bindings has sym)
      (set! type_
            (send bindings get sym)))
     (else
      (send bindings set-local (second exp) type_ "variable")))
    (new VariableDeclaration
         (list (new VariableDeclarator
                    (~> sym-compiled
                        (send set-type
                              (compile-type
                               type_ env options)))
                    (compile-expression
                     (send node get 2)
                     env
                     options)))
         "let"))))

;;; Compile a `(define/async ...)` expression.
(define (compile-define-async node env (options (js-obj)))
  (define inline-lisp-source-option
    (oget options "inlineLispSources"))
  (define result
    (compile-define node env options))
  (define result-f
    (if inline-lisp-source-option
        (first (get-field body result))
        result))
  (when (estree-type? result-f "FunctionDeclaration")
    (set-field! async result-f #t))
  (define return-type
    (get-field returnType result-f))
  (set-field! returnType
              result-f
              (new TSTypeReference
                   (new Identifier "Promise")
                   (new TSTypeParameterInstantiation
                        (list
                         (new TSAnyKeyword)))))
  result)

;;; Compile a `(define/generator ...)` expression.
(define (compile-define-generator node env (options (js-obj)))
  (define result
    (compile-define node env options))
  (set-field! generator result #t)
  result)

;;; Compile a `(/ ...)` expression.
(define (compile-div node env (options (js-obj)))
  (define exp
    (send node get-value))
  (cond
   ((= (array-list-length exp) 2)
    (compile-div
     (make-rose
      `(/ 1 ,(send node get 1))
      node)
     env options))
   (else
    (compile-binary-expression
     node env options
     (js-obj "identity" 1
             "operator" "/")))))

;;; Compile a `(send ...)` expression.
(define (compile-send node env (options (js-obj)))
  (define obj
    (send node get 1))
  (define method
    (send node get 2))
  (define args
    (send node drop 3))
  (make-expression-or-statement
   (new CallExpression
        (new MemberExpression
             (if (symbol? (send obj get-value))
                 (compile-symbol
                  obj env
                  (make-expression-options
                   options))
                 (compile-expression
                  obj env options))
             (compile-symbol
              method
              env
              options)
             #f)
        (map (lambda (x)
               (compile-expression
                x env options))
             args))
   options))

;;; Compile a `(send/apply ...)` expression.
(define (compile-send-apply node env (options (js-obj)))
  (define obj
    (send node get 1))
  (define method
    (send node get 2))
  (define args
    (send node drop 3))
  (make-expression-or-statement
   (compile-expression
    (make-rose
     `(apply (get-field ,method ,obj) ,@args)
     node)
    env options)
   options))

;;; Compile a `(js/=== ...)` expression.
(define (compile-js-is-strictly-equal node env (options (js-obj)))
  (compile-binary-expression
   node
   env
   options
   (js-obj "identity" #t
           "operator" "===")))

;;; Compile a `(js/== ...)` expression.
(define (compile-js-is-loosely-equal node env (options (js-obj)))
  (compile-binary-expression
   node
   env
   options
   (js-obj "identity" #t
           "operator" "==")))

;;; Compiler macro for `(foldl ...)` expressions.
(defmacro compile-foldl-macro (f v lst &environment env)
  ;; `foldl()` and `.reduce()` invoke the reducing function with
  ;; opposite argument order, and `.reduce()` passes additional
  ;; arguments to it. We therefore wrap it in a binary function
  ;; wrapper that reverses the order of the two first arguments
  ;; and disregards the other arguments.
  `(send ,lst reduce ,(flip-function-expression f env) ,v))

;;; Compiler macro for `(foldr ...)` expressions.
(defmacro compile-foldr-macro (f v lst &environment env)
  ;; Like `foldl`, but invokes the `reduceRight` method instead.
  `(send ,lst reduceRight ,(flip-function-expression f env) ,v))

;;; Given an expression that designates a binary function,
;;; produce a new expression that flips the argument order.
;;; Helper function for `compile-foldl-macro` and
;;; `compile-foldr-macro`.
(define (flip-function-expression exp env)
  (cond
   ;; Function expression is a symbol:
   ;; wrap it in a `lambda` form that reverses
   ;; the order of application.
   ((symbol? exp)
    `(lambda (acc x)
       (,exp x acc)))
   ;; Function expression is a `lambda` form:
   ;; swap the two first arguments.
   ((and (form? exp lambda_ env)
         (>= (array-list-length (second exp)) 2))
    `(lambda (,(second (second exp))
              ,(first (second exp))
              ,@(drop (second exp) 2))
       ,@(drop exp 2)))
   ;; Function expression is a function call:
   ;; pass it to a function that will
   ;; swap the arguments.
   (else
    ;; Curried **C** combinator, also known as `flip`.
    ;; Only the first argument is curried here, but
    ;; otherwise, this behaves similarly to Haskell's
    ;; `flip`.
    (define C-exp
      '(lambda (f)
         (lambda (x y)
           (f y x))))
    `(,C-exp ,exp))))

;;; Compile a `(funcall ...)` expression.
(define (compile-funcall node env (options (js-obj)))
  (compile-function-call
   (slice-rose node 1)
   env options))

;;; Compile a function call.
(define (compile-function-call node env (options (js-obj)))
  (define bindings
    (oget options "bindings"))
  (define referenced-symbols
    (oget options "referencedSymbols"))
  (define current-module
    (oget options "currentModule"))
  (define compilation-mapping-environment
    (oget options "compilationMappingEnvironment"))
  (define callee
    (send node get 0))
  (define op
    (send callee get-value))
  (define symbolic-op
    (symbol? op))
  (define should-inline-op
    (and symbolic-op
         (should-inline? op env options)
         ;; Do not inline the operator if a
         ;; compilation macro is defined for it.
         (not (send compilation-mapping-environment
                    has
                    (send env get op)))))
  (define args
    (send node drop 1))
  (define callee-exp
    (compile-expression
     callee env
     (if (and symbolic-op
              (not should-inline-op))
         ;; Set the `shouldInline` option to `#f`
         ;; if `op` is a symbol and there is a
         ;; compilation macro defined for it.
         ;; options
         (js-obj-append
          options
          (js-obj "shouldInline" #f))
         options)))
  (define args-exps
    (map (lambda (x)
           (compile-expression
            x env options))
         args))
  (make-expression-or-statement
   (new CallExpression callee-exp args-exps)
   options))

;;; Add symbol `sym` to `referencedSymbols` if it references a value
;;; not defined in the current module.
(define (add-referenced-symbol sym env (options (js-obj)))
  (define referenced-symbols
    (oget options "referencedSymbols"))
  (when (and referenced-symbols
             ;; Do not add if already added.
             (not (memq? sym referenced-symbols))
             (should-inline? sym env options))
    (push-right! referenced-symbols sym)))

;;; Whether the language binding for `sym` should be added to
;;; the global environment.
(define (should-inline? sym env (options (js-obj)))
  (define should-inline-option
    (oget options "shouldInline"))
  (define bindings
    (oget options "bindings"))
  (define compilation-mapping-environment
    (oget options "compilationMappingEnvironment"))
  (define current-module
    (oget options "currentModule"))
  ;; This may be disabled with the `shouldInline` option.
  (unless should-inline-option
    (return #f))
  (and (symbol? sym)
       ;; Do not inline if the symbol is listed in
       ;; `send compilation-variables-env`.
       (not (send compilation-variables-env has sym))
       ;; Do not inline if there is a local binding for the
       ;; value (e.g., a `let` variable).
       (not (and bindings
                 (send bindings has sym)))
       ;; Do not inline if the current module defines the
       ;; value.
       (not (and current-module
                 (send current-module has-symbol sym)))
       ;; Only inline if the environment binds the symbol.
       (send env
             find-frame
             sym
             (js-obj "filter"
                     (lambda (x)
                       ;; Do not inline if the value is a JavaScript
                       ;; value, i.e., if it is provided by the very
                       ;; language compiled to.
                       (not (eq? x js-environment)))))))

;;; Compile a `(> ...)` expression.
(define (compile-greater-than node env (options (js-obj)))
  (define exp (send node get-value))
  (cond
   ((< (array-list-length exp) 3)
    (compile-rose
     (make-rose #t)
     env options))
   ((= (array-list-length exp) 3)
    (compile-binary-expression
     node env
     options
     (js-obj "identity" #t
             "operator" ">")))
   (else
    ;; Create `(and ...)` expression.
    (define and-exp
      `(and))
    (for ((i (range 2 (array-list-length exp))))
      (push-right! and-exp
                   `(> ,(aget exp (- i 1))
                       ,(aget exp i))))
    (compile-rose
     (make-rose and-exp)
     env options))))

;;; Compile a `(>= ...)` expression.
(define (compile-greater-than-or-equal node env (options (js-obj)))
  (define exp
    (send node get-value))
  (cond
   ((< (array-list-length exp) 3)
    (compile-rose
     (make-rose #t)
     env options))
   ((= (array-list-length exp) 3)
    (compile-binary-expression
     node env
     options
     (js-obj "identity" #t
             "operator" ">=")))
   (else
    ;; Create `(and ...)` expression.
    (define and-exp
      `(and))
    (for ((i (range 2 (array-list-length exp))))
      (push-right! and-exp
                   `(>= ,(aget exp (- i 1))
                        ,(aget exp i))))
    (compile-rose
     (make-rose and-exp)
     env options))))

;;; Compile a binary expression.
;;; Returns a `BinaryExpression`.
(define (compile-binary-expression
         node
         env
         (options (js-obj))
         (settings (js-obj)))
  (define operator
    (oget settings "operator"))
  (define logical
    (oget settings "logical"))
  (define operands
    (send node drop 1))
  (cond
   ((= (array-list-length operands) 0)
    (define identity
      (oget settings "identity"))
    (make-expression-or-statement
     (compile-rose
      (make-rose identity)
      env options)
     options))
   ((= (array-list-length operands) 1)
    (make-expression-or-statement
     (compile-rose
      (first operands)
      env
      options)
     options))
   (else
    (define compiled-operands
      (map (lambda (arg)
             (compile-expression
              arg env
              options))
           operands))
    (make-expression-or-statement
     (foldl (lambda (x acc)
              (if logical
                  (new LogicalExpression
                       operator
                       acc
                       x)
                  (new BinaryExpression
                       operator
                       acc
                       x)))
            (first compiled-operands)
            (rest compiled-operands))
     options))))

;;; Compile a logical expression.
;;; Like `compile-binary-expression`, but
;;; returns a `LogicalExpression` instead.
(define (compile-logical-expression
         node
         env
         (options (js-obj))
         (settings (js-obj)))
  (compile-binary-expression
   node
   env
   options
   (js-obj-append
    settings
    (js-obj "logical" #t))))

;;; Compile a `(lambda ...)` expression.
(define (compile-lambda node env (options (js-obj)))
  ;; (compile-js-arrow node env options)
  (compile-js-function node env options))

;;; Compile a `(js/function ...)` expression.
(define (compile-js-function node env (options (js-obj)) (settings (js-obj)))
  (define inherited-options
    (js-obj-append options))
  (define exp
    (send node get-value))
  (define function-name
    (oget settings "functionName"))
  (define generator
    (oget settings "generator"))
  (define return-type
    (oget settings "returnType"))
  (define language
    (oget inherited-options "language"))
  (define params '())
  (define bindings
    (oget inherited-options "bindings"))
  (define args-list)
  (define regular-args)
  (define rest-arg)
  (set! bindings
        (extend-environment (new LispEnvironment)
                            bindings))
  (oset! inherited-options "bindings" bindings)
  ;; Parse the parameter list: sort the regular parameters
  ;; from the rest parameter, if any.
  (cond
   ((symbol? (second exp))
    (set! rest-arg (second exp)))
   ((dotted-list? (second exp))
    (set! args-list (second exp))
    (set! regular-args (linked-list-drop-right_ args-list 1))
    (set! rest-arg
          ;; (linked-list-last_ args-list)
          (dotted-list-tail args-list)))
   (else
    (set! regular-args (second exp))))
  (when regular-args
    (for ((arg regular-args))
      (cond
       ((colon-form? arg)
        (define sym
          (first arg))
        (define typ
          (third arg))
        (send bindings set-local sym #t "variable")
        (define result
          (~> (if (= (array-list-length arg) 4)
                  (new AssignmentPattern
                       (new Identifier
                            (print-estree
                             (compile-expression
                              (make-rose sym)
                              env inherited-options)
                             inherited-options))
                       (compile-expression
                        (make-rose (fourth arg))
                        env inherited-options))
                  (new Identifier
                       (print-estree
                        (compile-expression
                         (make-rose sym)
                         env inherited-options)
                        inherited-options)))
              (send set-type
                    (compile-type typ env options))))
        (push-right! params result))
       ((array? arg)
        (send bindings set-local (first arg) #t "variable")
        (push-right! params
                     (new AssignmentPattern
                          (new Identifier
                               (print-estree
                                (compile-expression
                                 (make-rose
                                  (first arg))
                                 env inherited-options)
                                inherited-options))
                          (compile-expression
                           (make-rose
                            (second arg))
                           env inherited-options))))
       (else
        (send bindings set-local arg #t "variable")
        (push-right! params
                     (new Identifier
                          (print-estree
                           (compile-expression
                            (make-rose arg)
                            env inherited-options)
                           inherited-options)))))))
  (when rest-arg
    (send bindings set-local rest-arg #t "variable")
    (push-right! params
                 (new RestElement
                      (compile-expression
                       (make-rose rest-arg)
                       env inherited-options))))
  (define body-statements
    (send node drop 2))
  (when (and (> (array-list-length body-statements) 0)
             (eq? (send (first body-statements) get-value)
                  ':))
    (set! body-statements (drop body-statements 2)))
  (define body
    (wrap-in-block-statement ; wrap-in-block-statement-smart
     (compile-statement-or-return-statement
      (~> (begin-wrap-rose-smart-1
           body-statements)
          (send set-parent node))
      env
      (js-obj-append
       inherited-options
       (js-obj "expressionType"
               (if (eq? return-type "void")
                   "statement"
                   "return"))))))
  (define result)
  (cond
   ((and function-name
         (not (eq? function-name "")))
    (set! result
          (new FunctionDeclaration
               (new Identifier function-name)
               params
               body)))
   (else
    (set! result
          (new FunctionExpression
               params
               body))))
  (when generator
    (set-field! generator result #t))
  (make-expression-or-statement
   result inherited-options))

;;; Compile a `(js/arrow ...)` expression.
(define (compile-js-arrow node env (options (js-obj)))
  (define f
    (compile-js-function node env options))
  (cond
   ((is-a? f FunctionExpression)
    (new ArrowFunctionExpression
         (get-field params f)
         (get-field body f)))
   (else
    f)))

;;; Compile a `(< ...)` expression.
(define (compile-less-than node env (options (js-obj)))
  (define exp
    (send node get-value))
  (cond
   ((< (array-list-length exp) 3)
    (compile-rose
     (make-rose #t)
     env options))
   ((= (array-list-length exp) 3)
    (compile-binary-expression
     node env
     options
     (js-obj "identity" #t
             "operator" "<")))
   (else
    ;; Create `(and ...)` expression.
    (define and-exp
      `(and))
    (for ((i (range 2 (array-list-length exp))))
      (push-right! and-exp
                   `(< ,(aget exp (- i 1))
                       ,(aget exp i))))
    (compile-rose
     (make-rose and-exp)
     env options))))

;;; Compile a `(<= ...)` expression.
(define (compile-less-than-or-equal node env (options (js-obj)))
  (define exp
    (send node get-value))
  (cond
   ((< (array-list-length exp) 3)
    (compile-rose
     (make-rose #t)
     env options))
   ((= (array-list-length exp) 3)
    (compile-binary-expression
     node env
     options
     (js-obj "identity" #t
             "operator" "<=")))
   (else
    ;; Create `(and ...)` expression.
    (define and-exp
      `(and))
    (for ((i (range 2 (array-list-length exp))))
      (push-right! and-exp
                   `(<= ,(aget exp (- i 1))
                        ,(aget exp i))))
    (compile-rose
     (make-rose and-exp)
     env options))))

;;; Compile a `(let ...)` expression.
(define (compile-let node env (options (js-obj)))
  ;; There is no distinction between `(let ...)` and `(let* ...)`
  ;; expressions---they are compiled in the same way.
  (compile-let-star node env options))

;;; Compile a `(let* ...)` expression.
(define (compile-let-star node env (options (js-obj)))
  (define inherited-options
    (js-obj-append options))
  (define exp
    (send node get-value))
  (define expression-type
    (oget inherited-options
          "expressionType"))
  (define bindings
    (oget inherited-options "bindings"))
  (define make-block #f)
  (set! bindings
        (extend-environment (new LispEnvironment)
                            bindings))
  (oset! inherited-options "bindings" bindings)
  (cond
   ((or (eq? expression-type "statement")
        (eq? expression-type "return"))
    (define let-nodes
      (~> node
          (send get 1)
          (send get-nodes)))
    (define body-nodes
      (send node drop 2))
    (define define-nodes
      (map (lambda (x)
             (define exp
               (send x get-value))
             (cond
              ((array? exp)
               (define sym
                 (first exp))
               (when (and (not make-block)
                          (send bindings has sym))
                 (set! make-block #t))
               (make-rose
                `(define ,(send x get 0)
                   ,(send x get 1))
                x))
              (else
               (define sym exp)
               (when (and (not make-block)
                          (send bindings has sym))
                 (set! make-block #t))
               (make-rose
                `(define ,x)
                x))))
           let-nodes))
    (define result
      (compile-rose
       (make-rose
        `(,(if make-block
               'block
               'begin)
          ,@define-nodes
          ,@body-nodes)
        node)
       env inherited-options))
    result)
   (else
    (compile-expression
     (wrap-in-arrow-call exp)
     env inherited-options))))

;;; Compile a `(let-values ...)` expression.
(define (compile-let-values node env (options (js-obj)))
  (define inherited-options
    (js-obj-append options))
  (define exp
    (send node get-value))
  (define expression-type
    (oget inherited-options
          "expressionType"))
  (define bindings
    (oget inherited-options "bindings"))
  (define make-block #f)
  (set! bindings
        (extend-environment (new LispEnvironment)
                            bindings))
  (oset! inherited-options "bindings" bindings)
  (cond
   ((or (eq? expression-type "statement")
        (eq? expression-type "return"))
    (define let-nodes
      (~> node
          (send get 1)
          (send get-nodes)))
    (define body-nodes
      (send node drop 2))
    (define define-nodes
      (map (lambda (x)
             (define exp
               (send x get-value))
             (cond
              ((symbol? exp)
               (define sym exp)
               (when (and (not make-block)
                          (send bindings has sym))
                 (set! make-block #t))
               (make-rose
                `(define ,x)))
              (else
               (define variables
                 (~> x
                     (send get 0)
                     (send get-value)))
               (cond
                ((symbol? variables)
                 (define sym variables)
                 (when (and (not make-block)
                            (send bindings has sym))
                   (set! make-block #t)))
                (else
                 (define syms
                   (flatten variables))
                 (unless make-block
                   (for ((sym (flatten variables)))
                     (when (send bindings has sym)
                       (set! make-block #t)
                       (break))))))
               (define expression
                 (send x get 1))
               (make-rose
                `(define-values ,(send x get 0)
                   ,(send x get 1))
                x))))
           let-nodes))
    (define result
      (compile-rose
       (make-rose
        `(,(if make-block
               'block
               'begin)
          ,@define-nodes
          ,@body-nodes)
        node)
       env inherited-options))
    result)
   (else
    (compile-expression
     (wrap-in-arrow-call exp)
     env inherited-options))))

;;; Compile a `(define-values ...)` expression.
(define (compile-define-values node env (options (js-obj)))
  (define exp
    (send node get-value))
  (define inherited-options
    (js-obj-append options))
  (define expression-type
    (oget inherited-options
          "expressionType"))
  (define bindings
    (oget inherited-options "bindings"))
  (define make-block #t)
  (define hole-marker '_)
  (set! bindings
        (extend-environment
         (new LispEnvironment)
         bindings))
  (oset! inherited-options "bindings" bindings)
  (define variables
    (~> node
        (send get 1)
        (send get-value)))
  (define expression
    (~> node
        (send get 2)))
  (define regular-vars '())
  (define rest-var undefined)
  (define var-decls '())
  (define declarator-id)
  (define declarator-init)
  (when (eq? (send expression get-value)
             ':hole-marker)
    (set! hole-marker
          (~> node
              (send get 3)
              (send get-value)))
    (set! expression
          (~> node
              (send get 4))))
  (cond
   ((symbol? variables)
    (set! declarator-id
          (new Identifier
               (print-estree
                (compile-symbol
                 (make-rose variables)
                 env inherited-options)
                inherited-options)))
    (send bindings set-local variables #t "variable"))
   (else
    (cond
     ((dotted-list? variables)
      (define var-list
        (flatten variables))
      (set! regular-vars
            (drop-right var-list 1))
      (set! rest-var
            (array-list-last var-list)))
     (else
      (set! regular-vars variables)))
    (set! var-decls
          (map (lambda (x)
                 (cond
                  ((eq? x hole-marker)
                   js/null)
                  (else
                   (send bindings set-local x #t "variable")
                   (new Identifier
                        (print-estree
                         (compile-symbol
                          (make-rose x)
                          env inherited-options)
                         inherited-options)))))
               regular-vars))
    (when rest-var
      (send bindings set-local rest-var #t "variable")
      (push-right! var-decls
                   (new RestElement
                        (new Identifier
                             (print-estree
                              (compile-symbol
                               (make-rose
                                rest-var)
                               env
                               inherited-options)
                              inherited-options)))))
    (set! declarator-id
          (new ArrayPattern var-decls))))
  (set! declarator-init
        (compile-expression
         expression env inherited-options))
  (new VariableDeclaration
       (list
        (new VariableDeclarator
             declarator-id
             declarator-init))
       "let"))

;;; Compile a `(set!-values ...)` expression.
(define (compile-set-values node env (options (js-obj)))
  (define exp
    (send node get-value))
  (define inherited-options
    (js-obj-append options))
  (define expression-type
    (oget inherited-options
          "expressionType"))
  (define make-block #t)
  (define bindings
    (oget inherited-options "bindings"))
  (define declaration)
  (define declarator)
  (define left)
  (define right)
  (set! bindings (extend-environment
                  (new LispEnvironment)
                  bindings))
  (oset! inherited-options "bindings" bindings)
  (set! declaration
        (compile-define-values
         (make-rose
          `(define-values ,@(send node drop 1))
          node)
         env inherited-options))
  (set! declarator
        (first (get-field declarations
                          declaration)))
  (set! left (get-field id declarator))
  (set! right (get-field init declarator))
  (wrap-expression-in-statement
   (new AssignmentExpression
        "="
        left
        right)
   inherited-options))

;;; Compile a `(let-js-obj ...)` expression.
(define (compile-let-js-obj node env (options (js-obj)))
  (define inherited-options
    (js-obj-append options))
  (define expression-type
    (oget options "expressionType"))
  (define bindings
    (oget inherited-options "bindings"))
  (define make-block #f)
  (set! bindings
        (extend-environment (new LispEnvironment)
                            bindings))
  (oset! inherited-options "bindings" bindings)
  (cond
   ((or (eq? expression-type "statement")
        (eq? expression-type "return"))
    (define let-nodes
      (~> node
          (send get 1)
          (send get-nodes)))
    (define body-nodes
      (send node drop 2))
    (define define-nodes
      (map (lambda (x)
             (define fields
               (send x get 0))
             (define fields-exp
               (send fields get-value))
             (define obj
               (send x get 1))
             (for ((f fields-exp))
               (define sym
                 (if (array? f)
                     (second f)
                     f))
               (when (and (not make-block)
                          (send bindings has sym))
                 (set! make-block #t)))
             (make-rose
              `(define-js-obj ,fields
                 ,obj)
              x))
           let-nodes))
    (define result
      (compile-rose
       (make-rose
        `(,(if make-block
               'block
               'begin)
          ,@define-nodes
          ,@body-nodes)
        node)
       env inherited-options))
    result)
   (else
    (define exp
      (send node get-value))
    (compile-expression
     (wrap-in-arrow-call exp)
     env options))))

;;; Compile a `(define-js-obj ...)` expression.
(define (compile-define-js-obj node env (options (js-obj)))
  (define expression-type
    (oget options "expressionType"))
  (define bindings
    (oget options "bindings"))
  (define fields
    (send node get 1))
  (define fields-exp
    (send fields get-value))
  (define obj
    (send node get 2))
  (for ((f fields-exp))
    (define sym
      (if (array? f)
          (second f)
          f))
    (send bindings set-local sym #t "variable"))
  (define expression-statement
    (compile-set-js-obj
     (make-rose
      `(set!-js-obj ,fields ,obj)
      node)
     env options))
  (define assignment-expression
    (get-field expression expression-statement))
  (define left
    (get-field left assignment-expression))
  (define right
    (get-field right assignment-expression))
  (new VariableDeclaration
       (list
        (new VariableDeclarator
             left
             right))
       "let"))

;;; Compile a `(set!-js-obj! ...)` expression.
(define (compile-set-js-obj node env (options (js-obj)))
  (define expression-type
    (oget options "expressionType"))
  (wrap-expression-in-statement
   (new AssignmentExpression
        "="
        (new
         ObjectPattern
         (map (lambda (x)
                (define exp
                  (send x get-value))
                (cond
                 ((array? exp)
                  (new Property
                       (compile-expression
                        (send x get 0) env options)
                       (compile-expression
                        (send x get 1) env options)))
                 (else
                  (define key
                    (compile-expression
                     x env options))
                  (new Property key key))))
              (send (send node get 1) drop 0)))
        (compile-expression
         (send node get 2) env options))
   options))

;;; Compile a `(list ...)` expression.
(define (compile-list node env (options (js-obj)))
  (make-expression-or-statement
   (new ArrayExpression
        (map (lambda (x)
               (compile-expression
                x env options))
             (send node drop 1)))
   options))

;;; Compile a macro call.
(define (compile-macro-call node env (options (js-obj)))
  ;; Only expand the macro a single step, as there might be
  ;; compilers defined for the immediate expansion.
  (define-values (expansion)
    (macroexpand-1 node env))
  (compile-rose expansion env options))

;;; Expand the macro call `exp` in `env`, and keep
;;; expanding the result until something that is not
;;; a macro call is obtained.
;;;
;;; Similar to [`macroexpand` in Common Lisp][cl:macroexpand]
;;; and [`macroexpand` in Emacs Lisp][el:macroexpand].
;;;
;;; [cl:macroexpand]: http://clhs.lisp.se/Body/f_mexp_.htm#macroexpand
;;; [el:macroexpand]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Expansion.html#index-macroexpand
(define (macroexpand exp env)
  (define result exp)
  (define expanded #f)
  (define expanded1 #t)
  (while expanded1
    (set!-values (result expanded1)
                 (macroexpand-1 result env))
    (set! expanded (or expanded expanded1)))
  (values result expanded))

;;; Expand the macro call `exp` in `env`.
;;;
;;; Similar to [`macroexpand-1` in Common Lisp][cl:macroexpand-1]
;;; and [`macroexpand-1` in Emacs Lisp][el:macroexpand-1].
;;;
;;; [cl:macroexpand-1]: http://clhs.lisp.se/Body/f_mexp_.htm#macroexpand-1
;;; [el:macroexpand-1]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Expansion.html#index-macroexpand_002d1
(define (macroexpand-1 exp env)
  (define node exp)
  (define result exp)
  (define expanded #f)
  (set! exp
        (if (is-a? node Rose)
            (send node get-value)
            node))
  (set! env
        (or env
            (current-environment_)
            (empty-environment)))
  (cond
   ((not (list? exp))
    (set! result exp))
   ((null? exp)
    (set! result exp))
   ((quote? exp)
    (set! result
          (text-of-quotation exp)))
   (else
    (define op
      (first exp))
    (define-values (macro-f typ)
      (send env get-typed-value op))
    (when (eq? typ "macro")
      (set! result (macro-f exp env))
      (set! expanded #t))))
  (values
   (if (is-a? node Rose)
       (make-rose result node)
       result)
   expanded))

;;; Expand the macro call `exp` in `env`, and keep
;;; expanding the result for a total number of `n`
;;; expansions, or until something that is not a
;;; macro call is obtained.
(define (macroexpand-n exp env (n 1))
  (define i n)
  (define result exp)
  (define expanded #f)
  (define expanded1 #t)
  (while (and expanded1
              (> i 0))
    (set!-values (result expanded1)
                 (macroexpand-1 result env))
    (set! expanded (or expanded expanded1))
    (set! i (- i 1)))
  (values result expanded))

;;; Expand the macro call `exp` in `env`, and keep
;;; expanding the result until `pred` returns `#f`,
;;; or until something that is not a macro call
;;; is obtained.
(define (macroexpand-until exp env pred)
  (define result exp)
  (while (and (macro-call? result env)
              (pred result))
    (set!-values (result) (macroexpand-1 result env)))
  result)

;;; Expand all macro calls in `exp` in `env`.
;;;
;;; Similar to [`macroexpand-all` in Emacs Lisp][el:macroexpand-all].
;;;
;;; [el:macroexpand-all]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Expansion.html#index-macroexpand_002dall
(define (macroexpand-all exp env)
  (macroexpand-all-until exp
                         env
                         (const #t)))

;;; Expand the macro calls in `exp` in `env`, and keep
;;; expanding until `pred` returns `#f`, or until
;;; something that is not a macro call is obtained.
(define (macroexpand-all-until exp
                               env
                               (pred undefined)
                               (stack '())
                               (bindings (new LispEnvironment)))
  (define (f x stack bindings)
    ;; Wrap `pred` in a function that checks
    ;; whether the operator symbol is locally
    ;; bound to something else than a macro.
    (define pred-f
      (or pred
          (const #t)))
    (define (pred-f-1 x)
      (define op
        (first x))
      (define-values b-type
        (send bindings get-type op))
      (and (or (eq? b-type "macro")
               (eq? b-type "undefined"))
           (pred-f x)))
    (cond
     ((macro-call? x env)
      (define result
        (macroexpand-until x env pred-f-1))
      (unless (macro-call? result env)
        (set! result
              (map-sexp f result env stack bindings)))
      result)
     (else
      x)))
  (map-sexp f exp env stack bindings))

;;; Macroexpand all compiler macros.
;;; This expands regular macros as well.
(define (macroexpand-compiler-macros exp env)
  (define compiler-macro-env
    (make-macro-environment env))
  (define result
    (macroexpand-all exp compiler-macro-env))
  result)

;;; Compile a `(. ...)` expression.
;;; Also handles `(.method obj ...)` calls.
(define (compile-dot node env (options (js-obj)))
  (define exp
    (send node get-value))
  (define match
    (regexp-match (regexp "^\\.(.*)$")
                  (symbol->string (first exp))))
  (define method
    (second match))
  (cond
   ;; Method call:
   ;; `(. foo bar ...)` = `(send foo bar ...)`.
   ((eq? method "")
    (cond
     ((set! match
            (regexp-match (regexp "^-(.*)$")
                          (symbol->string
                           (third exp))))
      (define field
        (second match))
      (define field-sym
        (string->symbol field))
      (define obj
        (send node get 1))
      (compile-get-field
       (make-rose
        `(get-field ,field-sym ,obj))
       env
       options))
     (else
      (compile-send node env options))))
   (else
    (define obj
      (send node get 1))
    (cond
     ;; Member expression:
     ;; `(.-foo bar)` = `(get-field foo bar)`.
     ((set! match
            (regexp-match (regexp "^-(.*)$")
                          method))
      (define field
        (second match))
      (compile-get-field
       (make-rose
        `(get-field ,(string->symbol field) ,obj)
        node)
       env options))
     ;; Method call:
     ;; `(.foo bar ...)` = `(send bar foo ...)`.
     (else
      (compile-send
       (make-rose
        `(send ,obj
               ,(string->symbol method)
               ,@(send node drop 2))
        node)
       env options))))))

;;; Compile a `(get-field ...)` expression.
(define (compile-get-field node env (options (js-obj)))
  (define field
    (send node get 1))
  (define obj
    (send node get 2))
  (define computed
    (not (symbol? (send field get-value))))
  (make-expression-or-statement
   (new MemberExpression
        (if (symbol? (send obj get-value))
            (compile-symbol
             obj env
             (make-expression-options
              options))
            (compile-expression
             obj env options))
        (if computed
            (compile-expression
             field env options)
            (compile-symbol
             field env options))
        computed)
   options))

;;; Compile a `(js/optional-chaining ...)` expression.
(define (compile-js-optional-chaining node env (options (js-obj)))
  (define obj
    (send node get 1))
  (define field
    (send node get 2))
  (define field-exp
    (send field get-value))
  (cond
   ((array? field-exp)
    (define result
      (compile-rose
       (make-rose
        `(,obj ,@(send field drop 0))
        node)
       env
       options))
    (set-field! optional result #t)
    result)
   (else
    (define result
      (compile-rose
       (make-rose
        `(get-field ,field ,obj)
        node)
       env
       options))
    (set-field! optional result #t)
    result)))

;;; Compile a `(set-field! ...)` expression.
(define (compile-set-field node env (options (js-obj)))
  (define field
    (send node get 1))
  (define obj
    (send node get 2))
  (define val
    (send node get 3))
  (compile-rose
   (make-rose
    `(set! (get-field ,field ,obj) ,val)
    node)
   env options))

;;; Compile a `(modulo ...)` expression.
(define (compile-modulo node env (options (js-obj)))
  (compile-binary-expression
   node env
   options
   (js-obj "identity" 1
           "operator" "%")))

;;; Compile a `(* ...)` expression.
(define (compile-mul node env (options (js-obj)))
  (compile-binary-expression
   node env
   options
   (js-obj "identity" 1
           "operator" "*")))

;;; "NO-OP" compilation operation.
;;; Creates an empty program fragment and does nothing else.
(define (compile-nop node env (options (js-obj)))
  (make-program-fragment))

;;; Compile a `(not ...)` expression.
(define (compile-not node env (options (js-obj)))
  (define (is-not-expression? x)
    (and (estree-type? x "UnaryExpression")
         (eq? (get-field operator x) "!")))
  (define operand
    (send node get 1))
  (define operand-compiled
    (compile-expression operand env options))
  (define result undefined)
  (cond
   ((and (estree-type? operand-compiled "BinaryExpression")
         (eq? (get-field operator operand-compiled) "==="))
    (set-field! operator operand-compiled "!==")
    (set! result operand-compiled))
   ((and (estree-type? operand-compiled "BinaryExpression")
         (eq? (get-field operator operand-compiled) "=="))
    (set-field! operator operand-compiled "!=")
    (set! result operand-compiled))
   (else
    (define not-expression
      (new UnaryExpression "!" #t operand-compiled))
    ;; Cancel out double negation. Not sure this really
    ;; belongs---perhaps we do want it in some cases, as a way
    ;; to force boolean values (e.g., `!!undefined` = `false`).
    (while (and (is-not-expression? not-expression)
                (is-not-expression?
                 (get-field argument not-expression)))
      (set! not-expression
            (~> not-expression
                (get-field argument _)
                (get-field argument _))))
    (set! result not-expression)))
  (make-expression-or-statement result options))

;;; Compile a `(begin ...)` expression.
(define (compile-begin node env (options (js-obj)))
  (define bindings
    (oget options "bindings"))
  (define expression-type
    (oget options "expressionType"))
  (define exp
    (send node get-value))
  (define body
    (send node drop 1))
  (define compiled-body '())
  ;; Add defined variables to `bindings` environment.
  ;; We have to handle them here since they may refer
  ;; to each other.
  (for ((i (range 0 (array-list-length body))))
    (define exp
      (send (aget body i) get-value))
    (cond
     ((form? exp define_ env)
      (define sym
        (if (array? (second exp))
            (first (second exp))
            (second exp)))
      (send bindings set-local sym #t "variable"))
     ((form? exp define-macro_ env)
      (define sym
        (first (second exp)))
      (send bindings set-local sym #t "macro"))
     ((form? exp defmacro_ env)
      (define sym
        (second exp))
      (send bindings set-local sym #t "macro"))))
  (cond
   ((or (eq? expression-type "statement")
        (eq? expression-type "return"))
    (define body-statements
      (compile-statements body env options))
    ;; Note that this returns a `Program` node, but in
    ;; some contexts, a `BlockStatement` node is wanted.
    ;; One can convert a `Program` node to a
    ;; `BlockStatement` node with
    ;; `wrap-in-block-statement`.
    (make-program-fragment body-statements))
   (else
    ;; Wrap in an arrow function.
    (cond
     ((= (array-list-length exp) 2)
      (compile-expression
       (send node get 1)
       env options))
     (else
      (compile-expression
       (wrap-in-arrow-call exp)
       env options))))))

;;; Compile a `(block ...)` expression.
(define (compile-block node env (options (js-obj)))
  (define expression-type
    (oget options "expressionType"))
  (cond
   ((or (eq? expression-type "statement")
        (eq? expression-type "return"))
    (wrap-in-block-statement
     (compile-begin node env options)))
   (else
    (compile-begin node env options))))

;;; Make and compile a `(require ...)` or `(define-values ...)` form
;;; that defines referenced values from the language environment.
;;; `symbols` is a list of symbols bound in the language environment.
(define (build-global-environment symbols env (options (js-obj)))
  (define exp
    (make-global-environment-exp symbols env options))
  (compile-global-environment exp env options))

;;; Make a form that defines referenced values
;;; from the language environment. Returns `#f`
;;; if there are no symbols.
(define (make-global-environment-exp symbols env options)
  (cond
   ((= (array-list-length symbols) 0)
    #f)
   ((oget options "inlineFunctions")
    (make-define-values-exp symbols env options))
   (else
    (make-require-exp symbols env options))))

;;; Make a `(define-values ...)` form for the global environment.
(define (make-define-values-exp symbols env options)
  (define inline-functions-option
    (oget options "inlineFunctions"))
  (define definitions #f)
  (define define-forms '())
  (define internal-symbols '())
  (define external-symbols '())
  (define referenced-symbols
    `(,@symbols))
  (define current-module
    (new Module))
  (define bindings
    (oget options "bindings"))
  (define seen '())
  (define exp)
  (define internal-symbol)
  (define symbol)
  (define value)
  (while (> (array-list-length referenced-symbols) 0)
    (set! symbol (pop! referenced-symbols))
    (push-right! seen symbol)
    (when (and (not (memq? symbol external-symbols))
               (send env has symbol))
      (set! value (send env get symbol))
      (cond
       ((source? value)
        (set! exp (source value))
        (when (tagged-list? exp 'define)
          (set! internal-symbol
                (if (array? (second exp))
                    (array-first (array-second exp))
                    (array-second exp)))
          (define referenced-symbols-1 '())
          (define bindings-1
            (if bindings
                (send bindings clone)
                undefined))
          (define compiled-expression
            (compile-rose
             (make-rose exp)
             env
             (js-obj-append
              options
              (js-obj "bindings"
                      bindings-1
                      "currentModule"
                      current-module
                      "referencedSymbols"
                      referenced-symbols-1))))
          (for ((symbol-1 referenced-symbols-1))
            (unless (or (memq? symbol-1 seen)
                        (memq? symbol-1 referenced-symbols))
              (push-right! referenced-symbols symbol-1)))))
       (else
        ;; Deal with the case when the value has no Lisp source.
        (cond
         ((procedure? value)
          (define js-string
            (string-append value ""))
          (define match)
          (set! match
                (regexp-match (regexp "^function ([^( ]+)")
                              js-string))
          (cond
           (match
               (set! internal-symbol
                     (string->symbol (second match)))
             (set! exp `(js ,js-string)))
           (else
            (set! internal-symbol symbol)
            (set! exp
                  `(define ,internal-symbol
                     (js ,js-string))))))
         ((js-obj? value)
          (define js-string
            (send JSON stringify value js/null 2))
          (set! internal-symbol symbol)
          (set! exp
                `(define ,internal-symbol
                   (js ,js-string))))
         (else
          (define js-string
            (string-append value ""))
          (set! internal-symbol symbol)
          (set! exp
                `(define ,internal-symbol
                   (js ,js-string)))))))
      (unless (memq? internal-symbol internal-symbols)
        ;; Do not push the same `define` form more than once.
        (push-right! define-forms exp))
      (when (memq? symbol symbols)
        (push-right! internal-symbols internal-symbol)
        (push-right! external-symbols symbol))))
  (when (> (array-list-length external-symbols) 0)
    (set! definitions
          `(define-values ,external-symbols
             ((js/arrow ()
                ,@define-forms
                (values ,@internal-symbols))))))
  definitions)

;;; Make a `(require ...)` form for the global environment.
(define (make-require-exp symbols env options)
  `(require (only-in ,package-name
                     ,@symbols)))

;;; Compile a `(define-values ...)` form that defines referenced values
;;; from the language environment.
(define (compile-global-environment exp env (options (js-obj)))
  (cond
   ((not exp)
    (make-program-fragment))
   ((tagged-list? exp 'define-values)
    (define define-values-form
      `(,(first exp) ,(second exp)
        (list)))
    (define body
      (aget exp 2))
    ;; Compile the body in a sandboxed environment.
    (define body-compiled
      (compile-sexp
       body
       env
       (js-obj-append
        options
        (js-obj "bindings" (new LispEnvironment)
                "expressionType" "expression"))))
    (define var-decl
      (compile-sexp define-values-form env options))
    (set-field! init
                (first
                 (get-field declarations var-decl))
                body-compiled)
    (define result
      (make-program-fragment
       (list var-decl)))
    result)
   (else
    (make-program-fragment
     (list
      (compile-sexp exp env options))))))

;;; Make a `((lambda () ...))` expression that evaluates to a single
;;; value from the language environment. `symbol` is a symbol bound in
;;; the language environment.
(define (make-inlined-value symbol env options)
  ;; We take the output of a call to `make-global-environment-exp`
  ;; and massage it into a simpler expression.
  (define global-environment-exp
    (make-global-environment-exp
     (list symbol)
     env
     (js-obj-append
      options
      (js-obj "inlineFunctions" #t))))
  (cond
   ((> (array-list-length global-environment-exp) 1)
    (define lambda-call
      (aget global-environment-exp 2))
    (define lambda-exp
      (aget lambda-call 0))
    (define values-exp
      (array-list-last lambda-exp))
    (define sym
      (second values-exp))
    (define result lambda-call)
    (cond
     ((and (= (array-list-length lambda-exp) 4)
           (symbol? (second (third lambda-exp))))
      ;; In simple cases, where there is only a single
      ;; `(define sym ...)` form, no `lambda` expression
      ;; is necessary.
      (set! result
            (third (third lambda-exp))))
     (else
      ;; Change the return value of the `lambda` function
      ;; from a `(values ...)` form to a single value.
      (list-set! lambda-exp
                 (- (array-list-length lambda-exp) 1)
                 sym)))
    result)
   (else
    global-environment-exp)))

;;; Compile an `(or ...)` expression.
(define (compile-or node env (options (js-obj)))
  (compile-logical-expression
   node env
   options
   (js-obj "identity" #f
           "operator" "||")))

;;; Compile a `(provide ...)` expression.
(define (compile-provide node env (options (js-obj)))
  (define expressions
    (send node drop 1))
  ;; Sort `all-from-out` expressions from the rest.
  (define all-from-out-expressions '())
  (define other-expressions '())
  (for ((x expressions))
    (cond
     ((tagged-list? (send x get-value) 'all-from-out)
      (push-right! all-from-out-expressions x))
     (else
      (push-right! other-expressions x))))
  ;; Compile `all-from-out` expressions.
  (define results '())
  (for ((x all-from-out-expressions))
    (define source
      (send x get 1))
    (define result
      (new ExportAllDeclaration
           (compile-expression source env options)))
    (push-right! results result))
  ;; Compile other expressions.
  (when (> (array-list-length other-expressions) 0)
    (define specifiers '())
    (define seen '())
    (for ((x other-expressions))
      (define exp
        (send x get-value))
      (cond
       ((tagged-list? exp 'rename-out)
        (for ((pair (rest exp)))
          (define x1
            (first pair))
          (define x2
            (second pair))
          (when (symbol? x1)
            (set! x1
                  (print-estree
                   (compile-symbol
                    (make-rose x1)
                    env
                    options
                    (js-obj "literalSymbol" #t))
                   options)))
          (when (symbol? x2)
            (set! x2
                  (print-estree
                   (compile-symbol
                    (make-rose x2)
                    env
                    options
                    (js-obj "literalSymbol" #t))
                   options)))
          (unless (memq? x2 seen)
            (push-right! seen x2)
            (push-right! specifiers
                         (new ExportSpecifier
                              (new Identifier x1)
                              (new Identifier x2))))))
       (else
        (define x1 exp)
        (when (symbol? x1)
          (set! x1
                (print-estree
                 (compile-symbol
                  (make-rose x1)
                  env
                  options
                  (js-obj "literalSymbol" #t))
                 options)))
        (unless (memq? x1 seen)
          (push-right! seen x1)
          (push-right! specifiers
                       (new ExportSpecifier
                            (new Identifier x1)))))))
    (define result
      (new ExportNamedDeclaration
           js/null
           specifiers))
    (push-right! results result))
  (cond
   ((= (array-list-length results) 1)
    (first results))
   (else
    (make-program-fragment results))))

;;; Compile a `(quote ...)` expression.
(define (compile-quote node env (options (js-obj)))
  (define exp
    (send node get-value))
  (define result)
  (cond
   ((array? (second exp))
    (set! result
          (compile-expression
           (make-rose
            `(list
              ,@(send (second exp)
                      map
                      (lambda (x)
                        `(quote ,x)))))
           env options)))
   ((symbol? (second exp))
    (set! result
          (compile-symbol
           (send node get 1)
           env
           options
           (js-obj "quotedSymbol" #t))))
   (else
    (set! result
          (compile-expression
           (send node get 1)
           env
           options))))
  (make-expression-or-statement
   result options))

;;; Compile a `(quasiquote ...)` expression.
(define (compile-quasiquote node env (options (js-obj)))
  (make-expression-or-statement
   (compile-quasiquote-helper
    (send node get 1) env options)
   options))

;;; Helper function for `compile-quasiquote`.
(define (compile-quasiquote-helper node env (options (js-obj)))
  (define exp
    (send node get-value))
  (cond
   ((not (array? exp))
    (compile-expression
     (make-rose
      `(quote ,exp))
     env options))
   (else
    (new ArrayExpression
         (map (lambda (x)
                (define exp
                  (send x get-value))
                (cond
                 ((tagged-list? exp 'quasiquote)
                  (compile-quote
                   (make-rose `(quote ,exp))
                   env options))
                 ((tagged-list? exp 'unquote)
                  (compile-expression
                   (send x get 1) env options))
                 ((tagged-list? exp 'unquote-splicing)
                  (new RestElement
                       (compile-expression
                        (send x get 1) env options)))
                 (else
                  (compile-quasiquote-helper
                   x env options))))
              (send node get-nodes))))))

;;; Compile a `(require ...)` expression.
(define (compile-require node env (options (js-obj)))
  (define es-module-interop
    (oget options "esModuleInterop"))
  (define bindings
    (oget options "bindings"))
  (define x-node
    (send node get 1))
  (define x-exp
    (send x-node get-value))
  (define y-node
    (or (send node get 2) x-node))
  (define y-exp
    (send y-node get-value))
  (define specifiers '())
  (define seen '())
  (define src js/null)
  (cond
   ((tagged-list? x-exp 'only-in)
    (for ((x (send x-node drop 2)))
      (define exp
        (send x get-value))
      (cond
       ((array? exp)
        (define x1
          (first exp))
        (define x2
          (second exp))
        (when (symbol? x1)
          (set! x1
                (print-estree
                 (compile-symbol
                  (make-rose x1)
                  env
                  options
                  (js-obj "literalSymbol" #t))
                 options)))
        (when (symbol? x2)
          (set! x2
                (print-estree
                 (compile-symbol
                  (make-rose x2)
                  env
                  options
                  (js-obj "literalSymbol" #t))
                 options)))
        (unless (memq? x2 seen)
          (when bindings
            (send bindings set-local x2 #t "variable"))
          (push-right! seen x2)
          (push-right! specifiers
                       (new ImportSpecifier
                            (new Identifier x1)
                            (new Identifier x2)))))
       (else
        (define x1 exp)
        (when (symbol? x1)
          (set! x1
                (print-estree
                 (compile-symbol
                  (make-rose x1)
                  env
                  options
                  (js-obj "literalSymbol" #t))
                 options)))
        (unless (memq? x1 seen)
          (when bindings
            (send bindings set-local x1 #t "variable"))
          (push-right! seen x1)
          (push-right! specifiers
                       (new ImportSpecifier
                            (new Identifier x1)))))))
    (set! y-exp (second x-exp)))
   (else
    (when (symbol? x-exp)
      (set! x-exp
            (print-estree
             (compile-symbol
              (make-rose x-exp)
              env
              options
              (js-obj "literalSymbol" #t))
             options)))
    (set! specifiers
          (list
           (if es-module-interop
               (new ImportDefaultSpecifier
                    (new Identifier x-exp))
               (new ImportNamespaceSpecifier
                    (new Identifier x-exp)))))))
  (when (symbol? y-exp)
    (set! y-exp
          (print-estree
           (compile-symbol
            (make-rose y-exp)
            env
            options
            (js-obj "literalSymbol" #t))
           options)))
  (set! src (new Literal y-exp))
  (when (and bindings
             (symbol? x-exp))
    (send bindings set-local x-exp #t "variable"))
  (cond
   ((null? specifiers)
    (empty-program))
   (else
    (new ImportDeclaration
         specifiers
         src))))

;;; Compile a `(set! ...)` expression.
(define (compile-set node env (options (js-obj)))
  (define expression-type
    (oget options "expressionType"))
  (define sym-node
    (send node get 1))
  (define sym-exp
    (send sym-node get-value))
  (define val-node
    (send node get 2))
  (define val-exp
    (send val-node get-value))
  (cond
   ((and (form? val-exp add_ env)
         (or (and (eq? (second val-exp) sym-exp)
                  (eq? (third val-exp) 1))
             (and (eq? (third val-exp) sym-exp)
                  (eq? (second val-exp) 1))))
    (set! val-exp `(add1 ,sym-exp))
    (set! val-node (make-rose val-exp)))
   ((and (form? val-exp sub_ env)
         (or (and (eq? (second val-exp) sym-exp)
                  (eq? (third val-exp) 1))
             (and (eq? (third val-exp) sym-exp)
                  (eq? (second val-exp) 1))))
    (set! val-exp `(sub1 ,sym-exp))
    (set! val-node (make-rose val-exp))))
  (define result "")
  (cond
   ((and (form? val-exp add1_ env)
         (eq? (second val-exp) sym-exp))
    (set! result
          (new UpdateExpression
               "++"
               (compile-expression
                sym-node env options)
               (or (eq? expression-type "return")
                   (not (eq? expression-type
                             "statement"))))))
   ((and (form? val-exp sub1_ env)
         (eq? (second val-exp) sym-exp))
    (set! result
          (new UpdateExpression
               "--"
               (compile-expression
                sym-node env options)
               (or (eq? expression-type "return")
                   (not (eq? expression-type
                             "statement"))))))
   (else
    (set! result
          (new AssignmentExpression
               "="
               (if (symbol? (send sym-node get-value))
                   (compile-symbol
                    sym-node env
                    (make-expression-options
                     options))
                   (compile-expression
                    sym-node env options))
               (compile-expression
                val-node env options)))))
  (make-expression-or-statement result options))

;;; Compile a string expression.
(define (compile-string node env (options (js-obj)))
  (define str
    (send node get-value))
  (cond
   ((regexp-match (regexp "\\n") str)
    (define lines
      (string-split str (regexp "^" "gm")))
    (cond
     ((<= (array-list-length lines) 1)
      (compile-atom node env options))
     (else
      ;; TODO: We could compile to a template literal instead.
      ;; We just have to take care to escape it properly.
      (compile-rose
       (transfer-comments
        node
        (make-rose
         `(string-append ,@lines)
         node))
       env options))))
   (else
    (compile-atom node env options))))

;;; Compile a `(- ...)` expression.
(define (compile-sub node env (options (js-obj)))
  (define exp
    (send node get-value))
  (cond
   ((= (array-list-length exp) 2)
    (define num
      (send node get 1))
    (define num-compiled
      (compile-expression
       num env options))
    (make-expression-or-statement
     (new UnaryExpression "-" #t num-compiled)
     options))
   (else
    (compile-binary-expression
     node env
     options
     (js-obj "identity" 0
             "operator" "-")))))

;;; Compile a variable expression.
(define (compile-variable node env (options (js-obj)))
  (define compilation-mapping-environment
    (oget options "compilationMappingEnvironment"))
  (define literal-symbol
    (oget options "literalSymbol"))
  (define quoted-symbol
    (oget options "quotedSymbol"))
  (define current-module
    (oget options "currentModule"))
  (define exp (send node get-value))
  (unless (or quoted-symbol
              literal-symbol
              (and (send env has exp)
                   (eq? (send compilation-mapping-environment
                              get-type
                              (send env get exp))
                        "variable")))
    (when (should-inline? exp env options)
      (cond
       (current-module
        (add-referenced-symbol exp env options))
       (else
        ;; Inlined expression. The symbol references a value
        ;; that is defined in the language environment.
        ;; Create an expression that will evaluate to this
        ;; value and compile that.
        (return
         (make-expression-or-statement
          (compile-expression
           (make-rose
            (make-inlined-value
             exp env options))
           env options)
          options))))))
  (make-expression-or-statement
   (compile-symbol node env options)
   options))

;;; Compile a symbol expression.
(define (compile-symbol node env (options (js-obj)) (settings (js-obj)))
  ;; TODO: Better handling of gensym'ed symbols.
  (define literal-symbol-option
    (or (oget settings "literalSymbol") #f))
  (define quoted-symbol-option
    (oget settings "quotedSymbol"))
  (define compile-environment-option
    (oget options "compileEnvironment"))
  (define camel-case-option
    (oget options "camelCase"))
  (define bindings
    (oget options "bindings"))
  (define exp
    (send node get-value))
  (define gensymed-symbol
    (gensym? exp))
  (define str
    (symbol->string exp))
  ;; Keyword symbols (i.e., symbols beginning with `:`,
  ;; e.g., `:foo`) are auto-quoted.
  (when (regexp-match (regexp "^:") str)
    (set! quoted-symbol-option #t))
  (cond
   (quoted-symbol-option
    (compile-expression
     (make-rose
      `(string->symbol ,str))
     env options))
   (literal-symbol-option
    (define name
      (make-js-identifier-string str options))
    (new Identifier name))
   ((send compilation-variables-env has exp)
    (send compilation-variables-env get exp))
   ((eq? str "this")
    (new ThisExpression))
   (gensymed-symbol
    (define gensym-map
      (oget options "gensymMap"))
    (unless gensym-map
      (set! gensym-map (make-hash))
      (oset! options "gensymMap" gensym-map))
    (cond
     ((hash-has-key? gensym-map exp)
      (define-values (gensym-name name i)
        (hash-ref gensym-map exp))
      (define identifier
        (new Identifier gensym-name))
      identifier)
     (else
      (define name
        (make-js-identifier-string str options))
      (define gensym-name name)
      (define i 1)
      (define regular-sym
        (string->symbol gensym-name))
      (while (send bindings has regular-sym)
        (set! gensym-name
              (string-append name (number->string i)))
        (set! regular-sym
              (string->symbol gensym-name))
        (set! i (+ i 1)))
      (define identifier
        (new Identifier gensym-name))
      (define entry
        (list gensym-name name i))
      (hash-set! gensym-map exp entry)
      (send bindings set-local regular-sym #t)
      identifier)))
   (else
    (define name
      (make-js-identifier-string str options))
    (new Identifier name))))

;;; Transform a string to a valid JavaScript identifier string.
(define (make-js-identifier-string str (options (js-obj)))
  ;; TODO: This function could benefit from memoization.
  ;; Need to use a custom memoization map to handle `options`,
  ;; though... an equality map using `equal?` should suffice.
  (define compile-environment-option
    (oget options "compileEnvironment"))
  ;; FIXME: Kludge.
  (unless compile-environment-option
    (return str))
  (define camel-case-option
    (oget options "camelCase"))
  (define result
    (make-js-identifier-string-helper str))
  (cond
   ((eq? camel-case-option #f)
    (kebab-case->snake-case result))
   (else
    (kebab-case->camel-case result))))

;;; Helper function for `make-js-identifier-string`.
(define (make-js-identifier-string-helper str)
  (define result
    (~> str
        (regexp-replace (regexp "^\\+$" "g") _ "_add")
        (regexp-replace (regexp "^-$" "g") _ "_sub")
        (regexp-replace (regexp "^\\*$" "g") _ "_mul")
        (regexp-replace (regexp "^/$" "g") _ "_div")
        (regexp-replace (regexp "%" "g") _ "")
        (regexp-replace (regexp "/" "g") _ "-")
        (regexp-replace (regexp "!" "g") _ "-x")
        (regexp-replace (regexp ":" "g") _ "-")
        (regexp-replace (regexp "->" "g") _ "-to-")
        (regexp-replace (regexp "\\+" "g") _ "_")
        (regexp-replace (regexp "\\*$" "g") _ "-star")
        (regexp-replace (regexp "\\*" "g") _ "star-")))
  (cond
   ((regexp-match (regexp "-" "g") result)
    (set! result
          (regexp-replace (regexp "\\?" "g") result "-p")))
   (else
    (set! result
          (regexp-replace (regexp "\\?" "g") result "p"))))
  result)

;;; Whether something is an equality expression.
(define (is-equality-expression exp env)
  (or (form? exp eq?_ env)
      (form? exp eqv?_ env)
      (form? exp equal?_ env)))

;;; Whether something is a `let` or `let*` expression.
(define (is-let-expression exp env)
  (form? exp let-star_ env))

;;; Compile a `(for ...)` expression.
(define (compile-for node env (options (js-obj)))
  ;; TODO: Implement `compile-js-for` and implement this
  ;; in terms of that?
  (define inherited-options
    (js-obj-append options))
  (define bindings
    (oget inherited-options "bindings"))
  (set! bindings
        (extend-environment (new LispEnvironment)
                            bindings))
  (oset! inherited-options "bindings" bindings)
  (define language
    (oget options "language"))
  (define decls-node
    (send node get 1))
  (define decls
    (send decls-node get-value))
  (define body-nodes
    (send node drop 2))
  (define body-node
    (begin-wrap-rose-smart-1 body-nodes))
  (define decl1-node
    (send decls-node get 0))
  (define decl1
    (send decl1-node get-value))
  (define sym-node
    (send decl1-node get 0))
  (define sym-exp
    (send sym-node get-value))
  (define vals-node
    (send decl1-node get 1))
  (define vals-exp
    (send vals-node get-value))
  (cond
   ((form? vals-exp range_ env)
    (define start
      (if (>= (array-list-length vals-exp) 2)
          (second vals-exp)
          undefined))
    (define end
      (if (>= (array-list-length vals-exp) 3)
          (third vals-exp)
          undefined))
    (define step
      (if (>= 4 (array-list-length vals-exp))
          (fourth vals-exp)
          undefined))
    (set! start
          (if (eq? end undefined)
              0
              start))
    (set! end
          (if (eq? end undefined)
              start
              end))
    ;; If `start`, `end` or `step` is a function call,
    ;; then rewrite the expression to a `let` expression,
    ;; storing the values in local variables so that
    ;; the function is only called once.
    (when (or (array? start)
              (array? end)
              (array? step))
      (define start-var
        (gensym "_start"))
      (define end-var
        (gensym "_end"))
      (define step-var
        (gensym "_step"))
      (return
       (compile-rose
        (make-rose
         `(let (,@(if (array? start)
                      `((,start-var ,start))
                      '())
                ,@(if (array? end)
                      `((,end-var ,end))
                      '())
                ,@(if (array? step)
                      `((,step-var ,step))
                      '()))
            (for ((,sym-exp
                   (range ,(if (array? start)
                               start-var
                               start)
                          ,(if (array? end)
                               end-var
                               end)
                          ,@(if step
                                (if (array? step)
                                    `(,step-var)
                                    `(,step))
                                '()))))
              ,@body-nodes))
         node)
        env options)))
    ;; Otherwise, proceed to create a `for` loop.
    (set! step (or step 1))
    (define init
      (compile-statement
       (make-rose
        `(define ,sym-exp ,start))
       env inherited-options))
    (define test)
    (define update)
    (cond
     ((number? step)
      (cond
       ((< step 0)
        (set! test
              (compile-expression
               (make-rose
                `(> ,sym-exp ,end))
               env inherited-options))
        (set! update
              (compile-statement
               (make-rose
                `(set! ,sym-exp
                       (- ,sym-exp
                          ,(send Math abs step))))
               env inherited-options)))
       (else
        (set! test
              (compile-expression
               (make-rose
                `(< ,sym-exp ,end))
               env inherited-options))
        (set! update
              (compile-statement
               (make-rose
                `(set! ,sym-exp
                       (+ ,sym-exp
                          ,step)))
               env inherited-options)))))
     (else
      (set! test
            (compile-expression
             (make-rose
              `(if (< ,step 0)
                   (> ,sym-exp ,end)
                   (< ,sym-exp ,end)))
             env inherited-options))
      (set! update
            (compile-statement
             (make-rose
              `(set! ,sym-exp
                     (+ ,sym-exp
                        ,step)))
             env inherited-options))))
    (when (estree-type? update "ExpressionStatement")
      (set! update
            (get-field expression update)))
    (define body
      (wrap-in-block-statement
       (compile-statement
        body-node env inherited-options)))
    (new ForStatement
         init
         test
         update
         body))
   (else
    (define left
      (compile-expression
       (make-rose
        `(define ,sym-exp))
       env inherited-options))
    (define right
      (compile-expression
       vals-node env inherited-options))
    (define body
      (wrap-in-block-statement
       (compile-statement
        body-node env inherited-options)))
    (new ForOfStatement
         left
         right
         body))))

;;; Compile a `(break)` expression.
(define (compile-break node env (options (js-obj)))
  (new BreakStatement
       (if (> (send node size) 1)
           (compile-expression
            (send node get 1)
            env options)
           js/null)))

;;; Compile a `(continue)` expression.
(define (compile-continue node env (options (js-obj)))
  (new ContinueStatement
       (if (> (send node size) 1)
           (compile-expression
            (send node get 1)
            env options)
           js/null)))

;;; Compile a `(js/typeof ...)` expression.
(define (compile-js-typeof node env (options (js-obj)))
  (make-expression-or-statement
   (new UnaryExpression
        "typeof"
        #t
        (compile-expression
         (send node get 1)
         env options))
   options))

;;; Compile a `(js/instanceof? ...)` expression.
(define (compile-js-instanceof node env (options (js-obj)))
  (make-expression-or-statement
   (new BinaryExpression
        "instanceof"
        (compile-expression
         (send node get 1)
         env options)
        (compile-expression
         (send node get 2)
         env options))
   options))

;;; Compile a `(js/in ...)` expression.
(define (compile-js-in node env (options (js-obj)))
  (make-expression-or-statement
   (new BinaryExpression
        "in"
        (compile-expression
         (send node get 1)
         env options)
        (compile-expression
         (send node get 2)
         env options))
   options))

;;; Compile a `(new ...)` expression.
(define (compile-new node env (options (js-obj)))
  (make-expression-or-statement
   (new NewExpression
        (compile-expression
         (send node get 1)
         env options)
        (map (lambda (x)
               (compile-expression
                x env options))
             (send node drop 2)))
   options))

;;; Compile a `(js/do-while ...)` expression.
(define (compile-js-do-while node env (options (js-obj)))
  (define body
    (send node get 1))
  (define test
    (send node get 2))
  (new DoWhileStatement
       (compile-expression
        test env options)
       (wrap-in-block-statement-smart
        (compile-statement-or-return-statement
         body env options))))

;;; Compile a `(js/while ...)` expression.
(define (compile-js-while node env (options (js-obj)))
  (define test
    (send node get 1))
  (define body
    (begin-wrap-rose (send node drop 2)))
  (new WhileStatement
       (compile-expression
        test env options)
       (wrap-in-block-statement-smart
        (compile-statement-or-return-statement
         body env options))))

;;; Compile a `(yield ...)` expression.
(define (compile-yield node env (options (js-obj)))
  (make-expression-or-statement
   (new YieldExpression
        (if (> (send node size) 1)
            (compile-expression
             (send node get 1)
             env options)
            js/null))
   options))

;;; Compile a `(throw ...)` expression.
(define (compile-throw node env (options (js-obj)))
  (new ThrowStatement
       (compile-expression
        (send node get 1)
        env options)))

;;; Compile a `(js/delete ...)` expression.
(define (compile-js-delete node env (options (js-obj)))
  (new UnaryExpression
       "delete"
       #t
       (compile-expression
        (send node get 1)
        env options)))

;;; Compile a `(return ...)` expression.
(define (compile-return node env (options (js-obj)))
  (new ReturnStatement
       (if (> (send node size) 1)
           (compile-expression
            (send node get 1)
            env options)
           js/null)))

;;; Compile a `(js/async ...)` expression.
(define (compile-js-async node env (options (js-obj)))
  (define result
    (compile-expression
     (send node get 1)
     env options))
  (when (or (estree-type? result "FunctionDeclaration")
            (estree-type? result "FunctionExpression")
            (estree-type? result "ArrowFunctionExpression"))
    (set-field! async result #t)
    (set-field! returnType
                result
                (new TSTypeReference
                     (new Identifier "Promise")
                     (new TSTypeParameterInstantiation
                          (list
                           (new TSAnyKeyword))))))
  (make-expression-or-statement
   result options))

;;; Compile a `(js/await ...)` expression.
(define (compile-js-await node env (options (js-obj)))
  (make-expression-or-statement
   (new AwaitExpression
        (compile-expression
         (send node get 1)
         env options))
   options))

;;; Compile a `(string-append ...)` expression.
(define (compile-string-append node env (options (js-obj)))
  (define exp
    (send node get-value))
  (cond
   ((<= (array-list-length exp) 0)
    (compile-rose "" env options))
   ((= (array-list-length exp) 2)
    (compile-rose
     (send node get 1) env options))
   (else
    (compile-binary-expression
     node env
     options
     (js-obj "identity" ""
             "operator" "+")))))

;;; Compile a `(define-class ...)` expression.
(define (compile-define-class node env (options (js-obj)))
  (compile-class node env options))

;;; Compile a `(class ...)` expression.
(define (compile-class node env (options (js-obj)))
  (define inherited-options
    (js-obj-append options))
  (define class-name-node
    (send node get 1))
  (define class-name
    (send class-name-node get-value))
  (define super-class
    js/null)
  (define id
    (if (symbol? class-name)
        (new Identifier
             (print-estree
              (compile-expression
               class-name-node env inherited-options)
              inherited-options))
        js/null))
  (define body-node
    (if (eq? id js/null)
        (slice-rose node 1)
        (slice-rose node 2)))
  (define body-exp
    (send body-node get-value))
  (define bindings
    (oget inherited-options "bindings"))
  (set! bindings
        (extend-environment (new LispEnvironment)
                            bindings))
  (oset! inherited-options "bindings" bindings)
  (send bindings set-local 'super #t "variable")
  (when (and (array? (first body-exp))
             (not (form? (first body-exp) define_ env)))
    (define super-classes-node
      (send body-node get 0))
    (define super-classes
      (send super-classes-node get-value))
    (set! body-node (slice-rose body-node 1))
    (set! body-exp (send body-node get-value))
    (when (> (array-list-length super-classes) 0)
      (set! super-class
            (new Identifier
                 (print-estree
                  (compile-expression
                   (make-rose
                    (first super-classes))
                   env inherited-options)
                  inherited-options)))))
  (define body-declarations '())
  (define accessibilities
    (make-hash))
  (for ((x (send body-node get-nodes)))
    (define exp
      (send x get-value))
    (cond
     ((tagged-list? exp 'public)
      (hash-set! accessibilities (second exp) "public"))
     ((tagged-list? exp 'private)
      (hash-set! accessibilities (second exp) "private"))
     (else
      (define is-initialized
        (>= (array-list-length exp) 3))
      (define id
        (second exp))
      (define is-method
        (array? id))
      (when is-method
        (set! id (first id)))
      (define id-node
        (if is-method
            (send (send x get 1) get 0)
            (send x get 1)))
      (define accessibility
        (cond
         ((hash-has-key? accessibilities id)
          (hash-ref accessibilities id))
         ((tagged-list? exp 'define/public)
          "public")
         (else
          "private")))
      (define is-generator
        (tagged-list? exp 'define/generator))
      (define is-constructor
        (and is-method
             (> (array-list-length (second exp)) 0)
             (eq? id 'constructor)))
      (when is-constructor
        (set! accessibility "public"))
      (define return-type
        (if is-constructor
            "void"
            undefined))
      (define is-computed
        (not (symbol? id)))
      (define id-compiled
        (if is-computed
            (compile-expression
             id-node env inherited-options)
            (compile-symbol
             id-node env
             (make-expression-options
              inherited-options))))
      (define init-compiled
        (cond
         ((not is-initialized)
          undefined)
         (is-method
          (compile-js-function
           (define->lambda x (js-obj "curried"  #f))
           env
           (make-expression-options
            inherited-options)
           (js-obj "generator" is-generator
                   "returnType" return-type)))
         (else
          (compile-expression
           (send x get 2)
           env
           (make-expression-options
            inherited-options)))))
      (cond
       (is-method
        (define kind
          (if is-constructor
              "constructor"
              "method"))
        (define method-definition
          (new MethodDefinition
               id-compiled
               init-compiled
               kind
               #f
               #f
               is-computed
               accessibility))
        (set! method-definition
              (transfer-and-compile-comments
               x method-definition
               inherited-options))
        (push-right! body-declarations
                     method-definition))
       (else
        (define property-definition
          (new PropertyDefinition
               id-compiled
               init-compiled
               #f
               accessibility))
        (set! property-definition
              (transfer-and-compile-comments
               x property-definition
               inherited-options))
        (push-right! body-declarations
                     property-definition))))))
  (define body
    (new ClassBody
         body-declarations))
  (if (eq? id js/null)
      (new ClassExpression
           body
           super-class)
      (new ClassDeclaration
           id
           body
           super-class)))

;;; Compile a `(js-obj ...)` expression.
(define (compile-js-obj node env (options (js-obj)))
  (define exp
    (send node get-value))
  (define properties '())
  (for ((i (range 1 (array-list-length exp) 2)))
    (define key-node
      (send node get i))
    (define key-value
      (send key-node get-value))
    (define compiled-key
      (compile-expression
       key-node
       env options))
    (define compiled-value
      (compile-expression
       (send node get (+ i 1))
       env
       options))
    (define computed
      (not (string? (send key-node get-value))))
    (define match)
    (when (and (string? key-value)
               (regexp-match (regexp "^[a-z]+$" "i")
                             key-value))
      (set! compiled-key
            (new Identifier key-value)))
    (push-right! properties
                 (new Property
                      compiled-key
                      compiled-value
                      computed)))
  (make-expression-or-statement
   (new ObjectExpression properties)
   options))

;;; Compile a `(js-obj-append ...)` expression.
(define (compile-js-obj-append node env (options (js-obj)))
  (define args
    (send node drop 1))
  (define properties '())
  (for ((arg args))
    (define exp
      (compile-expression arg env options))
    (cond
     ((is-a? exp ObjectExpression)
      (for ((prop (get-field properties exp)))
        (push-right! properties prop)))
     (else
      (push-right! properties (new SpreadElement exp)))))
  (make-expression-or-statement
   (new ObjectExpression properties)
   options))

;;; Compile a `(js/tag ...)` expression.
(define (compile-js-tagged-template node env (options (js-obj)))
  (define tag
    (send node get 1))
  (define tag-compiled
    (compile-rose tag env options))
  (define str
    (send node get 2))
  (define str-exp
    (send str get-value))
  (new TaggedTemplateExpression
       tag-compiled
       (new TemplateLiteral
            (list
             (new TemplateElement
                  #t
                  str-exp)))))

;;; Compile an `(append ...)` expression.
(define (compile-append node env (options (js-obj)))
  (define elements '())
  (for ((x (send node drop 1)))
    (define el
      (compile-expression
       x env options))
    (cond
     ((estree-type? el "ArrayExpression")
      (cond
       ((= (array-list-length (get-field elements el)) 0)
        ;; Ignore empty arrays.
        )
       ((= (array-list-length (get-field elements el)) 1)
        ;; Unwrap singleton arrays.
        (push-right! elements (aget (get-field elements el) 0)))
       (else
        (push-right! elements (new SpreadElement el)))))
     (else
      (push-right! elements (new SpreadElement el)))))
  (make-expression-or-statement
   (new ArrayExpression elements)
   options))

;;; Compile a `(js/try ...)` expression.
(define (compile-js-try node env (options (js-obj)))
  (define body-exps '())
  (define catch-clause js/null)
  (define finally-clause js/null)
  (for ((x (send node drop 1)))
    (cond
     ((tagged-list? x 'catch)
      (set! catch-clause x))
     ((tagged-list? x 'finally)
      (set! finally-clause x))
     (else
      (push-right! body-exps x))))
  (define block
    (wrap-in-block-statement-smart
     (compile-statement-or-return-statement
      (make-rose
       `(begin ,@body-exps))
      env options)))
  (define handler js/null)
  (when catch-clause
    ;; TODO: Permit destructuring.
    (define param
      (send catch-clause get 1))
    (define param-exp
      (send param get-value))
    (define param-compiled
      (if (eq? param-exp '_)
          js/null
          (compile-expression
           param env options)))
    (define body
      (make-rose
       `(begin ,@(send catch-clause drop 2))))
    (define body-compiled
      (compile-statement-or-return-statement
       body env options))
    (set! handler
          (new CatchClause
               param-compiled
               body-compiled)))
  (define finalizer
    (if finally-clause
        (wrap-in-block-statement-smart
         (compile-statement-or-return-statement
          (make-rose
           `(begin ,@(send finally-clause drop 1)))
          env options))
        js/null))
  (make-expression-or-statement
   (new TryStatement
        block
        handler
        finalizer)
   options))

;;; Compile a `(push-left! ...)` expression.
(define (compile-push-left node env (options (js-obj)))
  ;; `.unshift()` returns the length of the array, while `push!()`
  ;; returns the list.
  (compile-push-helper
   (make-rose
    `(send ,(send node get 1)
           unshift
           ,(send node get 2)))
   (make-rose
    `((lambda (lst x)
        (send lst unshift x)
        lst)
      ,(send node get 1)
      ,(send node get 2)))
   node env options))

;;; Compile a `(push-right! ...)` expression.
(define (compile-push-right node env (options (js-obj)))
  ;; `.push()` returns the length of the array, while `push-right!()`
  ;; returns the list.
  (compile-push-helper
   (make-rose
    `(send ,(send node get 1)
           push
           ,(send node get 2)))
   (make-rose
    `((lambda (lst x)
        (send lst push x)
        lst)
      ,(send node get 1)
      ,(send node get 2)))
   node env options))

;;; Helper function for `compile-push-left`
;;; and `compile-push-right`.
(define (compile-push-helper statement-exp
                             expression-exp
                             node
                             env
                             (options (js-obj)))
  (define expression-type
    (oget options "expressionType"))
  (cond
   ;; When compiled as a return statement, create a program fragment
   ;; if the list expression is a symbol. Otherwise, reuse the
   ;; expression logic and wrap in `(return ...)`.
   ((eq? expression-type "return")
    (cond
     ((symbol? (send (send node get 1)
                     get-value))
      (new Program
           (list
            (compile-statement
             statement-exp env options)
            (compile-return-statement
             (send node get 1)
             env options))))
     (else
      (compile-rose
       (make-rose
        `(return ,node))
       env options))))
   ;; When compiled as a statement, the return
   ;; type does not matter.
   ((eq? expression-type "statement")
    (compile-statement-or-return-statement
     statement-exp env options))
   ;; When compiled as an expression, we can use the comma
   ;; operator if the list expression is a symbol.
   ((symbol? (send (send node get 1)
                   get-value))
    (new SequenceExpression
         (list
          (compile-expression
           statement-exp env options)
          (compile-expression
           (send node get 1)
           env options))))
   ;; In more complicated cases, we compile to
   ;; a lambda expression.
   (else
    (compile-expression
     expression-exp env options))))

;;; Compile a `(define-macro ...)` expression.
(define (compile-define-macro node env (options (js-obj)))
  (define exp
    (send node get-value))
  (define name-and-args
    (second exp))
  (define name
    (car name-and-args))
  (define macro-fn-form
    (define-macro->lambda-form exp))
  (define args
    (second macro-fn-form))
  (define body
    (drop macro-fn-form 2))
  (define bindings
    (oget options "bindings"))
  (define result
    (compile-rose
     (transfer-comments
      node
      (make-rose
       `(begin
          (define (,name ,@args)
            ,@body)
          (set-field! lispMacro ,name #t))
       node))
     env options))
  (when bindings
    (send bindings set-local name #t "macro"))
  result)

;;; Compiler macro for `(make-hash ...)` expressions.
(defmacro compile-make-hash-macro (assocs)
  (cond
   (assocs
    (cond
     ((and (or (tagged-list? assocs 'quasiquote)
               (tagged-list? assocs 'quote))
           (list? (second assocs))
           (= (array-list-length
               (filter
                (lambda (x)
                  (or (not (array? x))
                      (and (= (array-list-length x) 2)
                           (or (tagged-list? x 'unquote)
                               (and (tagged-list? x 'unquote-splicing)
                                    (not (tagged-list?
                                          (second x)
                                          'hash->list)))))))
                (second assocs)))
              0))
      ;; If we have a quoted list of pairs, rewrite it to a simpler
      ;; expression that does not call `flatten`.
      `(new Map
            (ann (,(first assocs)
                  ,(map (lambda (x)
                          (cond
                           ((and (tagged-list? x 'unquote-splicing)
                                 (tagged-list? (second x) 'hash->list))
                            (cons (first x)
                                  (list `(send
                                          ,(second (second x))
                                          entries))))
                           (else
                            (list (car x) (cdr x)))))
                        (second assocs)))
                 Any)))
     (else
      ;; If the `assocs` form is not simple, then we have map
      ;; `flatten` over it in order to convert a list of pairs to a
      ;; list of lists.
      `(new Map (map flatten ,assocs)))))
   (else
    `(new Map))))

;;; Compiler macro for `(hash-clear ...)` expressions.
(defmacro compile-hash-clear-macro (ht)
  (cond
   ((symbol? ht)
    `(begin
       (send ,ht clear)
       ,ht))
   (else
    `((lambda (ht)
        (send ht clear)
        ht)
      ,ht))))

;;; Compiler macro for `(hash-remove! ...)` expressions.
(defmacro compile-hash-remove-macro (ht key)
  (cond
   ((symbol? ht)
    `(begin
       (send ,ht delete ,key)
       ,ht))
   (else
    `((lambda (ht key)
        (send ht delete key)
        ht)
      ,ht ,key))))

;;; Compiler macro for `(hash-ref ...)` expressions.
(defmacro compile-hash-ref-macro (ht key failure-result)
  (cond
   (failure-result
    (definition->macro
      '(define (hash-ref ht key failure-result)
         (if (send ht has key)
             (send ht get key)
             failure-result))
      (list ht key failure-result)))
   (else
    `(send ,ht get ,key))))

;;; Compiler macro for `(map ...)` expressions.
(defmacro compile-map-macro (f x)
  ;; Note that `` `(send ,x map ,f) `` is too simple, as JavaScript's
  ;; `.map()` method calls the function with multiple arguments. This
  ;; can lead to unintuitive bugs in cases where the function has an
  ;; optional second parameter. To avoid this, we enclose `f` in a
  ;; unary function wrapper.
  (define f-exp
    (compile-map-macro-helper f env))
  `(send ,x map ,f-exp))

;;; Compiler macro for `(values ...)` expressions.
(defmacro compile-values-macro (&rest args)
  `(list ,@args))

;;; Compiler macro for `(string? ...)` expressions.
(defmacro compile-stringp-macro (x)
  `(eq? (type-of ,x) "string"))

;;; Compiler macro for `(string-trim ...)` expressions.
(defmacro compile-string-trim-macro (&rest args)
  (cond
   ((= (array-length args) 1)
    `(send ,(array-first args) trim))
   (else
    (definition->macro (source string-trim_) args))))

;;; Compiler macro for `(member? ...)` expressions.
(defmacro compile-member-p-macro (v lst is-equal)
  (cond
   ((not is-equal)
    (definition->macro
      '(define (member?_ v lst)
         (memf? (lambda (x)
                  (equal? v x))
                lst))
      (list v lst)))
   (else
    (definition->macro
      '(define (member?_ v lst is-equal)
         (memf? (lambda (x)
                  (is-equal v x))
                lst))
      (list v lst is-equal)))))

;;; Compiler macro for `(substring ...)` expressions.
(defmacro compile-substring-macro (str &rest args)
  `(send ,str substring ,@args))

;;; Compiler macro for `(array-drop ...)` expressions.
(defmacro compile-array-drop-macro (arr n)
  (cond
   ((number? n)
    (cond
     ((= n 0)
      arr)
     (else
      `(send ,arr slice ,n))))
   (else
    (definition->macro
      (source array-drop_)
      (list arr n)))))

;;; Compiler macro for `(drop-right ...)` expressions.
(defmacro compile-array-drop-right-macro (arr n)
  (cond
   ((number? n)
    (cond
     ((= n 0)
      arr)
     (else
      `(send ,arr slice 0 (- ,n)))))
   (else
    (definition->macro
      (source array-drop-right_)
      (list arr n)))))

;;; Compiler macro for `(drop ...)` expressions.
(defmacro compile-drop-macro (lst pos)
  (cond
   ((number? pos)
    (cond
     ((= pos 0)
      lst)
     (else
      `(send ,lst slice ,pos))))
   (else
    (definition->macro (source drop_) (list lst pos)))))

;;; Compiler macro for `(drop-right ...)` expressions.
(defmacro compile-drop-right-macro (lst n)
  (cond
   ((number? n)
    (cond
     ((= n 0)
      lst)
     (else
      `(send ,lst slice 0 (- ,n)))))
   (else
    (definition->macro (source drop-right_) (list lst n)))))

;;; Compiler macro for `(array-list-drop ...)` expressions.
(defmacro compile-array-list-drop-macro (lst n)
  (cond
   ((number? n)
    (cond
     ((= n 0)
      lst)
     (else
      `(send ,lst slice ,n))))
   (else
    (definition->macro
      (source array-list-drop_)
      (list lst n)))))

;;; Compiler macro for `(array-list-drop-right ...)` expressions.
(defmacro compile-array-list-drop-right-macro (lst n)
  (cond
   ((number? n)
    (cond
     ((= n 0)
      lst)
     (else
      `(send ,lst slice 0 (- ,n)))))
   (else
    (definition->macro
      (source array-list-drop-right_)
      (list lst n)))))

;;; Compiler macro for `(regexp ...)` expressions.
(defmacro compile-regexp-macro (&rest args)
  `(new RegExp ,@args))

;;; Compiler macro for `(assert ...)` expressions.
(defmacro compile-assert-macro (&rest args)
  `(send console assert ,@args))

;;; Compiler macro for `(display ...)` expressions.
(defmacro compile-display-macro (&rest args)
  `(send console log ,@args))

;;; Compiler macro for `(current-environment)` expressions.
(defmacro compile-current-environment-macro ()
  (define arg-sym
    (gensym "_arg"))
  (define str-sym
    (gensym "_str"))
  (define identifier-regexp
    '(regexp "^\\w+$"))
  `(js-obj "get"
           (js/arrow (,arg-sym)
             (try
               (define ,str-sym
                 (symbol->string ,arg-sym))
               (cond
                ((regexp-match? ,identifier-regexp ,str-sym)
                 (return (js/eval ,str-sym)))
                (else
                 (return undefined)))
               (catch Error e
                 (return undefined))))
           "has"
           (js/arrow (,arg-sym)
             (try
               (define ,str-sym
                 (symbol->string ,arg-sym))
               (cond
                ((regexp-match? ,identifier-regexp ,str-sym)
                 (js/eval ,str-sym)
                 (return #t))
                (else
                 (return #f)))
               (catch Error e
                 (return #f))))))

;;; Compile a `(js ...)` expression.
(define (compile-js node env (options (js-obj)))
  (define eval-option
    (oget options "eval"))
  (set! eval-option #t)
  (define str
    (send node get 1))
  (define str-exp
    (send str get-value))
  (cond
   ((not eval-option)
    (make-expression-or-statement
     (new Literal undefined)
     options))
   ((string? str-exp)
    (make-expression-or-statement
     (new XRawJavaScript str-exp)
     options))
   (else
    (compile-js-eval node env options))))

;;; Compile a `(js/eval ...)` expression.
(define (compile-js-eval node env (options (js-obj)))
  ;; TODO: Disable if `eval-option` is `#f`.
  (define eval-option
    (oget options "eval"))
  ;; TODO: Make `#f` the default.
  (set! eval-option #t)
  (define str
    (send node get 1))
  (define str-exp
    (send str get-value))
  (cond
   ((not eval-option)
    (make-expression-or-statement
     (new Literal undefined)
     options))
   (else
    (make-expression-or-statement
     (new CallExpression
          (new Identifier "eval")
          (list
           (compile-expression str env options)))
     options))))

;;; Expand a `(quote ...)` expression.
;;;
;;; Similar to [`quote` in Racket][rkt:quote] and
;;; [`quote` in Common Lisp][cl:quote].
;;;
;;; [rkt:quote]: https://docs.racket-lang.org/reference/quote.html
;;; [cl:quote]: http://clhs.lisp.se/Body/s_quote.htm#quote
(defmacro quote_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(quasiquote ...)` form.
;;; Like `(quote ...)`, but treats `(unquote ...)` and
;;; `(unquote-splicing ...)` forms as escaping mechanisms.
;;;
;;; Similar to [`quasiquote` in Racket][rkt:quasiquote].
;;; Also known as "[backquote][cl:backquote]".
;;;
;;; [rkt:quasiquote]: https://docs.racket-lang.org/reference/quasiquote.html
;;; [cl:backquote]: http://clhs.lisp.se/Body/02_df.htm
(defmacro quasiquote_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(set! ...)` expression.
;;;
;;; Similar to [`set!` in Racket][rkt:setx] and
;;; [`setq` in Common Lisp][cl:setq].
;;;
;;; [rkt:setx]: https://docs.racket-lang.org/reference/set_.html#%28form._%28%28quote._~23~25kernel%29._set%21%29%29
;;; [cl:setq]: http://clhs.lisp.se/Body/s_setq.htm#setq
(defmacro set!_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(module ...)` expression.
(defmacro module_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(begin ...)` expression.
(defmacro begin_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(block ...)` expression.
(defmacro block_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(let* ...)` expression.
(defmacro let-star_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(let-values ...)` expression.
(defmacro let-values_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(define-values ...)` expression.
(defmacro define-values_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(set!-values ...)` expression.
(defmacro set-values_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(define ...)` expression.
(defmacro define_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(define/generator ...)` expression.
(defmacro define-generator_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(define/async ...)` expression.
(defmacro define-async_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(define-macro ...)` expression.
;;;
;;; Similar to [`define-macro` in Guile][guile:define-macro] and
;;; [`defmacro` in Common Lisp][cl:defmacro].
;;;
;;; [guile:define-macro]: https://www.gnu.org/software/guile/docs/docs-2.2/guile-ref/Defmacros.html
;;; [cl:defmacro]: http://clhs.lisp.se/Body/m_defmac.htm#defmacro
(defmacro define-macro_ (&whole exp &environment env)
  (define name
    (first (second exp)))
  (define f-exp
    (compile-sexp
     exp
     env
     (current-compilation-options)))
  (define f
    (eval_ f-exp env))
  (send env set name f "macro")
  f-exp)

;;; Expand a `(for ...)` expression.
(defmacro for_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/while ...)` expression.
(defmacro js-while_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/do-while ...)` expression.
(defmacro js-do-while_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(break)` expression.
(defmacro break_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(continue)` expression.
(defmacro continue_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(yield ...)` expression.
(defmacro yield_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(return ...)` expression.
(defmacro return_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(throw ...)` expression.
;;;
;;; Similar to the [`throw`][clj:throw] special form in Clojure.
;;;
;;; [clj:throw]: https://clojuredocs.org/clojure.core/throw
(defmacro throw_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/async ...)` expression.
(defmacro js-async_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/await ...)` expression.
(defmacro js-await_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(lambda ...)` expression.
;;;
;;; Returns an anonymous function. The name `lambda` is
;;; a reference to [lambda calculus][w:Lambda calculus].
;;;
;;; [w:Lambda calculus]: https://en.wikipedia.org/wiki/Lambda_calculus
(defmacro lambda_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/function ...)` expression.
;;;
;;; Creates an anonymous JavaScript function.
(defmacro js-function_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/arrow ...)` expression.
;;;
;;; Creates a JavaScript arrow function.
(defmacro js-arrow_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(cond ...)` expression.
(defmacro cond_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand an `(and ...)` expression.
(defmacro and_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand an `(or ...)` expression.
(defmacro or_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Call a method on an object.
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
                (string-append "Not a method: " method))))))

;;; Expand a `(send ...)` expression.
;;;
;;; Similar to [`send`][rkt:send] in Racket.
;;;
;;; [rkt:send]: https://docs.racket-lang.org/guide/classes.html#(part._methods)
(defmacro send_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(send/apply ...)` expression.
(defmacro send-apply_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(. ...)` expression.
;;;
;;; Similar to the [`.` special form][clj:dot] in Clojure and
;;; [ClojureScript][cljs:dot].
;;;
;;; [clj:dot]: https://clojure.org/reference/java_interop#dot
;;; [cljs:dot]: https://cljs.github.io/api/syntax/dot
(defmacro dot_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(get-field ...)` expression.
(defmacro get-field_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/optional-chaining ...)` expression.
(defmacro js-optional-chaining_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(set-field! ...)` expression.
(defmacro set-field_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Evaluate an `(make-object ...)` expression.
(define (new_ constructor . args)
  ;; TODO: Rename to `new_`.
  ;; TODO: Express as `(apply new ...)`.
  (apply make-object constructor args))

;;; Expand a `(class ...)` expression.
;;;
;;; Loosely based on [`class` in Racket][rkt:class] and
;;; [`define-class` in CLOS][cl:define-class].
;;;
;;; [rkt:class]: https://docs.racket-lang.org/guide/classes.html
;;; [cl:define-class]: http://clhs.lisp.se/Body/07_.htm
(defmacro class_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(define-class ...)` expression.
;;;
;;; Loosely based on [`class` in Racket][rkt:class] and
;;; [`defclass` in CLOS][cl:defclass].
;;;
;;; [rkt:class]: https://docs.racket-lang.org/guide/classes.html
;;; [cl:defclass]: http://clhs.lisp.se/Body/m_defcla.htm#defclass
(defmacro define-class_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/try ...)` expression.
(defmacro js-try_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(provide ...)` expression.
(defmacro provide_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(require ...)` expression.
(defmacro require_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Evaluate a JavaScript string.
(define (js_ str)
  (js/eval str))

;;; Get the Lisp source of a function.
(define (source x)
  (get-field lispSource x))

;;; Whether a function has Lisp source.
(define (source? x)
  (and (not (eq? x undefined))
       (not (eq? (get-field lispSource x) undefined))))

;;; Map the function `f` over the rose tree-wrapped
;;; S-expression `node`. The S-expression is processed
;;; in bottom-up order.
(define (map-rose f
                  node
                  (env (new LispEnvironment))
                  (stack '())
                  (bindings (new LispEnvironment)))
  (cond
   ((not (is-a? node Rose))
    (map-sexp f node env stack bindings))
   (else
    (map-visit-rose f node env stack bindings))))

;;; Map a function `f` over a rose tree using the Visitor pattern.
(define (map-visit-rose f
                        node
                        (env (new LispEnvironment))
                        (stack '())
                        (bindings (new LispEnvironment)))
  (define (skip-node node stack bindings)
    node)
  (define (visit-node node stack bindings)
    (f node stack bindings))
  ;; Nonatomic value (i.e., a list form some sort).
  (define (visit-nonatomic node stack bindings (skip 0))
    (define result
      (visit-forms-node node `(,@stack ,node) bindings skip))
    (f result stack bindings))
  ;; Macro call.
  (define (visit-macro-call-p node)
    (let ((exp (send node get-value)))
      (macro-call? exp env)))
  (define visit-macro-call visit-node)
  ;; Special form.
  (define (visit-special-form-p node)
    (let ((exp (send node get-value)))
      (special-form? exp env)))
  (define visit-special-form visit-node)
  ;; Function call.
  (define (visit-function-call-p node)
    (let ((exp (send node get-value)))
      (function-call? exp env)))
  (define visit-function-call visit-nonatomic)
  (define (visit-else-p node)
    #t)
  (define (visit-forms-node-with visitor node stack bindings (skip 0))
    (define exp (send node get-value))
    (unless (array? exp)
      ;; `node` is not a list expression; early return.
      (return (visit visitor node stack bindings)))
    (define nodes (send node get-nodes))
    (define result-nodes
      (visit-forms-list-with visitor nodes stack bindings skip))
    (cond
     ((eq? result-nodes nodes)
      node)
     (else
      (define exp '())
      (define result
        (transfer-comments node (new Rose exp)))
      (for ((node result-nodes))
        (push-right! exp (send node get-value))
        (send result insert node))
      result)))
  (define (visit-forms-list-with visitor nodes stack bindings (skip 0))
    (unless (array? nodes)
      ;; `nodes` is not a list; early return.
      (return (visit visitor nodes stack bindings)))
    ;; Keep track of whether any of the expressions are modified
    ;; by visitation. If none of them are, return the original list.
    (define is-modified #f)
    (define i 0)
    (define result
      (map (lambda (x)
             (cond
              ((< i skip)
               (set! i (+ i 1))
               x)
              (else
               (define x1 (visit visitor x stack bindings))
               (unless (eq? x x1)
                 (set! is-modified #t))
               (set! i (+ i 1))
               x1)))
           nodes))
    ;; Return the original list if none of the sub-expressions
    ;; were modified.
    (unless is-modified
      (set! result nodes))
    result)
  (define (visit-forms-node node stack bindings (skip 0))
    (visit-forms-node-with visitor node stack bindings skip))
  (define (visit-forms-list nodes stack bindings (skip 0))
    (visit-forms-list-with visitor nodes stack bindings skip))
  (define (visit-clauses-node node stack bindings (skip 0))
    (visit-forms-node-with visit-forms-node node stack bindings skip))
  (define (visit-clauses-list nodes stack bindings (skip 0))
    (visit-forms-list-with visit-forms-node nodes stack bindings skip))
  ;; `(module ...)` form.
  (define (visit-module-p node)
    (form? node module_ env))
  (define (visit-module node stack bindings)
    (visit-nonatomic node stack bindings 3))
  ;; `(begin ...)` form.
  (define (visit-begin-p node)
    (form? node begin_ env))
  (define (visit-begin node stack bindings)
    (visit-nonatomic node stack bindings 1))
  ;; `(begin0 ...)` form.
  (define (visit-begin0-p node)
    (form? node begin0_ env))
  (define visit-begin0 visit-begin)
  ;; `(let ...)` form.
  (define (visit-let-p node)
    (form? node let-star_ env))
  (define (visit-let node stack bindings)
    (define result node)
    (define bindings-env
      (extend-environment (new LispEnvironment)
                          bindings))
    (define sym (~> node (send get 0) (send get-value)))
    (define let-bindings (send node get 1))
    (define body (send node drop 2))
    (for ((let-binding (send let-bindings get-value)))
      (define binding-sym
        (if (array? let-binding)
            (first let-binding)
            let-binding))
      (send bindings-env set-local binding-sym #t "variable"))
    (define visited-let-bindings
      (visit-clauses-node let-bindings `(,@stack ,node) bindings-env))
    (define visited-body
      (visit-forms-list body `(,@stack ,node) bindings-env))
    (unless (and (eq? let-bindings visited-let-bindings)
                 (eq? body visited-body))
      (set! result (transfer-comments
                    node
                    (make-rose
                     `(,sym ,visited-let-bindings
                            ,@visited-body)))))
    (f result stack bindings))
  (define (visit-let-values-p node)
    (form? node let-values_ env))
  (define (visit-let-values node stack bindings)
    (define result node)
    (define bindings-env
      (extend-environment (new LispEnvironment) bindings))
    (define sym (~> node (send get 0) (send get-value)))
    (define let-bindings (send node get 1))
    (define body (send node drop 2))
    (define visited-let-bindings
      (visit-forms-node-with
       (lambda (x)
         (define x-result x)
         (define ids (send x get 0))
         (define val (send x get 1))
         (define ids-exp (send ids get-value))
         (cond
          ((symbol? ids-exp)
           (send bindings-env set-local ids-exp #t "variable"))
          (else
           (for ((let-binding ids-exp))
             (when (symbol? let-binding)
               (send bindings-env set let-binding #t "variable")))))
         (define visited-ids
           (visit-forms-node ids `(,@stack ,node) bindings-env))
         (define visited-val
           (visit visitor val `(,@stack ,node) bindings-env))
         (unless (and (eq? visited-ids ids)
                      (eq? visited-val val))
           (set! x-result (transfer-comments
                           x
                           (make-rose
                            `(,visited-ids
                              ,visited-val)))))
         x-result)
       let-bindings
       `(,@stack ,node)
       bindings-env))
    (define visited-body
      (visit-forms-list body `(,@stack ,node) bindings-env))
    (unless (and (eq? let-bindings visited-let-bindings)
                 (eq? body visited-body))
      (set! result (transfer-comments
                    node
                    (make-rose
                     `(,sym ,visited-let-bindings
                            ,@visited-body)))))
    (f result stack bindings))
  ;; `(for ...)` form.
  (define (visit-for-p node)
    (form? node for_ env))
  (define visit-for visit-let)
  ;; `(while ...)` form.
  (define (visit-while-p node)
    (form? node js-while_ env))
  (define visit-while visit-function-call)
  ;; `(cond ...)` form.
  (define (visit-cond-p node)
    (form? node cond_ env))
  (define (visit-cond node stack bindings)
    (define result node)
    (define sym (~> node (send get 0) (send get-value)))
    (define clauses (send node drop 1))
    (define visited-clauses
      (visit-clauses-list clauses `(,@stack ,node) bindings))
    (unless (eq? visited-clauses clauses)
      (set! result (transfer-comments
                    node
                    (make-rose
                     `(,sym ,@visited-clauses)))))
    (f result stack bindings))
  ;; `(lambda ...)` form.
  (define (visit-lambda-p node)
    (or (form? node lambda_ env)
        (form? node js-function_ env)
        (form? node js-arrow_ env)))
  (define (visit-lambda node stack bindings)
    (define result node)
    (define bindings-env
      (extend-environment (new LispEnvironment)
                          bindings))
    (define sym (~> node (send get 0) (send get-value)))
    (define params (send node get 1))
    (define params-exp (send params get-value))
    (define body (send node drop 2))
    (cond
     ((symbol? params-exp)
      (send bindings-env set-local params-exp #t "variable"))
     (else
      (for ((param params-exp))
        (when (array? param)
          (set! param (first param)))
        (send bindings-env set-local param #t "variable"))))
    (define visited-params
      (visit-clauses-node params `(,@stack ,node) bindings-env))
    (define visited-body
      (visit-forms-list body `(,@stack ,node) bindings-env))
    (unless (and (eq? params visited-params)
                 (eq? body visited-body))
      (set! result (transfer-comments
                    node
                    (make-rose
                     `(,sym ,visited-params
                            ,@visited-body)))))
    (f result stack bindings))
  ;; `(define ...)` form.
  (define (visit-define-p node)
    (form? node define_ env))
  (define (visit-define node stack bindings)
    (define result node)
    (define define-sym (~> node (send get 0) (send get-value)))
    (define id (send node get 1))
    (define id-exp (send id get-value))
    (define id-sym
      (if (array? id-exp)
          (first id-exp)
          id-exp))
    (define bindings-env bindings)
    (cond
     ((array? id-exp)
      (send bindings set-local id-sym #t "procedure")
      (for ((param (rest id-exp)))
        (when (array? param)
          (set! param (first param)))
        (send bindings set-local param #t "variable"))
      (set! bindings-env
            (extend-environment (new LispEnvironment)
                                bindings)))
     (else
      (send bindings set-local id-sym #t "variable")))
    (define body (send node drop 2))
    (define visited-id
      (if (array? id-exp)
          (visit-clauses-node id `(,@stack ,node) bindings-env)
          (visit-node id `(,@stack ,node) bindings-env)))
    (define visited-body
      (visit-forms-list body `(,@stack ,node) bindings-env))
    (unless (and (eq? id visited-id)
                 (eq? body visited-body))
      (set! result (transfer-comments
                    node
                    (make-rose
                     `(,define-sym
                        ,visited-id
                        ,@visited-body)))))
    (f result stack bindings))
  ;; `(define-values ...)` form.
  (define (visit-define-values-p node)
    (form? node define-values_ env))
  (define (visit-define-values node stack bindings)
    (visit-forms-node node stack bindings 2))
  ;; `(defmacro ...)` form.
  (define (visit-defmacro-p node)
    (form? node defmacro_ env))
  (define (visit-defmacro node stack bindings)
    (define result node)
    (define defmacro-sym (~> node (send get 0) (send get-value)))
    (define id (send node get 1))
    (define id-sym (send id get-value))
    (define params (send node get 2))
    (define params-exp (send params get-value))
    (define body (send node drop 3))
    (send bindings set-local id-sym #t "macro")
    (define bindings-env
      (extend-environment (new LispEnvironment) bindings))
    (cond
     ((symbol? params-exp)
      (send bindings-env set-local params-exp #t "variable"))
     (else
      (for ((param (flatten_ params-exp)))
        (send bindings-env set-local params #t "variable"))))
    (define visited-id
      (visit-node id `(,@stack ,node) bindings-env))
    (define visited-params
      (visit-forms-node params `(,@stack ,node) bindings-env))
    (define visited-body
      (visit-forms-list body `(,@stack ,node) bindings-env))
    (unless (and (eq? id visited-id)
                 (eq? params visited-params)
                 (eq? body visited-body))
      (set! result (transfer-comments
                    node
                    (make-rose
                     `(,defmacro-sym
                        ,visited-id
                        ,visited-params
                        ,@visited-body)))))
    (set! result (f result stack bindings))
    (send bindings set-local id-sym #t "macro")
    result)
  ;; `(define-macro ...)` form.
  (define (visit-define-macro-p node)
    (form? node define-macro_ env))
  (define (visit-define-macro node stack bindings)
    (define result node)
    (define define-macro-sym (~> node (send get 0) (send get-value)))
    (define name-and-args (send node get 1))
    (define name-and-args-exp (send name-and-args get-value))
    (define id-sym (car name-and-args-exp))
    (define id (make-rose id-sym name-and-args))
    (define params-exp (cdr name-and-args-exp))
    (define params (make-rose params-exp name-and-args))
    (define body (send node drop 2))
    (send bindings set-local id-sym #t "macro")
    (define bindings-env
      (extend-environment (new LispEnvironment) bindings))
    (cond
     ((symbol? params-exp)
      (send bindings-env set-local params-exp #t "variable"))
     (else
      (for ((param (flatten_ params-exp)))
        (send bindings-env set-local params #t "variable"))))
    (define visited-id
      (visit-node id `(,@stack ,node) bindings-env))
    (define visited-params
      (visit-forms-node params `(,@stack ,node) bindings-env))
    (define visited-body
      (visit-forms-list body `(,@stack ,node) bindings-env))
    (unless (and (eq? id visited-id)
                 (eq? params visited-params)
                 (eq? body visited-body))
      (set! result (transfer-comments
                    node
                    (make-rose
                     `(,define-macro-sym
                        ,(cons visited-id visited-params)
                        ,@visited-body)))))
    (set! result (f result stack bindings))
    (send bindings set-local id-sym #t "macro")
    result)
  ;; `(define-class ...)` form.
  (define (visit-define-class-p node)
    (form? node class_ env))
  (define visit-define-class visit-function-call)
  ;; `(ann ...)` form.
  (define (visit-ann-p node)
    (form? node ann_ env))
  (define (visit-ann node stack bindings)
    (visit-node node stack bindings))
  ;; `(and ...)` form.
  (define (visit-and-p node)
    (form? node and_ env))
  (define visit-and visit-function-call)
  ;; `(or ...)` form.
  (define (visit-or-p node)
    (form? node or_ env))
  (define visit-or visit-function-call)
  ;; `(when ...)` form.
  (define (visit-when-p node)
    (form? node when_ env))
  (define (visit-when node stack bindings)
    (visit-nonatomic node stack bindings 1))
  ;; `(unless ...)` form.
  (define (visit-unless-p node)
    (form? node unless_ env))
  (define (visit-unless node stack bindings)
    (visit-nonatomic node stack bindings 1))
  ;; `(make-object ...)` form.
  (define (visit-make-object-p node)
    (form? node new_ env))
  (define visit-make-object visit-function-call)
  ;; `(return ...)` form.
  (define (visit-return-p node)
    (form? node return_ env))
  (define visit-return visit-function-call)
  ;; `(send ...)` form.
  (define (visit-send-p node)
    (form? node send_ env))
  (define visit-send visit-function-call)
  ;; `(set! ...)` form.
  (define (visit-setq-p node)
    (form? node set!_ env))
  (define visit-setq visit-function-call)
  ;; `(set-field! ...)` form.
  (define (visit-set-field-p node)
    (form? node set-field_ env))
  (define visit-set-field visit-function-call)
  ;; `(get-field ...)` form.
  (define (visit-get-field-p node)
    (form? node get-field_ env))
  (define visit-get-field visit-function-call)
  ;; Quoted value.
  (define (visit-quote-p node)
    (form? node quote_ env))
  (define visit-quote visit-node)
  ;; Quasiquoted value.
  (define (visit-quasiquote-p node)
    (form? node quasiquote_ env))
  (define (visit-quasiquote node stack bindings)
    (define (visit-quasiquote-form node stack bindings)
      (define result node)
      (define sym (send node get 0))
      (define val (send node get 1))
      ;; Visit `unquote` and `unquote-splicing` expressions, if any.
      (define visited-val
        (visit quasiquote-visitor val stack bindings))
      (unless (eq? val visited-val)
        (set! result (transfer-comments
                      node
                      (make-rose
                       `(,sym ,visited-val)))))
      ;; Visit the `unquote` expression.
      (f result stack bindings))
    (define (visit-unquote-p node)
      (tagged-list? node 'unquote))
    (define (visit-unquote node stack)
      ;; When visiting unquoted expressions,
      ;; use the regular visitor.
      (visit-forms-node-with visitor node stack bindings 1))
    (define (visit-unquote-splicing-p node)
      (tagged-list? node 'unquote-splicing))
    (define visit-unquote-splicing visit-unquote)
    (define (visit-quoted-list node stack bindings)
      (visit-forms-node-with quasiquote-visitor node stack bindings))
    (define quasiquote-visitor
      (make-visitor
       `((,visit-unquote-p ,visit-unquote)
         (,visit-unquote-splicing-p ,visit-unquote-splicing)
         (,visit-nonatomic-p ,visit-quoted-list)
         (,visit-else-p ,skip-node))))
    (visit-quasiquote-form node `(,@stack ,node) bindings))
  ;; List.
  (define (visit-nonatomic-p node)
    (let ((exp (send node get-value)))
      (array? exp)))
  ;; Atomic value.
  (define visit-atom-p visit-else-p)
  (define visit-atom visit-node)
  ;; Rename this to `map-visitor` to distinguish it from
  ;; the `visitor` parameter of many functions.
  (define visitor
    (make-visitor
     `((,visit-module-p ,visit-module)
       (,visit-begin-p ,visit-begin)
       (,visit-begin0-p ,visit-begin0)
       (,visit-let-p ,visit-let)
       (,visit-let-values-p ,visit-let-values)
       (,visit-cond-p ,visit-cond)
       (,visit-lambda-p ,visit-lambda)
       (,visit-define-p ,visit-define)
       (,visit-define-values-p ,visit-define-values)
       (,visit-define-macro-p ,visit-define-macro)
       (,visit-defmacro-p ,visit-defmacro)
       (,visit-ann-p ,visit-ann)
       (,visit-and-p ,visit-and)
       (,visit-or-p ,visit-or)
       (,visit-for-p ,visit-for)
       (,visit-while-p ,visit-while)
       (,visit-when-p ,visit-when)
       (,visit-send-p ,visit-send)
       (,visit-setq-p ,visit-setq)
       (,visit-set-field-p ,visit-set-field)
       (,visit-get-field-p ,visit-get-field)
       (,visit-unless-p ,visit-unless)
       (,visit-define-class-p ,visit-define-class)
       (,visit-make-object-p ,visit-make-object)
       (,visit-return-p ,visit-return)
       (,visit-quote-p ,visit-quote)
       (,visit-quasiquote-p ,visit-quasiquote)
       (,visit-macro-call-p ,visit-macro-call)
       (,visit-special-form-p ,visit-special-form)
       (,visit-function-call-p ,visit-function-call)
       (,visit-nonatomic-p ,visit-nonatomic)
       (,visit-else-p ,visit-atom))))
  (visit visitor node stack bindings))

;;; Map the function `f` over the S-expression `exp`.
;;; The S-expression is processed in bottom-up order.
(define (map-sexp f
                  exp
                  (env (new LispEnvironment))
                  (stack '())
                  (bindings (new LispEnvironment)))
  (let* ((f1 (lambda (x stack bindings)
               (let* ((exp (send x get-value))
                      (stack1 (map (lambda (x)
                                     (if (is-a? x Rose)
                                         (send x get-value)
                                         x))
                                   stack))
                      (result (f exp stack1 bindings)))
                 (if (eq? result exp)
                     x
                     (make-rose result x)))))
         (is-rose (is-a? exp Rose))
         (node (if is-rose
                   exp
                   (make-rose exp)))
         (result (map-rose f1 node env stack bindings)))
    ;; If the input is a rose tree node,
    ;; return a rose tree node as output too.
    (if is-rose
        result
        (send result get-value))))

;;; Call the function `f` on each node of a rose tree,
;;; but do not create a new rose tree in the process.
(define (iterate-rose f node (env (new LispEnvironment)))
  (map-rose (lambda (x stack)
              (f x stack)
              x)
            node
            env))

;;; Expand an `(ann ...)` expression.
(defmacro ann_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(: ...)` expression.
(defmacro colon_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(define-type ...)` expression.
(defmacro define-type_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(let-js-obj ...)` expression.
(defmacro let-js-obj_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(define-js-obj ...)` expression.
(defmacro define-js-obj_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(set!-js-obj ...)` expression.
(defmacro set-js-obj_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Compile a `(js/switch ...)` form.
(define (compile-js-switch node env (options (js-obj)))
  (define expression-type
    (oget options "expressionType"))
  (cond
   ((or (eq? expression-type "statement")
        (eq? expression-type "return"))
    (define discriminant
      (send node get 1))
    (define discriminant-compiled
      (compile-expression
       discriminant env options))
    (define cases
      (send node drop 2))
    (define cases-compiled
      (map (lambda (x)
             (define op
               (~> x
                   (send get 0)
                   (send get-value)))
             (define test-compiled)
             (define consequent-compiled)
             (cond
              ((eq? op 'case)
               (define test
                 (send x get 1))
               (set! test-compiled
                     (compile-expression
                      test env options))
               (define consequent
                 (send x drop 2))
               ;; It is advisable to wrap cases in a block statement.
               ;; <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/switch#lexical_scoping>
               (set! consequent-compiled
                     (list
                      (compile-statement-or-return-statement
                       (make-rose
                        `(block ,@consequent))
                       env options))))
              (else
               (set! test-compiled js/null)
               (define consequent
                 (send x drop 1))
               (set! consequent-compiled
                     (list
                      (compile-statement-or-return-statement
                       (make-rose
                        `(block ,@consequent))
                       env options)))))
             (new SwitchCase
                  test-compiled
                  consequent-compiled))
           cases))
    (new SwitchStatement
         discriminant-compiled
         cases-compiled))
   (else
    (compile-expression
     (wrap-in-arrow-call node)
     env options))))

;;; Expand a `(js/switch ...)` expression.
(defmacro js-switch_ (&whole exp &environment env)
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Simple `call-with-current-continuation` implementation.
;;; Also known as `call/cc`.
;;;
;;; Similar to
;;; [`call-with-current-continuation` in Racket][rkt:call-with-current-continuation].
;;;
;;; [rkt:call-with-current-continuation]: https://docs.racket-lang.org/reference/cont.html#%28def._%28%28quote._~23~25kernel%29._call-with-current-continuation%29%29
(define (call-with-current-continuation_ proc (prompt-tag undefined))
  (define-class CallCCWrapper ()
    (define/public value)
    (define/public (constructor value)
      (set-field! value this value)))
  (try
    (return
     (proc
      (js/arrow (value)
        (throw (new CallCCWrapper value)))))
    (catch Object e
      (cond
       ((is-a? e CallCCWrapper)
        (return (get-field value e)))
       (else
        (throw e))))))

;;; Traverse an ESTree tree.
(define (traverse-estree node
                         (enter undefined)
                         (leave undefined)
                         (replace undefined))
  (define result node)
  (define el)
  (define el1)
  (define val)
  (define val1)
  (unless (is-a? node Node)
    (return result))
  (when enter
    (enter node))
  (for ((key (js-keys node)))
    (set! val (oget node key))
    (cond
     ((array? val)
      (for ((i (range 0 (array-list-length val))))
        (set! el (aget val i))
        (set! el1 (traverse-estree el enter leave replace))
        (unless (eq? el el1)
          (list-set! val i el1))))
     (else
      (set! val1 (traverse-estree val enter leave replace))
      (unless (eq? val val1)
        (oset! node key val1)))))
  (when leave
    (leave node))
  (when replace
    (set! result (replace node)))
  result)

;;; Find ESTree nodes matching a predicate.
(define (find-estree pred node)
  (define nodes '())
  (traverse-estree node
                   (lambda (x)
                     (when (pred x)
                       (push-right! nodes x))))
  nodes)

;;; Optimize an S-expression.
(define (optimize-sexp exp env)
  (cond
   ((rose? exp)
    (optimize-rose exp env))
   (else
    (~> (make-rose exp)
        (optimize-rose _ env)
        (send _ get-value)))))

;;; Optimize a rose tree-wrapped S-expression.
(define (optimize-rose exp env)
  (apply-optimizations exp env))

;;; Optimize a module.
(define (optimize-module m env)
  (send m
        set-nodes
        (map (lambda (x)
               (optimize-sexp x env))
             (get-field main-nodes m))))

;;; Optimize an ESTree tree.
(define (optimize-estree exp)
  (~> exp
      (let-vars-to-const-vars)))

(define (let-vars-to-const-vars program)
  (define variables '())
  (traverse-estree
   program
   (lambda (node)
     (define var-names '())
     (cond
      ((estree-type? node "AssignmentExpression")
       (cond
        ((estree-type? (get-field left node) "Identifier")
         (push! var-names (get-field name (get-field left node))))
        ((estree-type? (get-field left node) "ArrayPattern")
         (for ((element (get-field elements (get-field left node))))
           (when (and element
                      (estree-type? element "Identifier"))
             (push! var-names (get-field name element)))))))
      ((estree-type? node "UpdateExpression")
       (when (estree-type? (get-field argument node) "Identifier")
         (push! var-names (get-field name (get-field argument node))))))
     (for ((var-name var-names))
       (unless (memq? var-name variables)
         (push! variables var-name)))))
  (traverse-estree
   program
   undefined
   undefined
   (lambda (node)
     (cond
      ((estree-type? node "VariableDeclaration")
       (unless (findf
                (lambda (x)
                  (or (not (get-field init x))
                      (not (zero?
                            (array-list-length
                             (find-estree
                              (lambda (y)
                                (and (estree-type? y "Identifier")
                                     (memq? (get-field name y)
                                            variables)))
                              (get-field id x)))))))
                (get-field declarations node))
         (set-field! kind node "const"))
       node)
      (else
       node)))))

;;; Find a optimization rule matching `node`.
(define (find-optimization node env (rules optimizations))
  (for ((rule rules))
    (define-values (predicate)
      rule)
    (when (predicate node env)
      (return rule)))
  #f)

;;; Apply optimizations to `node`.
(define (apply-optimizations node env (rules optimizations))
  (define result node)
  (define rule #f)
  (while (set! rule (find-optimization result env rules))
    (define-values (predicate optimizer)
      rule)
    (set! result (optimizer result env)))
  result)

;;; List of `(predicate optimizer)` tuples.
(define optimizations '())

;;; Module class.
(define-class Module ()
  (define/public name "")

  (define/public header-expressions '())

  (define/public header-nodes '())

  (define/public require-expressions '())

  (define/public require-nodes '())

  (define/public provide-expressions '())

  (define/public provide-nodes '())

  (define/public main-expressions '())

  (define/public main-nodes '())

  (define/public expressions '())

  (define/public nodes '())

  (define/public inline-lisp-sources-flag #f)

  (define/public seen-modules '())

  (define/public environment)

  (define/public parent-environment)

  (define/public module-map)

  (define/public symbol-map (make-hash))

  (define/public (constructor (nodes '())
                              (parent lang-environment)
                              (name ""))
    (set-field! parent-environment this parent)
    (set-field! name this name)
    (send this initialize-nodes nodes))

  (define/public (get-expressions)
    (get-field expressions this))

  (define/public (get-environment)
    (cond
     ((get-field environment this)
      (get-field environment this))
     (else
      (send this make-environment (get-field parent-environment this)))))

  (define/public (get-module-map)
    (get-field module-map this))

  (define/public (get-name)
    (get-field name this))

  ;;; Whether a particular symbol is bound in this module's scope
  ;;; (i.e., whether the module imports or defines the symbol).
  (define/public (has-symbol sym)
    (define key
      (if (string? sym)
          (string->symbol sym)
          sym))
    (send (get-field symbol-map this) has key))

  (define/public (make-header-node (nodes '()))
    ;; Create header node if there is more than one comment, or if
    ;; there is a single comment ending in a blank line.
    (when (> (length nodes) 0)
      (define initial-node
        (first nodes))
      (define comments
        (send initial-node get-property "comments"))
      (define initial-node-comments '())
      (define initial-node-comment-string undefined)
      (define header-comments '())
      (define header-comment-strings '())
      (when comments
        (send this find-inline-lisp-sources-comment comments)
        (define comment-strings '())
        (for ((comment comments))
          (set! comment-strings
                (append comment-strings
                        (split-comments
                         (get-field value comment)))))
        (when (> (length comment-strings) 0)
          (set! header-comment-strings
                (array-drop-right comment-strings 1))
          (set! initial-node-comment-string
                (array-list-last comment-strings))
          (when (regexp-match (regexp "\\n\\n$")
                              initial-node-comment-string)
            (push-right! header-comment-strings
                         initial-node-comment-string)
            (set! initial-node-comment-string undefined))
          (when (> (length header-comment-strings) 0)
            (aset! header-comment-strings
                   (- (array-length header-comment-strings) 1)
                   (regexp-replace
                    (regexp "\\n*$")
                    (aget header-comment-strings
                          (- (array-length header-comment-strings) 1))
                    "")))))
      (when (> (length header-comment-strings) 0)
        (define header-exp
          '(begin))
        (define header-node
          (make-rose header-exp))
        (set! header-comments
              (map (lambda (x)
                     (new LeadingCommentToken x))
                   header-comment-strings))
        (send header-node
              set-property
              "comments"
              header-comments)
        (push-right! (get-field header-nodes this)
                     header-node)
        (push-right! (get-field header-expressions this)
                     header-exp)
        (when initial-node-comment-string
          (set! initial-node-comments
                (list
                 (new LeadingCommentToken
                      initial-node-comment-string))))
        (send initial-node
              set-property
              "comments"
              initial-node-comments))))

  (define/public (find-inline-lisp-sources-comment (comments '()))
    (unless (send this get-inline-lisp-sources-flag)
      (define pattern
        (regexp "; inline-lisp-sources: t"))
      (for ((comment comments))
        (define text
          (get-field value comment))
        (when (regexp-match pattern text)
          (send this set-inline-lisp-sources-flag #t)
          (break)))))

  (define/public (initialize-nodes (nodes '()))
    (define exp)
    (define match)
    (define node)
    (send this make-header-node nodes)
    ;; Sort the expressions into `require` expressions, `provide`
    ;; expressions and main expressions.
    (for ((node nodes))
      ;; Handle both S-expressions and rose tree values---for now.
      ;; In the future, we might want to simplify this to only
      ;; rose tree values.
      (cond
       ((is-a? node Rose)
        (set! exp (send node get-value))
        (define comments
          (send node get-property "comments"))
        (when comments
          ;; Look for `inline-lisp-sources: true` magic comment.
          (send this find-inline-lisp-sources-comment comments)))
       (else
        (set! exp node)
        (set! node (make-rose exp))))
      (cond
       ((tagged-list? exp 'require)
        (push-right! (get-field require-expressions this) exp)
        (push-right! (get-field require-nodes this) node))
       ((tagged-list? exp 'provide)
        (push-right! (get-field provide-expressions this) exp)
        (push-right! (get-field provide-nodes this) node))
       (else
        (push-right! (get-field main-expressions this) exp)
        (push-right! (get-field main-nodes this) node))))
    ;; Iterate over `require-expressions`.
    (for ((node (get-field require-nodes this)))
      (set! exp (send node get-value))
      (cond
       ((and (tagged-list? exp 'require)
             (> (array-list-length exp) 1)
             (tagged-list? (second exp) 'only-in))
        (define module-name
          (second (second exp)))
        (when (symbol? module-name)
          (set! module-name
                (symbol->string module-name)))
        (when (set! match
                    (regexp-match (regexp "^\\./(.*)$")
                                  module-name))
          (set! module-name (second match)))
        (unless (or (not match)
                    (memq? module-name (get-field seen-modules this)))
          (push-right! (get-field seen-modules this) module-name))
        ;; Add imported symbols to `.symbol-map`.
        (for ((x (rest (second exp))))
          (cond
           ((array? x)
            (send (get-field symbol-map this) set (cadr x) #t))
           (else
            (send (get-field symbol-map this) set x #t)))))
       ((and (tagged-list? exp 'require)
             (> (array-list-length exp) 1))
        (let* ((module-name-symbol (array-list-last exp))
               (module-name module-name-symbol))
          (cond
           ((symbol? module-name-symbol)
            (set! module-name
                  (symbol->string
                   module-name-symbol)))
           (else
            (set! module-name-symbol
                  (string->symbol module-name))))
          (set! module-name (get-module-name module-name))
          (unless (memq? module-name (get-field seen-modules this))
            (push-right! (get-field seen-modules this) module-name))
          ;; Add module symbol to `symbol-map`.
          (send (get-field symbol-map this)
                set
                module-name-symbol
                #t)))))
    ;; Iterate over `main-expressions`.
    (for ((node (get-field main-nodes this)))
      (set! exp (send node get-value))
      (when (or (tagged-list? exp 'define)
                (tagged-list? exp 'define-class))
        (define name
          (if (array? (second exp))
              (first (second exp))
              (second exp)))
        (send (get-field symbol-map this) set name #t)))
    (set-field! nodes
                this
                (append (get-field require-nodes this)
                        (get-field main-nodes this)
                        (get-field provide-nodes this)))
    (send
     this
     set-expressions
     (append (get-field require-expressions this)
             (get-field main-expressions this)
             (get-field provide-expressions this)))
    this)

  (define/public (make-environment (parent undefined))
    (define module-env
      (new LispEnvironment '() parent))
    (define imported)
    (define local)
    (define module)
    (define env)
    (define module-name)
    (set-field! parent-environment this parent)
    (set-field! environment this module-env)
    ;; Iterate over `require-nodes`, importing definitions
    ;; from other modules.
    (for ((node (get-field require-nodes this)))
      (define exp
        (send node get-value))
      (cond
       ((and (tagged-list? exp 'require)
             (> (array-list-length exp) 1)
             (tagged-list? (second exp) 'only-in))
        (set! module-name (second (second exp)))
        (when (symbol? module-name)
          (set! module-name
                (symbol->string module-name)))
        (set! module-name (send module-name
                                replace
                                (regexp "^\\./") ""))
        (cond
         ((and (get-field module-map this)
               (send (get-field module-map this) has module-name))
          (set! module (send (get-field module-map this) get module-name))
          (set! env (send module get-environment)))
         (else
          (set! env undefined)))
        (for ((exp1 (drop (second exp) 2)))
          (cond
           ((array? exp1)
            (set! local (first exp1))
            (set! imported (second exp1)))
           (else
            (set! local exp1)
            (set! imported exp1)))
          (send module-env set-local imported #t "variable")
          (when env
            (define-values (f f-type)
              (send env get-typed-value local))
            (unless (eq? f-type "undefined")
              (send module-env set-local imported f f-type)))))))
    ;; Iterate over `main-nodes`, evaluating definition forms
    ;; in the module environment.
    (for ((node (get-field main-nodes this)))
      (define exp
        (send node get-value))
      (cond
       ((or (definition? exp)
            (macro-definition? exp))
        ;; Evaluate `define` and `defmacro` forms in the module
        ;; environment. Be error-tolerant since the module
        ;; environment is not needed in many cases.
        (define name
          (second exp))
        (when (array? name)
          (set! name (first name)))
        (define typ
          (if (macro-definition? exp)
              "macro"
              "procedure"))
        (send module-env
              set-local
              name
              (thunk
               (lambda ()
                 (define result undefined)
                 (try
                   (set! result (eval_ exp module-env))
                   (catch Error e
                     ;; Do nothing
                     ))
                 result))
              typ))))
    module-env)

  (define/public (set-module-map module-map)
    (set-field! module-map this module-map)
    this)

  (define/public (set-nodes nodes)
    (set-field! main-nodes this nodes)
    (set-field! main-expressions
                this
                (map (lambda (x)
                       (send x get-value))
                     nodes))
    this)

  (define/public (set-expressions (expressions '()))
    (set-field! expressions this expressions))

  (define/public (set-inline-lisp-sources-flag val)
    (set-field! inline-lisp-sources-flag this val))

  (define/public (get-inline-lisp-sources-flag)
    (get-field inline-lisp-sources-flag this)))

;;; Convert a map of `module` forms to a map of `Module` objects,
;;; interlinking them in the process.
(define (make-module-map module-expression-map env)
  (define module-map
    (new ThunkedMap))
  (for ((key (send module-expression-map keys)))
    (send module-map
          set
          key
          (thunk
           (lambda ()
             (define val
               (send module-expression-map get key))
             (define m
               (if (is-a? val Module)
                   val
                   (module-expression-to-module-object
                    val env)))
             (send m set-module-map module-map)
             m))))
  module-map)

;;; Convert a `(module ...)` expression to a
;;; `Module` object.
(define (module-expression-to-module-object node env)
  (define name
    (~> node
        (send get 1)
        (send get-value)))
  (when (symbol? name)
    (set! name
          (symbol->string name)))
  (new Module
       (send node drop 3)
       env
       name))

;;; Whether `env` extends the Lisp environment.
(define (extends-lisp-environment? env)
  ;; TODO: Check `parent`.
  (or (eq? env lisp-environment)
      (and (is-a? env EnvironmentStack)
           (send env has-environment lisp-environment))))

;;; Extract the module name from a `(require ...)` expression.
(define (get-module-name name-obj)
  (define name name-obj)
  (when (symbol? name)
    (set! name (symbol->string name)))
  (set! name
        (regexp-replace (regexp "^\\./") name ""))
  name)

;;; Return the current environment.
(define (current-compilation-options)
  current-compilation-options-pointer)

;;; Run `f` with `current-compilation-options-pointer` bound to `options`.
;;; The return value is the result of invoking `f`.
(define (with-compilation-options options f)
  (let ((result undefined)
        (tmp current-compilation-options-pointer))
    (try
      (set! current-compilation-options-pointer options)
      (set! result (f))
      (finally
        (set! current-compilation-options-pointer tmp)))
    result))

;;; Whether an expression is a definition.
(define (definition? exp)
  (tagged-list? exp 'define))

;;; Whether an expression is a function definition.
(define (function-definition? exp)
  (and (definition? exp)
       (or (cons? (second exp))
           (function-expression? (third exp)))))

;;; Whether an expression is a function expression.
(define (function-expression? exp)
  (or (tagged-list? exp 'lambda)
      (tagged-list? exp 'js/function)
      (tagged-list? exp 'js/arrow)))

;;; Whether an expression is a macro definition.
(define (macro-definition? exp)
  (or (tagged-list? exp 'define-macro)
      (tagged-list? exp 'defmacro)))

;;; Lisp environment.
(define lisp-environment
  (new LispEnvironment
       `(
         ;; Constants.
         (_ ,__ "variable")
         (__ ,__ "variable")
         ;; (___ ,__ "variable")
         ;; (____ ,__ "variable")
         ;; (_____ ,__ "variable")
         ;; (______ ,__ "variable")
         ;; (_______ ,__ "variable")
         ;; (________ ,__ "variable")
         ;; (_________ ,__ "variable")
         ;; (__________ ,__ "variable")
         (,(string->symbol "#f") ,false_ "variable")
         (,(string->symbol "#t") ,true_ "variable")
         (,(string->symbol "#u") ,undefined_ "variable")
         (false ,false_ "variable")
         (,(string->symbol "nil") ,null_ "variable")
         (null ,null_ "variable")
         (js/null ,js-null_ "variable")
         (js-null ,js-null_ "variable")
         (,(string->symbol "t") ,true_ "variable")
         (true ,true_ "variable")
         (js-undefined ,undefined_ "variable")
         (js/undefined ,undefined_ "variable")
         (undefined ,undefined_ "variable")
         (,(string->symbol "*cons-dot*") ,cons-dot_ "variable")
         ;; Procedures.
         ;; (eval ,interpret "procedure")
         ;; (js/eval ,js-eval_ "procedure")
         ;; (obj ,js-obj_ "procedure")
         ;; (scm/eval ,interpret "procedure")
         ;; (seval ,eval_ "procedure")
         ($ ,funcall_ "procedure")
         (% ,modulo_ "procedure")
         (* ,mul_ "procedure")
         (+ ,add_ "procedure")
         (- ,sub_ "procedure")
         (/ ,div_ "procedure")
         (< ,lt_ "procedure")
         (<= ,lte_ "procedure")
         (= ,eq?_ "procedure")
         (=? ,eq?_ "procedure")
         (> ,gt_ "procedure")
         (>= ,gte_ "procedure")
         ;; (S ,S "procedure")
         (add ,add_ "procedure")
         (add1 ,add1_ "procedure")
         (aget ,array-ref_ "procedure")
         (append ,append_ "procedure")
         (apply ,apply_ "procedure")
         (aref ,array-ref_ "procedure")
         (array-drop ,array-drop_ "procedure")
         (array-drop-right ,array-drop-right_ "procedure")
         (array-eighth ,array-eighth_ "procedure")
         (array-fifth ,array-fifth_ "procedure")
         (array-first ,array-first_ "procedure")
         (array-fourth ,array-fourth_ "procedure")
         (array-get ,array-ref_ "procedure")
         (array-last ,array-last_ "procedure")
         (array-length ,array-length_ "procedure")
         (array-list->linked-list ,array-list->linked-list_ "procedure")
         (array-list-car ,car_ "procedure")
         (array-list-cdr ,array-list-cdr_ "procedure")
         (array-list-drop ,array-list-drop_ "procedure")
         (array-list-drop-right ,array-list-drop-right_ "procedure")
         (array-list-eighth ,array-list-eighth_ "procedure")
         (array-list-fifth ,array-list-fifth_ "procedure")
         (array-list-first ,array-list-first_ "procedure")
         (array-list-fourth ,array-list-fourth_ "procedure")
         (array-list-last ,array-list-last_ "procedure")
         (array-list-length ,array-list-length_ "procedure")
         (array-list-ninth ,array-list-ninth_ "procedure")
         (array-list-nth ,array-list-nth_ "procedure")
         (array-list-nthcdr ,array-list-nthcdr_ "procedure")
         (array-list-rest ,array-list-rest_ "procedure")
         (array-list-reverse ,array-list-reverse_ "procedure")
         (array-list-second ,array-list-second_ "procedure")
         (array-list-seventh ,array-list-seventh_ "procedure")
         (array-list-sixth ,array-list-sixth_ "procedure")
         (array-list-take ,array-list-take_ "procedure")
         (array-list-tenth ,array-list-tenth_ "procedure")
         (array-list-third ,array-list-third_ "procedure")
         (array-list? ,array-list?_ "procedure")
         (array-ninth ,array-ninth_ "procedure")
         (array-ref ,array-ref_ "procedure")
         (array-rest ,array-rest_ "procedure")
         (array-reverse ,array-reverse_ "procedure")
         (array-second ,array-second_ "procedure")
         (array-set ,array-set_ "procedure")
         (array-set! ,array-set_ "procedure")
         (array-seventh ,array-seventh_ "procedure")
         (array-sixth ,array-sixth_ "procedure")
         (array-take ,array-take_ "procedure")
         (array-tenth ,array-tenth_ "procedure")
         (array-third ,array-third_ "procedure")
         (array? ,array?_ "procedure")
         (aset ,array-set_ "procedure")
         (aset! ,array-set_ "procedure")
         (assert ,assert_ "procedure")
         (boolean? ,boolean?_ "procedure")
         (booleanp ,boolean?_ "procedure")
         (build-list ,build-list_ "procedure")
         (cadr ,cadr_ "procedure")
         (call-cc ,call-with-current-continuation_ "procedure")
         (call-with-current-continuation ,call-with-current-continuation_ "procedure")
         (call/cc ,call-with-current-continuation_ "procedure")
         (car ,car_ "procedure")
         (cdr ,cdr_ "procedure")
         (circular-list-p ,circular-list?_ "procedure")
         (circular-list? ,circular-list?_ "procedure")
         (cons ,cons_ "procedure")
         (cons* ,list-star_ "procedure")
         (cons-dot ,cons-dot-f_ "procedure")
         (cons-dot? ,cons-dot?_ "procedure")
         (cons? ,cons?_ "procedure")
         (console.log ,(get-field log console) "procedure")
         (consp ,cons?_ "procedure")
         (const ,const_ "procedure")
         (constantly ,const_ "procedure")
         (current-environment ,current-environment_ "procedure")
         (curry ,curry "procedure")
         (curry-n ,curry-n "procedure")
         (delete ,js-delete_ "procedure")
         (display ,display_ "procedure")
         (div ,div_ "procedure")
         (dotted-list->proper-list ,linked-list->array-list_ "procedure")
         (dotted-list-car ,linked-list-car_ "procedure")
         (dotted-list-cdr ,linked-list-cdr_ "procedure")
         (dotted-list-head ,linked-list-head_ "procedure")
         (dotted-list-last ,linked-list-last_ "procedure")
         (dotted-list-last-cdr ,linked-list-last-cdr_ "procedure")
         (dotted-list-length ,linked-list-length_ "procedure")
         (dotted-list-nth ,linked-list-nth_ "procedure")
         (dotted-list-nthcdr ,linked-list-nthcdr_ "procedure")
         (dotted-list-p ,dotted-list?_ "procedure")
         (dotted-list-tail ,linked-list-tail_ "procedure")
         (dotted-list? ,dotted-list?_ "procedure")
         (dotted-pair-cdr ,linked-pair-cdr_ "procedure")
         (dotted-pair-p ,dotted-pair-p_ "procedure")
         (dotted-pair? ,dotted-pair-p_ "procedure")
         (drop ,list-tail_ "procedure")
         (drop-right ,drop-right_ "procedure")
         (eighth ,eighth_ "procedure")
         (eq ,eq?_ "procedure")
         (eq? ,eq?_ "procedure")
         (eql ,eqv?_ "procedure")
         (eql? ,eqv?_ "procedure")
         (equal ,equal?_ "procedure")
         (equal? ,equal?_ "procedure")
         (eqv ,eqv?_ "procedure")
         (eqv? ,eqv?_ "procedure")
         (error ,error_ "procedure")
         (even? ,even?_ "procedure")
         (extend-environment ,extend-environment "procedure")
         (false? ,false?_ "procedure")
         (falsep ,false?_ "procedure")
         (fexpr? ,fexpr?_ "procedure")
         (fexprp ,fexpr?_ "procedure")
         (field-names ,field-names_ "procedure")
         (fifth ,fifth_ "procedure")
         (filter ,filter_ "procedure")
         (findf ,findf_ "procedure")
         (findf-index ,findf-index_ "procedure")
         (first ,first_ "procedure")
         (flatten ,flatten_ "procedure")
         (foldl ,foldl_ "procedure")
         (foldr ,foldr_ "procedure")
         (fourth ,fourth_ "procedure")
         (funcall ,funcall_ "procedure")
         (function-object? ,js-function-object?_ "procedure")
         (function-type? ,js-function-type?_ "procedure")
         (function? ,procedure?_ "procedure")
         (functionp ,procedure?_ "procedure")
         (gensym ,gensym_ "procedure")
         (gensym? ,gensym?_ "procedure")
         (get ,array-ref_ "procedure")
         (hash ,make-hash_ "procedure")
         (hash->list ,hash->list_ "procedure")
         (hash-clear ,hash-clear_ "procedure")
         (hash-clear! ,hash-clear!_ "procedure")
         (hash-copy ,hash-copy_ "procedure")
         (hash-entries ,hash-entries_ "procedure")
         (hash-has-key? ,hash-has-key?_ "procedure")
         (hash-keys ,hash-keys_ "procedure")
         (hash-ref ,hash-ref_ "procedure")
         (hash-remove ,hash-remove_ "procedure")
         (hash-remove! ,hash-remove!_ "procedure")
         (hash-set ,hash-set!_ "procedure")
         (hash-set! ,hash-set!_ "procedure")
         (hash-size ,hash-size_ "procedure")
         (hash-values ,hash-values_ "procedure")
         (hash? ,hash?_ "procedure")
         (head ,car_ "procedure")
         (id ,identity_ "procedure")
         (identity ,identity_ "procedure")
         (improper-list-p ,improper-list?_ "procedure")
         (improper-list? ,improper-list?_ "procedure")
         (in-range ,range_ "procedure")
         (index-of ,index-of_ "procedure")
         (index-where ,index-where_ "procedure")
         (instance-of ,is-a?_ "procedure")
         (instance-of? ,is-a?_ "procedure")
         (instanceof ,is-a?_ "procedure")
         (instanceof? ,is-a?_ "procedure")
         (intern ,string->symbol_ "procedure")
         (intersection ,intersection_ "procedure")
         (is-a? ,is-a?_ "procedure")
         (js ,js_ "procedure")
         (js-field ,array-ref_ "procedure")
         (js-keys ,js-keys_ "procedure")
         (js-obj ,js-obj_ "procedure")
         (js-obj-append ,js-obj-append_ "procedure")
         (js-obj-keys ,js-keys_ "procedure")
         (js-obj? ,js-obj-p_ "procedure")
         (js/+ ,js-plus_ "procedure")
         (js/== ,js-is-loosely-equal?_ "procedure")
         (js/=== ,js-is-strictly-equal?_ "procedure")
         (js/===? ,js-is-strictly-equal?_ "procedure")
         (js/==? ,js-is-loosely-equal?_ "procedure")
         (js/console.log ,(get-field log console) "procedure")
         (js/delete ,js-delete_ "procedure")
         (js/find-index ,js-find-index_ "procedure")
         (js/findf-index ,js-find-index_ "procedure")
         (js/function-object? ,js-function-object?_ "procedure")
         (js/function-type? ,js-function-type?_ "procedure")
         (js/function? ,js-function?_ "procedure")
         (js/in ,js-in_ "procedure")
         (js/instanceof ,js-instanceof_ "procedure")
         (js/instanceof? ,js-instanceof_ "procedure")
         (js/is-loosely-equal? ,js-is-loosely-equal?_ "procedure")
         (js/is-strictly-equal? ,js-is-strictly-equal?_ "procedure")
         (js/js-obj ,js-obj_ "procedure")
         (js/js-obj-append ,js-obj-append_ "procedure")
         (js/js-obj? ,js-obj-p_ "procedure")
         (js/new ,new_ "procedure")
         (js/null? ,js-null?_ "procedure")
         (js/obj ,js-obj_ "procedure")
         (js/obj-append ,js-obj-append_ "procedure")
         (js/obj? ,js-obj-p_ "procedure")
         (js/object-type? ,js-object-type?_ "procedure")
         (js/object? ,js-object-type?_ "procedure")
         (js/regexp ,regexp_ "procedure")
         (js/regexp-quote ,regexp-quote_ "procedure")
         (js/regexp? ,regexp?_ "procedure")
         (js/same-value-zero? ,js-same-value-zero?_ "procedure")
         (js/same-value? ,js-same-value?_ "procedure")
         (js/tag ,js-tagged-template_ "procedure")
         (js/tagged-template ,js-tagged-template_ "procedure")
         (js/typeof ,js-typeof_ "procedure")
         (keyword? ,keyword?_ "procedure")
         (keywordp ,keyword?_ "procedure")
         (last ,last_ "procedure")
         (last-cdr ,last-cdr_ "procedure")
         (last-cons ,last-pair_ "procedure")
         (last-pair ,last-pair_ "procedure")
         (length ,length_ "procedure")
         (length* ,length_ "procedure")
         (linked-list-car ,linked-list-car_ "procedure")
         (linked-list-cdr ,linked-list-cdr_ "procedure")
         (linked-list-eighth ,linked-list-eighth_ "procedure")
         (linked-list-fifth ,linked-list-fifth_ "procedure")
         (linked-list-first ,linked-list-first_ "procedure")
         (linked-list-fourth ,linked-list-fourth_ "procedure")
         (linked-list-head ,linked-list-head_ "procedure")
         (linked-list-last ,linked-list-last_ "procedure")
         (linked-list-last-cdr ,linked-list-last-cdr_ "procedure")
         (linked-list-length ,linked-list-length_ "procedure")
         (linked-list-link-car ,linked-list-link-car_ "procedure")
         (linked-list-link-cdr ,linked-list-link-cdr_ "procedure")
         (linked-list-link-p ,linked-list-link?_ "procedure")
         (linked-list-link? ,linked-list-link?_ "procedure")
         (linked-list-ninth ,linked-list-ninth_ "procedure")
         (linked-list-nth ,linked-list-nth_ "procedure")
         (linked-list-nthcdr ,linked-list-nthcdr_ "procedure")
         (linked-list-p ,linked-list?_ "procedure")
         (linked-list-second ,linked-list-second_ "procedure")
         (linked-list-seventh ,linked-list-seventh_ "procedure")
         (linked-list-sixth ,linked-list-sixth_ "procedure")
         (linked-list-tail ,linked-list-tail_ "procedure")
         (linked-list-tenth ,linked-list-tenth_ "procedure")
         (linked-list-third ,linked-list-third_ "procedure")
         (linked-list? ,linked-list?_ "procedure")
         (linked-pair-car ,linked-pair-car_ "procedure")
         (linked-pair-cdr ,linked-pair-cdr_ "procedure")
         (linked-pair? ,linked-pair?_ "procedure")
         (list ,list_ "procedure")
         (list* ,list-star_ "procedure")
         (list-ref ,nth_ "procedure")
         (list-set ,array-set_ "procedure")
         (list-set! ,array-set_ "procedure")
         (list-star ,list-star_ "procedure")
         (list-tail ,list-tail_ "procedure")
         (list? ,list?_ "procedure")
         (listp ,list?_ "procedure")
         (log ,(get-field log console) "procedure")
         (make ,new_ "procedure")
         (make-hash ,make-hash_ "procedure")
         (make-list ,make-list_ "procedure")
         (make-object ,new_ "procedure")
         (map ,map_ "procedure")
         (mapcar ,map_ "procedure")
         (member ,member_ "procedure")
         (member-p ,member?_ "procedure")
         (member? ,member?_ "procedure")
         (memberp ,member?_ "procedure")
         (memf ,memf_ "procedure")
         (memf? ,memf?_ "procedure")
         (memq ,memq_ "procedure")
         (memq? ,memq?_ "procedure")
         (mod ,modulo_ "procedure")
         (modulo ,modulo_ "procedure")
         (mul ,mul_ "procedure")
         (new ,new_ "procedure")
         (new* ,new_ "procedure")
         (ninth ,ninth_ "procedure")
         (not ,not_ "procedure")
         (nth ,nth_ "procedure")
         (nthcdr ,nthcdr_ "procedure")
         (null? ,null?_ "procedure")
         (nullp ,null?_ "procedure")
         (number->string ,number->string_ "procedure")
         (number? ,number?_ "procedure")
         (numberp ,number?_ "procedure")
         (object? ,js-obj-p_ "procedure")
         (objectp ,js-obj-p_ "procedure")
         (odd? ,odd?_ "procedure")
         (oget ,object-ref_ "procedure")
         (one? ,one?_ "procedure")
         (onep ,one?_ "procedure")
         (oref ,array-ref_ "procedure")
         (oset ,object-set!_ "procedure")
         (oset! ,object-set!_ "procedure")
         (plist->alist ,plist->alist_ "procedure")
         (plist-copy ,plist-copy_ "procedure")
         (plist-get ,plist-get_ "procedure")
         (plist-has ,plist-has?_ "procedure")
         (plist-has? ,plist-has?_ "procedure")
         (plist-ref ,plist-get_ "procedure")
         (plist-set ,plist-set!_ "procedure")
         (plist-set! ,plist-set!_ "procedure")
         (plist? ,plist?_ "procedure")
         (pop ,pop-left!_ "procedure")
         (pop! ,pop-left!_ "procedure")
         (pop-left ,pop-left!_ "procedure")
         (pop-left! ,pop-left!_ "procedure")
         (pop-right ,pop-right!_ "procedure")
         (pop-right! ,pop-right!_ "procedure")
         (print ,print "procedure")
         (print-estree ,print-estree "procedure")
         (procedure? ,procedure?_ "procedure")
         (proper-list->dotted-list ,array-list->linked-list_ "procedure")
         (proper-list-p ,proper-list?_ "procedure")
         (proper-list? ,proper-list?_ "procedure")
         (push ,push-left!_ "procedure")
         (push! ,push-left!_ "procedure")
         (push-left ,push-left!_ "procedure")
         (push-left! ,push-left!_ "procedure")
         (push-right ,push-right!_ "procedure")
         (push-right! ,push-right!_ "procedure")
         (range ,range_ "procedure")
         (re ,regexp_ "procedure")
         (re-pattern ,regexp_ "procedure")
         (regexp ,regexp_ "procedure")
         (regexp-match ,regexp-match_ "procedure")
         (regexp-match? ,regexp-match?_ "procedure")
         (regexp-quote ,regexp-quote_ "procedure")
         (regexp-replace ,regexp-replace_ "procedure")
         (regexp? ,regexp?_ "procedure")
         (rest ,rest_ "procedure")
         (reverse ,reverse_ "procedure")
         (rx ,regexp_ "procedure")
         (scm/new ,new_ "procedure")
         (second ,second_ "procedure")
         (self-evaluating? ,self-evaluating?_ "procedure")
         (set-car! ,set-car!_ "procedure")
         (set-cdr! ,set-cdr!_ "procedure")
         (set-mcar! ,set-car!_ "procedure")
         (set-mcdr! ,set-cdr!_ "procedure")
         (set-nth ,array-set_ "procedure")
         (set-nth! ,array-set_ "procedure")
         (seventh ,seventh_ "procedure")
         (sixth ,sixth_ "procedure")
         (source ,source "procedure")
         (string->number ,string->number_ "procedure")
         (string->symbol ,string->symbol_ "procedure")
         (string-append ,string-append_ "procedure")
         (string-downcase ,string-downcase_ "procedure")
         (string-join ,string-join_ "procedure")
         (string-length ,length_ "procedure")
         (string-object? ,string-object?_ "procedure")
         (string-primitive? ,string-primitive?_ "procedure")
         (string-ref ,string-ref_ "procedure")
         (string-repeat ,string-repeat_ "procedure")
         (string-replace ,string-replace_ "procedure")
         (string-split ,string-split_ "procedure")
         (string-to-symbol ,string->symbol_ "procedure")
         (string-trim ,string-trim_ "procedure")
         (string-upcase ,string-upcase_ "procedure")
         (string? ,string?_ "procedure")
         (stringp ,string?_ "procedure")
         (sub ,sub_ "procedure")
         (sub1 ,sub1_ "procedure")
         (substring ,substring_ "procedure")
         (symbol->string ,symbol->string_ "procedure")
         (symbol-to-string ,symbol->string_ "procedure")
         (symbol? ,symbol?_ "procedure")
         (symbolp ,symbol?_ "procedure")
         (tail ,cdr_ "procedure")
         (take ,take_ "procedure")
         (tenth ,tenth_ "procedure")
         (third ,third_ "procedure")
         (true? ,true?_ "procedure")
         (truep ,true?_ "procedure")
         (type-of ,type-of_ "procedure")
         (typeof ,type-of_ "procedure")
         (undefined? ,undefined?_ "procedure")
         (union ,union_ "procedure")
         (values ,values_ "procedure")
         (vector ,list_ "procedure")
         (vector-ref ,nth_ "procedure")
         (vector-set ,array-set_ "procedure")
         (vector-set! ,array-set_ "procedure")
         (vector? ,array?_ "procedure")
         (zero? ,zero?_ "procedure")
         (zerop ,zero?_ "procedure")
         ;; Macros.
         (,(string->symbol ".") ,dot_ "macro")
         (,(string->symbol ":") ,colon_ "macro")
         (,quasiquote-sym_ ,quasiquote_ "macro")
         (,quote-sym_ ,quote_ "macro")
         (-> ,thread-first_ "macro")
         (->> ,thread-last_ "macro")
         (and ,and_ "macro")
         (ann ,ann_ "macro")
         (as-> ,thread-as_ "macro")
         (async ,js-async_ "macro")
         (as~> ,thread-as_ "macro")
         (await ,js-await_ "macro")
         (begin ,begin_ "macro")
         (begin0 ,begin0_ "macro")
         (block ,block_ "macro")
         (break ,break_ "macro")
         (call-method ,send_ "macro")
         (case ,case_ "macro")
         (case/eq ,case-eq_ "macro")
         (class ,class_ "macro")
         (clj/try ,clj-try_ "macro")
         (cond ,cond_ "macro")
         (continue ,continue_ "macro")
         (defclass ,defclass_ "macro")
         (define ,define_ "macro")
         (define-class ,define-class_ "macro")
         (define-js-obj ,define-js-obj_ "macro")
         (define-macro ,define-macro_ "macro")
         (define-type ,define-type_ "macro")
         (define-values ,define-values_ "macro")
         (define/async ,define-async_ "macro")
         (define/generator ,define-generator_ "macro")
         (define/private ,define-private_ "macro")
         (define/public ,define-public_ "macro")
         (defmacro ,defmacro_ "macro")
         (defun ,defun_ "macro")
         (destructuring-bind ,multiple-value-bind_ "macro")
         (do ,do_ "macro")
         (field-bound? ,field-bound?_ "macro")
         (fn ,lambda_ "macro")
         (for ,for_ "macro")
         (fset ,set_ "macro")
         (get-field ,get-field_ "macro")
         (if ,if_ "macro")
         (js/?. ,js-optional-chaining_ "macro")
         (js/arrow ,js-arrow_ "macro")
         (js/async ,js-async_ "macro")
         (js/await ,js-await_ "macro")
         (js/do-while ,js-do-while_ "macro")
         (js/for ,js/for_ "macro")
         (js/for-in ,js/for-in_ "macro")
         (js/for-of ,js/for-of_ "macro")
         (js/function ,js-function_ "macro")
         (js/switch ,js-switch_ "macro")
         (js/try ,js-try_ "macro")
         (js/while ,js-while_ "macro")
         (lambda ,lambda_ "macro")
         (let ,let-star_ "macro")
         (let* ,let-star_ "macro")
         (let*-values ,let-values_ "macro")
         (let-env ,let-env_ "macro")
         (let-js-obj ,let-js-obj_ "macro")
         (let-values ,let-values_ "macro")
         (letrec ,let-star_ "macro")
         (letrec-values ,let-values_ "macro")
         (module ,module_ "macro")
         (multiple-value-bind ,multiple-value-bind_ "macro")
         (multiple-values-bind ,multiple-value-bind_ "macro")
         (new/apply ,new-apply_ "macro")
         (or ,or_ "macro")
         (prog1 ,begin0_ "macro")
         (progn ,begin_ "macro")
         (provide ,provide_ "macro")
         (require ,require_ "macro")
         (return ,return_ "macro")
         (rkt/new ,rkt-new_ "macro")
         (send ,send_ "macro")
         (send/apply ,send-apply_ "macro")
         (set ,set_ "macro")
         (set! ,set!_ "macro")
         (set!-js-obj ,set-js-obj_ "macro")
         (set!-values ,set-values_ "macro")
         (set-field! ,set-field_ "macro")
         (setq ,set!_ "macro")
         (throw ,throw_ "macro")
         (try ,clj-try_ "macro")
         (unless ,unless_ "macro")
         (unwind-protect ,unwind-protect_ "macro")
         (when ,when_ "macro")
         (while ,while_ "macro")
         (yield ,yield_ "macro")
         (~> ,thread-first_ "macro")
         (~>> ,thread-last_ "macro")
         (λ ,lambda_ "macro")
         ;; (define/public ,define_ "macro")
         ;; (new ,rkt-new_ "macro")
         ;; (set!-field ,set-field_ "macro")
         ;; (set-js-obj! ,set-js-obj_ "macro")
         ;; (set-values! ,set-values_ "macro")
         ;; Special forms, expressed as macros.
         )))

;;; Evaluation environment.
(define eval-environment
  (new LispEnvironment
       `((eval ,interpret "procedure")
         (js/eval ,js-eval_ "procedure")
         (scm/eval ,interpret "procedure")
         (seval ,eval_ "procedure"))))

;;; JavaScript environment.
(define js-environment
  (new JavaScriptEnvironment))

;;; Interpretation environment.
;;; Includes `eval`.
(define interpretation-environment
  (new EnvironmentStack
       lisp-environment
       eval-environment
       js-environment))

;;; Interpretation environment.
;;; No `eval`.
(define interpretation-environment-no-eval
  (new EnvironmentStack
       lisp-environment
       js-environment))

;;; Compilation environment.
(define compilation-environment
  (new EnvironmentStack
       lisp-environment
       eval-environment))

;;; Language environment.
(define lang-environment
  interpretation-environment)

;;; Default options used when compiling.
(define default-compilation-options
  (js-obj "lispEnvironment"
          lang-environment
          "compilationMappingEnvironment"
          compilation-mapping-env
          "inlineFunctions"
          #t
          "gensymMap"
          (make-hash)))

;;; Pointer to the current compilation options.
(define current-compilation-options-pointer
  default-compilation-options)

(provide
  (all-from-out "./array")
  (all-from-out "./constants")
  (all-from-out "./curry")
  (all-from-out "./env")
  (all-from-out "./equal")
  (all-from-out "./eval")
  (all-from-out "./hash")
  (all-from-out "./javascript")
  (all-from-out "./list")
  (all-from-out "./macros")
  (all-from-out "./object")
  (all-from-out "./plist")
  (all-from-out "./printer")
  (all-from-out "./procedures")
  (all-from-out "./regexp")
  (all-from-out "./rose")
  (all-from-out "./string")
  (all-from-out "./symbol")
  (rename-out (and_ and))
  (rename-out (ann_ ann))
  (rename-out (begin_ begin))
  (rename-out (block_ block))
  (rename-out (call-with-current-continuation_ call-with-current-continuation))
  (rename-out (call-with-current-continuation_ call/cc))
  (rename-out (clj-try_ try))
  (rename-out (clj-try_ try_))
  (rename-out (colon_ colon))
  (rename-out (compile compile-lisp))
  (rename-out (compile compile-lisp-to-javascript))
  (rename-out (cond_ cond))
  (rename-out (define-async_ define-async))
  (rename-out (define-async_ define/async))
  (rename-out (define-class_ define-class))
  (rename-out (define-generator_ define-generator))
  (rename-out (define-generator_ define/generator))
  (rename-out (define-js-obj_ define-js-obj))
  (rename-out (define-macro_ define-macro))
  (rename-out (define-public_ define-public))
  (rename-out (define-public_ define/public))
  (rename-out (define-type_ define-type))
  (rename-out (define-values_ define-values))
  (rename-out (define_ define))
  (rename-out (dot_ dot))
  (rename-out (get-field_ get-field))
  (rename-out (js-async_ async))
  (rename-out (js-async_ async_))
  (rename-out (js-async_ js-async))
  (rename-out (js-await_ await))
  (rename-out (js-await_ await_))
  (rename-out (js-await_ js-await))
  (rename-out (js_ js))
  (rename-out (lambda_ compile-function))
  (rename-out (lambda_ fn))
  (rename-out (lambda_ lambda))
  (rename-out (let-js-obj_ let-js-obj))
  (rename-out (let-star_ let*))
  (rename-out (let-star_ let-star))
  (rename-out (let-star_ let_))
  (rename-out (let-star_ letrec))
  (rename-out (let-values_ let*-values))
  (rename-out (let-values_ let-values))
  (rename-out (let-values_ letrec-values))
  (rename-out (lisp-environment lisp-1-environment))
  (rename-out (new_ js/new))
  (rename-out (new_ make))
  (rename-out (new_ make-object))
  (rename-out (new_ make-object_))
  (rename-out (new_ new*))
  (rename-out (new_ rkt/make-object))
  (rename-out (new_ scm/new))
  (rename-out (nop_ nop))
  (rename-out (or_ or))
  (rename-out (provide_ provide))
  (rename-out (quasiquote_ quasiquote))
  (rename-out (quote_ quote))
  (rename-out (require_ require))
  (rename-out (send-apply_ send/apply))
  (rename-out (send_ call-method))
  (rename-out (send_ send))
  (rename-out (set!_ set!))
  (rename-out (set!_ setq))
  (rename-out (set!_ setq_))
  (rename-out (set-field_ set-field!))
  (rename-out (set-field_ set-field))
  (rename-out (set-js-obj_ set!-js-obj))
  (rename-out (set-js-obj_ set-js-obj!))
  (rename-out (set-js-obj_ set-js-obj))
  (rename-out (set-values_ set!-values))
  (rename-out (set-values_ set-values))
  (rename-out (sexp read-from-string))
  ;; (rename-out (macroexpand-1 macroexpand1))
  Module
  and_
  ann_
  apply-optimizations
  begin_
  break_
  class_
  clj-try_
  colon_
  compilation-environment
  compile
  compile-file!
  compile-files!
  compile-module-map
  compile-modules
  cond_
  continue_
  define->define-class
  define-async_
  define-generator_
  define-js-obj_
  define-macro->function
  define-macro->lambda-form
  define-macro_
  define-type_
  define-values_
  define_
  definition->macro
  dot_
  find-estree
  for_
  get-field_
  interpret
  interpret-files
  interpret-string
  interpretation-environment
  is-a?_
  iterate-rose
  js-async_
  js-await_
  js_
  lambda_
  lang-environment
  let-js-obj_
  let-star_
  let-values_
  let-vars-to-const-vars
  lisp
  lisp-environment
  macroexpand
  macroexpand-1
  macroexpand-all
  macroexpand-all-until
  macroexpand-until
  make-lisp
  make-module-map
  map-rose
  map-sexp
  map-visit-rose
  module-expression-to-module-object
  module_
  new_
  nop_
  optimizations
  optimize-estree
  optimize-module
  optimize-rose
  optimize-sexp
  or_
  provide_
  quasiquote_
  quote?
  quote_
  read
  read-rose
  read-sexp
  require_
  return_
  s
  send-apply_
  send-method
  send_
  set!_
  set-field_
  set-js-obj_
  set-values_
  sexp
  source
  source?
  split-comments
  throw_
  tokenize
  traverse-estree
  type-of_
  yield_)
