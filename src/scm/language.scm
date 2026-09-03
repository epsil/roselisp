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
                  array-copy_
                  array-drop-right_
                  array-drop_
                  array-eighth_
                  array-fifth_
                  array-first_
                  array-fourth_
                  array-last_
                  array-length_
                  array-ninth_
                  array-pop-left!_
                  array-pop-right!_
                  array-push-left!_
                  array-push-right!_
                  array-ref_
                  array-at_
                  array-rest_
                  array-reverse!_
                  array-concat_
                  array-reverse_
                  array-second_
                  array-nlast_
                  array-set!_
                  array-set_
                  array-seventh_
                  array-sixth_
                  array-slice_
                  array-take_
                  array-tenth_
                  array-third_
                  array?_))
(require (only-in "./constants"
                  default-language
                  false_
                  js/null_
                  license
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
(require (only-in "./decompiler"
                  (decompile decompile1)))
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
                  ForInStatement
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
                  estree-type
                  estree-type?
                  estree?))
(require (only-in "./eval"
                  call-evaluator
                  default-evaluator
                  eval_
                  eval-estree))
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
                  (js/new_ new_)
                  js/abs_
                  js/and_
                  js/array?_
                  js/arrow?_
                  js/bitwise-and_
                  js/bitwise-not_
                  js/bitwise-or_
                  js/bitwise-shift-left_
                  js/bitwise-shift-right_
                  js/bitwise-xor_
                  js/delete_
                  js/dot_
                  js/eval_
                  js/find-index_
                  js/function-object?_
                  js/function-type?_
                  js/function?_
                  js/get_
                  js/gt_
                  js/gte_
                  js/in_
                  js/instanceof_
                  js/keys_
                  js/length_
                  js/loosely-equal?_
                  js/lt_
                  js/lte_
                  js/mod_
                  js/nan?_
                  js/new_
                  js/not_
                  js/null?_
                  js/obj-append_
                  js/obj-spread_
                  js/obj?_
                  js/obj_
                  js/object-type?_
                  js/optional-chaining_
                  js/or_
                  js/plus_
                  js/promise?_
                  js/promise_
                  js/reduce-right_
                  js/reduce_
                  js/regexp-match_
                  js/regexp-replace_
                  js/regexp?_
                  js/regexp_
                  js/return_
                  js/same-value-zero?_
                  js/same-value?_
                  js/slice_
                  js/source_
                  js/strictly-equal?_
                  js/string-concat_
                  js/string-literal?_
                  js/string-object?_
                  js/string?_
                  js/tagged-template_
                  js/to-string_
                  js/typeof_
                  js/unsigned-bitwise-shift-right_
                  js/yield_))
(require (only-in "./list"
                  append_
                  build-list_
                  cadr_
                  car_
                  cdr_
                  circular-list?_
                  cons_
                  dotted-improper-list?_
                  dotted-list->list_
                  dotted-list-eighth_
                  dotted-list-fifth_
                  dotted-list-first_
                  dotted-list-fourth_
                  dotted-list-head_
                  dotted-list-last-cdr_
                  dotted-list-last_
                  dotted-list-length_
                  dotted-list-ninth_
                  dotted-list-nth_
                  dotted-list-nthcdr_
                  dotted-list-parse_
                  dotted-list-ref_
                  dotted-list-second_
                  dotted-list-set!_
                  dotted-list-set_
                  dotted-list-seventh_
                  dotted-list-sixth_
                  dotted-list-tail_
                  dotted-list-tenth_
                  dotted-list-link_
                  dotted-list-third_
                  dotted-list?_
                  dotted-pair-cdr_
                  dotted-pair?_
                  dotted-proper-list?_
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
                  list->dotted-list_
                  list-ref_
                  list-set!_
                  list-set_
                  list-star_
                  list-tail_
                  list?_
                  list_
                  make-list_
                  ninth_
                  nth_
                  nthcdr_
                  null?_
                  pair-or-list?_
                  pair?_
                  pop-left!_
                  pop-right!_
                  proper-list?_
                  push-left!_
                  push-right!_
                  rest_
                  reverse!_
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
                  while_
                  with-gensyms_))
(require (only-in "./object"
                  field-names_
                  object-ref_
                  object-set!_))
(require (only-in "./parser"
                  LeadingCommentToken
                  TrailingCommentToken
                  get-comment-level
                  read
                  read-syntax
                  read-sexp
                  tokenize))
(require (only-in "./plist"
                  plist->alist_
                  plist->object_
                  plist-copy_
                  plist-get_
                  plist-has?_
                  plist-map_
                  plist-set!_
                  plist?_))
(require (only-in "./printer"
                  print
                  print-estree
                  print-sexp
                  print-sexp-as-expression
                  write-to-string))
(require (only-in "./procedures"
                  abs_
                  add1_
                  add_
                  apply_
                  arity_
                  assert_
                  atom?_
                  boolean?_
                  compiler-type?
                  const_
                  display_
                  div_
                  error_
                  even?_
                  false?_
                  fexpr-type?
                  fexpr?_
                  filter_
                  findf-index_
                  findf_
                  foldl_
                  foldr_
                  for-each_
                  funcall_
                  gt_
                  gte_
                  identity_
                  index-of_
                  index-where_
                  intersection_
                  is-a?_
                  keyword->symbol_
                  keyword?_
                  lt_
                  lte_
                  macro-type?
                  macro?_
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
                  procedure-type?
                  procedure?_
                  range_
                  self-evaluating?_
                  special-type?
                  sub1_
                  sub_
                  syntax-transformer-type?_
                  syntax-transformer?_
                  true?_
                  type-of_
                  undefined-type?
                  undefined?_
                  union_
                  values_
                  variable-type?
                  zero?_))
(require (only-in "./regexp"
                  regexp-match?_
                  regexp-match_
                  regexp-quote_
                  regexp-replace_
                  regexp?_
                  regexp_))
(require (only-in "./rose"
                  Syntax
                  begin-wrap-rose
                  begin-wrap-rose-smart
                  begin-wrap-rose-smart-1
                  datum->syntax
                  slice-rose
                  syntax->datum
                  syntax->list
                  syntax-e
                  syntax?
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
                  string-length_
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
                  InternalPromise
                  PromiseMap
                  delay_
                  force_
                  lazy_
                  promise-forced?_
                  promise-running?_
                  promise?_
                  thunk?_
                  thunk_))
(require (only-in "./util"
                  begin-wrap
                  colon-form?
                  form?
                  lambda->let
                  list-expression->pattern
                  make-identifier-string
                  map-tree
                  number->letter
                  quote?
                  tagged-list?
                  text-of-quotation
                  valid-js-casing-style?))
(require (only-in "./visitor"
                  make-visitor
                  visit))

;;; Default options for interpretation and compilation.
;;; See also `default-compilation-options`.
(define default-options
  (js/obj :comments #t
          :compile-environment #t
          :expression-type "expression"
          :feval-bindings #f
          :finline-functions #f
          :fsemicolon #t
          :gensym-map (make-hash)
          :should-import #t))

;;; Add `default-options` to an options object.
;;; If `modify` is `#t`, the original object
;;; is modified, otherwise a new object is returned.
(define (add-default-options options (modify #f))
  (define result
    (if modify
        options
        (js/obj-append options)))
  (for ((key (js/keys default-options)))
    (when (undefined? (oget result key))
      (oset! result key (oget default-options key))))
  result)

;;; Inlined functions.
;;;
;;; A list of functions whose definition is so simple
;;; that it might be inlined directly into the call site.
(define inlined-functions
  (list
   abs_
   add1_
   arity_
   array-at_
   array-copy_
   array-eighth_
   array-fifth_
   array-first_
   array-fourth_
   array-last_
   array-length_
   array-ninth_
   array-nlast_
   array-pop-left!_
   array-pop-right!_
   array-rest_
   array-reverse!_
   array-reverse_
   array-second_
   array-seventh_
   array-sixth_
   array-slice_
   array-take_
   array-tenth_
   array-third_
   array?_
   atom?_
   boolean?_
   const_
   dotted-list-head_
   dotted-list-tail_
   dotted-list?_
   dotted-pair?_
   eq?_
   eqv?_
   error_
   even?_
   false?_
   field-names_
   filter_
   findf-index_
   for-each_
   force_
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
   js/abs_
   js/and_
   js/array?_
   js/bitwise-and_
   js/bitwise-not_
   js/bitwise-or_
   js/bitwise-shift-left_
   js/bitwise-shift-right_
   js/bitwise-xor_
   js/find-index_
   js/function-object?_
   js/function-type?_
   js/function?_
   js/in_
   js/instanceof_
   js/keys_
   js/length_
   js/nan?_
   js/null?_
   js/obj?_
   js/object-type?_
   js/or_
   js/reduce-right_
   js/reduce_
   js/regexp-match_
   js/regexp-replace_
   js/regexp?_
   js/same-value?_
   js/slice_
   js/source_
   js/string-literal?_
   js/string-object?_
   js/string?_
   js/to-string_
   js/typeof_
   js/unsigned-bitwise-shift-right_
   memf?_
   memq?_
   null?_
   number->string_
   number?_
   object-ref_
   odd?_
   one?_
   pair?_
   plist-copy_
   plist?_
   pop-left!_
   pop-right!_
   procedure?_
   promise-forced?_
   promise-running?_
   promise?_
   push-left!_
   push-right!_
   regexp-match?_
   regexp-match_
   regexp-quote_
   regexp-replace_
   regexp?_
   reverse!_
   string->number_
   string->symbol_
   string-downcase_
   string-join_
   string-length_
   string-ref_
   string-repeat_
   string-split_
   string-upcase_
   sub1_
   symbol->string_
   symbol?_
   thunk?_
   true?_
   type-of_
   undefined?_
   zero?_))

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
       `((,(string->symbol "#f") ,(new Literal #f) Any)
         (,(string->symbol "#t") ,(new Literal #t) Any)
         (,(string->symbol "#n") ,(new Literal #n) Any)
         (,(string->symbol "#u") ,(new Identifier "undefined") Any)
         (,(string->symbol "js-null") ,(new Literal #n) Any)
         (,(string->symbol "js-undefined") ,(new Identifier "undefined") Any)
         (,(string->symbol "js/arguments") ,(new Identifier "arguments") Any)
         (,(string->symbol "js/null") ,(new Literal #n) Any)
         (,(string->symbol "js/require") ,(new Identifier "require") Any)
         (,(string->symbol "js/undefined") ,(new Identifier "undefined") Any)
         (,(string->symbol "nil") ,(new ArrayExpression) Any)
         (,(string->symbol "null") ,(new ArrayExpression) Any)
         (,(string->symbol "t") ,(new Literal #t) Any)
         (,(string->symbol "undefined") ,(new Identifier "undefined") Any))))

;;; Compiler procedures mapping environment.
(define compilation-compiler-mapping-env
  (new CompilationEnvironment
       `((,add_ ,compile-add (compiler-> Any * Any))
         (,ann_ ,compile-ann (compiler-> Any * Any))
         (,append_ ,compile-append (compiler-> Any * Any))
         (,apply_ ,compile-apply (compiler-> Any * Any))
         (,begin_ ,compile-begin (compiler-> Any * Any))
         (,break_ ,compile-break (compiler-> Any * Any))
         (,class_ ,compile-class (compiler-> Any * Any))
         (,colon_ ,compile-colon (compiler-> Any * Any))
         (,continue_ ,compile-continue (compiler-> Any * Any))
         (,declare_ ,compile-declare (compiler-> Any * Any))
         (,define-async_ ,compile-define-async (compiler-> Any * Any))
         (,define-class_ ,compile-define-class (compiler-> Any * Any))
         (,define-fields_ ,compile-define-fields (compiler-> Any * Any))
         (,define-generator_ ,compile-define-generator (compiler-> Any * Any))
         (,define-type_ ,compile-define-type (compiler-> Any * Any))
         (,define-values_ ,compile-define-values (compiler-> Any * Any))
         (,define_ ,compile-define (compiler-> Any * Any))
         (,div_ ,compile-div (compiler-> Any * Any))
         (,dot_ ,compile-send (compiler-> Any * Any))
         (,funcall_ ,compile-funcall (compiler-> Any * Any))
         (,gt_ ,compile-greater-than (compiler-> Any * Any))
         (,gte_ ,compile-greater-than-or-equal (compiler-> Any * Any))
         (,if_ ,compile-if (compiler-> Any * Any))
         (,js/arrow_ ,compile-js/arrow (compiler-> Any * Any))
         (,js/assignment_ ,compile-js/assignment (compiler-> Any * Any))
         (,js/async_ ,compile-js/async (compiler-> Any * Any))
         (,js/await_ ,compile-js/await (compiler-> Any * Any))
         (,js/block_ ,compile-js/block (compiler-> Any * Any))
         (,js/const_ ,compile-js/const (compiler-> Any * Any))
         (,js/delete_ ,compile-js/delete (compiler-> Any * Any))
         (,js/do-while_ ,compile-js/do-while (compiler-> Any * Any))
         (,js/dot_ ,compile-js/dot (compiler-> Any * Any))
         (,js/eval_ ,compile-js/eval (compiler-> Any * Any))
         (,js/for-in_ ,compile-js/for-in (compiler-> Any * Any))
         (,js/for-of_ ,compile-js/for-of (compiler-> Any * Any))
         (,js/for_ ,compile-js/for (compiler-> Any * Any))
         (,js/function_ ,compile-js/function (compiler-> Any * Any))
         (,js/get_ ,compile-js/get (compiler-> Any * Any))
         (,js/gt_ ,compile-greater-than (compiler-> Any * Any))
         (,js/gte_ ,compile-greater-than-or-equal (compiler-> Any * Any))
         (,js/if_ ,compile-js/if (compiler-> Any * Any))
         (,js/let_ ,compile-js/let (compiler-> Any * Any))
         (,js/loosely-equal?_ ,compile-js/loosely-equal (compiler-> Any * Any))
         (,js/lt_ ,compile-less-than (compiler-> Any * Any))
         (,js/lte_ ,compile-less-than-or-equal (compiler-> Any * Any))
         (,js/mod_ ,compile-modulo (compiler-> Any * Any))
         (,js/new_ ,compile-js/new (compiler-> Any * Any))
         (,js/not_ ,compile-not (compiler-> Any * Any))
         (,js/obj-append_ ,compile-js/obj-append (compiler-> Any * Any))
         (,js/obj-spread_ ,compile-js/obj-spread (compiler-> Any * Any))
         (,js/obj_ ,compile-js/obj (compiler-> Any * Any))
         (,js/op_ ,compile-js/op (compiler-> Any * Any))
         (,js/optional-chaining_ ,compile-js/optional-chaining (compiler-> Any * Any))
         (,js/plus_ ,compile-add (compiler-> Any * Any))
         (,js/raw_ ,compile-js/raw (compiler-> Any * Any))
         (,js/return_ ,compile-return (compiler-> Any * Any))
         (,js/sequence_ ,compile-js/sequence (compiler-> Any * Any))
         (,js/statement-or-expression_ ,compile-js/statement-or-expression (compiler-> Any * Any))
         (,js/strictly-equal?_ ,compile-js/strictly-equal (compiler-> Any * Any))
         (,js/switch_ ,compile-js/switch (compiler-> Any * Any))
         (,js/tagged-template_ ,compile-js/tagged-template (compiler-> Any * Any))
         (,js/ternary-operator_ ,compile-js/ternary-operator (compiler-> Any * Any))
         (,js/try_ ,compile-js/try (compiler-> Any * Any))
         (,js/var_ ,compile-js/var (compiler-> Any * Any))
         (,js/while_ ,compile-js/while (compiler-> Any * Any))
         (,js/yield_ ,compile-yield (compiler-> Any * Any))
         (,lambda_ ,compile-lambda (compiler-> Any * Any))
         (,let-fields_ ,compile-let-fields (compiler-> Any * Any))
         (,let-star_ ,compile-let (compiler-> Any * Any))
         (,let-values_ ,compile-let-values (compiler-> Any * Any))
         (,list_ ,compile-list (compiler-> Any * Any))
         (,lt_ ,compile-less-than (compiler-> Any * Any))
         (,lte_ ,compile-less-than-or-equal (compiler-> Any * Any))
         (,module_ ,compile-module (compiler-> Any * Any))
         (,modulo_ ,compile-modulo (compiler-> Any * Any))
         (,mul_ ,compile-mul (compiler-> Any * Any))
         (,not_ ,compile-not (compiler-> Any * Any))
         (,provide_ ,compile-provide (compiler-> Any * Any))
         (,quasiquote_ ,compile-quasiquote (compiler-> Any * Any))
         (,quote_ ,compile-quote (compiler-> Any * Any))
         (,require_ ,compile-require (compiler-> Any * Any))
         (,return_ ,compile-return (compiler-> Any * Any))
         (,send/apply_ ,compile-send/apply (compiler-> Any * Any))
         (,send_ ,compile-send (compiler-> Any * Any))
         (,set!_ ,compile-set (compiler-> Any * Any))
         (,set-field_ ,compile-set-field (compiler-> Any * Any))
         (,set-fields_ ,compile-set-fields (compiler-> Any * Any))
         (,set-values_ ,compile-set-values (compiler-> Any * Any))
         (,sub_ ,compile-sub (compiler-> Any * Any))
         (,throw_ ,compile-throw (compiler-> Any * Any))
         (,yield_ ,compile-yield (compiler-> Any * Any)))))

;;; Compiler macros mapping environment.
(define compilation-macro-mapping-env
  (new CompilationEnvironment
       `((,array-concat_ ,compile-array-concat-macro (macro-> Any * Any))
         (,array-drop-right_ ,compile-array-drop-right-macro (macro-> Any * Any))
         (,array-drop_ ,compile-array-drop-macro (macro-> Any * Any))
         (,array-push-left!_ ,compile-array-push-left!-macro (macro-> Any * Any))
         (,array-push-right!_ ,compile-array-push-right!-macro (macro-> Any * Any))
         (,array-ref_ ,compile-array-ref-macro (macro-> Any * Any))
         (,array-set!_ ,compile-array-set!-macro (macro-> Any * Any))
         (,assert_ ,compile-assert-macro (macro-> Any * Any))
         (,cdr_ ,compile-cdr-macro (macro-> Any * Any))
         (,cons_ ,compile-cons-macro (macro-> Any * Any))
         (,display_ ,compile-display-macro (macro-> Any * Any))
         (,dotted-list-link_ ,compile-dotted-list-link-macro (macro-> Any * Any))
         (,drop-right_ ,compile-drop-right-macro (macro-> Any * Any))
         (,drop_ ,compile-drop-macro (macro-> Any * Any))
         (,eighth_ ,compile-eighth-macro (macro-> Any * Any))
         (,fifth_ ,compile-fifth-macro (macro-> Any * Any))
         (,first_ ,compile-first-macro (macro-> Any * Any))
         (,foldl_ ,compile-foldl-macro (macro-> Any * Any))
         (,foldr_ ,compile-foldr-macro (macro-> Any * Any))
         (,fourth_ ,compile-fourth-macro (macro-> Any * Any))
         (,hash-clear_ ,compile-hash-clear-macro (macro-> Any * Any))
         (,hash-ref_ ,compile-hash-ref-macro (macro-> Any * Any))
         (,hash-remove!_ ,compile-hash-remove-macro (macro-> Any * Any))
         (,hash-remove_ ,compile-hash-remove-macro (macro-> Any * Any))
         (,js/regexp_ ,compile-js/regexp-macro (macro-> Any * Any))
         (,js/string-concat_ ,compile-js/string-concat-macro (macro-> Any * Any))
         (,last_ ,compile-last-macro (macro-> Any * Any))
         (,length_ ,compile-length-macro (macro-> Any * Any))
         (,list-ref_ ,compile-list-ref-macro (macro-> Any * Any))
         (,list-set!_ ,compile-list-set!-macro (macro-> Any * Any))
         (,list?_ ,compile-list?-macro (macro-> Any * Any))
         (,make-hash_ ,compile-make-hash-macro (macro-> Any * Any))
         (,map_ ,compile-map-macro (macro-> Any * Any))
         (,member?_ ,compile-member-p-macro (macro-> Any * Any))
         (,ninth_ ,compile-ninth-macro (macro-> Any * Any))
         (,nth_ ,compile-nth-macro (macro-> Any * Any))
         (,object-set!_ ,compile-object-set!-macro (macro-> Any * Any))
         (,pair-or-list?_ ,compile-pair-or-list-macro (macro-> Any * Any))
         (,print ,compile-display-macro (macro-> Any * Any))
         (,rest_ ,compile-rest-macro (macro-> Any * Any))
         (,reverse_ ,compile-reverse-macro (macro-> Any * Any))
         (,second_ ,compile-second-macro (macro-> Any * Any))
         (,seventh_ ,compile-seventh-macro (macro-> Any * Any))
         (,sixth_ ,compile-sixth-macro (macro-> Any * Any))
         (,string-append_ ,compile-string-append-macro (macro-> Any * Any))
         (,string-trim_ ,compile-string-trim-macro (macro-> Any * Any))
         (,string?_ ,compile-string?-macro (macro-> Any * Any))
         (,substring_ ,compile-substring-macro (macro-> Any * Any))
         (,take_ ,compile-take-macro (macro-> Any * Any))
         (,tenth_ ,compile-tenth-macro (macro-> Any * Any))
         (,third_ ,compile-third-macro (macro-> Any * Any))
         (,values_ ,compile-values-macro (macro-> Any * Any)))))

;;; Compilation mapping environment.
;;;
;;; An environment mapping Lisp functions to compiler procedures
;;; or compiler macros.
(define compilation-mapping-env
  (new EnvironmentStack
       compilation-macro-mapping-env
       compilation-compiler-mapping-env))

;;; Compilation map.
;;;
;;; Map from languages to compilation mapping environments.
(define compilation-map
  ;; TODO: Remove.
  (make-hash
   `(("javascript" . ,compilation-mapping-env)
     ("typescript" . ,compilation-mapping-env))))

;;; Compile a Lisp expression to JavaScript or TypeScript.
;;; Returns a string of JavaScript or TypeScript code.
;;;
;;; `exp` may be an S-expression, an S-expression wrapped
;;; in a rose tree, or a module object.
;;; `args` may be a property list or, if called with
;;; two arguments, a JavaScript object.
(define (compile exp . args)
  (define options
    (normalize-options args))
  (define from-language
    (or (oget options :from)
        "roselisp"))
  (define to-language
    (or (oget options :to)
        default-language))
  (cond
   ((eq? to-language "roselisp")
    (define inherited-options
      (js/obj-append
       (js/obj :language from-language
               :sexp #t)
       options))
    (decompile1 exp inherited-options))
   (else
    (define expression-type
      (or (oget options :as)
          "statement"))
    (define case-option
      (or (oget options :case)
          "camelcase"))
    (define inherited-options
      (js/obj-append
       (js/obj :case case-option
               :language to-language
               :expression-type expression-type)
       options))
    (define env
      (or (oget options :environment)
          (new LispEnvironment)))
    (compile-with-environment
     exp env inherited-options))))

;;; Decompile a JavaScript or TypeScript string to
;;; a Lisp expression. The inverse of `compile`.
(define (decompile exp . args)
  ;; This function is little more than a wrapper
  ;; around `compile` that defaults to Roselisp
  ;; as the target language.
  (define options
    (normalize-options args))
  (define from-language
    (or (oget options :from)
        default-language))
  (define to-language
    (or (oget options :to)
        "roselisp"))
  (define inherited-options
    (js/obj-append
     options
     (js/obj :from from-language
             :to to-language)))
  (compile exp inherited-options))

;;; Compile a Lisp expression to JavaScript or TypeScript
;;; in the context of a given environment, `env`.
;;; Returns a string of JavaScript or TypeScript code.
(define (compile-with-environment exp
                                  (env (new LispEnvironment))
                                  (options (js/obj)))
  (define language-option
    (or (oget options :language)
        default-language))
  (define estree-option
    (oget options :estree))
  (define optimize-option
    (oget options :optimize))
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
  (define continuation-env
    (new LispEnvironment
         '()
         lang-env))
  (oset! compilation-options :language-environment lang-env)
  (oset! compilation-options
         :compilation-mapping-environment
         mapping-env)
  (oset! compilation-options :compiled-environment compiled-env)
  (set! compilation-options
        (js/obj-append
         default-compilation-options
         compilation-options))
  (with-compilation-options
   compilation-options
   (lambda ()
     (define ast
       (cond
        ((is-a? exp Module)
         (compile-module exp continuation-env compilation-options))
        ((syntax? exp)
         (compile-syntax exp continuation-env compilation-options))
        (else
         (compile-sexp exp continuation-env compilation-options))))
     (when optimize-option
       (set! ast (optimize-estree ast)))
     (if estree-option
         ast
         (print-estree ast compilation-options)))))

;;; Compile a set of modules together.
;;; The modules may reference one another.
(define (compile-modules modules env (options (js/obj)))
  (define module-map
    (make-hash))
  (define compiled-module-map)
  (define module-name)
  (for ((module modules))
    (unless (syntax? module)
      (set! module (datum->syntax #f module)))
    (set! module-name
          (~> (send module get 1)
              (syntax->datum _)))
    (when (symbol? module-name)
      (set! module-name
            (symbol->string module-name)))
    (set! module-name
          (regexp-replace (regexp "^\\./") module-name ""))
    (hash-set! module-map module-name module))
  (set! compiled-module-map
        (compile-module-map module-map env options))
  (append (send compiled-module-map values)))

;;; Compile a module map.
;;; Returns a new map containing compiled modules.
(define (compile-module-map module-map env (options (js/obj)))
  (define result
    (make-hash))
  (define module-object-map
    (make-module-map module-map env))
  (define compiled-module)
  (define module)
  (for ((key (send module-object-map keys)))
    (set! module (send module-object-map get key))
    (set! compiled-module
          (compile-with-environment module env options))
    (hash-set! result key compiled-module))
  result)

;;; Compile a module expression or object.
(define (compile-module obj env (options (js/obj)))
  (cond
   ((is-a? obj Module)
    (compile-module-object obj env options))
   (else
    (compile-module-expression obj env options))))

;;; Compile a `(module ...)` expression.
(define (compile-module-expression node env (options (js/obj)))
  (define module
    (module-expression->module-object node env))
  (define compilation-options
    (js/obj-append
     options
     (js/obj :current-module module)))
  (compile-module-object module env compilation-options))

;;; Compile a `Module` object.
(define (compile-module-object module env (options (js/obj)))
  (define expressions
    (send module get-expressions))
  (define module-environment
    (send module get-environment))
  (define module-options
    (js/obj-append
     (js/obj :current-module
             module
             :referenced-symbols
             '()
             :inline-lisp-sources
             (send module get-inline-lisp-sources-flag))
     options))
  (define header-statements
    (compile-statement
     (begin-wrap-rose
      (get-field header-nodes module))
     module-environment
     module-options))
  (define require-statements
    (compile-statement
     (begin-wrap-rose
      (get-field require-nodes module))
     module-environment
     module-options))
  (define main-statements
    (compile-statement-or-return-statement
     (begin-wrap-rose
      (get-field main-nodes module))
     module-environment
     module-options))
  (define provide-statements
    (compile-statement
     (begin-wrap-rose
      (get-field provide-nodes module))
     module-environment
     module-options))
  (define global-environment
    (build-global-environment
     (oget module-options :referenced-symbols)
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
(define (compile-files! files (options (js/obj)))
  (define module-expression-map
    (new PromiseMap))
  (define filename-map
    (new PromiseMap))
  (define indent-option
    (oget options :indent))
  (define language-option
    (or (oget options :language)
        default-language))
  (define out-dir-option
    (or (oget options :out-dir) ""))
  (define comments-option
    (oget options :comments))
  (define quick-option
    (oget options :quick))
  (define compilation-options
    (js/obj-append
     options
     (js/obj :expression-type "statement"
             :language language-option)))
  (define extension
    (if (eq? language-option "typescript")
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
               (delay
                 (define data
                   (~> file
                       (readFileSync _ (js/obj :encoding "utf8"))
                       (regexp-replace (regexp "^#!.*") _ "")
                       (string-append
                        "(module m scheme\n"
                        _
                        "\n)")))
                 (define node
                   (read-syntax data
                                (js/obj :comments
                                        comments-option)))
                 node))
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
                         lang-environment))
  (for ((module-name module-names))
    (set! module
          (send module-map get module-name))
    (set! code
          (compile-with-environment module
                                    lang-environment
                                    compilation-options))
    (set! out-file
          (join out-dir-option
                (string-append module-name
                               extension)))
    (mkdirSync out-dir-option
               (js/obj :recursive #t))
    (writeFileSync out-file
                   code
                   (js/obj :encoding "utf8"))
    (display
     (string-append "Compiled "
                    (hash-ref filename-map module-name)
                    " to "
                    out-file)))
  module-map)

;;; Compile a file.
;;; This function writes to disk.
(define (compile-file! infile outfile (options (js/obj)))
  ;; TODO: `outfile`. Maybe by adding an
  ;; `outFileMap` option to `compile-files!`?
  (compile-files! (list infile) options))

;;; Compile a syntax object.
(define (compile-syntax node env (options (js/obj)))
  (define language-env
    (oget options :language-environment))
  (define (lang-filter x)
    (not (eq? x language-env)))
  (define comments-option
    (oget options :comments))
  (define exp
    (syntax->datum node))
  (define result)
  (cond
   ((pair-or-list? exp)
    (cond
     ((= (length exp) 0)
      (set! result
            (compile-list
             node env options)))
     (else
      (define op
        (first exp))
      (cond
       ((not (symbol? op))
        (set! result
              (compile-function-call
               node env
               options)))
       ((send env has-promise? op (js/obj :filter lang-filter))
        (define op-type
          (send env get-type op))
        (cond
         ;; Call to locally defined macro.
         ((macro-type? op-type)
          (set! result
                (compile-macro-call
                 node env
                 options)))
         ;; Call to locally defined fexpr.
         ((fexpr-type? op-type)
          (set! result
                (compile-fexpr-call
                 node env
                 options)))
         ;; Call to locally defined function.
         (else
          (set! result
                (compile-function-call
                 node env
                 options)))))
       ((regexp-match (regexp "^\\.")
                      (symbol->string op))
        (set! result
              (compile-dot
               node env options)))
       (else
        (define-values (f op-type)
          (send env get-typed-value op))
        (cond
         ((undefined-type? op-type)
          (set! result
                (compile-function-call
                 node env options)))
         ((inlined-function? f)
          (set! result
                (compile-inlined-function-call
                 node env options)))
         (else
          (define compilation-mapping-environment
            (oget options :compilation-mapping-environment))
          (define-values (compilation-f compilation-type)
            (send compilation-mapping-environment get-typed-value f))
          (cond
           ;; Compiler function.
           ((compiler-type? compilation-type)
            (set! result
                  (compilation-f
                   node env
                   options)))
           ;; Compilation macro.
           ((macro-type? compilation-type)
            (set! result
                  (compile-syntax
                   (datum->syntax
                    node
                    (compilation-f exp env))
                   env
                   options)))
           ;; Macro call.
           ((or (macro?_ f)
                (macro-type? op-type))
            (set! result
                  (compile-macro-call
                   node env options)))
           ;; Fexpr call.
           ((fexpr-type? op-type)
            (set! result
                  (compile-fexpr-call
                   node env options)))
           (else
            (set! result
                  (compile-function-call
                   node env options)))))))))))
   ((string? exp)
    (set! result
          (compile-string
           node env options)))
   ((symbol? exp)
    (set! result
          (compile-variable
           node env options)))
   ((estree? exp)
    (set! result exp))
   (else
    (set! result
          (compile-atom
           node env options))))
  (when (and comments-option
             (send node has-property "comments"))
    (define comments
      (send node get-property "comments"))
    (when (> (length comments) 0)
      (set-field! comments
                  result
                  (compile-comments comments))))
  result)

;;; Compile a S-expression.
(define (compile-sexp exp env (options (js/obj)))
  (~> exp
      (datum->syntax #f _)
      (compile-syntax _ env options)))

;;; Compile `node` as an expression.
(define (compile-expression node env (options (js/obj)))
  (compile-syntax node env (make-expression-options options)))

;;; Compile `node` as a regular statement.
(define (compile-statement node env (options (js/obj)))
  (compile-syntax node env (make-statement-options options)))

;;; Compile `node` as a return statement.
(define (compile-return-statement node env (options (js/obj)))
  (compile-syntax node env (make-return-statement-options options)))

;;; Compile `node` as a regular statement or as a return statement,
;;; depending on the value of the `expressionType` option.
(define (compile-statement-or-return-statement node env (options (js/obj)))
  (cond
   ((eq? (oget options :expression-type) "return")
    (compile-return-statement node env options))
   (else
    (compile-statement node env options))))

;;; Helper function for compiling a list of statements.
;;; The last statement is compiled as a `return` statement
;;; if the `expressionType` option is `"return"`.
(define (compile-statements statements env options)
  (define expression-type
    (oget options :expression-type))
  (define result '())
  (define return-idx -1)
  (when (eq? expression-type "return")
    (for ((i (range (- (length statements) 1) -1 -1)))
      (define statement
        (list-ref statements i))
      (unless (or (form? statement break_ env)
                  (form? statement continue_ env)
                  (form? statement yield_ env))
        (set! return-idx i)
        (break))))
  (for ((i (range 0 (length statements))))
    (define statement
      (list-ref statements i))
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
(define (interpret exp (env (default-environment)) (options (js/obj)))
  (define expression-type
    (or (oget options :expression-type)
        "statement"))
  (define inherited-options
    (js/obj-append
     options
     (js/obj
      :case "none"
      :expression-type expression-type
      :estree #t
      :should-import #f)))
  (define environment
    (make-interpretation-environment env inherited-options))
  ;; TODO: Memoize compilation?
  (define ast
    (compile-with-environment exp environment inherited-options))
  (define result
    (eval-estree ast environment inherited-options))
  result)

;;; Evaluate a Lisp expression `exp` with environment `env`.
;;;
;;; `env`, if specified, must be a Lisp environment as returned
;;; by {@link Environment}. The expression is evaluated in
;;; context of a basic Lisp environment defining such constructs
;;; as `(if ...)`, `(cond ...)`, and so on.
(define interpret1
  (dashify
   (lambda (exp (env (default-environment)) (options (js/obj)))
     (define evaluator
       (or (oget options :evaluator)
           eval_
           default-evaluator))
     (define environment
       (make-interpretation-environment env options))
     (call-evaluator evaluator
                     exp
                     environment
                     options))))

;;; Interpret a string of Lisp code.
(define (interpret-string str (env #u) (options (js/obj)))
  (interpret (read-sexp str) env options))

;;; Interpret a list of files.
(define (interpret-files files (env #u) (options (js/obj)))
  (map (lambda (file)
         (define str
           (~> file
               (readFileSync _ (js/obj :encoding "utf8"))
               (regexp-replace (regexp "^#!.*") _ "")
               (string-append "(begin\n" _ "\n)")))
         (define result
           (interpret-string str env options))
         result)
       files))

;;; Interpret a string of Lisp code.
;;; Alias for `interpret-string`.
(define (lisp str (env #u))
  (interpret-string str env))

;;; Make a Lisp environment.
(define (make-lisp (variables '())
                   (is-lisp-2 #f))
  (new LispEnvironment
       variables
       lisp-environment))

;;; Make a Lisp interpretation environment.
(define (make-interpretation-environment env (options (js/obj)))
  (define eval-option
    (oget options :feval-bindings))
  ;; TODO: Make `#f` the default.
  (when (undefined? eval-option)
    (set! eval-option #t))
  (cond
   ((or (eq? env lang-environment)
        (and (is-a? env EnvironmentStack)
             (send env has-environment? lang-environment)))
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
  (js/obj-append
   options
   (js/obj :expression-type "expression")))

;;; Make compilation options for compiling a form as
;;; a statement.
(define (make-statement-options options)
  (js/obj-append
   options
   (js/obj :expression-type "statement")))

;;; Make compilation options for compiling a form as
;;; a return statement.
(define (make-return-statement-options options)
  (js/obj-append
   options
   (js/obj :expression-type "return")))

;;; Convert an ESTree node to an expression.
(define (make-expression node (options (js/obj)))
  (cond
   ((promise? node)
    (delay
      (make-expression (force node) options)))
   ((estree-type? node "ExpressionStatement")
    (get-field expression node))
   (else
    node)))

;;; Convert an ESTree node to a statement.
;;;
;;; Wraps an expression in a statement. An `ExpressionStatement`
;;; or `ReturnStatement` node is returned, conditional on options.
(define (make-statement node (options (js/obj)))
  (define expression-type
    (oget options :expression-type))
  (cond
   ((promise? node)
    (delay
      (make-statement (force node) options)))
   ((not (is-a? node Expression))
    node)
   ((eq? expression-type "return")
    (new ReturnStatement node))
   (else
    (new ExpressionStatement node))))

;;; Convert an ESTree node to a return statement.
(define (make-return-statement node (options (js/obj)))
  (cond
   ((promise? node)
    (delay
      (make-return-statement (force node) options)))
   ((estree-type? node "ReturnStatement")
    node)
   (else
    (new ReturnStatement (make-expression node)))))

;;; Make an expression or statement ESTree node,
;;; conditional on options.
(define (make-expression-or-statement node (options (js/obj)))
  (define expression-type
    (oget options :expression-type))
  (cond
   ((or (eq? expression-type "statement")
        (eq? expression-type "return"))
    (make-statement node options))
   (else
    node)))

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
  (datum->syntax
   #f
   `((lambda () ,exp))))

;;; Wrap `exp` in a `js/arrow` call.
(define (wrap-in-arrow-call exp)
  (datum->syntax
   #f
   `((js/arrow () ,exp))))

;;; Make an immediately invoked function expression
;;; (IIFE). Defaults to using an arrow function.
(define (make-iife exp (arrow #t))
  (if arrow
      (wrap-in-arrow-call exp)
      (wrap-in-lambda-call exp)))

;;; Make a `BlockStatement`.
;;; Handles `Program` fragments.
(define (make-block-statement body)
  (cond
   ((pair-or-list? body)
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
       ((> (length fragment-statements) 0)
        (transfer-comments fragment (first fragment-statements))
        (set! statements
              (append statements fragment-statements)))
       ((> (length fragment-comments) 0)
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
  (while (and (= (length (get-field body unwrapped-exp))
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
(define (transfer-and-compile-comments node1 node2 (options (js/obj)))
  (define comments-option
    (oget options :comments))
  (define comments
    (if (syntax? node1)
        (send node1 get-property "comments")
        (get-field comments node1)))
  (when (and comments-option comments)
    (cond
     ((syntax? node2)
      (send node2
            set-property
            "comments"
            (append comments
                    (or (send node2
                              get-property
                              "comments")
                        '()))))
     (else
      (set! comments
            (compile-comments comments))
      (set-field! comments
                  node2
                  (append comments
                          (or (get-field comments node2)
                              '()))))))
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
   ((syntax? exp)
    (macro-call? (syntax->datum exp) env))
   (else
    (and (pair-or-list? exp)
         (> (length exp) 1)
         (symbol? (first exp))
         (procedure-type?
          (send env get-type (first exp)))))))

;;; Whether `exp` is a macro call, given `env`.
(define (macro-call? exp env)
  (cond
   ((syntax? exp)
    (macro-call? (syntax->datum exp) env))
   (else
    (and (pair-or-list? exp)
         (> (length exp) 1)
         (symbol? (first exp))
         (macro-type?
          (send env get-type (first exp)))))))

;;; Whether `exp` is a special form, given `env`.
(define (special-form? exp env)
  (cond
   ((syntax? exp)
    (macro-call? (syntax->datum exp) env))
   (else
    (and (pair-or-list? exp)
         (> (length exp) 1)
         (symbol? (first exp))
         (special-type?
          (send env get-type (first exp)))))))

;;; Convert a `(define (...) ...)` form to
;;; a `(js/function (...) ...)` form.
(define (define->function node (options (js/obj)))
  (define function-type
    (or (oget options :function-type)
        'js/function))
  (define curried-option
    (oget options :curried))
  (define exp
    (syntax->datum node))
  (define name-and-params
    (second exp))
  (define name
    (car name-and-params))
  (define params
    (cdr name-and-params))
  (define should-curry
    (or curried-option
        (and (undefined? curried-option)
             (pair-or-list? name))))
  (when should-curry
    (set! name (first (flatten name-and-params)))
    (set! params (rest (flatten name-and-params)))
    (when (and (dotted-list? name-and-params)
               (= (length params) 1))
      (set! params (first params))))
  (define body
    (send node drop 2))
  (define return-type '())
  (when (and (>= (length body) 2)
             (eq? (syntax->datum (first body)) ':))
    (set! return-type (take body 2))
    (set! body (drop body 2)))
  (define plist
    (if (and name
             (eq? function-type 'js/function))
        `(:name ,name)
        '()))
  (datum->syntax
   #f
   `(,function-type
     ,params
     ,@return-type
     ,@plist
     ,@body)))

;;; Convert a function to a macro on the basis
;;; of its `(define ...)` form.
(define (definition->macro exp args)
  (define params
    (cdr (second exp)))
  (define-values (regular-params rest-param)
    (parse-params-list params))
  (when rest-param
    (set! params
          (append regular-params
                  (list rest-param))))
  (define params-list
    (map (lambda (x)
           (if (pair-or-list? x)
               (first x)
               x))
         params))
  (define regular-args '())
  (define rest-arg '(list))
  (for ((i (range 0 (length args))))
    (define arg
      (list-ref args i))
    (cond
     ((< i (length regular-params))
      (push-right! regular-args arg))
     (rest-param
      (push-right! rest-arg arg))))
  (define args-list
    (append regular-args
            (if (and rest-param
                     (> (length rest-arg 1)))
                (list rest-arg)
                '())))
  (define body
    (drop exp 2))
  (cond
   ((= (length params-list) 0)
    (cond
     ((= (length body) 1)
      (first body))
     (else
      `(begin ,@body))))
   (else
    (define counts
      (build-list (length args-list)
                  (const 0)))
    (define should-make-iife #f)
    (define result
      (map (lambda (x)
             (map-tree
              (lambda (y)
                (define idx
                  (js/find-index
                   (lambda (z)
                     (eq? z y))
                   params-list))
                (cond
                 ((>= idx 0)
                  (list-set! counts
                             idx
                             (+ (list-ref counts idx)
                                1))
                  (cond
                   ((< idx (length args-list))
                    (list-ref args-list idx))
                   (else
                    (define current-param
                      (list-ref params idx))
                    (cond
                     ((pair-or-list? current-param)
                      (second current-param))
                     (else
                      #u)))))
                 (else
                  y)))
              x))
           body))
    ;; Determine whether a complex argument is referenced
    ;; more than once. If so, we need to make an IIFE.
    (for ((i (range 0 (length args-list))))
      (define count
        (list-ref counts i))
      (define arg
        (list-ref args-list i))
      (when (and (> count 1)
                 (not (or (symbol? arg)
                          (boolean? arg)
                          (string? arg)
                          (number? arg))))
        (set! should-make-iife #t)
        (break)))
    (cond
     ;; If the expression is complex, then make an IIFE.
     (should-make-iife
      `(js/iife (js/arrow ,params
                  ,@body)
                (list ,@args)))
     ;; Otherwise, the expression is simple, and no
     ;; IIFE is needed.
     ((= (length result) 1)
      (first result))
     (else
      `(begin ,@result))))))

;;; Convert a `(define ... (class ...))` expression to
;;; a `(define-class ...)` expression.
(define (define->define-class node)
  (cond
   ((syntax? node)
    (define superclass
      (send (send node get 2) get 1))
    (define superclass-exp
      (syntax->datum superclass))
    (define superclass-list
      (if (memq? superclass-exp
                 '(object%
                   object
                   Object))
          '()
          (list superclass)))
    (transfer-comments
     node
     (datum->syntax
      #f
      `(define-class ,(send node get 1)
         ,(datum->syntax #f superclass-list)
         ,@(send (send node get 2) drop 2)))))
   (else
    (~> node
        (datum->syntax #f _)
        (define->define-class _)
        (syntax->datum _)))))

;;; Compile an `(ann ...)` expression.
(define (compile-ann node env (options (js/obj)))
  (define language
    (oget options :language))
  (define e_
    (send node get 1))
  (cond
   ((eq? language "typescript")
    (define t_
      (send node get 2))
    (make-expression-or-statement
     (new TSAsExpression
          (compile-expression e_ env options)
          (compile-type t_ env options))
     options))
   (else
    (compile-syntax e_ env options))))

;;; Compile a `(define-type ...)` expression.
(define (compile-define-type node env (options (js/obj)))
  (define language
    (oget options :language))
  (cond
   ((eq? language "typescript")
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
(define (compile-type node env (options (js/obj)))
  (define exp
    (if (syntax? node)
        (syntax->datum node)
        node))
  (compile-type-exp exp env options))

;;; Helper function for `compile-type`.
(define (compile-type-exp exp env (options (js/obj)))
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
    (define params
      (drop exp 1))
    (define return-value
      (last params))
    (set! params (drop-right params 1))
    (define plist '())
    (for ((i (range 0 (length params))))
      (when (keyword? (list-ref params i))
        (set! plist (drop params i))
        (set! params
              (drop-right params
                          (- (length params) i)))
        (break)))
    (define rest-param #u)
    (cond
     ((eq? (last params) '*)
      (pop-right! params)
      (set! rest-param (pop-right! params)))
     (else
      (set! rest-param (plist-get_ plist ':rest))))
    (define mandatory-params
      (if (and (tagged-list? exp '->*)
               (>= (length params) 1))
          (first params)
          params))
    (define optional-params
      (if (and (tagged-list? exp '->*)
               (>= (length params) 2))
          (second params)
          '()))
    (define pos 0)
    (define (compile-param param
                           (options
                            (js/obj :optional #f
                                    :rest #f)))
      (define-fields (optional rest)
        options)
      (define var-name
        (number->letter pos))
      (set! pos (+ pos 1))
      (define identifier
        (new Identifier var-name optional))
      (when rest
        (set! identifier
              (new RestElement identifier)))
      (define type_
        (compile-type-exp param env options))
      (set-type identifier type_))
    (define mandatory-params-compiled
      (map (lambda (param)
             (compile-param param))
           mandatory-params))
    (define optional-params-compiled
      (map (lambda (param)
             (compile-param param
                            (js/obj :optional #t)))
           optional-params))
    (define rest-params-compiled
      (if rest-param
          (list
           (compile-param rest-param
                          (js/obj :rest #t)))
          '()))
    (define return-value-compiled
      (compile-type-exp return-value env options))
    (new TSFunctionType
         (append mandatory-params-compiled
                 optional-params-compiled
                 rest-params-compiled)
         return-value-compiled))
   ((and (pair-or-list? exp)
         (> (length exp) 0))
    (define name
      (new Identifier
           (symbol->string (first exp))))
    (define params
      (map symbol->string (rest exp)))
    (cond
     ((> (length params) 0)
      (new TSTypeReference
           name
           (new TSTypeParameterInstantiation
                params)))
     (else
      (new TSTypeReference name))))
   (else
    (new TSAnyKeyword))))

;;; "NO-OP" operation.
(define (nop_ exp env)
  #u)

;;; Compile a `(+ ...)` expression.
(define (compile-add node env (options (js/obj)))
  (compile-binary-expression
   node env options
   (js/obj :identity 0
           :operator "+")))

;;; Compile an `(apply ...)` expression.
(define (compile-apply node env (options (js/obj)))
  (define exp
    (syntax->datum node))
  (define f
    (second exp))
  (define is-new
    (eq? (send env get f) new_))
  (define callee
    (if is-new
        (third exp)
        f))
  (define args
    (if is-new
        (drop exp 3)
        (drop exp 2)))
  (define callee-compiled
    (compile-expression
     (datum->syntax #f callee)
     env options))
  (define args-compiled '())
  (when (> (length args) 0)
    (define regular-args
      (drop-right args 1))
    (for ((arg regular-args))
      (push-right! args-compiled
                   (compile-expression
                    (datum->syntax #f arg)
                    env options)))
    (define rest-arg
      (last args))
    (define rest-arg-compiled
      (compile-expression (datum->syntax #f rest-arg) env options))
    (define spread-element
      (new SpreadElement rest-arg-compiled))
    ;; Simplify the expression if the rest argument
    ;; is nothing more than a simple list.
    (cond
     ((estree-type? rest-arg-compiled "ArrayExpression")
      (define elements
        (get-field elements rest-arg-compiled))
      (define is-simple-list #t)
      (for ((x elements))
        (when (estree-type? x "SpreadElement")
          (set! is-simple-list #f)
          (break)))
      (cond
       (is-simple-list
        (for ((x elements))
          (insert-into-array-exp! x args-compiled)))
       (else
        (push-right! args-compiled spread-element))))
     (else
      (push-right! args-compiled spread-element))))
  (cond
   (is-new
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

;;; Compile a `(js/get ...)` expression.
(define (compile-js/get node env (options (js/obj)))
  (define language
    (oget options :language))
  (define variable
    (send node get 1))
  (define indices
    (send node drop 2))
  (define indices-compiled
    (map (lambda (x)
           (define x-exp
             (syntax->datum x))
           (define is-quoted-symbol #f)
           (when (and (quoted-expression? x-exp)
                      (symbol? (second x-exp)))
             (set! x-exp (second x-exp))
             (set! x (datum->syntax x x-exp))
             (set! is-quoted-symbol #t))
           (when (keyword? x-exp)
             (set! x-exp (keyword->symbol_ x-exp))
             (set! x (datum->syntax x x-exp))
             (set! is-quoted-symbol #t))
           (cond
            (is-quoted-symbol
             (define identifier
               (compile-symbol x env options))
             (define literal
               (new Literal (get-field name identifier)))
             literal)
            (else
             (compile-expression x env options))))
         indices))
  ;; Kludge: prevent TypeScript errors with expressions
  ;; like `x[y]`, where `y` is `any`-typed.
  (when (and (eq? language "typescript")
             (not (form? variable ann_ env))
             (not (estree-type? (first indices-compiled)
                                '("Literal"
                                  "UnaryExpression"
                                  "BinaryExpression"))))
    (set! variable
          (datum->syntax
           variable
           `(ann ,variable Any))))
  (define variable-compiled
    (compile-expression variable env options))
  (define computed #t)
  (define optional
    (form? variable js/optional-chaining_ env))
  (define result
    (foldl (lambda (idx arr)
             (new MemberExpression
                  arr
                  idx
                  computed
                  optional))
           variable-compiled
           indices-compiled))
  (make-expression-or-statement result options))

;;; Compile a `(js/= ...)` expression.
(define (compile-js/assignment node env (options (js/obj)))
  (define left
    (send node get 1))
  (define right
    (send node get 2))
  (cond
   ((tagged-list? left
                  '(aset!
                    define
                    define-fields
                    define-values
                    list-set!
                    oset!
                    set!
                    set!-fields
                    set!-values))
    (compile-syntax
     (datum->syntax
      node
      `(,@(syntax->list left) ,right))
     env options))
   (else
    (define left-compiled #u)
    (cond
     ((tagged-list? left 'quote)
      (set! left-compiled
            (compile-pattern (send left get 1) env options)))
     ((tagged-list? left
                    '(list
                      list*
                      values))
      (set! left-compiled
            (compile-pattern
             (datum->syntax
              left
              (list-expression->pattern
               (syntax->datum left)))
             env options)))
     ((tagged-list? left 'js/obj)
      (define fields
        (send left drop 1))
      (define properties '())
      (for ((i (range 0 (length fields) 2)))
        (push-right! properties
                     (new Property
                          (compile-symbol
                           (list-ref fields i)
                           env options)
                          (compile-symbol
                           (list-ref fields (+ i 1))
                           env options))))
      (set! left-compiled
            (new ObjectPattern properties)))
     (else
      (set! left-compiled
            (if (symbol? (syntax->datum left))
                (compile-symbol left env options)
                (compile-expression left env options)))))
    (define right-compiled
      (compile-expression right env options))
    (make-expression-or-statement
     (new AssignmentExpression
          "="
          left-compiled
          right-compiled)
     options))))

;;; Compile a list pattern to an `ArrayPattern`.
(define (compile-pattern node env (options (js/obj)))
  (define exp
    (syntax->datum node))
  (cond
   ((not exp)
    #n)
   ((symbol? exp)
    (compile-symbol node env options))
   ((pair-or-list? exp)
    (cond
     ((dotted-list? exp)
      (define head
        (send node drop-right 2))
      (define tail
        (send node last))
      (new ArrayPattern
           (append
            (map (lambda (x)
                   (compile-pattern x env options))
                 head)
            (list
             (new RestElement
                  (compile-pattern tail env options))))))
     (else
      (new ArrayPattern
           (map (lambda (x)
                  (compile-pattern x env options))
                (syntax->list node))))))
   (else
    (compile-expression node env options))))

;;; Convert an assignment expression to a
;;; variable declaration.
(define (assignment-expression->variable-declaration exp (kind "var"))
  (new VariableDeclaration
       (list (assignment-expression->variable-declarator exp))
       kind))

;;; Convert an assignment expression to a
;;; variable declaration.
(define (assignment-expression->variable-declarator exp)
  (define assignment-expression exp)
  (when (estree-type? assignment-expression
                      "ExpressionStatement")
    (set! assignment-expression
          (get-field expression assignment-expression)))
  (new VariableDeclarator
       (get-field left assignment-expression)
       (get-field right assignment-expression)))

;;; Compiler macro for `(object-set! ...)` expressions.
(define-macro (compile-object-set!-macro obj key val)
  `(js/= (js/get ,obj ,key) ,val))

;;; Compile an atomic expression, such as `foo`.
(define (compile-atom node env (options (js/obj)))
  (make-expression-or-statement
   (new Literal (syntax->datum node))
   options))

;;; Compile a `(: ...)` expression.
(define (compile-colon node env (options (js/obj)))
  (define sym
    (send node get 1))
  (define sym-exp
    (syntax->datum sym))
  (define type_
    (send node get 2))
  (define type-exp
    (syntax->datum type_))
  (send env set-local-type! sym-exp type-exp)
  (compile-nop node env options))

;;; Compile an `(if ...)` expression.
(define (compile-if node env (options (js/obj)))
  (define expression-type
    (oget options :expression-type))
  (cond
   ((eq? expression-type "expression")
    (compile-js/ternary-operator node env options))
   (else
    (compile-js/if node env options))))

;;; Compile a `(js/if ...)` expression.
(define (compile-js/if node env (options (js/obj)))
  (define expression-type
    (oget options :expression-type))
  (cond
   ((eq? expression-type "expression")
    (compile-expression
     (make-iife node)
     env options))
   (else
    (define condition
      (send node get 1))
    (define then-exp
      (datum->syntax
       #f
       `(js/block ,(send node get 2))))
    (define else-exp
      (send node get 3))
    (when (and else-exp
               (not (form? else-exp js/if_ env))
               (not (form? else-exp if_ env)))
      (set! else-exp
            (datum->syntax
             #f
             `(js/block ,else-exp))))
    (define condition-compiled
      (compile-expression
       condition env options))
    (define then-compiled
      (compile-statement-or-return-statement
       then-exp env options))
    (define else-compiled
      (if else-exp
          (compile-statement-or-return-statement
           else-exp env options)
          #n))
    (transfer-and-compile-comments
     node
     (new IfStatement
          condition-compiled
          then-compiled
          else-compiled)
     options))))

;;; Compile a `(js/? ...)` expression.
(define (compile-js/ternary-operator node env (options (js/obj)))
  (define condition
    (send node get 1))
  (define then-exp
    (send node get 2))
  (define else-exp
    (or (send node get 3)
        (datum->syntax #f #u)))
  (transfer-and-compile-comments
   node
   (make-expression-or-statement
    (new ConditionalExpression
         (compile-expression
          condition env options)
         (compile-expression
          then-exp env options)
         (compile-expression
          else-exp env options))
    options)
   options))

;;; Compile a `(define ...)` expression.
(define (compile-define node env (options (js/obj)))
  (define language-env
    (oget options :language-environment))
  (define (lang-filter x)
    (not (eq? x language-env)))
  (define language
    (oget options :language))
  (define inline-lisp-sources
    (oget options :inline-lisp-sources))
  (define exp
    (syntax->datum node))
  (define type_ 'Any)
  (cond
   ;; Function definition.
   ((pair-or-list? (second exp))
    (define sym
      (first (second exp)))
    (when (pair-or-list? sym)
      (set! sym (first (flatten sym))))
    (define function-exp
      (define->function node))
    (define return-type
      (cond
       ((eq? (~> node
                 (send _ get 2)
                 (syntax->datum _))
             ':)
        (~> node
            (send _ get 3)
            (syntax->datum _)))
       (else
        'Any)))
    (define params
      (~> function-exp
          (send _ get 1)
          (syntax->datum _)))
    (define declared-type
      (send env get-local-type sym))
    (cond
     ((or (eq? declared-type 'Any)
          (eq? declared-type 'Undefined))
      (set! type_
            `(->
              ,@(cond
                 ((symbol? params)
                  (list '(Listof Any)))
                 ((dotted-list? params)
                  (append
                   (make-list (- (length params) 2) 'Any)
                   (list '(Listof Any))))
                 (else
                  (make-list (length params) 'Any)))
              ,return-type)))
     (else
      (set! type_ declared-type)))
    (send env
          set-local!
          sym
          (new InternalPromise
               (delay
                 (define result #u)
                 (try
                   (set! result
                         (interpret `(begin ,exp ,sym)
                                    env))
                   (catch Error e
                     ;; Do nothing
                     ))
                 result))
          type_)
    (define result
      (compile-js/function
       function-exp
       env
       (make-expression-options
        options)
       (js/obj :type type_)))
    (cond
     (inline-lisp-sources
      (define lisp-code-exp
        (compile-sexp
         `(declare ,sym (fsource ,exp))
         env options))
      (new Program (list result lisp-code-exp)))
     (else
      result)))
   ;; Uninitialized variable.
   ((= (length exp) 2)
    (compile-js/let node env options))
   ;; Asynchronous function definition.
   ((and (form? (third exp) js/async_ env)
         (form? (second (third exp)) lambda_ env))
    (define lambda-node
      (send (send node get 2) get 1))
    (define name
      (send node get 1))
    (define args
      (syntax->list (send lambda-node get 1)))
    (define da-form
      (transfer-comments
       node
       (datum->syntax
        #f
        `(define/async
           (,name ,@args)
           ,@(send lambda-node drop 2)))))
    (compile-define-async da-form env options))
   ;; Class definition.
   ((form? (third exp) class_ env)
    (compile-define-class
     (define->define-class node)
     env options))
   ;; Initialized variable.
   (else
    (compile-js/let node env options))))

;;; Compile a `(js/var ...)` expression.
(define (compile-js/var node env (options (js/obj)))
  (define language-env
    (oget options :language-environment))
  (define (lang-filter x)
    (not (eq? x language-env)))
  (define language
    (oget options :language))
  (define inline-lisp-sources
    (oget options :inline-lisp-sources))
  (define exp
    (syntax->datum node))
  (define type_ 'Any)
  (cond
   ;; Uninitialized variable.
   ((= (length exp) 2)
    (define sym
      (send node get 1))
    (define result
      (assignment-expression->variable-declaration
       (compile-js/assignment
        (datum->syntax
         node
         `(js/= ,sym #u))
        env options)))
    (define declarator
      (first (get-field declarations result)))
    (set-field! init declarator #n)
    (send env set-local! (syntax->datum sym) #u 'Any)
    result)
   ;; Initialized variable.
   (else
    (define declarators '())
    (for ((i (range 1 (length exp) 2)))
      (define sym
        (send node get i))
      (define sym-exp
        (syntax->datum sym))
      (define val
        (send node get (+ i 1)))
      (define val-exp
        (syntax->datum val))
      (set! type_
            (send env
                  get-local-type
                  sym-exp
                  (js/obj :not-found 'Any)))
      (define val-promise
        (new InternalPromise
             (delay
               (define result #u)
               (try
                 (set! result
                       (interpret val-exp env))
                 (catch Error e
                   ;; Do nothing
                   ))
               result)))
      (send env set-local! sym-exp val-promise type_)
      (define declarator
        (assignment-expression->variable-declarator
         (compile-js/assignment
          (datum->syntax
           node
           `(js/= ,sym ,val))
          env options)))
      (push-right! declarators declarator))
    (define result
      (new VariableDeclaration
           declarators))
    (set-type (~> result
                  (get-field declarations _)
                  (first _)
                  (get-field id _))
              (compile-type type_ env options))
    result)))

;;; Compile a `(js/let ...)` expression.
(define (compile-js/let node env (options (js/obj)))
  (define result
    (compile-js/var node env options))
  (set-field! kind result "let")
  result)

;;; Compile a `(js/const ...)` expression.
(define (compile-js/const node env (options (js/obj)))
  (define result
    (compile-js/var node env options))
  (set-field! kind result "const")
  result)

;;; Compile a `(define/async ...)` expression.
(define (compile-define-async node env (options (js/obj)))
  (define inline-lisp-sources
    (oget options :inline-lisp-sources))
  (define result
    (compile-define node env options))
  (define result-f
    (if inline-lisp-sources
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
(define (compile-define-generator node env (options (js/obj)))
  (define result
    (compile-define node env options))
  (set-field! generator result #t)
  result)

;;; Compile a `(/ ...)` expression.
(define (compile-div node env (options (js/obj)))
  (define exp
    (syntax->datum node))
  (cond
   ((= (length exp) 1)
    (compile-expression
     (datum->syntax node #u)
     env options))
   ((= (length exp) 2)
    (compile-div
     (datum->syntax
      node
      `(/ 1 ,(send node get 1)))
     env options))
   (else
    (compile-binary-expression
     node env options
     (js/obj :identity 1
             :operator "/")))))

;;; Compile a `(send ...)` expression.
(define (compile-send node env (options (js/obj)))
  (define obj
    (send node get 1))
  (define method
    (send node get 2))
  (define args
    (send node drop 3))
  (make-expression-or-statement
   (new CallExpression
        (new MemberExpression
             (if (symbol? (syntax->datum obj))
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
(define (compile-send/apply node env (options (js/obj)))
  (define obj
    (send node get 1))
  (define method
    (send node get 2))
  (define args
    (send node drop 3))
  (make-expression-or-statement
   (compile-expression
    (datum->syntax
     node
     `(apply (get-field ,method ,obj) ,@args))
    env options)
   options))

;;; Compile a `(js/=== ...)` expression.
(define (compile-js/strictly-equal node env (options (js/obj)))
  (compile-binary-expression
   node env options
   (js/obj :identity #t
           :operator "===")))

;;; Compile a `(js/== ...)` expression.
(define (compile-js/loosely-equal node env (options (js/obj)))
  (compile-binary-expression
   node env options
   (js/obj :identity #t
           :operator "==")))

;;; Compiler macro for `(list? ...)` expressions.
(define-macro (compile-list?-macro x)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall list? ,x))
   (else
    (definition->macro
      '(define (list?_ x)
         (and (array? x)
              (not (and (>= (array-length x) 3)
                        (eq? (array-at x -2) '|.|)
                        (not (array? (array-last x)))))))
      (list x)))))

;;; Compiler macro for `(length ...)` expressions.
(define-macro (compile-length-macro x)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall length ,x))
   (else
    `(js/length ,x))))

;;; Compiler macro for `(first ...)` expressions.
(define-macro (compile-first-macro x)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall first ,x))
   (else
    `(array-first ,x))))

;;; Compiler macro for `(second ...)` expressions.
(define-macro (compile-second-macro x)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall second ,x))
   (else
    `(array-second ,x))))

;;; Compiler macro for `(third ...)` expressions.
(define-macro (compile-third-macro x)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall third ,x))
   (else
    `(array-third ,x))))

;;; Compiler macro for `(fourth ...)` expressions.
(define-macro (compile-fourth-macro x)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall fourth ,x))
   (else
    `(array-fourth ,x))))

;;; Compiler macro for `(fifth ...)` expressions.
(define-macro (compile-fifth-macro x)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall fifth ,x))
   (else
    `(array-fifth ,x))))

;;; Compiler macro for `(sixth ...)` expressions.
(define-macro (compile-sixth-macro x)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall sixth ,x))
   (else
    `(array-sixth ,x))))

;;; Compiler macro for `(seventh ...)` expressions.
(define-macro (compile-seventh-macro x)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall seventh ,x))
   (else
    `(array-seventh ,x))))

;;; Compiler macro for `(eighth ...)` expressions.
(define-macro (compile-eighth-macro x)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall eighth ,x))
   (else
    `(array-eighth ,x))))

;;; Compiler macro for `(ninth ...)` expressions.
(define-macro (compile-ninth-macro x)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall ninth ,x))
   (else
    `(array-ninth ,x))))

;;; Compiler macro for `(tenth ...)` expressions.
(define-macro (compile-tenth-macro x)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall tenth ,x))
   (else
    `(array-tenth ,x))))

;;; Compiler macro for `(pair-or-list? ...)` expressions.
(define-macro (compile-pair-or-list-macro x)
  `(array? ,x))

;;; Compiler macro for `(cons ...)` expressions.
(define-macro (compile-cons-macro x y)
  (cond
   ((self-evaluating? y)
    ``(,,x . ,,y))
   ((or (tagged-list? y 'list)
        (and (tagged-list? y 'list*)
             (> (length y) 2))
        (and (tagged-list? y '(quote quasiquote))
             (pair-or-list? (second y))))
    ``(,,x ,@,y))
   (else
    (definition->macro
      (source cons_)
      (list x y)))))

;;; Compiler macro for `(dotted-list-link ...)` expressions.
(define-macro (compile-dotted-list-link-macro x)
  (cond
   ((self-evaluating? x)
    (list '|.| x))
   ((or (tagged-list? x 'list)
        (and (tagged-list? x 'list*)
             (> (length x) 2))
        (and (tagged-list? x '(quote quasiquote))
             (pair-or-list? (second x))))
    x)
   (else
    (definition->macro
      '(define (dotted-list-link_ x)
         (js/? (pair-or-list? x)
               x
               (list '|.| x)))
      (list x)))))

;;; Compiler macro for `(cdr ...)` expressions.
(define-macro (compile-cdr-macro x)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall cdr ,x))
   (else
    (definition->macro
      '(define (cdr_ x)
         (js/? (and (= (array-length x) 3)
                    (eq? (array-ref x 1) '|.|))
               (array-third x)
               (array-rest x)))
      (list x)))))

;;; Compiler macro for `(rest ...)` expressions.
(define-macro (compile-rest-macro x)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall rest ,x))
   (else
    `(array-rest ,x))))

;;; Compiler macro for `(nth ...)` expressions.
(define-macro (compile-nth-macro n lst)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall nth ,n ,lst))
   (else
    `(list-ref ,lst ,n))))

;;; Compiler macro for `(take ...)` expressions.
(define-macro (compile-take-macro lst n)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall take ,lst ,n))
   ((eq? n 0)
    '())
   (else
    (definition->macro
      '(define (take_ lst n)
         (drop-right lst (- (length lst) n)))
      (list lst n)))))

;;; Compiler macro for `(drop ...)` expressions.
(define-macro (compile-drop-macro lst n)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall drop ,lst ,n))
   (else
    `(array-drop ,lst ,n))))

;;; Compiler macro for `(drop-right ...)` expressions.
(define-macro (compile-drop-right-macro lst n)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall drop-right ,lst ,n))
   (else
    `(array-drop-right ,lst ,n))))

;;; Compiler macro for `(array-drop ...)` expressions.
(define-macro (compile-array-drop-macro arr n)
  (cond
   ((eq? n 0)
    arr)
   (else
    `(array-slice ,arr ,n))))

;;; Compiler macro for `(array-drop-right ...)` expressions.
(define-macro (compile-array-drop-right-macro arr n)
  (cond
   ((number? n)
    (cond
     ((= n 0)
      arr)
     (else
      `(array-slice ,arr 0 (- ,n)))))
   (else
    (definition->macro
      (source array-drop-right_)
      (list arr n)))))

;;; Compiler macro for `(array-push-left! ...)` expressions.
(define-macro (compile-array-push-left!-macro arr x)
  `(js/statement-or-expression
    :statement (send ,arr unshift ,x)
    :expression ,(definition->macro
                   (source array-push-left!_)
                   (list arr x))))

;;; Compiler macro for `(array-push-right! ...)` expressions.
(define-macro (compile-array-push-right!-macro arr x)
  `(js/statement-or-expression
    :statement (send ,arr push ,x)
    :expression ,(definition->macro
                   (source array-push-right!_)
                   (list arr x))))

;;; Compiler macro for `(array-concat ...)` expressions.
(define-macro (compile-array-concat-macro &rest args)
  (cond
   ((eq? (length args) 0)
    '())
   ((eq? (length args) 1)
    (first args))
   (else
    `(send ,(first args) concat ,@(rest args)))))

;;; Compiler macro for `(reverse ...)` expressions.
(define-macro (compile-reverse-macro lst)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall reverse ,lst))
   (else
    `(array-reverse ,lst))))

;;; Compiler macro for `(last ...)` expressions.
(define-macro (compile-last-macro lst)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall last ,lst))
   (else
    `(array-last ,lst))))

;;; Compiler macro for `(list-ref ...)` expressions.
(define-macro (compile-list-ref-macro lst &rest indices)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall list-ref ,lst ,@indices))
   (else
    `(array-ref ,lst ,@indices))))

;;; Compiler macro for `(list-set! ...)` expressions.
(define-macro (compile-list-set!-macro lst &rest indices-and-value)
  (define-fields (fdottedlists)
    (current-compilation-options))
  (cond
   (fdottedlists
    `(funcall list-set! ,lst ,@indices-and-value))
   (else
    `(array-set! ,lst ,@indices-and-value))))

;;; Compiler macro for `(array-ref ...)` expressions.
(define-macro (compile-array-ref-macro arr &rest indices)
  `(js/get ,arr ,@indices))

;;; Compiler macro for `(array-set! ...)` expressions.
(define-macro (compile-array-set!-macro arr &rest indices-and-value)
  (define indices
    (drop-right indices-and-value 1))
  (define value
    (last indices-and-value))
  `(js/= (js/get ,arr ,@indices) ,value))

;;; Compiler macro for `(foldl ...)` expressions.
(define-macro (compile-foldl-macro f v lst &environment env)
  ;; `foldl()` and `.reduce()` invoke the reducing function with
  ;; opposite argument order, and `.reduce()` passes additional
  ;; arguments to it. We therefore wrap it in a binary function
  ;; wrapper that reverses the order of the two first arguments
  ;; and disregards the other arguments.
  `(js/reduce ,lst ,(flip-function-expression f env) ,v))

;;; Compiler macro for `(foldr ...)` expressions.
(define-macro (compile-foldr-macro f v lst &environment env)
  ;; Like `foldl`, but invokes the `reduceRight` method instead.
  `(js/reduce-right ,lst ,(flip-function-expression f env) ,v))

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
         (>= (length (second exp)) 2))
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
(define (compile-funcall node env (options (js/obj)))
  (define-fields (should-import)
    options)
  (compile-function-call
   (slice-rose node 1)
   env
   options
   (js/obj :should-import should-import)))

;;; Compile a function call.
(define (compile-function-call node env (options (js/obj)) (settings (js/obj)))
  (define referenced-symbols
    (oget options :referenced-symbols))
  (define current-module
    (oget options :current-module))
  (define compilation-mapping-environment
    (oget options :compilation-mapping-environment))
  (define should-import-setting
    (oget settings :should-import))
  (define callee
    (send node get 0))
  (define op
    (syntax->datum callee))
  (define symbolic-op
    (symbol? op))
  (define should-import-op
    (or should-import-setting
        (and symbolic-op
             (should-import? op env options)
             ;; Do not inline the operator if a
             ;; compilation macro is defined for it.
             (not (send env has-promise? op))
             (not (send compilation-mapping-environment
                        has?
                        (send env get op))))))
  (define args
    (send node drop 1))
  (define callee-exp
    (compile-expression
     callee
     env
     (if (and symbolic-op
              (not should-import-op))
         ;; Set the `should-import` option to `#f`
         ;; if `op` is a symbol and there is a
         ;; compilation macro defined for it.
         (js/obj-append
          options
          (js/obj :should-import #f))
         options)))
  (define args-exps
    (map (lambda (x)
           (compile-expression
            x env options))
         args))
  (make-expression-or-statement
   (new CallExpression callee-exp args-exps)
   options))

;;; Compile an inlined function call.
(define (compile-inlined-function-call node env (options (js/obj)))
  (define exp
    (syntax->datum node))
  (define op
    (first exp))
  (define f
    (send env get op))
  (define inlined-exp
    (definition->macro (source f) (rest exp)))
  (define inlined-node
    (datum->syntax node inlined-exp))
  (compile-syntax inlined-node env options))

;;; Whether a function should be inlined.
(define (inlined-function? f)
  (memq? f inlined-functions))

;;; Add symbol `sym` to `referencedSymbols` if it references a value
;;; not defined in the current module.
(define (add-referenced-symbol sym env (options (js/obj)))
  (define referenced-symbols
    (oget options :referenced-symbols))
  (when (and referenced-symbols
             ;; Do not add if already added.
             (not (memq? sym referenced-symbols))
             (should-import? sym env options))
    (push-right! referenced-symbols sym)))

;;; Whether to import the language binding for `sym`.
;;; If yes, it might be added to the global environment
;;; or inlined into the call site.
(define (should-import? sym env (options (js/obj)))
  ;; This may be disabled with the `should-import` option.
  (define should-import-option
    (oget options :should-import))
  (unless should-import-option
    (return #f))
  ;; Otherwise, determine whether importing is applicable
  ;; by inspecting the value `sym` is bound to.
  (define language-env
    (oget options :language-environment))
  (define (lang-filter x)
    (not (eq? x language-env)))
  (define (js-filter x)
    (not (eq? x js-environment)))
  (define compilation-mapping-environment
    (oget options :compilation-mapping-environment))
  (define current-module
    (oget options :current-module))
  (and (symbol? sym)
       ;; Do not import if the symbol is listed in
       ;; `compilation-variables-env`.
       (not (send compilation-variables-env has? sym))
       ;; Do not import if there is a local binding for the
       ;; value (e.g., a `let` variable).
       (not (send env has? sym (js/obj :filter lang-filter)))
       ;; Do not import if the current module defines the
       ;; value.
       (not (and current-module
                 (send current-module has-symbol sym)))
       ;; Only import if the language environment binds the symbol.
       ;; However, do not import if the value is a JavaScript
       ;; value, i.e., if it is provided by the very language
       ;; compiled to.
       (send language-env has? sym (js/obj :filter js-filter))))

;;; Compile a `(< ...)` expression.
(define (compile-less-than node env (options (js/obj)))
  ;; TODO: Convert to macro.
  (define exp
    (syntax->datum node))
  (cond
   ((< (length exp) 3)
    (compile-syntax
     (datum->syntax #f #t)
     env options))
   ((= (length exp) 3)
    (compile-binary-expression
     node env options
     (js/obj :identity #t
             :operator "<")))
   (else
    ;; Create `(and ...)` expression.
    (define and-exp
      `(and))
    (for ((i (range 2 (length exp))))
      (push-right! and-exp
                   `(< ,(list-ref exp (- i 1))
                       ,(list-ref exp i))))
    (compile-syntax
     (datum->syntax #f and-exp)
     env options))))

;;; Compile a `(<= ...)` expression.
(define (compile-less-than-or-equal node env (options (js/obj)))
  ;; TODO: Convert to macro.
  (define exp
    (syntax->datum node))
  (cond
   ((< (length exp) 3)
    (compile-syntax
     (datum->syntax #f #t)
     env options))
   ((= (length exp) 3)
    (compile-binary-expression
     node env options
     (js/obj :identity #t
             :operator "<=")))
   (else
    ;; Create `(and ...)` expression.
    (define and-exp
      `(and))
    (for ((i (range 2 (length exp))))
      (push-right! and-exp
                   `(<= ,(list-ref exp (- i 1))
                        ,(list-ref exp i))))
    (compile-syntax
     (datum->syntax #f and-exp)
     env options))))

;;; Compile a `(> ...)` expression.
(define (compile-greater-than node env (options (js/obj)))
  ;; TODO: Convert to macro.
  (define exp
    (syntax->datum node))
  (cond
   ((< (length exp) 3)
    (compile-syntax
     (datum->syntax #f #t)
     env options))
   ((= (length exp) 3)
    (compile-binary-expression
     node env options
     (js/obj :identity #t
             :operator ">")))
   (else
    ;; Create `(and ...)` expression.
    (define and-exp
      `(and))
    (for ((i (range 2 (length exp))))
      (push-right! and-exp
                   `(> ,(list-ref exp (- i 1))
                       ,(list-ref exp i))))
    (compile-syntax
     (datum->syntax #f and-exp)
     env options))))

;;; Compile a `(>= ...)` expression.
(define (compile-greater-than-or-equal node env (options (js/obj)))
  ;; TODO: Convert to macro.
  (define exp
    (syntax->datum node))
  (cond
   ((< (length exp) 3)
    (compile-syntax
     (datum->syntax #f #t)
     env options))
   ((= (length exp) 3)
    (compile-binary-expression
     node env options
     (js/obj :identity #t
             :operator ">=")))
   (else
    ;; Create `(and ...)` expression.
    (define and-exp
      `(and))
    (for ((i (range 2 (length exp))))
      (push-right! and-exp
                   `(>= ,(list-ref exp (- i 1))
                        ,(list-ref exp i))))
    (compile-syntax
     (datum->syntax #f and-exp)
     env options))))

;;; Compile a binary expression.
;;; Returns a `BinaryExpression`.
(define (compile-binary-expression
         node
         env
         (options (js/obj))
         (settings (js/obj)))
  (define operator
    (oget settings :operator))
  (define logical
    (oget settings :logical))
  (define fold
    (or (oget options :fold)
        'left))
  (define operands
    (send node drop 1))
  (cond
   ((= (length operands) 0)
    (define identity
      (oget settings :identity))
    (make-expression-or-statement
     (compile-syntax
      (datum->syntax #f identity)
      env options)
     options))
   ((= (length operands) 1)
    (make-expression-or-statement
     (compile-syntax
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
    (define (combine right left)
      (if logical
          (new LogicalExpression
               operator
               left
               right)
          (new BinaryExpression
               operator
               left
               right)))
    (define init
      (first compiled-operands))
    (define operands1
      (rest compiled-operands))
    (make-expression-or-statement
     (if (eq? fold 'right)
         (foldr combine init operands1)
         (foldl combine init operands1))
     options))))

;;; Compile a logical expression.
;;; Like `compile-binary-expression`, but
;;; returns a `LogicalExpression` instead.
(define (compile-logical-expression
         node
         env
         (options (js/obj))
         (settings (js/obj)))
  (compile-binary-expression
   node env options
   (js/obj-append
    settings
    (js/obj :logical #t))))

;;; Compile an unary expression.
;;; Returns an `UnaryExpression`.
(define (compile-unary-expression
         node
         env
         (options (js/obj))
         (settings (js/obj)))
  (define op
    (oget settings :operator))
  (define arg
    (send node get 1))
  (define arg-compiled
    (compile-expression arg env options))
  (make-expression-or-statement
   (new UnaryExpression
        op
        #t
        arg-compiled)
   options))

;;; Compile a `(js/op ...)` expression.
(define (compile-js/op node env (options (js/obj)))
  (define op
    (~> (send node get 1)
        (syntax->datum _)))
  (when (symbol? op)
    (set! op (symbol->string op)))
  (define logical
    (memq? op '("&&" "||")))
  (define node1
    (datum->syntax
     node
     (send node drop 1)))
  (cond
   ((= (send node size) 3)
    (compile-unary-expression
     node1 env options
     (js/obj :operator op)))
   (logical
    (compile-logical-expression
     node1 env options
     (js/obj :operator op)))
   (else
    (compile-binary-expression
     node1 env options
     (js/obj :operator op)))))

;;; Compile a `(lambda ...)` expression.
(define (compile-lambda node env (options (js/obj)))
  (compile-js/function node env options))

;;; Compile a function definition or function expression,
;;; producing a `FunctionDeclaration`, a `FunctionExpression`
;;; or an `ArrowFunctionExpression`.
(define (compile-function node env (options (js/obj)) (settings (js/obj)))
  (define inherited-options
    (js/obj-append options))
  (define exp
    (syntax->datum node))
  (define name
    (oget settings :name))
  (define function-type
    (or (oget settings :function-type)
        'js/function))
  (define type_
    (oget settings :type))
  (define return-type
    (or (oget settings :return-type)
        (if (tagged-list? type_ '->)
            (last type_)
            #u)))
  (define generator
    (oget settings :generator))
  (define language
    (oget inherited-options :language))
  (define params '())
  (define language-env
    (oget inherited-options :language-environment))
  (define (lang-filter x)
    (not (eq? x language-env)))
  (define env1
    (extend-environment (new LispEnvironment)
                        env))
  (define args-list)
  (define regular-args)
  (define rest-arg)
  ;; Parse the parameter list: sort the regular parameters
  ;; from the rest parameter, if any.
  (cond
   ((symbol? (second exp))
    (set! rest-arg (second exp)))
   ((dotted-list? (second exp))
    (set! args-list (second exp))
    (set! regular-args (dotted-list-head args-list))
    (set! rest-arg (dotted-list-tail args-list)))
   (else
    (set! regular-args (second exp))))
  (when regular-args
    ;; TypeScript-ism: TypeScript permits the type of `this` to be
    ;; specified with `this` as the first parameter, which is since
    ;; compiled away by the TypeScript compiler.
    (when (and (not (eq? language "typescript"))
               (> (length regular-args) 0)
               (or (eq? (first regular-args) 'this)
                   (tagged-list? (first regular-args) 'this)))
      (set! regular-args (cdr regular-args)))
    (for ((arg regular-args))
      (cond
       ((colon-form? arg)
        (define sym
          (first arg))
        (define typ
          (third arg))
        (make-type-binding env1 sym 'Any lang-filter)
        (define result
          (~> (if (= (length arg) 4)
                  (new AssignmentPattern
                       (compile-symbol
                        (datum->syntax #f sym)
                        env1 inherited-options)
                       (compile-expression
                        (datum->syntax #f (fourth arg))
                        env1 inherited-options))
                  (compile-symbol
                   (datum->syntax #f sym)
                   env1 inherited-options))
              (set-type
               _
               (compile-type typ env1 options))))
        (push-right! params result))
       ((pair-or-list? arg)
        (make-type-binding env1 (first arg) 'Any lang-filter)
        (push-right! params
                     (new AssignmentPattern
                          (compile-symbol
                           (datum->syntax
                            #f
                            (first arg))
                           env1
                           inherited-options
                           (js/obj :literal-symbol #t))
                          (compile-expression
                           (datum->syntax
                            #f
                            (second arg))
                           env1 inherited-options))))
       (else
        (make-type-binding env1 arg 'Any lang-filter)
        (push-right! params
                     (compile-symbol
                      (datum->syntax #f arg)
                      env1
                      inherited-options
                      (js/obj :literal-symbol #t)))))))
  (when rest-arg
    (make-type-binding env1 rest-arg 'Any lang-filter)
    (push-right! params
                 (new RestElement
                      (compile-expression
                       (datum->syntax #f rest-arg)
                       env1 inherited-options))))
  (define body-statements
    (send node drop 2))
  (when (and (>= (length body-statements) 2)
             (eq? (syntax->datum (first body-statements))
                  ':))
    (set! return-type
          (syntax->datum (second body-statements)))
    (set! body-statements (drop body-statements 2)))
  (when (and (>= (length body-statements) 2)
             (eq? (syntax->datum (first body-statements))
                  ':name))
    (set! name
          (syntax->datum (second body-statements)))
    (set! body-statements (drop body-statements 2)))
  (define body
    (compile-statement-or-return-statement
     (datum->syntax
      node
      `(js/block ,@body-statements))
     env1
     (js/obj-append
      inherited-options
      (js/obj :expression-type
              (if (eq? return-type 'Void)
                  "statement"
                  "return")))))
  (define result #u)
  (define result-f #u)
  (cond
   ((and name
         (not (eq? name "")))
    (when (string? name)
      (set! name
            (string->symbol name)))
    (define name-compiled
      (compile-symbol
       (datum->syntax #f name)
       env
       (make-expression-options options)))
    (cond
     ((eq? function-type 'js/arrow)
      (set! result-f
            (new ArrowFunctionExpression
                 params
                 body))
      (set! result
            (new VariableDeclaration
                 (list (new VariableDeclarator
                            name-compiled
                            result-f))
                 "let")))
     (else
      (set! result-f
            (new FunctionDeclaration
                 name-compiled
                 params
                 body))
      (set! result result-f))))
   (else
    (cond
     ((eq? function-type 'js/arrow)
      (set! result-f
            (new ArrowFunctionExpression
                 params
                 body))
      (set! result result-f))
     (else
      (set! result-f
            (new FunctionExpression
                 params
                 body))
      (set! result result-f)))))
  (when generator
    (set-field! generator result-f #t))
  (define type-compiled
    (if type_
        (compile-type type_ env options)
        #u))
  (when (is-a? type-compiled TSFunctionType)
    (for ((i (range 0 (length (get-field params result-f)))))
      (define param
        (list-ref (get-field params result-f) i))
      (define type-param
        (list-ref (get-field params type-compiled) i))
      (define type-param-annotation
        (if type-param
            (get-field typeAnnotation type-param)
            (new TSAnyKeyword)))
      (unless (and (estree? param)
                   (send param has-type))
        (set! param
              (set-type param type-param-annotation))))
    (set-field! returnType
                result-f
                (get-field returnType type-compiled)))
  (when return-type
    (set-field! returnType
                result-f
                (compile-type return-type env options)))
  (make-expression-or-statement result inherited-options))

;;; Compile a `(js/function ...)` expression.
(define (compile-js/function node env (options (js/obj)) (settings (js/obj)))
  (compile-function node
                    env
                    options
                    (js/obj-append
                     settings
                     (js/obj :function-type 'js/function))))

;;; Compile a `(js/arrow ...)` expression.
(define (compile-js/arrow node env (options (js/obj)) (settings (js/obj)))
  (compile-function node
                    env
                    options
                    (js/obj-append
                     settings
                     (js/obj :function-type 'js/arrow))))

;;; Expand a `(js/iife ...)` expression.
(define-macro (js/iife_ f args)
  (define args-list
    (drop args 1))
  (define fapply
    (if (tagged-list? args '(cons* list*))
        'apply
        'funcall))
  (define params
    (second f))
  (define-values (regular-params rest-param)
    (parse-params-list params))
  (when rest-param
    (set! params
          (append regular-params
                  (list rest-param))))
  (define params-list
    (map (lambda (x)
           (if (pair-or-list? x)
               (first x)
               x))
         params))
  (define regular-args '())
  (define rest-arg '(list))
  (for ((i (range 0 (length args-list))))
    (define arg
      (list-ref args-list i))
    (cond
     ((< i (length regular-params))
      (push-right! regular-args arg))
     (rest-param
      (push-right! rest-arg arg))))
  (define args-list-1
    (append regular-args
            (if (and rest-param
                     (> (length rest-arg 1)))
                (list rest-arg)
                '())))
  (define body
    (drop f 2))
  (define renamings '())
  (define defs '())
  (for ((i (range 0 (length params-list))))
    (define arg-exp
      (cond
       ((< i (length args-list-1))
        (list-ref args-list-1 i))
       (else
        (define current-param
          (list-ref params i))
        (cond
         ((pair-or-list? current-param)
          (second current-param))
         (else
          #u)))))
    (define param-exp
      (list-ref params-list i))
    (define param
      (if (pair-or-list? param-exp)
          (first param-exp)
          param-exp))
    (cond
     ;; Rename parameter symbol to argument symbol. This presupposes
     ;; that the function does not mutate its parameter bindings
     ;; (it may mutate internal variables, and modify the data
     ;; structures the variables point to, but the parameter binding
     ;; must remain unchanged).
     ((symbol? arg-exp)
      (unless (eq? param arg-exp)
        (push-right! renamings (list param arg-exp))))
     ;; Otherwise, define a `gensym`'ed variable for storing the value
     ;; of the argument expression, and rename the parameter to that.
     (else
      (define gsym
        (gensym (symbol->string param)))
      (push-right! defs `(define ,gsym ,arg-exp))
      (push-right! renamings (list param gsym)))))
  (define rename-form
    `(begin
       ,@defs
       (js/rename ,renamings
                  ,@body)))
  (define apply-form
    `(,fapply
      ,f
      ,@args-list))
  `(js/statement-or-expression
    :statement ,rename-form
    :return ,rename-form
    :expression ,apply-form))

;;; Expand a `(js/rename ...)` expression.
(define-macro (js/rename_ renamings &rest body)
  ;; FIXME: This is not perfect. For example, it might not handle
  ;; quoted expressions correctly---in `(list 'x x)`, say, `'x`
  ;; should never be substituted. What we probably want to do is
  ;; to integrate renamings into the environment and have the
  ;; compiler deal with them as it compiles the code, which would
  ;; also handle macro expansion correctly.
  (let ((renaming-map (new Map renamings)))
    `(begin
       ,@(map-tree (lambda (x)
                     (cond
                      ((and (symbol? x)
                            (hash-has-key? renaming-map x))
                       (hash-ref renaming-map x))
                      (else
                       x)))
                   body))))

;;; Compile a `(js/statement-or-expression ...)` expression.
(define (compile-js/statement-or-expression node env (options (js/obj)))
  (define expression-type
    (oget options :expression-type))
  (define plist
    (map (lambda (x)
           (define exp
             (syntax->datum x))
           (if (keyword? exp)
               exp
               x))
         (send node drop 1)))
  (define expression
    (plist-get_ plist :expression))
  (define statement
    (plist-get_ plist :statement))
  (define return-statement
    (plist-get_ plist :return))
  (cond
   ((eq? expression-type "expression")
    (compile-expression expression env options))
   ((eq? expression-type "return")
    (compile-statement-or-return-statement
     (cond
      (return-statement
       return-statement)
      (expression
       expression)
      (else
       statement))
     env options))
   (else
    (compile-statement
     (cond
      (statement
       statement)
      (else
       expression))
     env options))))

;;; Compile a `(let ...)` expression.
(define (compile-let node env (options (js/obj)))
  ;; There is no distinction between `(let ...)` and `(let* ...)`
  ;; expressions---they are compiled in the same way.
  (compile-let-star node env options))

;;; Compile a `(let* ...)` expression.
(define (compile-let-star node env (options (js/obj)))
  (define expression-type
    (oget options :expression-type))
  (cond
   ((eq? expression-type "expression")
    (compile-expression
     (make-iife node)
     env options))
   (else
    (define language-env
      (oget options :language-environment))
    (define (lang-filter x)
      (not (eq? x language-env)))
    (define inherited-options
      (js/obj-append options))
    (define make-block #f)
    (define let-nodes
      (~> node
          (send _ get 1)
          (syntax->list _)))
    (define body-nodes
      (send node drop 2))
    (define define-nodes
      (map (lambda (x)
             (define exp
               (syntax->datum x))
             (cond
              ((pair-or-list? exp)
               (define sym
                 (first exp))
               (when (and (not make-block)
                          (send env
                                has-local? ; has?
                                sym
                                (js/obj :filter lang-filter)))
                 (set! make-block #t))
               (datum->syntax
                x
                `(define ,(send x get 0)
                   ,(send x get 1))))
              (else
               (define sym exp)
               (when (and (not make-block)
                          (send env
                                has-local? ; has?
                                sym
                                (js/obj :filter lang-filter)))
                 (set! make-block #t))
               (datum->syntax
                x
                `(define ,x)))))
           let-nodes))
    (define env1
      (if make-block
          (extend-environment (new LispEnvironment)
                              env)
          env))
    (define result
      (compile-syntax
       (datum->syntax
        node
        `(,(if make-block
               'js/block
               'begin)
          ,@define-nodes
          ,@body-nodes))
       env1 inherited-options))
    result)))

;;; Compile a `(let-values ...)` expression.
(define (compile-let-values node env (options (js/obj)))
  (define expression-type
    (oget options :expression-type))
  (cond
   ((eq? expression-type "expression")
    (compile-expression
     (make-iife node)
     env options))
   (else
    (define language-env
      (oget options :language-environment))
    (define (lang-filter x)
      (not (eq? x language-env)))
    (define inherited-options
      (js/obj-append options))
    (define make-block #f)
    (define let-nodes
      (~> node
          (send _ get 1)
          (syntax->list _)))
    (define body-nodes
      (send node drop 2))
    (define define-nodes
      (map (lambda (x)
             (define exp
               (syntax->datum x))
             (cond
              ((symbol? exp)
               (define sym exp)
               (when (and (not make-block)
                          (send env
                                has?
                                sym
                                (js/obj :filter lang-filter)))
                 (set! make-block #t))
               (datum->syntax
                #f
                `(define ,x)))
              (else
               (define variables
                 (~> x
                     (send _ get 0)
                     (syntax->datum _)))
               (cond
                ((symbol? variables)
                 (define sym variables)
                 (when (and (not make-block)
                            (send env
                                  has?
                                  sym
                                  (js/obj :filter lang-filter)))
                   (set! make-block #t)))
                (else
                 (define syms
                   (flatten variables))
                 (unless make-block
                   (for ((sym (flatten variables)))
                     (when (send env
                                 has?
                                 sym
                                 (js/obj :filter lang-filter))
                       (set! make-block #t)
                       (break))))))
               (define expression
                 (send x get 1))
               (datum->syntax
                x
                `(define-values ,(send x get 0)
                   ,(send x get 1))))))
           let-nodes))
    (define env1
      (if make-block
          (extend-environment (new LispEnvironment)
                              env)
          env))
    (define result
      (compile-syntax
       (datum->syntax
        node
        `(,(if make-block
               'js/block
               'begin)
          ,@define-nodes
          ,@body-nodes))
       env1 inherited-options))
    result)))

;;; Compile a `(define-values ...)` expression.
(define (compile-define-values node env (options (js/obj)))
  (define hole-marker '_)
  (define variables
    (~> node
        (send _ get 1)
        (syntax->datum _)))
  (define expression
    (~> node
        (send get 2)))
  (define regular-vars '())
  (define rest-var #u)
  (when (eq? (syntax->datum expression)
             ':hole-marker)
    (set! hole-marker
          (~> node
              (send _ get 3)
              (syntax->datum _)))
    (set! expression
          (~> node
              (send _ get 4))))
  (define expression-promise
    (delay
      (define result '())
      (try
        (set! result
              (interpret expression env))
        (catch Error e
          ;; Do nothing
          ))
      result))
  (define i 0)
  (cond
   ((symbol? variables)
    (send env set-local!
          variables
          (new InternalPromise
               expression-promise)
          'Any))
   (else
    (cond
     ((dotted-list? variables)
      (define var-list
        (flatten variables))
      (set! regular-vars
            (drop-right var-list 1))
      (set! rest-var
            (last var-list)))
     (else
      (set! regular-vars variables)))
    (for ((x regular-vars))
      (unless (eq? x hole-marker)
        (define idx i)
        (define var-promise
          (new InternalPromise
               (delay
                 (define result '())
                 (try
                   (set! result
                         (list-ref (force expression-promise) idx))
                   (catch Error e
                     ;; Do nothing
                     ))
                 result)))
        (send env set-local! x var-promise 'Any))
      (set! i (+ i 1)))
    (when rest-var
      (define idx i)
      (define rest-var-promise
        (new InternalPromise
             (delay
               (define result '())
               (try
                 (set! result
                       (drop (force expression-promise) idx))
                 (catch Error e
                   ;; Do nothing
                   ))
               result)))
      (send env set-local! rest-var rest-var-promise 'Any))))
  (assignment-expression->variable-declaration
   (compile-set-values
    node env options)
   "let"))

;;; Compile a `(set!-values ...)` expression.
(define (compile-set-values node env (options (js/obj)))
  (define variables
    (~> node
        (send _ get 1)))
  (define variables-exp
    (syntax-e variables))
  (define left #u)
  (define right
    (~> node
        (send get 2)))
  (define hole-marker '_)
  (when (eq? (syntax->datum right)
             ':hole-marker)
    (set! hole-marker
          (~> node
              (send _ get 3)
              (syntax->datum _)))
    (set! right
          (~> node
              (send _ get 4))))
  (define regular-vars '())
  (define rest-var #u)
  (cond
   ((symbol? variables-exp)
    (set! left variables))
   (else
    (cond
     ((dotted-list? variables-exp)
      (define var-list
        (flatten variables-exp))
      (set! regular-vars (drop-right var-list 1))
      (set! rest-var (last var-list)))
     (else
      (set! regular-vars variables-exp)))
    (define var-patterns
      (map (lambda (x)
             (if (eq? (syntax->datum x) hole-marker)
                 (datum->syntax x #f)
                 x))
           regular-vars))
    (cond
     (rest-var
      (set! left
            (if (= (length var-patterns) 0)
                rest-var
                `(,@var-patterns . ,rest-var))))
     (else
      (set! left var-patterns)))))
  (compile-js/assignment
   (datum->syntax
    node
    `(js/= ',left ,right))
   env options))

;;; Compile a `(let-fields ...)` expression.
(define (compile-let-fields node env (options (js/obj)))
  (define expression-type
    (oget options :expression-type))
  (cond
   ((eq? expression-type "expression")
    (compile-expression
     (make-iife node)
     env options))
   (else
    (define language-env
      (oget options :language-environment))
    (define (lang-filter x)
      (not (eq? x language-env)))
    (define inherited-options
      (js/obj-append options))
    (define make-block #f)
    (define let-nodes
      (~> node
          (send _ get 1)
          (syntax->list _)))
    (define body-nodes
      (send node drop 2))
    (define define-nodes
      (map (lambda (x)
             (define fields
               (send x get 0))
             (define fields-exp
               (syntax->datum fields))
             (define obj
               (send x get 1))
             (for ((f fields-exp))
               (define sym
                 (if (pair-or-list? f)
                     (second f)
                     f))
               (when (and (not make-block)
                          (send env
                                has?
                                sym
                                (js/obj :filter lang-filter)))
                 (set! make-block #t)))
             (datum->syntax
              x
              `(define-fields ,fields
                 ,obj)))
           let-nodes))
    (define env1
      (if make-block
          (extend-environment (new LispEnvironment)
                              env)
          env))
    (define result
      (compile-syntax
       (datum->syntax
        node
        `(,(if make-block
               'js/block
               'begin)
          ,@define-nodes
          ,@body-nodes))
       env1 inherited-options))
    result)))

;;; Compile a `(define-fields ...)` expression.
(define (compile-define-fields node env (options (js/obj)))
  (define expression-type
    (oget options :expression-type))
  (define language-env
    (oget options :language-environment))
  (define (lang-filter x)
    (not (eq? x language-env)))
  (define exp
    (syntax->datum node))
  (define fields
    (send node get 1))
  (define fields-exp
    (syntax->datum fields))
  (define obj
    (send node get 2))
  (define obj-exp
    (syntax->datum obj))
  (define obj-promise
    (delay
      (define result (js/obj))
      (try
        (set! result
              (interpret obj-exp env))
        (catch Error e
          ;; Do nothing
          ))
      result))
  (for ((f fields-exp))
    (define is-array
      (pair-or-list? f))
    (define prop
      (if is-array
          (first f)
          f))
    (define sym
      (if is-array
          (second f)
          f))
    (define prop-str
      (symbol->string prop))
    (define prop-promise
      (new InternalPromise
           (delay
             (define result #u)
             (try
               (set! result
                     (oget (force obj-promise) prop-str))
               (catch Error e
                 ;; Do nothing
                 ))
             result)))
    (send env set-local! sym prop-promise 'Any))
  (assignment-expression->variable-declaration
   (compile-set-fields
    (datum->syntax
     node
     `(set!-fields ,fields ,obj))
    env
    (make-statement-options options))
   "let"))

;;; Compile a `(set!-fields ...)` expression.
(define (compile-set-fields node env (options (js/obj)))
  (define fields
    (~> (send node get 1)
        (syntax->list _)))
  (define expression
    (send node get 2))
  (define properties '())
  (for ((x fields))
    (cond
     ((pair-or-list? (syntax->datum x))
      (push-right! properties (send x get 0))
      (push-right! properties (send x get 1)))
     (else
      (push-right! properties x)
      (push-right! properties x))))
  (compile-js/assignment
   (datum->syntax
    node
    `(js/= (js/obj ,@properties) ,expression))
   env options))

;;; Compile a `(list ...)` expression.
(define (compile-list node env (options (js/obj)))
  (make-expression-or-statement
   (new ArrayExpression
        (map (lambda (x)
               (compile-expression
                x env options))
             (send node drop 1)))
   options))

;;; Compile a fexpr call.
(define (compile-fexpr-call node env (options (js/obj)))
  (define op
    (send node get 0))
  (define args
    (send node drop 1))
  (define quoted-args
    (map (lambda (arg)
           (datum->syntax arg `(quote ,arg)))
         args))
  (define call
    (datum->syntax node `(,op ,@quoted-args)))
  (compile-function-call call env options))

;;; Compile a macro call.
(define (compile-macro-call node env (options (js/obj)))
  ;; Only expand the macro a single step, as there might be
  ;; compilers defined for the immediate expansion.
  (define expansion
    (macroexpand-1 node env))
  (compile-syntax expansion env options))

;;; Expand the macro call `exp` in `env`, and keep
;;; expanding the result until something that is not
;;; a macro call is obtained.
;;;
;;; Similar to [`macroexpand` in Guile][guile:macroexpand]
;;; and [`macroexpand` in Emacs Lisp][el:macroexpand].
;;;
;;; [guile:macroexpand]: https://doc.guix.gnu.org/guile/latest/en/html_node/Macro-Expansion.html
;;; [el:macroexpand]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Expansion.html#index-macroexpand
(define (macroexpand exp (env #u))
  (define-values (expansion)
    (macroexpand* exp env))
  expansion)

;;; Expand the macro call `exp` in `env`, and keep
;;; expanding the result until something that is not
;;; a macro call is obtained. Returns a tuple
;;; `(expansion expanded)`, where `expanded` is `#t`
;;; if macro expansion took place and `#f` otherwise.
;;;
;;; Similar to [`macroexpand` in Common Lisp][cl:macroexpand].
;;;
;;; [cl:macroexpand]: http://clhs.lisp.se/Body/f_mexp_.htm#macroexpand
(define (macroexpand* exp (env #u))
  (define expansion exp)
  (define expanded #f)
  (define expanded1 #t)
  (while expanded1
    (set!-values (expansion expanded1)
                 (macroexpand*-1 expansion env))
    (set! expanded (or expanded expanded1)))
  (values expansion expanded))

;;; Expand the macro call `exp` in `env` a single step.
;;;
;;; Similar to [`macroexpand-1` in Emacs Lisp][el:macroexpand-1].
;;;
;;; [el:macroexpand-1]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Expansion.html#index-macroexpand_002d1
(define (macroexpand-1 exp (env #u))
  (define-values (expansion)
    (macroexpand*-1 exp env))
  expansion)

;;; Expand the macro call `exp` in `env` a single step.
;;; Returns a tuple `(expansion expanded)`, where `expanded`
;;; is `#t` if macro expansion took place and `#f` otherwise.
;;;
;;; Similar to [`macroexpand-1` in Common Lisp][cl:macroexpand-1].
;;;
;;; [cl:macroexpand-1]: http://clhs.lisp.se/Body/f_mexp_.htm#macroexpand-1
(define (macroexpand*-1 exp (env #u))
  (define exp1
    (if (syntax? exp)
        (syntax->datum exp)
        exp))
  (define env1
    (or env
        (current-environment_)
        (empty-environment)))
  (define expansion exp)
  (define expanded #f)
  (cond
   ((not (list? exp1))
    (set! expansion exp1))
   ((null? exp1)
    (set! expansion exp1))
   ((quote? exp1)
    (set! expansion
          (text-of-quotation exp1)))
   (else
    (define op
      (first exp1))
    (define-values (macro-f typ)
      (send env1 get-typed-value op))
    (when (or (macro?_ macro-f)
              (macro-type? typ))
      (cond
       ((or (syntax-transformer?_ macro-f)
            (syntax-transformer-type?_ typ))
        (define node
          (if (syntax? exp)
              exp
              (datum->syntax #f exp)))
        (set! expansion
              (funcall macro-f node)))
       (else
        (set! expansion
              (funcall macro-f exp1 env1))))
      (set! expanded #t))))
  (cond
   ((and (syntax? exp)
         (not (syntax? expansion)))
    (set! expansion (datum->syntax exp expansion)))
   ((and (not (syntax? exp))
         (syntax? expansion))
    (set! expansion (syntax->datum expansion))))
  (values expansion expanded))

;;; Expand the macro call `exp` in `env`, and keep
;;; expanding the result for a total number of `n`
;;; expansions, or until something that is not a
;;; macro call is obtained.
(define (macroexpand-n exp env (n 1))
  (define-values (expansion)
    (macroexpand*-n exp env n))
  expansion)

;;; Expand the macro call `exp` in `env`, and keep
;;; expanding the result for a total number of `n`
;;; expansions, or until something that is not a
;;; macro call is obtained. Returns a tuple
;;; `(expansion expanded)`, where `expanded` is `#t`
;;; if macro expansion took place and `#f` otherwise.
(define (macroexpand*-n exp env (n 1))
  (define i n)
  (define expansion exp)
  (define expanded #f)
  (define expanded1 #t)
  (while (and expanded1
              (> i 0))
    (set!-values (expansion expanded1)
                 (macroexpand*-1 expansion env))
    (set! expanded (or expanded expanded1))
    (set! i (- i 1)))
  (values expansion expanded))

;;; Expand the macro call `exp` in `env`, and keep
;;; expanding the result until `pred` returns `#f`,
;;; or until something that is not a macro call
;;; is obtained.
(define (macroexpand-until exp env pred)
  (define expansion exp)
  (while (and (macro-call? expansion env)
              (pred expansion))
    (set!-values (expansion)
                 (macroexpand*-1 expansion env)))
  expansion)

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
                               (pred #u)
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
      (and (or (macro-type? b-type)
               (undefined-type? b-type))
           (pred-f x)))
    (cond
     ((macro-call? x env)
      (define expansion
        (macroexpand-until x env pred-f-1))
      (unless (macro-call? expansion env)
        (set! expansion
              (map-sexp f expansion env stack bindings)))
      expansion)
     (else
      x)))
  (map-sexp f exp env stack bindings))

;;; Macroexpand all compiler macros.
;;; This expands regular macros as well.
(define (macroexpand-compiler-macros exp env)
  (define compiler-macro-env
    (make-macro-environment env))
  (define expansion
    (macroexpand-all exp compiler-macro-env))
  expansion)

;;; Compile a `(. ...)` expression.
;;; Also handles `(.method obj ...)` calls.
(define (compile-dot node env (options (js/obj)))
  (define exp
    (syntax->datum node))
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
      (compile-js/dot
       (datum->syntax
        #f
        `(js/. ,obj ,field-sym))
       env
       options))
     (else
      (compile-send node env options))))
   (else
    (define obj
      (send node get 1))
    (cond
     ;; Member expression:
     ;; `(.-foo bar)` = `(js/. bar foo)`.
     ((set! match
            (regexp-match (regexp "^-(.*)$")
                          method))
      (define field
        (second match))
      (compile-js/dot
       (datum->syntax
        node
        `(js/. ,obj ,(string->symbol field)))
       env options))
     ;; Method call:
     ;; `(.foo bar ...)` = `(send bar foo ...)`.
     (else
      (compile-send
       (datum->syntax
        node
        `(send ,obj
               ,(string->symbol method)
               ,@(send node drop 2)))
       env options))))))

;;; Compile a `(js/. ...)` expression.
(define (compile-js/dot node env (options (js/obj)))
  (cond
   ((> (send node size) 3)
    (compile-js/dot
     (datum->syntax
      node
      (foldl (lambda (prop obj)
               `(js/. ,obj ,prop))
             (send node get 1)
             (send node drop 2)))
     env options))
   (else
    (define language
      (oget options :language))
    (define obj
      (send node get 1))
    (define prop
      (send node get 2))
    (define prop-exp
      (syntax->datum prop))
    (define computed
      (not (symbol? prop-exp)))
    (when (and (quoted-expression? prop-exp)
               (symbol? (second prop-exp)))
      (set! prop-exp (second prop-exp))
      (set! prop (datum->syntax prop prop-exp))
      (set! computed #f))
    (when (keyword? prop-exp)
      (set! prop-exp (keyword->symbol_ prop-exp))
      (set! prop (datum->syntax prop prop-exp))
      (set! computed #f))
    (define prop-compiled
      (if (symbol? (syntax->datum prop))
          (compile-symbol prop env options)
          (compile-expression prop env options)))
    ;; Kludge: prevent TypeScript errors with expressions
    ;; like `x[y]`, where `y` is `any`-typed.
    (when (and computed
               (eq? language "typescript")
               (not (form? obj ann_ env))
               (not (estree-type? prop-compiled
                                  '("Literal"
                                    "UnaryExpression"
                                    "BinaryExpression"))))
      (set! obj
            (datum->syntax
             obj
             `(ann ,obj Any))))
    (define obj-compiled
      (if (symbol? (syntax->datum obj))
          (compile-symbol
           obj env
           (make-expression-options
            options))
          (compile-expression
           obj env options)))
    (make-expression-or-statement
     (new MemberExpression
          obj-compiled
          prop-compiled
          computed)
     options))))

;;; Compile a `(js/?. ...)` expression.
(define (compile-js/optional-chaining node env (options (js/obj)))
  (cond
   ((> (send node size) 3)
    (compile-js/optional-chaining
     (datum->syntax
      node
      (foldl (lambda (prop obj)
               `(js/?. ,obj ,prop))
             (send node get 1)
             (send node drop 2)))
     env options))
   ((= (send node size) 2)
    (compile-syntax (send node get 1) env options))
   (else
    (define obj
      (send node get 1))
    (define field
      (send node get 2))
    (define result
      (if (pair-or-list? (syntax->datum field))
          (compile-expression
           (datum->syntax
            node
            `(,obj ,@(syntax->list field)))
           env options)
          (compile-expression
           (datum->syntax
            node
            `(js/. ,obj ,field))
           env options)))
    (set-field! optional result #t)
    (make-expression-or-statement
     result options))))

;;; Compile a `(set-field! ...)` expression.
(define (compile-set-field node env (options (js/obj)))
  (define field
    (send node get 1))
  (define obj
    (send node get 2))
  (define val
    (send node get 3))
  (compile-syntax
   (datum->syntax
    node
    `(set! (get-field ,field ,obj) ,val))
   env options))

;;; Compile a `(modulo ...)` expression.
(define (compile-modulo node env (options (js/obj)))
  (compile-binary-expression
   node env options
   (js/obj :identity 1
           :operator "%")))

;;; Compile a `(* ...)` expression.
(define (compile-mul node env (options (js/obj)))
  (compile-binary-expression
   node env options
   (js/obj :identity 1
           :operator "*")))

;;; "NO-OP" compilation operation.
;;; Creates an empty program fragment and does nothing else.
(define (compile-nop node env (options (js/obj)))
  (make-program-fragment))

;;; Compile a `(not ...)` expression.
(define (compile-not node env (options (js/obj)))
  (define (is-not-expression? x)
    (and (estree-type? x "UnaryExpression")
         (eq? (get-field operator x) "!")))
  (define operand
    (send node get 1))
  (define operand-compiled
    (compile-expression operand env options))
  (define result #u)
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
(define (compile-begin node env (options (js/obj)))
  (define language-env
    (oget options :language-environment))
  (define (lang-filter x)
    (not (eq? x language-env)))
  (define expression-type
    (oget options :expression-type))
  (define exp
    (syntax->datum node))
  (define body
    (send node drop 1))
  (define compiled-body '())
  ;; TODO: Remove kludge that looks ahead and adds defined variables
  ;; to the environment. Should replace this with something better
  ;; (e.g., delayed compilation of `gensym`'ed symbols).
  (for ((i (range 0 (length body))))
    (define exp
      (syntax->datum (list-ref body i)))
    (cond
     ((form? exp define_ env)
      (define sym
        (if (pair-or-list? (second exp))
            (first (second exp))
            (second exp)))
      (make-type-binding env sym 'Any lang-filter))
     ((form? exp define-macro_ env)
      (define sym
        (first (second exp)))
      (make-type-binding env sym '(macro-> Any * Any) lang-filter))
     ((form? exp defmacro_ env)
      (define sym
        (second exp))
      (make-type-binding env sym '(macro-> Any * Any) lang-filter))))
  (cond
   ((eq? expression-type "expression")
    (cond
     ((= (length exp) 2)
      (compile-expression
       (send node get 1)
       env options))
     (else
      (define statements
        (compile-statements
         body env
         (make-statement-options options)))
      (define only-expressions #t)
      (define expressions '())
      (for ((x statements))
        (cond
         ((estree-type? x "ExpressionStatement")
          (push-right! expressions
                       (get-field expression x)))
         (else
          (set! only-expressions #f)
          (break))))
      ;; Compile to a `SequenceExpression` if possible;
      ;; otherwise, compile to an IIFE.
      (cond
       (only-expressions
        (new SequenceExpression expressions))
       (else
        (compile-expression
         (make-iife node)
         env options))))))
   (else
    (define statements
      (compile-statements body env options))
    ;; Note that this returns a `Program` node, but in
    ;; some contexts, a `BlockStatement` node is wanted.
    ;; One can convert a `Program` node to a
    ;; `BlockStatement` node with
    ;; `wrap-in-block-statement`.
    (make-program-fragment statements))))

;;; Compile a `(js/block ...)` expression.
(define (compile-js/block node env (options (js/obj)))
  (define expression-type
    (oget options :expression-type))
  (cond
   ((eq? expression-type "expression")
    (compile-begin node env options))
   (else
    (wrap-in-block-statement
     (compile-begin node env options)))))

;;; Compile a `(js/sequence ...)` expression.
(define (compile-js/sequence node env (options (js/obj)))
  (define expressions
    (send node drop 1))
  (make-expression-or-statement
   (new SequenceExpression
        (map (lambda (x)
               (compile-expression x env options))
             expressions))
   options))

;;; Make and compile a `(require ...)` or `(define-values ...)` form
;;; that defines referenced values from the language environment.
;;; `symbols` is a list of symbols bound in the language environment.
(define (build-global-environment symbols env (options (js/obj)))
  (define exp
    (make-global-environment-exp symbols env options))
  (compile-global-environment exp env options))

;;; Make a form that defines referenced values
;;; from the language environment. Returns `#f`
;;; if there are no symbols.
(define (make-global-environment-exp symbols env options)
  (cond
   ((= (length symbols) 0)
    #f)
   ((oget options :finline-functions)
    (make-define-values-exp symbols env options))
   (else
    (make-require-exp symbols env options))))

;;; Make a `(define-values ...)` form for the global environment.
(define (make-define-values-exp symbols env options)
  (define inline-functions-option
    (oget options :finline-functions))
  (define env1
    (new LispEnvironment
         '()
         env))
  (define definitions #f)
  (define define-forms '())
  (define internal-symbols '())
  (define external-symbols '())
  (define referenced-symbols
    `(,@symbols))
  (define current-module
    (new Module))
  (define seen '())
  (define exp)
  (define internal-symbol)
  (define symbol)
  (define value)
  (while (> (length referenced-symbols) 0)
    (set! symbol (pop! referenced-symbols))
    (push-right! seen symbol)
    (when (and (not (memq? symbol external-symbols))
               (send env1 has? symbol))
      (set! value (send env1 get symbol))
      (cond
       ((source? value)
        (set! exp (source value))
        (when (tagged-list? exp 'define)
          (set! internal-symbol
                (if (pair-or-list? (second exp))
                    (first (second exp))
                    (second exp)))
          (define referenced-symbols-1 '())
          (define env2
            (send env1 clone))
          (define compiled-expression
            (compile-syntax
             (datum->syntax #f exp)
             env2
             (js/obj-append
              options
              (js/obj :current-module
                      current-module
                      :referenced-symbols
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
             (set! exp `(js/raw ,js-string)))
           (else
            (set! internal-symbol symbol)
            (set! exp
                  `(define ,internal-symbol
                     (js/raw ,js-string))))))
         ((js/obj? value)
          (define js-string
            (send JSON stringify value #n 2))
          (set! internal-symbol symbol)
          (set! exp
                `(define ,internal-symbol
                   (js/raw ,js-string))))
         ((symbol? value)
          (define str
            (symbol->string value))
          (set! internal-symbol symbol)
          (set! exp
                `(define ,internal-symbol
                   (send Symbol for ,str))))
         (else
          (define js-string
            (string-append value ""))
          (set! internal-symbol symbol)
          (set! exp
                `(define ,internal-symbol
                   (js/raw ,js-string)))))))
      (unless (memq? internal-symbol internal-symbols)
        ;; Do not push the same `define` form more than once.
        (push-right! define-forms exp))
      (when (memq? symbol symbols)
        (push-right! internal-symbols internal-symbol)
        (push-right! external-symbols symbol))))
  (when (> (length external-symbols) 0)
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
(define (compile-global-environment exp env (options (js/obj)))
  (cond
   ((not exp)
    (empty-program))
   (else
    ;; Compile in a sandboxed environment.
    (define env1
      (new LispEnvironment
           '()
           env))
    (cond
     ((tagged-list? exp 'define-values)
      (define define-values-form
        `(,(first exp) ,(second exp)
          (list)))
      (define body
        (list-ref exp 2))
      (define body-compiled
        (compile-sexp
         body
         env1
         (js/obj-append
          options
          (js/obj :continuation-environment
                  (new LispEnvironment)
                  :expression-type
                  "expression"))))
      (define var-decl
        (compile-sexp define-values-form env1 options))
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
        (compile-sexp exp env1 options))))))))

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
     (js/obj-append
      options
      (js/obj :finline-functions #t))))
  (cond
   ((> (length global-environment-exp) 1)
    (define lambda-call
      (list-ref global-environment-exp 2))
    (define lambda-exp
      (list-ref lambda-call 0))
    (define values-exp
      (last lambda-exp))
    (define sym
      (second values-exp))
    (define result lambda-call)
    (cond
     ((and (= (length lambda-exp) 4)
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
                 (- (length lambda-exp) 1)
                 sym)))
    result)
   (else
    global-environment-exp)))

;;; Compile a `(quote ...)` expression.
(define (compile-quote node env (options (js/obj)))
  (define exp
    (syntax->datum node))
  (define result)
  (cond
   ((pair-or-list? (second exp))
    (set! result
          (compile-expression
           (datum->syntax
            #f
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
           (js/obj :quoted-symbol #t))))
   (else
    (set! result
          (compile-expression
           (send node get 1)
           env
           options))))
  (make-expression-or-statement
   result options))

;;; Compile a `(quasiquote ...)` expression.
(define (compile-quasiquote node env (options (js/obj)))
  (make-expression-or-statement
   (compile-quasiquote-helper
    1
    (send node get 1)
    env options)
   options))

;;; Helper function for `compile-quasiquote`.
;;; Keeps track of the quotation level and
;;; only escapes if `level` is `0`.
(define (compile-quasiquote-helper level node env (options (js/obj)))
  ;; Compile an `(unquote ...)` or
  ;; `(unquote-splicing ...)` expression.
  (define (compile-unquote-exp node level (splicing #f))
    (cond
     ((> level 1)
      (define sym
        (compile-symbol
         (send node get 0)
         env options
         (js/obj :quoted-symbol #t)))
      (define result
        (new ArrayExpression
             (list sym)))
      (for ((x (send node drop 1)))
        (insert-into-array-exp!
         (compile-quasiquote-helper
          (- level 1)
          x env options)
         result))
      (define elements
        (get-field elements result))
      ;; Rewrite `(unquote x y z)` to
      ;; `(unquote x) (unquote y) (unquote z)`
      ;; (and likewise for `unquote-splicing`).
      (when (> (length elements) 2)
        (set! result
              (new SpreadElement
                   (new ArrayExpression
                        (map (lambda (x)
                               (new ArrayExpression
                                    (list sym x)))
                             (drop elements 1))))))
      result)
     (else
      (cond
       ((= (send node size) 2)
        (define result
          (compile-quasiquote-helper
           (- level 1)
           (send node get 1)
           env options))
        (when splicing
          (set! result
                (new SpreadElement result)))
        result)
       (else
        (define result
          (new ArrayExpression '()))
        (for ((x (send node drop 1)))
          (define element
            (compile-quasiquote-helper
             (- level 1)
             x env options))
          (when splicing
            (set! element
                  (new SpreadElement
                       element)))
          (insert-into-array-exp!
           element
           result))
        (new SpreadElement result))))))
  (cond
   ((zero? level)
    (compile-expression node env options))
   (else
    (define exp
      (syntax->datum node))
    (cond
     ((pair-or-list? exp)
      (cond
       ((tagged-list? exp 'quasiquote)
        (new ArrayExpression
             (list
              (compile-symbol
               (send node get 0)
               env options
               (js/obj :quoted-symbol #t))
              (compile-quasiquote-helper
               (+ level 1)
               (send node get 1)
               env options))))
       ((tagged-list? exp 'unquote)
        (compile-unquote-exp node level))
       ((tagged-list? exp 'unquote-splicing)
        (compile-unquote-exp node level #t))
       (else
        (define result (new ArrayExpression '()))
        (for ((node1 (send node get-nodes)))
          (insert-into-array-exp!
           (compile-quasiquote-helper
            level node1 env options)
           result))
        result)))
     ((symbol? exp)
      (compile-symbol
       node env options
       (js/obj :quoted-symbol #t)))
     (else
      (compile-expression node env options))))))

;;; Compile a `(require ...)` expression.
(define (compile-require node env (options (js/obj)))
  (define fcommonjs
    (oget options :fcommonjs))
  (define fes-module-interop
    (oget options :fes-module-interop))
  (define language-env
    (oget options :language-environment))
  (define (lang-filter x)
    (not (eq? x language-env)))
  (define x-node
    (send node get 1))
  (define x-exp
    (syntax->datum x-node))
  (define y-node
    (or (send node get 2) x-node))
  (define y-exp
    (syntax->datum y-node))
  (cond
   (fcommonjs
    (cond
     ((tagged-list? x-exp 'only-in)
      (compile-statement
       (datum->syntax
        #f
        `(define-fields ,(drop x-exp 2)
           (js/require ,(second x-exp))))
       env options))
     (else
      (when (string? x-exp)
        (set! x-exp (string->symbol x-exp)))
      (compile-statement
       (datum->syntax
        #f
        `(define ,x-exp
           (js/require ,y-node)))
       env options))))
   (else
    (define specifiers '())
    (define seen '())
    (define src #n)
    (cond
     ((tagged-list? x-exp 'only-in)
      (for ((x (send x-node drop 2)))
        (define exp
          (syntax->datum x))
        (cond
         ((pair-or-list? exp)
          (define x1
            (first exp))
          (define x2
            (second exp))
          (unless (memq? x2 seen)
            (unless (send env has? x2 (js/obj :filter lang-filter))
              (make-type-binding env x2 'Any lang-filter))
            (push-right! seen x2)
            (push-right! specifiers
                         (new ImportSpecifier
                              (compile-symbol
                               (datum->syntax #f x1)
                               env
                               options
                               (js/obj :literal-symbol #t))
                              (compile-symbol
                               (datum->syntax #f x2)
                               env
                               options
                               (js/obj :literal-symbol #t))))))
         (else
          (define x1 exp)
          (unless (memq? x1 seen)
            (unless (send env has? x1 (js/obj :filter lang-filter))
              (make-type-binding env x1 'Any lang-filter))
            (push-right! seen x1)
            (push-right! specifiers
                         (new ImportSpecifier
                              (compile-symbol
                               (datum->syntax #f x1)
                               env
                               options
                               (js/obj :literal-symbol #t))))))))
      (set! y-exp (second x-exp)))
     (else
      (when (string? x-exp)
        (set! x-exp (string->symbol x-exp)))
      (set! specifiers
            (list
             (if fes-module-interop
                 (new ImportDefaultSpecifier
                      (compile-symbol
                       (datum->syntax #f x-exp)
                       env
                       options
                       (js/obj :literal-symbol #t)))
                 (new ImportNamespaceSpecifier
                      (compile-symbol
                       (datum->syntax #f x-exp)
                       env
                       options
                       (js/obj :literal-symbol #t))))))))
    (when (symbol? y-exp)
      (set! y-exp (symbol->string y-exp)))
    (set! src
          (compile-expression
           (datum->syntax #f y-exp)
           env
           options))
    (when (symbol? x-exp)
      (unless (send env has? x-exp (js/obj :filter lang-filter))
        (make-type-binding env x-exp 'Any lang-filter)))
    (cond
     ((null? specifiers)
      (empty-program))
     (else
      (new ImportDeclaration
           specifiers
           src))))))

;;; Compile a `(provide ...)` expression.
(define (compile-provide node env (options (js/obj)))
  (define fcommonjs
    (oget options :fcommonjs))
  (define expressions
    (send node drop 1))
  (cond
   (fcommonjs
    (define properties '())
    (for ((exp expressions))
      (cond
       ((tagged-list? exp 'all-from-out)
        (define name
          (~> (send exp get 1)
              (syntax->datum _)
              (string->symbol _)))
        (push-right! properties `(js/obj-spread ,name)))
       ((tagged-list? exp 'rename-out)
        (push-right! properties `(quote ,(send exp get 1 0)))
        (push-right! properties (send exp get 1 1)))
       (else
        (push-right! properties `(quote ,exp))
        (push-right! properties exp))))
    (compile-statement
     (datum->syntax
      #f
      `(set-field! exports
                   module
                   (js/obj ,@properties)))
     env options))
   (else
    ;; Sort `all-from-out` expressions from the rest.
    (define all-from-out-expressions '())
    (define other-expressions '())
    (for ((x expressions))
      (cond
       ((tagged-list? (syntax->datum x) 'all-from-out)
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
    (when (> (length other-expressions) 0)
      (define specifiers '())
      (define seen '())
      (for ((x other-expressions))
        (define exp
          (syntax->datum x))
        (cond
         ((tagged-list? exp 'rename-out)
          (for ((pair (rest exp)))
            (define x1
              (first pair))
            (define x2
              (second pair))
            (unless (memq? x2 seen)
              (push-right! seen x2)
              (push-right! specifiers
                           (new ExportSpecifier
                                (compile-symbol
                                 (datum->syntax #f x1)
                                 env
                                 options
                                 (js/obj :literal-symbol #t))
                                (compile-symbol
                                 (datum->syntax #f x2)
                                 env
                                 options
                                 (js/obj :literal-symbol #t)))))))
         (else
          (define x1 exp)
          (unless (memq? x1 seen)
            (push-right! seen x1)
            (push-right! specifiers
                         (new ExportSpecifier
                              (compile-symbol
                               (datum->syntax #f x1)
                               env
                               options
                               (js/obj :literal-symbol #t))))))))
      (define result
        (new ExportNamedDeclaration
             #n
             specifiers))
      (push-right! results result))
    (cond
     ((= (length results) 1)
      (first results))
     (else
      (make-program-fragment results))))))

;;; Compile a `(set! ...)` expression.
(define (compile-set node env (options (js/obj)))
  (define expression-type
    (oget options :expression-type))
  (define sym-node
    (send node get 1))
  (define sym-exp
    (syntax->datum sym-node))
  (define val-node
    (send node get 2))
  (define val-exp
    (syntax->datum val-node))
  (cond
   ((and (form? val-exp add_ env)
         (or (and (eq? (second val-exp) sym-exp)
                  (eq? (third val-exp) 1))
             (and (eq? (third val-exp) sym-exp)
                  (eq? (second val-exp) 1))))
    (set! val-exp `(add1 ,sym-exp))
    (set! val-node (datum->syntax #f val-exp)))
   ((and (form? val-exp sub_ env)
         (or (and (eq? (second val-exp) sym-exp)
                  (eq? (third val-exp) 1))
             (and (eq? (third val-exp) sym-exp)
                  (eq? (second val-exp) 1))))
    (set! val-exp `(sub1 ,sym-exp))
    (set! val-node (datum->syntax #f val-exp))))
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
          (compile-js/assignment node env options))))
  (make-expression-or-statement result options))

;;; Compile a string expression.
(define (compile-string node env (options (js/obj)))
  (define str
    (syntax->datum node))
  (cond
   ((regexp-match (regexp "\\n") str)
    (define lines
      (string-split str (regexp "^" "gm")))
    (cond
     ((<= (length lines) 1)
      (compile-atom node env options))
     (else
      ;; TODO: We could compile to a template literal instead.
      ;; We just have to take care to escape it properly.
      (compile-syntax
       (transfer-comments
        node
        (datum->syntax
         node
         `(string-append ,@lines)))
       env options))))
   (else
    (compile-atom node env options))))

;;; Compile a `(- ...)` expression.
(define (compile-sub node env (options (js/obj)))
  (define exp
    (syntax->datum node))
  (cond
   ((= (length exp) 2)
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
     node env options
     (js/obj :identity 0
             :operator "-")))))

;;; Compile a variable expression.
(define (compile-variable node env (options (js/obj)))
  (define compilation-mapping-environment
    (oget options :compilation-mapping-environment))
  (define literal-symbol
    (oget options :literal-symbol))
  (define quoted-symbol
    (oget options :quoted-symbol))
  (define current-module
    (oget options :current-module))
  (define exp (syntax->datum node))
  (unless (or quoted-symbol
              literal-symbol)
    (when (should-import? exp env options)
      (cond
       (current-module
        (add-referenced-symbol exp env options)
        ;; TODO: Use a global rather than a local binding.
        ;; Fetch the module's import environment and
        ;; set it there.
        ;; (define-values (val typ)
        ;;   (send env get-typed-value exp))
        ;; (send env set-local! exp val typ)
        )
       (else
        ;; Inlined expression. The symbol references a value
        ;; that is defined in the language environment.
        ;; Create an expression that will evaluate to this
        ;; value and compile that.
        (return
         (make-expression-or-statement
          (compile-expression
           (datum->syntax
            #f
            (make-inlined-value
             exp env options))
           env options)
          options))))))
  (make-expression-or-statement
   (compile-symbol node env options)
   options))

;;; Compile a symbol expression.
(define (compile-symbol node env (options (js/obj)) (settings (js/obj)))
  ;; TODO: Better handling of gensym'ed symbols.
  (define literal-symbol-option
    (or (oget settings :literal-symbol) #f))
  (define quoted-symbol-option
    (oget settings :quoted-symbol))
  (define compile-environment-option
    (oget options :compile-environment))
  (define language-env
    (oget options :language-environment))
  (define (lang-filter x)
    (not (eq? x language-env)))
  (define exp
    (syntax->datum node))
  (define gensymed-symbol
    (gensym? exp))
  (define str
    (symbol->string exp))
  ;; Keyword symbols are auto-quoted, as is the
  ;; special symbol `'|.|`.
  (when (or (regexp-match (regexp "^:") str)
            (eq? str "."))
    (set! quoted-symbol-option #t))
  (cond
   (gensymed-symbol
    (define gensym-map
      (oget options :gensym-map))
    (unless gensym-map
      (set! gensym-map (make-hash))
      (oset! options :gensym-map gensym-map))
    (define gensym-name-promise #u)
    (cond
     ((hash-has-key? gensym-map exp)
      (set! gensym-name-promise
            (hash-ref gensym-map exp)))
     (else
      ;; In order to prevent naming conflicts, use a thunk
      ;; to delay the task of translating a `gensym`'ed
      ;; symbol to a JavaScript identifier.
      (set! gensym-name-promise
            (new InternalPromise
                 (delay
                   (define name
                     (make-identifier-string str options))
                   (define gensym-name name)
                   (define i 1)
                   (define regular-sym
                     (string->symbol gensym-name))
                   (while (send env
                                has?
                                regular-sym
                                (js/obj :filter lang-filter))
                     (set! gensym-name
                           (string-append
                            name
                            (number->string i)))
                     (set! regular-sym
                           (string->symbol gensym-name))
                     (set! i (+ i 1)))
                   (send env set-local! regular-sym #u 'Any)
                   gensym-name)))
      (hash-set! gensym-map exp gensym-name-promise)))
    (cond
     (quoted-symbol-option
      (new CallExpression
           (new MemberExpression
                (new Identifier "Symbol")
                (new Identifier "for"))
           (list
            (new Literal
                 gensym-name-promise))))
     (else
      (new Identifier
           gensym-name-promise))))
   (quoted-symbol-option
    (compile-expression
     (datum->syntax
      #f
      `(string->symbol ,str))
     env options))
   (literal-symbol-option
    (define name
      (make-identifier-string str options))
    (new Identifier name))
   ((send compilation-variables-env has? exp)
    (send compilation-variables-env get exp))
   ((eq? str "this")
    (new ThisExpression))
   (else
    (define name
      (make-identifier-string str options))
    (new Identifier name))))

;;; Whether something is an equality expression.
(define (is-equality-expression exp env)
  (or (form? exp eq?_ env)
      (form? exp eqv?_ env)
      (form? exp equal?_ env)))

;;; Whether something is a `let` or `let*` expression.
(define (is-let-expression exp env)
  (form? exp let-star_ env))

;;; Compile a `(break)` expression.
(define (compile-break node env (options (js/obj)))
  (new BreakStatement
       (if (> (send node size) 1)
           (compile-expression
            (send node get 1)
            env options)
           #n)))

;;; Compile a `(continue)` expression.
(define (compile-continue node env (options (js/obj)))
  (new ContinueStatement
       (if (> (send node size) 1)
           (compile-expression
            (send node get 1)
            env options)
           #n)))

;;; Compile a `(js/new ...)` expression.
(define (compile-js/new node env (options (js/obj)))
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
(define (compile-js/do-while node env (options (js/obj)))
  (define env1
    (extend-environment (new LispEnvironment)
                        env))
  (define body
    (send node get 1))
  (define body-exp
    (datum->syntax
     node
     `(js/block ,@(syntax->list body))))
  (define test
    (send node get 2))
  (new DoWhileStatement
       (compile-expression
        test env1 options)
       (compile-statement-or-return-statement
        body-exp env1 options)))

;;; Compile a `(js/while ...)` expression.
(define (compile-js/while node env (options (js/obj)))
  (define env1
    (extend-environment (new LispEnvironment)
                        env))
  (define test
    (send node get 1))
  (define body
    (begin-wrap-rose (send node drop 2)))
  (new WhileStatement
       (compile-expression
        test env1 options)
       (wrap-in-block-statement-smart
        (compile-statement-or-return-statement
         body env1 options))))

;;; Compile a `(js/for ...)` expression.
(define (compile-js/for node env (options (js/obj)))
  (define env1
    (extend-environment (new LispEnvironment)
                        env))
  (define body
    (datum->syntax
     node
     `(js/block ,@(send node drop 2))))
  (define init
    (send node get 1 0))
  (define init-exp
    (syntax->datum init))
  (define test
    (send node get 1 1))
  (define test-exp
    (syntax->datum test))
  (define update
    (send node get 1 2))
  (define update-exp
    (syntax->datum update))
  (define sym #u)
  (define (binding? x)
    (and x
         (= (length x) 2)
         (symbol? (first x))))
  (cond
   ((binding? init-exp)
    (set! sym (first init-exp))
    (set! init
          (datum->syntax
           init
           `(define ,@(syntax->list init)))))
   ((form? init-exp define_ env1)
    (set! sym (second init-exp)))
   ((form? init-exp set!_ env1)
    (set! sym (second init-exp))))
  (define init-compiled
    (if (or (not init-exp)
            (null? init-exp))
        #n
        (compile-statement init env1 options)))
  (when (estree-type? init-compiled
                      '("Program"
                        "BlockStatement"))
    (set! init-compiled
          (new SequenceExpression
               (map make-expression
                    (get-field body init-compiled)))))
  (define test-compiled
    (if (or (not test-exp)
            (null? test-exp))
        #n
        (compile-expression test env1 options)))
  (define (increment? x)
    (or (form? x add_ env1)
        (form? x sub_ env1)))
  (when (increment? update-exp)
    (unless sym
      (cond
       ((symbol? (second update-exp))
        (set! sym (second update-exp)))
       ((symbol? (third update-exp))
        (set! sym (third update-exp)))))
    (when sym
      (set! update
            (datum->syntax
             update
             `(set! ,sym ,update)))))
  (define update-compiled
    (if (or (not update-exp)
            (null? update-exp))
        #n
        (compile-statement update env1 options)))
  (cond
   ((estree-type? update-compiled
                  '("Program"
                    "BlockStatement"))
    (set! update-compiled
          (new SequenceExpression
               (map make-expression
                    (get-field body update-compiled)))))
   ((estree-type? update-compiled
                  "ExpressionStatement")
    (set! update-compiled
          (get-field expression update-compiled))))
  (define body-compiled
    (compile-statement body env1 options))
  (new ForStatement
       init-compiled
       test-compiled
       update-compiled
       body-compiled))

;;; Compile a `(js/for-in ...)` expression.
(define (compile-js/for-in node env (options (js/obj)))
  (define env1
    (extend-environment (new LispEnvironment)
                        env))
  (define left
    (datum->syntax
     node
     `(define ,(send node get 1 0 0))))
  (define right
    (send node get 1 0 1))
  (define body
    (datum->syntax
     node
     `(js/block ,@(send node drop 2))))
  (define left-compiled
    (compile-statement left env1 options))
  (define right-compiled
    (compile-expression right env1 options))
  (define body-compiled
    (compile-statement body env1 options))
  (new ForInStatement
       left-compiled
       right-compiled
       body-compiled))

;;; Compile a `(js/for-of ...)` expression.
(define (compile-js/for-of node env (options (js/obj)))
  (define env1
    (extend-environment (new LispEnvironment)
                        env))
  (define left
    (datum->syntax
     node
     `(define ,(send node get 1 0 0))))
  (define right
    (send node get 1 0 1))
  (define body
    (datum->syntax
     node
     `(js/block ,@(send node drop 2))))
  (define left-compiled
    (compile-statement left env1 options))
  (define right-compiled
    (compile-expression right env1 options))
  (define body-compiled
    (compile-statement body env1 options))
  (new ForOfStatement
       left-compiled
       right-compiled
       body-compiled))

;;; Compile a `(yield ...)` expression.
(define (compile-yield node env (options (js/obj)))
  (make-expression-or-statement
   (new YieldExpression
        (if (> (send node size) 1)
            (compile-expression
             (send node get 1)
             env options)
            #n))
   options))

;;; Compile a `(throw ...)` expression.
(define (compile-throw node env (options (js/obj)))
  (new ThrowStatement
       (compile-expression
        (send node get 1)
        env options)))

;;; Compile a `(js/delete ...)` expression.
(define (compile-js/delete node env (options (js/obj)))
  (make-expression-or-statement
   (new UnaryExpression
        "delete"
        #t
        (compile-expression
         (send node get 1)
         env options))
   options))

;;; Compile a `(return ...)` expression.
(define (compile-return node env (options (js/obj)))
  (new ReturnStatement
       (if (> (send node size) 1)
           (compile-expression
            (send node get 1)
            env options)
           #n)))

;;; Compile a `(js/async ...)` expression.
(define (compile-js/async node env (options (js/obj)))
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
(define (compile-js/await node env (options (js/obj)))
  (make-expression-or-statement
   (new AwaitExpression
        (compile-expression
         (send node get 1)
         env options))
   options))

;;; Compile a `(js/string-concat ...)` expression.
(define (compile-js/string-concat node env (options (js/obj)))
  (define exp
    (syntax->datum node))
  (cond
   ((<= (length exp) 1)
    (compile-syntax
     (datum->syntax node "")
     env options))
   ((= (length exp) 2)
    (compile-syntax
     (send node get 1)
     env options))
   (else
    (compile-binary-expression
     node env options
     (js/obj :identity ""
             :operator "+")))))

;;; Compile a `(class ...)` expression.
(define (compile-class node env (options (js/obj)))
  (compile-class-helper node env options))

;;; Compile a `(define-class ...)` expression.
(define (compile-define-class node env (options (js/obj)))
  (compile-class-helper node env options))

;;; Helper function for `compile-class` and `compile-define-class`.
(define (compile-class-helper node env (options (js/obj)))
  (define inherited-options
    (js/obj-append options))
  (define exp
    (syntax->datum node))
  (define class-name-node
    (send node get 1))
  (define class-name
    (syntax->datum class-name-node))
  (define has-name
    (symbol? class-name))
  (define super-class
    #n)
  (define id
    (if has-name
        (compile-symbol
         class-name-node env inherited-options)
        #n))
  (define body-node
    (if (eq? id #n)
        (slice-rose node 1)
        (slice-rose node 2)))
  (define body-exp
    (syntax->datum body-node))
  (define env1
    (extend-environment (new LispEnvironment)
                        env))
  (make-type-binding env1 'super 'Any)
  (when (and (pair-or-list? (first body-exp))
             (not (form? (first body-exp) define_ env1)))
    (define super-classes-node
      (send body-node get 0))
    (define super-classes
      (syntax->datum super-classes-node))
    (set! body-node (slice-rose body-node 1))
    (set! body-exp (syntax->datum body-node))
    (when (> (length super-classes) 0)
      (set! super-class
            (compile-expression
             (datum->syntax
              #f
              (first super-classes))
             env1 inherited-options))))
  (define body-declarations '())
  (define accessibilities
    (make-hash))
  (for ((x (syntax->list body-node)))
    (define exp
      (syntax->datum x))
    (cond
     ((tagged-list? exp 'public)
      (hash-set! accessibilities (second exp) "public"))
     ((tagged-list? exp 'private)
      (hash-set! accessibilities (second exp) "private"))
     (else
      (define is-initialized
        (>= (length exp) 3))
      (define id
        (second exp))
      (define is-method
        (pair-or-list? id))
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
             (> (length (second exp)) 0)
             (eq? id 'constructor)))
      (when (or is-constructor is-generator)
        (set! accessibility "public"))
      (define return-type
        (if is-constructor
            'Void
            #u))
      (define is-computed
        (not (symbol? id)))
      (define id-compiled
        (if is-computed
            (compile-expression
             id-node env1 inherited-options)
            (compile-symbol
             id-node env1
             (make-expression-options
              inherited-options))))
      (define init-compiled
        (cond
         ((not is-initialized)
          #u)
         (is-method
          (compile-js/function
           (define->function
             x
             (js/obj :function-type 'lambda
                     :curried #f))
           env1
           (make-expression-options
            inherited-options)
           (js/obj :generator is-generator
                   :return-type return-type)))
         (else
          (compile-expression
           (send x get 2)
           env1
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
  (when has-name
    (send env
          set-local!
          class-name
          (new InternalPromise
               (delay
                 (define result #u)
                 (try
                   (set! result
                         (interpret `(begin ,exp ,class-name)
                                    env))
                   (catch Error e
                     ;; Do nothing
                     ))
                 result))
          'Any))
  (if has-name
      (new ClassDeclaration
           id
           body
           super-class)
      (new ClassExpression
           body
           super-class)))

;;; Compile a `(js/obj ...)` expression.
(define (compile-js/obj node env (options (js/obj)))
  (define exp
    (syntax->datum node))
  (define properties '())
  (define i 1)
  (while (< i (length exp))
    (define key-node
      (send node get i))
    (cond
     ((tagged-list? key-node 'js/obj-spread)
      (define compiled-key
        (compile-expression key-node env options))
      (push-right! properties compiled-key)
      (set! i (+ i 1)))
     (else
      (define key-exp
        (syntax->datum key-node))
      (define computed
        (not (string? key-exp)))
      (define val-node
        (send node get (+ i 1)))
      (define is-quoted-symbol #f)
      (when (and (quoted-expression? key-exp)
                 (symbol? (second key-exp)))
        (set! key-exp (second key-exp))
        (set! key-node (datum->syntax key-node key-exp))
        (set! is-quoted-symbol #t)
        (set! computed #f))
      (when (keyword? key-exp)
        (set! key-exp (keyword->symbol_ key-exp))
        (set! key-node (datum->syntax key-node key-exp))
        (set! is-quoted-symbol #t)
        (set! computed #f))
      (define compiled-key
        (if is-quoted-symbol
            (compile-symbol key-node env options)
            (compile-expression key-node env options)))
      (define compiled-value
        (compile-expression val-node env options))
      (when (and (string? key-exp)
                 (regexp-match (regexp "^[a-z]+$" "i")
                               key-exp))
        (set! compiled-key
              (new Identifier key-exp)))
      (define shorthand
        (and (not computed)
             (estree-type? compiled-key "Identifier")
             (estree-type? compiled-value "Identifier")
             (eq? (get-field name compiled-key)
                  (get-field name compiled-value))))
      (push-right! properties
                   (new Property
                        compiled-key
                        compiled-value
                        computed
                        shorthand))
      (set! i (+ i 2)))))
  (make-expression-or-statement
   (new ObjectExpression properties)
   options))

;;; Compile a `(js/obj-append ...)` expression.
(define (compile-js/obj-append node env (options (js/obj)))
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

(define (compile-js/obj-spread node env (options (js/obj)))
  (define arg
    (send node get 1))
  (define arg-compiled
    (compile-expression arg env options))
  (make-expression-or-statement
   (new SpreadElement arg-compiled)
   options))

;;; Compile a `(js/tag ...)` expression.
(define (compile-js/tagged-template node env (options (js/obj)))
  (define tag
    (send node get 1))
  (define tag-compiled
    (compile-expression tag env options))
  (define str
    (send node get 2))
  (define str-exp
    (syntax->datum str))
  (make-expression-or-statement
   (new TaggedTemplateExpression
        tag-compiled
        (new TemplateLiteral
             (list
              (new TemplateElement
                   #t
                   str-exp))))
   options))

;;; Compile an `(append ...)` expression.
(define (compile-append node env (options (js/obj)))
  (define elements '())
  (for ((x (send node drop 1)))
    (define el
      (compile-expression
       x env options))
    (cond
     ((estree-type? el "ArrayExpression")
      (cond
       ((= (length (get-field elements el)) 0)
        ;; Ignore empty arrays.
        )
       ((= (length (get-field elements el)) 1)
        ;; Unwrap singleton arrays.
        (push-right! elements (list-ref (get-field elements el) 0)))
       (else
        (push-right! elements (new SpreadElement el)))))
     (else
      (push-right! elements (new SpreadElement el)))))
  (make-expression-or-statement
   (new ArrayExpression elements)
   options))

;;; Compile a `(js/try ...)` expression.
(define (compile-js/try node env (options (js/obj)))
  (define body-exps '())
  (define catch-clause #n)
  (define finally-clause #n)
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
      (datum->syntax
       #f
       `(begin ,@body-exps))
      env options)))
  (define handler #n)
  (when catch-clause
    ;; TODO: Permit destructuring.
    (define param
      (send catch-clause get 1))
    (define param-exp
      (syntax->datum param))
    (define param-compiled
      (if (eq? param-exp '_)
          #n
          (compile-expression
           param env options)))
    (define body
      (datum->syntax
       #f
       `(begin ,@(send catch-clause drop 2))))
    (define body-compiled
      (wrap-in-block-statement-smart
       (compile-statement
        body env options)))
    (set! handler
          (new CatchClause
               param-compiled
               body-compiled)))
  (define finalizer
    (if finally-clause
        (wrap-in-block-statement-smart
         (compile-statement
          (datum->syntax
           #f
           `(begin ,@(send finally-clause drop 1)))
          env options))
        #n))
  (make-expression-or-statement
   (new TryStatement
        block
        handler
        finalizer)
   options))

;;; Compile a `(declare ...)` expression.
(define (compile-declare node env (options (js/obj)))
  (define language-env
    (oget options :language-environment))
  (define (lang-filter x)
    (not (eq? x language-env)))
  (define exp
    (syntax->datum node))
  (define name
    (second exp))
  (define specs
    (drop exp 2))
  (for ((spec specs))
    (define field
      (first spec))
    (when (eq? field 'ftype)
      (define value
        (second spec))
      (define type_
        (parse-ftype value))
      (make-type-binding env name type_ lang-filter)))
  (define expansion
    (funcall declare_ exp env))
  (compile-sexp expansion env options))

;;; Compiler macro for `(make-hash ...)` expressions.
(define-macro (compile-make-hash-macro assocs)
  (cond
   (assocs
    (cond
     ((and (or (tagged-list? assocs 'quasiquote)
               (tagged-list? assocs 'quote))
           (list? (second assocs))
           (= (length
               (filter
                (lambda (x)
                  (or (not (pair-or-list? x))
                      (and (= (length x) 2)
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
(define-macro (compile-hash-clear-macro ht)
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
(define-macro (compile-hash-remove-macro ht key)
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
(define-macro (compile-hash-ref-macro ht key failure-result)
  (cond
   ((undefined? failure-result)
    `(send ,ht get ,key))
   (else
    (definition->macro
      '(define (hash-ref ht key failure-result)
         (if (send ht has key)
             (send ht get key)
             failure-result))
      (list ht key failure-result)))))

;;; Compiler macro for `(map ...)` expressions.
(define-macro (compile-map-macro f x)
  ;; Note that `` `(send ,x map ,f) `` is too simple, as JavaScript's
  ;; `.map()` method calls the function with multiple arguments. This
  ;; can lead to unintuitive bugs in cases where the function has an
  ;; optional second parameter. To avoid this, we enclose `f` in a
  ;; unary function wrapper.
  (define f-exp
    (compile-map-macro-helper f env))
  `(send ,x map ,f-exp))

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
             (form? f-exp js/function_ env)
             (form? f-exp js/arrow_ env))
         (pair-or-list? (second f-exp))
         (= (length (second f-exp)) 1))
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

;;; Compiler macro for `(values ...)` expressions.
(define-macro (compile-values-macro &rest args)
  `(list ,@args))

;;; Compiler macro for `(string? ...)` expressions.
(define-macro (compile-string?-macro x)
  (define-fields (fstringobjects)
    (current-compilation-options))
  (cond
   (fstringobjects
    `(funcall string? ,x))
   (else
    `(js/string-literal? ,x))))

;;; Compiler macro for `(string-append ...)` expressions.
(define-macro (compile-string-append-macro &rest args)
  `(js/string-concat ,@args))

;;; Compiler macro for `(js/string-concat ...)` expressions.
(define-macro (compile-js/string-concat-macro &rest args)
  `(js/op/apply + (list ,@args) :identity ""))

;;; Compiler macro for `(string-trim ...)` expressions.
(define-macro (compile-string-trim-macro &rest args)
  (cond
   ((= (length args) 1)
    `(send ,(first args) trim))
   (else
    (definition->macro (source string-trim_) args))))

;;; Compiler macro for `(member? ...)` expressions.
(define-macro (compile-member-p-macro v lst is-equal)
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
(define-macro (compile-substring-macro str &rest args)
  `(send ,str substring ,@args))

;;; Compiler macro for `(js/regexp ...)` expressions.
(define-macro (compile-js/regexp-macro &rest args)
  `(new RegExp ,@args))

;;; Compiler macro for `(assert ...)` expressions.
(define-macro (compile-assert-macro &rest args)
  `(send console assert ,@args))

;;; Compiler macro for `(display ...)` expressions.
(define-macro (compile-display-macro &rest args)
  `(send console log ,@args))

;;; Compiler macro for `(current-environment)` expressions.
(define-macro (compile-current-environment-macro )
  (define arg-sym
    (gensym "_arg"))
  (define str-sym
    (gensym "_str"))
  (define identifier-regexp
    '(regexp "^\\w+$"))
  `(js/obj :get
           (js/arrow (,arg-sym)
             (try
               (define ,str-sym
                 (symbol->string ,arg-sym))
               (cond
                ((regexp-match? ,identifier-regexp ,str-sym)
                 (return (js/eval ,str-sym)))
                (else
                 (return #u)))
               (catch Error e
                 (return #u))))
           :has
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

;;; Compile a `(js/raw ...)` expression.
(define (compile-js/raw node env (options (js/obj)))
  (define eval-option
    (oget options :feval-bindings))
  (set! eval-option #t)
  (define str
    (send node get 1))
  (define str-exp
    (syntax->datum str))
  (cond
   ((not eval-option)
    (make-expression-or-statement
     (new Literal #u)
     options))
   ((string? str-exp)
    (make-expression-or-statement
     (new XRawJavaScript str-exp)
     options))
   (else
    (compile-js/eval node env options))))

;;; Compile a `(js/eval ...)` expression.
(define (compile-js/eval node env (options (js/obj)))
  ;; TODO: Disable if `eval-option` is `#f`.
  (define eval-option
    (oget options :feval-bindings))
  ;; FIXME: Kludge.
  (define compiling-to-js
    (valid-js-casing-style? (oget options :case)))
  (define eval-f
    (if compiling-to-js
        "eval"
        "js/eval"))
  ;; TODO: Make `#f` the default.
  (set! eval-option #t)
  (define str
    (send node get 1))
  (define str-exp
    (syntax->datum str))
  (cond
   ((not eval-option)
    (make-expression-or-statement
     (new Literal #u)
     options))
   (else
    (make-expression-or-statement
     (new CallExpression
          (new Identifier eval-f)
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
(define-macro (quote_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
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
(define-macro (quasiquote_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(set! ...)` expression.
;;;
;;; Similar to [`set!` in Racket][rkt:setx] and
;;; [`set!` in Guile][guile:setx].
;;;
;;; [rkt:setx]: https://docs.racket-lang.org/reference/set_.html#%28form._%28%28quote._~23~25kernel%29._set%21%29%29
;;; [guile:setx]: https://doc.guix.gnu.org/guile/latest/en/html_node/Definition.html
(define-macro (set!_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(module ...)` expression.
(define-macro (module_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/block ...)` expression.
(define-macro (js/block_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/sequence ...)` expression.
(define-macro (js/sequence_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(begin ...)` expression.
(define-macro (begin_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(let* ...)` expression.
(define-macro (let-star_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(let-values ...)` expression.
(define-macro (let-values_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(define-values ...)` expression.
(define-macro (define-values_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(set!-values ...)` expression.
(define-macro (set-values_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(define ...)` expression.
(define-macro (define_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/var ...)` expression.
(define-macro (js/var_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/let ...)` expression.
(define-macro (js/let_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/const ...)` expression.
(define-macro (js/const_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(define/generator ...)` expression.
(define-macro (define-generator_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(define/async ...)` expression.
(define-macro (define-async_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/for ...)` expression.
(define-macro (js/for_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/for-in ...)` expression.
(define-macro (js/for-in_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/for-of ...)` expression.
(define-macro (js/for-of_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/while ...)` expression.
(define-macro (js/while_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/do-while ...)` expression.
(define-macro (js/do-while_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(break)` expression.
(define-macro (break_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(continue)` expression.
(define-macro (continue_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(yield ...)` expression.
(define-macro (yield_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(return ...)` expression.
(define-macro (return_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(throw ...)` expression.
;;;
;;; Similar to the [`throw`][clj:throw] special form in Clojure.
;;;
;;; [clj:throw]: https://clojuredocs.org/clojure.core/throw
(define-macro (throw_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/async ...)` expression.
(define-macro (js/async_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/await ...)` expression.
(define-macro (js/await_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
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
(define-macro (lambda_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/function ...)` expression.
;;;
;;; Creates an anonymous JavaScript function.
(define-macro (js/function_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/arrow ...)` expression.
;;;
;;; Creates a JavaScript arrow function.
(define-macro (js/arrow_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/= ...)` expression.
(define-macro (js/assignment_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/op ...)` expression.
;;;
;;; Creates a JavaScript operator expression.
(define-macro (js/op_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/op/apply ...)` expression.
;;; This macro generalizes a binary operator
;;; to multiple operands, using a left fold.
(define-macro (js/op/apply_ op args . options)
  ;; TODO: Option for single-argument behavior.
  ;; Should it return the argument unchanged
  ;; or combine it with the identity value?
  (define identity
    (plist-get_ options :identity))
  (define fold
    (if (eq? (plist-get_ options :fold) 'right)
        'foldr
        'foldl))
  (cond
   ;; If `args` is a variable, then fold over it
   ;; at runtime.
   ((symbol? args)
    (if (undefined? identity)
        `(,fold (lambda (right left)
                  (js/op ,op left right))
                (first ,args)
                (rest ,args))
        `(,fold (lambda (right left)
                  (js/op ,op left right))
                ,identity
                ,args)))
   ;; If `args` is a list expression, however,
   ;; then it is actually possible to perform
   ;; the fold at compile time, which produces
   ;; neater code.
   ((tagged-list? args 'list)
    (define args1
      (rest args))
    (cond
     ((= (length args1) 0)
      identity)
     ((= (length args1) 1)
      (first args1))
     (else
      (if (eq? fold 'foldr)
          (foldr (lambda (right left)
                   `(js/op ,op ,left ,right))
                 (first args1)
                 (rest args1))
          (foldl (lambda (right left)
                   `(js/op ,op ,left ,right))
                 (first args1)
                 (rest args1))))))
   ;; A quoted list is just another way of
   ;; writing a list.
   ((tagged-list? args 'quote)
    `(js/op/apply ,op
                  (list
                   ,(map (lambda (x)
                           `(quote ,x))
                         (second args)))
                  ,@options))
   ;; A function call can be stored in a variable.
   (else
    (let ((args-var (gensym "_args")))
      `(let ((,args-var ,args))
         (js/op/apply ,op ,args-var ,@options))))))

;;; Expand a `(js/if ...)` expression.
(define-macro (js/if_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/? ...)` expression.
(define-macro (js/ternary-operator_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand an `(if ...)` expression.
;;;
;;; Similar to [`if` in Racket][rkt:if], [`if` in Guile][guile:if]
;;; and [`if` in Common Lisp][cl:if].
;;;
;;; [rkt:if]: https://docs.racket-lang.org/reference/if.html#%28form._%28%28quote._~23~25kernel%29._if%29%29
;;; [guile:if]: https://doc.guix.gnu.org/guile/2.0.14/en/html_node/Conditionals.html#index-if-1
;;; [cl:if]: http://clhs.lisp.se/Body/s_if.htm#if
(define-macro (if_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
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
(define-macro (send_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(send/apply ...)` expression.
(define-macro (send/apply_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
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
(define-macro (dot_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(get-field ...)` expression.
(define-macro (get-field_ field obj)
  `(js/. ,obj ,field))

;;; Expand a `(set-field! ...)` expression.
(define-macro (set-field_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(class ...)` expression.
;;;
;;; Loosely based on [`class` in Racket][rkt:class] and
;;; [`define-class` in CLOS][cl:define-class].
;;;
;;; [rkt:class]: https://docs.racket-lang.org/guide/classes.html
;;; [cl:define-class]: http://clhs.lisp.se/Body/07_.htm
(define-macro (class_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(define-class ...)` expression.
;;;
;;; Loosely based on [`define-class` in Guile][guile:define-class],
;;; [`class` in Racket][rkt:class] and
;;; [`defclass` in CLOS][cl:defclass].
;;;
;;; [guile:define-class]: https://doc.guix.gnu.org/guile/latest/en/html_node/Class-Definition.html#index-define_002dclass-1
;;; [rkt:class]: https://docs.racket-lang.org/guide/classes.html
;;; [cl:defclass]: http://clhs.lisp.se/Body/m_defcla.htm#defclass
(define-macro (define-class_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/try ...)` expression.
(define-macro (js/try_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(js/statement-or-expression ...)` expression.
(define-macro (js/statement-or-expression_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  ;; TODO: `js/statement`, `js/expression`.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(provide ...)` expression.
(define-macro (provide_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(require ...)` expression.
(define-macro (require_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Evaluate a JavaScript string.
(define (js/raw_ str)
  (js/eval str))

;;; Get the Lisp source of a function.
(define (source x)
  (get-field fsource x))

;;; Whether a function has Lisp source.
(define (source? x)
  (and (not (undefined? x))
       (not (undefined? (get-field fsource x)))))

;;; Map the function `f` over the rose tree-wrapped
;;; S-expression `node`. The S-expression is processed
;;; in bottom-up order.
(define (map-syntax f
                    node
                    (env (new LispEnvironment))
                    (stack '())
                    (bindings (new LispEnvironment)))
  ;; TODO: Rename to `map-syntax`.
  (cond
   ((not (syntax? node))
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
    (let ((exp (syntax->datum node)))
      (macro-call? exp env)))
  (define visit-macro-call visit-node)
  ;; Special form.
  (define (visit-special-form-p node)
    (let ((exp (syntax->datum node)))
      (special-form? exp env)))
  (define visit-special-form visit-node)
  ;; Function call.
  (define (visit-function-call-p node)
    (let ((exp (syntax->datum node)))
      (function-call? exp env)))
  (define visit-function-call visit-nonatomic)
  (define (visit-else-p node)
    #t)
  (define (visit-forms-node-with visitor node stack bindings (skip 0))
    (define exp (syntax->datum node))
    (unless (pair-or-list? exp)
      ;; `node` is not a list expression; early return.
      (return (visit visitor node stack bindings)))
    (define nodes (syntax->list node))
    (define result-nodes
      (visit-forms-list-with visitor nodes stack bindings skip))
    (cond
     ((eq? result-nodes nodes)
      node)
     (else
      (define exp '())
      (define result
        (transfer-comments node (datum->syntax #f exp)))
      (for ((node result-nodes))
        (push-right! exp (syntax->datum node))
        (send result insert node))
      result)))
  (define (visit-forms-list-with visitor nodes stack bindings (skip 0))
    (unless (pair-or-list? nodes)
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
    (define bindings-2
      (extend-environment (new LispEnvironment)
                          bindings))
    (define sym
      (~> node
          (send _ get 0)
          (syntax->datum _)))
    (define let-bindings-env (send node get 1))
    (define body (send node drop 2))
    (for ((let-binding (syntax->datum let-bindings-env)))
      (define binding-sym
        (if (pair-or-list? let-binding)
            (first let-binding)
            let-binding))
      (make-type-binding bindings-2 binding-sym 'Any))
    (define visited-let-bindings-env
      (visit-clauses-node let-bindings-env `(,@stack ,node) bindings-2))
    (define visited-body
      (visit-forms-list body `(,@stack ,node) bindings-2))
    (unless (and (eq? let-bindings-env visited-let-bindings-env)
                 (eq? body visited-body))
      (set! result (transfer-comments
                    node
                    (datum->syntax
                     #f
                     `(,sym ,visited-let-bindings-env
                            ,@visited-body)))))
    (f result stack bindings))
  (define (visit-let-values-p node)
    (form? node let-values_ env))
  (define (visit-let-values node stack bindings)
    (define result node)
    (define bindings-2
      (extend-environment (new LispEnvironment) bindings))
    (define sym
      (~> node
          (send _ get 0)
          (syntax->datum _)))
    (define let-bindings-env (send node get 1))
    (define body (send node drop 2))
    (define visited-let-bindings-env
      (visit-forms-node-with
       (lambda (x)
         (define x-result x)
         (define ids (send x get 0))
         (define val (send x get 1))
         (define ids-exp (syntax->datum ids))
         (cond
          ((symbol? ids-exp)
           (make-type-binding bindings-2 ids-exp 'Any))
          (else
           (for ((let-binding ids-exp))
             (when (symbol? let-binding)
               (make-type-binding bindings-2 let-binding 'Any)))))
         (define visited-ids
           (visit-forms-node ids `(,@stack ,node) bindings-2))
         (define visited-val
           (visit visitor val `(,@stack ,node) bindings-2))
         (unless (and (eq? visited-ids ids)
                      (eq? visited-val val))
           (set! x-result (transfer-comments
                           x
                           (datum->syntax
                            #f
                            `(,visited-ids
                              ,visited-val)))))
         x-result)
       let-bindings-env
       `(,@stack ,node)
       bindings-2))
    (define visited-body
      (visit-forms-list body `(,@stack ,node) bindings-2))
    (unless (and (eq? let-bindings-env visited-let-bindings-env)
                 (eq? body visited-body))
      (set! result (transfer-comments
                    node
                    (datum->syntax
                     #f
                     `(,sym ,visited-let-bindings-env
                            ,@visited-body)))))
    (f result stack bindings))
  ;; `(for ...)` form.
  (define (visit-for-p node)
    (form? node for_ env))
  (define visit-for visit-let)
  ;; `(while ...)` form.
  (define (visit-while-p node)
    (form? node js/while_ env))
  (define visit-while visit-function-call)
  ;; `(cond ...)` form.
  (define (visit-cond-p node)
    (form? node cond_ env))
  (define (visit-cond node stack bindings)
    (define result node)
    (define sym
      (~> node
          (send _ get 0)
          (syntax->datum _)))
    (define clauses (send node drop 1))
    (define visited-clauses
      (visit-clauses-list clauses `(,@stack ,node) bindings))
    (unless (eq? visited-clauses clauses)
      (set! result (transfer-comments
                    node
                    (datum->syntax
                     #f
                     `(,sym ,@visited-clauses)))))
    (f result stack bindings))
  ;; `(lambda ...)` form.
  (define (visit-lambda-p node)
    (or (form? node lambda_ env)
        (form? node js/function_ env)
        (form? node js/arrow_ env)))
  (define (visit-lambda node stack bindings)
    (define result node)
    (define bindings-2
      (extend-environment (new LispEnvironment)
                          bindings))
    (define sym
      (~> node
          (send _ get 0)
          (syntax->datum _)))
    (define params (send node get 1))
    (define params-exp (syntax->datum params))
    (define body (send node drop 2))
    (cond
     ((symbol? params-exp)
      (make-type-binding bindings-2 params-exp 'Any))
     (else
      (for ((param params-exp))
        (when (pair-or-list? param)
          (set! param (first param)))
        (make-type-binding bindings-2 param 'Any))))
    (define visited-params
      (visit-clauses-node params `(,@stack ,node) bindings-2))
    (define visited-body
      (visit-forms-list body `(,@stack ,node) bindings-2))
    (unless (and (eq? params visited-params)
                 (eq? body visited-body))
      (set! result (transfer-comments
                    node
                    (datum->syntax
                     #f
                     `(,sym ,visited-params
                            ,@visited-body)))))
    (f result stack bindings))
  ;; `(define ...)` form.
  (define (visit-define-p node)
    (form? node define_ env))
  (define (visit-define node stack bindings)
    (define result node)
    (define define-sym
      (~> node
          (send _ get 0)
          (syntax->datum _)))
    (define id (send node get 1))
    (define id-exp (syntax->datum id))
    (define id-sym
      (if (pair-or-list? id-exp)
          (first id-exp)
          id-exp))
    (define bindings-2 bindings)
    (cond
     ((pair-or-list? id-exp)
      (make-type-binding bindings id-sym '(-> Any * Any))
      (for ((param (rest id-exp)))
        (when (pair-or-list? param)
          (set! param (first param)))
        (make-type-binding bindings param 'Any))
      (set! bindings-2
            (extend-environment (new LispEnvironment)
                                bindings)))
     (else
      (make-type-binding bindings id-sym 'Any)))
    (define body (send node drop 2))
    (define visited-id
      (if (pair-or-list? id-exp)
          (visit-clauses-node id `(,@stack ,node) bindings-2)
          (visit-node id `(,@stack ,node) bindings-2)))
    (define visited-body
      (visit-forms-list body `(,@stack ,node) bindings-2))
    (unless (and (eq? id visited-id)
                 (eq? body visited-body))
      (set! result (transfer-comments
                    node
                    (datum->syntax
                     #f
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
    (define defmacro-sym
      (~> node
          (send _ get 0)
          (syntax->datum _)))
    (define id (send node get 1))
    (define id-sym (syntax->datum id))
    (define params (send node get 2))
    (define params-exp (syntax->datum params))
    (define body (send node drop 3))
    (make-type-binding bindings id-sym '(macro-> Any * Any))
    (define bindings-2
      (extend-environment (new LispEnvironment) bindings))
    (cond
     ((symbol? params-exp)
      (make-type-binding bindings-2 params-exp 'Any))
     (else
      (for ((param (flatten_ params-exp)))
        (make-type-binding bindings-2 params 'Any))))
    (define visited-id
      (visit-node id `(,@stack ,node) bindings-2))
    (define visited-params
      (visit-forms-node params `(,@stack ,node) bindings-2))
    (define visited-body
      (visit-forms-list body `(,@stack ,node) bindings-2))
    (unless (and (eq? id visited-id)
                 (eq? params visited-params)
                 (eq? body visited-body))
      (set! result (transfer-comments
                    node
                    (datum->syntax
                     #f
                     `(,defmacro-sym
                        ,visited-id
                        ,visited-params
                        ,@visited-body)))))
    (set! result (f result stack bindings))
    (make-type-binding bindings id-sym '(macro-> Any * Any))
    result)
  ;; `(define-macro ...)` form.
  (define (visit-define-macro-p node)
    (form? node define-macro_ env))
  (define (visit-define-macro node stack bindings)
    (define result node)
    (define define-macro-sym
      (~> node
          (send _ get 0)
          (syntax->datum _)))
    (define name-and-args (send node get 1))
    (define name-and-args-exp (syntax->datum name-and-args))
    (define id-sym (car name-and-args-exp))
    (define id (datum->syntax name-and-args id-sym))
    (define params-exp (cdr name-and-args-exp))
    (define params (datum->syntax name-and-args params-exp))
    (define body (send node drop 2))
    (make-type-binding bindings id-sym '(macro-> Any * Any))
    (define bindings-2
      (extend-environment (new LispEnvironment) bindings))
    (cond
     ((symbol? params-exp)
      (make-type-binding bindings-2 params-exp 'Any))
     (else
      (for ((param (flatten_ params-exp)))
        (make-type-binding bindings-2 params 'Any))))
    (define visited-id
      (visit-node id `(,@stack ,node) bindings-2))
    (define visited-params
      (visit-forms-node params `(,@stack ,node) bindings-2))
    (define visited-body
      (visit-forms-list body `(,@stack ,node) bindings-2))
    (unless (and (eq? id visited-id)
                 (eq? params visited-params)
                 (eq? body visited-body))
      (set! result (transfer-comments
                    node
                    (datum->syntax
                     #f
                     `(,define-macro-sym
                        ,(cons visited-id visited-params)
                        ,@visited-body)))))
    (set! result (f result stack bindings))
    (make-type-binding bindings id-sym '(macro-> Any * Any))
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
  ;; `(new ...)` form.
  (define (visit-new-p node)
    (form? node new_ env))
  (define visit-new visit-function-call)
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
                      (datum->syntax
                       #f
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
    (let ((exp (syntax->datum node)))
      (pair-or-list? exp)))
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
       (,visit-new-p ,visit-new)
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
               (let* ((exp (syntax->datum x))
                      (stack1 (map (lambda (x)
                                     (if (syntax? x)
                                         (syntax->datum x)
                                         x))
                                   stack))
                      (result (f exp stack1 bindings)))
                 (if (eq? result exp)
                     x
                     (datum->syntax x result)))))
         (is-rose (syntax? exp))
         (node (if is-rose
                   exp
                   (datum->syntax #f exp)))
         (result (map-syntax f1 node env stack bindings)))
    ;; If the input is a rose tree node,
    ;; return a rose tree node as output too.
    (if is-rose
        result
        (syntax->datum result))))

;;; Call the function `f` on each node of a rose tree,
;;; but do not create a new rose tree in the process.
(define (iterate-rose f node (env (new LispEnvironment)))
  (map-syntax (lambda (x stack)
                (f x stack)
                x)
              node
              env))

;;; Expand an `(ann ...)` expression.
(define-macro (ann_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(: ...)` expression.
(define-macro (colon_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(define-type ...)` expression.
(define-macro (define-type_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(let-fields ...)` expression.
(define-macro (let-fields_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(define-fields ...)` expression.
(define-macro (define-fields_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(set!-fields ...)` expression.
(define-macro (set-fields_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Compile a `(js/switch ...)` form.
(define (compile-js/switch node env (options (js/obj)))
  (define expression-type
    (oget options :expression-type))
  (cond
   ((eq? expression-type "expression")
    (compile-expression
     (make-iife node)
     env options))
   (else
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
                   (send _ get 0)
                   (syntax->datum _)))
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
               (define has-break
                 (form? (last consequent) break_ env))
               ;; It is advisable to wrap cases in a block statement.
               ;; <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/switch#lexical_scoping>
               (define consequent-block
                 (datum->syntax
                  x
                  `(js/block ,@consequent)))
               (set! consequent-compiled
                     (list
                      (if has-break
                          (compile-statement-or-return-statement
                           consequent-block env options)
                          (compile-statement
                           consequent-block env options)))))
              (else
               (set! test-compiled #n)
               (define consequent
                 (send x drop 1))
               (set! consequent-compiled
                     (list
                      (compile-statement-or-return-statement
                       (datum->syntax
                        #f
                        `(js/block ,@consequent))
                       env options)))))
             (new SwitchCase
                  test-compiled
                  consequent-compiled))
           cases))
    (new SwitchStatement
         discriminant-compiled
         cases-compiled))))

;;; Expand a `(js/switch ...)` expression.
(define-macro (js/switch_ &whole exp &environment env)
  ;; TODO: Convert to fexpr.
  (compile-sexp
   exp
   env
   (current-compilation-options)))

;;; Expand a `(field-bound? ...)` expression.
(define-macro (field-bound?_ id obj)
  (define prop
    (make-identifier-string
     (symbol->string id)
     (current-compilation-options)))
  (cond
   ((symbol? obj)
    `(and ,obj
          (js/in ,prop ,obj)))
   (else
    (define obj-sym
      (gensym "obj"))
    `(let ((,obj-sym ,obj))
       (and ,obj-sym
            (js/in ,prop ,obj-sym))))))

;;; Simple `call-with-current-continuation` implementation.
;;; Also known as `call/cc`.
;;;
;;; Similar to
;;; [`call-with-current-continuation` in Racket][rkt:call-with-current-continuation].
;;;
;;; [rkt:call-with-current-continuation]: https://docs.racket-lang.org/reference/cont.html#%28def._%28%28quote._~23~25kernel%29._call-with-current-continuation%29%29
(define (call-with-current-continuation_ proc (prompt-tag #u))
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
                         (enter #u)
                         (leave #u)
                         (replace #u))
  (define result node)
  (define el)
  (define el1)
  (define val)
  (define val1)
  (unless (is-a? node Node)
    (return result))
  (when enter
    (enter node))
  (for ((key (js/keys node)))
    (set! val (oget node key))
    (cond
     ((pair-or-list? val)
      (for ((i (range 0 (length val))))
        (set! el (list-ref val i))
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
   ((syntax? exp)
    (optimize-syntax exp env))
   (else
    (~> exp
        (datum->syntax #f _)
        (optimize-syntax _ env)
        (syntax->datum _)))))

;;; Optimize a rose tree-wrapped S-expression.
(define (optimize-syntax exp env)
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
      (let-vars->const-vars)))

(define (let-vars->const-vars program)
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
   #u
   #u
   (lambda (node)
     (cond
      ((estree-type? node "VariableDeclaration")
       (unless (findf
                (lambda (x)
                  (or (not (get-field init x))
                      (not (zero?
                            (length
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

  (define/public interpretation-environment)

  (define/public module-map)

  (define/public symbol-map (make-hash))

  (define/public (constructor (nodes '())
                              (parent lang-environment)
                              (name ""))
    (set-field! parent-environment this parent)
    (set-field! name this name)
    (send this initialize-nodes nodes))

  (define/public (get-continuation-env)
    (new LispEnvironment
         '()
         (send this get-environment)))

  (define/public (get-expressions)
    (get-field expressions this))

  (define/public (get-environment)
    (cond
     ((get-field environment this)
      (get-field environment this))
     (else
      (send this
            make-environment
            (get-field parent-environment this)))))

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
      (define initial-node-comment-string #u)
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
                (drop-right comment-strings 1))
          (set! initial-node-comment-string
                (last comment-strings))
          (when (regexp-match (regexp "\\n\\n$")
                              initial-node-comment-string)
            (push-right! header-comment-strings
                         initial-node-comment-string)
            (set! initial-node-comment-string #u))
          (when (> (length header-comment-strings) 0)
            (list-set! header-comment-strings
                       (- (length header-comment-strings) 1)
                       (regexp-replace
                        (regexp "\\n*$")
                        (list-ref header-comment-strings
                                  (- (length header-comment-strings) 1))
                        "")))))
      (when (> (length header-comment-strings) 0)
        (define header-exp
          '(begin))
        (define header-node
          (datum->syntax #f header-exp))
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
       ((syntax? node)
        (set! exp (syntax->datum node))
        (define comments
          (send node get-property "comments"))
        (when comments
          ;; Look for `inline-lisp-sources: true` magic comment.
          (send this find-inline-lisp-sources-comment comments)))
       (else
        (set! exp node)
        (set! node (datum->syntax #f exp))))
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
      (set! exp (syntax->datum node))
      (cond
       ((and (tagged-list? exp 'require)
             (> (length exp) 1)
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
           ((pair-or-list? x)
            (send (get-field symbol-map this) set (cadr x) #t))
           (else
            (send (get-field symbol-map this) set x #t)))))
       ((and (tagged-list? exp 'require)
             (> (length exp) 1))
        (let* ((module-name-symbol (last exp))
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
      (set! exp (syntax->datum node))
      (when (or (tagged-list? exp 'define)
                (tagged-list? exp 'define-class))
        (define name
          (if (pair-or-list? (second exp))
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

  (define/public (make-environment (parent #u))
    (define module-env
      (new LispEnvironment '() parent))
    (define module-interpretation-env
      (new EnvironmentStack
           module-env
           js/environment))
    (define imported)
    (define local)
    (define module)
    (define env)
    (define module-name)
    (set-field! parent-environment this parent)
    (set-field! environment this module-env)
    (set-field! interpretation-environment
                this
                module-interpretation-env)
    ;; Iterate over `require-nodes`, importing definitions
    ;; from other modules.
    (for ((node (get-field require-nodes this)))
      (define exp
        (syntax->datum node))
      (cond
       ((and (tagged-list? exp 'require)
             (> (length exp) 1)
             (tagged-list? (second exp) 'only-in))
        (set! module-name (second (second exp)))
        (when (symbol? module-name)
          (set! module-name
                (symbol->string module-name)))
        (set! module-name
              (regexp-replace (regexp "^\\./")
                              module-name
                              ""))
        (cond
         ((and (get-field module-map this)
               (send (get-field module-map this) has module-name))
          (set! module (send (get-field module-map this) get module-name))
          (set! env (send module get-environment)))
         (else
          (set! env #u)))
        (for ((exp1 (drop (second exp) 2)))
          (cond
           ((pair-or-list? exp1)
            (set! local (first exp1))
            (set! imported (second exp1)))
           (else
            (set! local exp1)
            (set! imported exp1)))
          (make-type-binding module-env imported 'Any)
          (when env
            (define-values (f f-type)
              (send env get-typed-value local))
            (unless (undefined-type? f-type)
              (send module-env set-local! imported f f-type)))))))
    ;; Iterate over `main-nodes`, evaluating definition forms
    ;; in the module environment.
    (for ((node (get-field main-nodes this)))
      (define exp
        (syntax->datum node))
      (cond
       ((or (definition? exp)
            (macro-definition? exp))
        ;; Evaluate `define` and `defmacro` forms in the module
        ;; environment. Be error-tolerant since the module
        ;; environment is not needed in many cases.
        (define name
          (second exp))
        (when (pair-or-list? name)
          (set! name (first name)))
        (define typ
          (if (macro-definition? exp)
              '(macro-> Any * Any)
              '(-> Any * Any)))
        (send module-env
              set-local!
              name
              (new InternalPromise
                   (delay
                     (define result #u)
                     (try
                       (define begin-exp
                         `(begin ,exp ,name))
                       (set! result
                             (interpret begin-exp
                                        module-interpretation-env))
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
                       (syntax->datum x))
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
    (new PromiseMap))
  (for ((key (send module-expression-map keys)))
    (send module-map
          set
          key
          (delay
            (define val
              (send module-expression-map get key))
            (define m
              (if (is-a? val Module)
                  val
                  (module-expression->module-object
                   val env)))
            (send m set-module-map module-map)
            m)))
  module-map)

;;; Convert a `(module ...)` expression to a
;;; `Module` object.
(define (module-expression->module-object node env)
  (define name
    (~> node
        (send _ get 1)
        (syntax->datum _)))
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
           (send env has-environment? lisp-environment))))

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
  (let ((result #u)
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
       (or (pair? (second exp))
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

;;; Parse a parameter list into regular parameters
;;; and rest parameter, if any.
(define (parse-params-list params)
  (define regular-params '())
  (define rest-param #u)
  (cond
   ((symbol? params)
    (set! rest-param params))
   ((dotted-list? params)
    (set! regular-params (dotted-list-head params))
    (set! rest-param (dotted-list-tail params)))
   (else
    (set! regular-params params)))
  (values regular-params rest-param))

;;; Make a type binding for `sym` in `env`,
;;; which should be a typed environment.
(define (make-type-binding env sym typ (filter #u))
  (cond
   ((send env has? sym (js/obj :filter filter))
    (send env set-type! sym typ))
   (else
    (send env set-local! sym #u typ))))

;;; Whether `x` is a simple type whose function call
;;; can be compiled without further ado.
(define (simple-type? x)
  (and (not (macro-type? x))
       (not (fexpr-type? x))))

;;; Parse the value of the `ftype` spec.
(define (parse-ftype x)
  (cond
   ((eq? x "macro")
    '(macro-> Any * Any))
   ((eq? x "fexpr")
    '(fexpr-> Any * Any))
   (else
    x)))

;;; Normalize an options args list.
(define (normalize-options args)
  (cond
   ;; If the args list contains a single object,
   ;; just use that.
   ((= (length args) 1)
    (first args))
   (else
    ;; Otherwise, treat the args list as a property list
    ;; and convert that to an object.
    (~> args
        (plist-map_
         (lambda (entry)
           (define-values (prop val)
             entry)
           (when (symbol? val)
             (set! val
                   (symbol->string val)))
           (values prop val))
         _)
        (plist->object_
         _
         (js/obj :case "camelcase"))))))

;;; Whether `exp` is a quoted expression.
(define (quoted-expression? exp)
  (or (tagged-list? exp 'quote)
      (tagged-list? exp 'quasiquote)))

;;; Set the type of the ESTree node `node` to `typ`.
(define (set-type node typ)
  (cond
   ((promise? node)
    (delay
      (set-type (force node) typ)))
   (else
    (send node set-type typ)
    node)))

;;; Interactive definition of `load`.
(define (load_ file)
  (define file-path
    (if (symbol? file)
        (symbol->string file)
        file))
  (define file-contents
    (readFileSync file-path
                  (js/obj :encoding "utf8")))
  (define exp
    (~> file-contents
        (string-append "(module m scheme\n" _ "\n)")
        (read-syntax _)))
  (interpret exp))

;;; Rewrite `[...[x]]` to `[x]`, recursively.
(define (insert-into-array-exp! x arr)
  (cond
   ((and (estree-type? x "SpreadElement")
         (estree-type? (get-field argument x)
                       "ArrayExpression"))
    (define elements
      (~> x
          (get-field argument _)
          (get-field elements _)))
    (for ((y elements))
      (insert-into-array-exp! y arr)))
   ((estree-type? arr "ArrayExpression")
    (insert-into-array-exp! x (get-field elements arr)))
   (else
    (push-right! arr x))))

;;; Normalize an array expression like `[1, Symbol.for('.'), []]'
;;; to `[1]`.
(define (normalize-array-expression exp)
  ;; TODO: Pattern matching for ESTree nodes.
  ;; TODO: Handle `quote`, `quasiquote`,
  ;; `unquote`, `unquote-splicing`.
  (define (array-exp? exp)
    (estree-type? exp "ArrayExpression"))
  (define (dotted-array-exp? exp)
    (and (estree-type? exp "ArrayExpression")
         (>= (length (get-field elements exp)) 3)
         (cons-dot-exp?
          (list-ref (get-field elements exp)
                    (- (length (get-field elements exp)) 2)))
         (estree-type?
          (list-ref (get-field elements exp)
                    (- (length (get-field elements exp)) 1))
          "ArrayExpression")))
  (define (cons-dot-exp? exp)
    (and (estree-type? exp "CallExpression")
         (estree-type? (get-field callee exp)
                       "MemberExpression")
         (estree-type? (get-field object
                                  (get-field callee exp))
                       "Identifier")
         (eq? (get-field name (get-field object
                                         (get-field callee exp)))
              "Symbol")
         (estree-type? (get-field property
                                  (get-field callee exp))
                       "Identifier")
         (estree-type? (get-field property
                                  (get-field callee exp))
                       "Identifier")
         (eq? (get-field name
                         (get-field property
                                    (get-field callee exp)))
              "for")
         (not (get-field computed exp))
         (= (length (get-field arguments exp)) 1)
         (estree-type? (first (get-field arguments exp))
                       "Literal")
         (eq? (get-field value
                         (first (get-field arguments exp)))
              ".")))
  (define result exp)
  (when (array-exp? exp)
    (define normalized-subarrays #f)
    (define elements
      (map (lambda (x)
             (define result
               (normalize-array-expression x))
             (unless (eq? result x)
               (set! normalized-subarrays #t))
             result)
           (get-field elements exp)))
    (cond
     ((dotted-array-exp? exp)
      (define tail
        (last (get-field elements exp)))
      (when (array-exp? tail)
        (set! result
              (new ArrayExpression
                   `(,@(drop-right elements 2)
                     ,@(get-field elements tail))))))
     (normalized-subarrays
      (set! result
            (new ArrayExpression elements))))
    result)
  result)

;;; Lisp environment.
(define lisp-environment
  (new LispEnvironment
       `(
         ;; Constants.
         (_ ,__ Any)
         (__ ,__ Any)
         (,(string->symbol "#f") ,false_ Any)
         (,(string->symbol "#t") ,true_ Any)
         (,(string->symbol "#n") ,js/null_ Any)
         (,(string->symbol "#u") ,undefined_ Any)
         (false ,false_ Any)
         (,(string->symbol "nil") ,null_ Any)
         (null ,null_ Any)
         (js/null ,js/null_ Any)
         (js-null ,js/null_ Any)
         (,(string->symbol "t") ,true_ Any)
         (true ,true_ Any)
         (js-undefined ,undefined_ Any)
         (js/undefined ,undefined_ Any)
         (undefined ,undefined_ Any)
         (license ,license Any)
         (Rose ,Syntax Any)
         (Syntax ,Syntax Any)
         ;; Procedures.
         ($ ,funcall_ (-> Any * Any))
         (% ,modulo_ (-> Any * Any))
         (* ,mul_ (-> Any * Any))
         (+ ,add_ (-> Any * Any))
         (- ,sub_ (-> Any * Any))
         (/ ,div_ (-> Any * Any))
         (< ,lt_ (-> Any * Any))
         (<= ,lte_ (-> Any * Any))
         (= ,eq?_ (-> Any * Any))
         (=? ,eq?_ (-> Any * Any))
         (> ,gt_ (-> Any * Any))
         (>= ,gte_ (-> Any * Any))
         (abs ,abs_ (-> Any * Any))
         (add ,add_ (-> Any * Any))
         (add1 ,add1_ (-> Any * Any))
         (aget ,list-ref_ (-> Any * Any))
         (append ,append_ (-> Any * Any))
         (apply ,apply_ (-> Any * Any))
         (aref ,array-ref_ (-> Any * Any))
         (arity ,arity_ (-> Any * Any))
         (array-at ,array-at_ (-> Any * Any))
         (array-concat ,array-concat_ (-> Any * Any))
         (array-copy ,array-copy_ (-> Any * Any))
         (array-drop ,array-drop_ (-> Any * Any))
         (array-drop-right ,array-drop-right_ (-> Any * Any))
         (array-eighth ,array-eighth_ (-> Any * Any))
         (array-fifth ,array-fifth_ (-> Any * Any))
         (array-first ,array-first_ (-> Any * Any))
         (array-fourth ,array-fourth_ (-> Any * Any))
         (array-get ,array-ref_ (-> Any * Any))
         (array-last ,array-last_ (-> Any * Any))
         (array-length ,array-length_ (-> Any * Any))
         (array-ninth ,array-ninth_ (-> Any * Any))
         (array-nlast ,array-nlast_ (-> Any * Any))
         (array-pop-left ,array-pop-left!_ (-> Any * Any))
         (array-pop-left! ,array-pop-left!_ (-> Any * Any))
         (array-pop-right ,array-pop-right!_ (-> Any * Any))
         (array-pop-right! ,array-pop-right!_ (-> Any * Any))
         (array-push-left! ,array-push-left!_ (-> Any * Any))
         (array-push-right! ,array-push-right!_ (-> Any * Any))
         (array-ref ,array-ref_ (-> Any * Any))
         (array-rest ,array-rest_ (-> Any * Any))
         (array-reverse ,array-reverse_ (-> Any * Any))
         (array-reverse! ,array-reverse!_ (-> Any * Any))
         (array-second ,array-second_ (-> Any * Any))
         (array-set ,array-set_ (-> Any * Any))
         (array-set! ,array-set!_ (-> Any * Any))
         (array-seventh ,array-seventh_ (-> Any * Any))
         (array-sixth ,array-sixth_ (-> Any * Any))
         (array-slice ,array-slice_ (-> Any * Any))
         (array-take ,array-take_ (-> Any * Any))
         (array-tenth ,array-tenth_ (-> Any * Any))
         (array-third ,array-third_ (-> Any * Any))
         (array? ,array?_ (-> Any * Any))
         (aset ,array-set_ (-> Any * Any))
         (aset! ,array-set!_ (-> Any * Any))
         (assert ,assert_ (-> Any * Any))
         (atom? ,atom?_ (-> Any * Any))
         (bit-and ,js/bitwise-and_ (-> Any * Any))
         (bit-not ,js/bitwise-not_ (-> Any * Any))
         (bit-or ,js/bitwise-or_ (-> Any * Any))
         (bit-shift-left ,js/bitwise-shift-left_ (-> Any * Any))
         (bit-shift-right ,js/bitwise-shift-right_ (-> Any * Any))
         (bit-xor ,js/bitwise-xor_ (-> Any * Any))
         (bitwise-and ,js/bitwise-and_ (-> Any * Any))
         (bitwise-negation ,js/bitwise-not_ (-> Any * Any))
         (bitwise-not ,js/bitwise-not_ (-> Any * Any))
         (bitwise-or ,js/bitwise-or_ (-> Any * Any))
         (bitwise-shift-left ,js/bitwise-shift-left_ (-> Any * Any))
         (bitwise-shift-right ,js/bitwise-shift-right_ (-> Any * Any))
         (bitwise-xor ,js/bitwise-xor_ (-> Any * Any))
         (boolean? ,boolean?_ (-> Any * Any))
         (booleanp ,boolean?_ (-> Any * Any))
         (build-list ,build-list_ (-> Any * Any))
         (cadr ,cadr_ (-> Any * Any))
         (call-cc ,call-with-current-continuation_ (-> Any * Any))
         (call-with-current-continuation ,call-with-current-continuation_ (-> Any * Any))
         (call/cc ,call-with-current-continuation_ (-> Any * Any))
         (car ,car_ (-> Any * Any))
         (cdr ,cdr_ (-> Any * Any))
         (circular-list-p ,circular-list?_ (-> Any * Any))
         (circular-list? ,circular-list?_ (-> Any * Any))
         (cl/listp ,pair-or-list?_ (-> Any * Any))
         (compile ,compile (-> Any * Any))
         (cons ,cons_ (-> Any * Any))
         (cons* ,list-star_ (-> Any * Any))
         (cons-or-list? ,pair-or-list?_ (-> Any * Any))
         (cons? ,pair?_ (-> Any * Any))
         (console.log ,(get-field log console) (-> Any * Any))
         (consp ,pair?_ (-> Any * Any))
         (const ,const_ (-> Any * Any))
         (constantly ,const_ (-> Any * Any))
         (current-environment ,current-environment_ (-> Any * Any))
         (curry ,curry (-> Any * Any))
         (curry-n ,curry-n (-> Any * Any))
         (datum->syntax ,datum->syntax (-> Any * Any))
         (decompile ,decompile (-> Any * Any))
         (delete ,js/delete_ (-> Any * Any))
         (display ,display_ (-> Any * Any))
         (div ,div_ (-> Any * Any))
         (dotted-improper-list? ,dotted-improper-list?_ (-> Any * Any))
         (dotted-list->list ,dotted-list->list_ (-> Any * Any))
         (dotted-list->list ,dotted-list->list_ (-> Any * Any))
         (dotted-list->proper-list ,dotted-list->list_ (-> Any * Any))
         (dotted-list-car ,car_ (-> Any * Any))
         (dotted-list-cdr ,cdr_ (-> Any * Any))
         (dotted-list-eighth ,dotted-list-eighth_ (-> Any * Any))
         (dotted-list-fifth ,dotted-list-fifth_ (-> Any * Any))
         (dotted-list-first ,dotted-list-first_ (-> Any * Any))
         (dotted-list-fourth ,dotted-list-fourth_ (-> Any * Any))
         (dotted-list-head ,dotted-list-head_ (-> Any * Any))
         (dotted-list-last ,dotted-list-last_ (-> Any * Any))
         (dotted-list-last-cdr ,dotted-list-last-cdr_ (-> Any * Any))
         (dotted-list-last-pair ,dotted-list-last-cdr_ (-> Any * Any))
         (dotted-list-length ,dotted-list-length_ (-> Any * Any))
         (dotted-list-link ,dotted-list-link_ (-> Any * Any))
         (dotted-list-ninth ,dotted-list-ninth_ (-> Any * Any))
         (dotted-list-nth ,dotted-list-nth_ (-> Any * Any))
         (dotted-list-nthcdr ,dotted-list-nthcdr_ (-> Any * Any))
         (dotted-list-p ,dotted-list?_ (-> Any * Any))
         (dotted-list-parse ,dotted-list-parse_ (-> Any * Any))
         (dotted-list-ref ,dotted-list-ref_ (-> Any * Any))
         (dotted-list-second ,dotted-list-second_ (-> Any * Any))
         (dotted-list-set ,dotted-list-set_ (-> Any * Any))
         (dotted-list-set! ,dotted-list-set!_ (-> Any * Any))
         (dotted-list-seventh ,dotted-list-seventh_ (-> Any * Any))
         (dotted-list-sixth ,dotted-list-sixth_ (-> Any * Any))
         (dotted-list-tail ,dotted-list-tail_ (-> Any * Any))
         (dotted-list-tenth ,dotted-list-tenth_ (-> Any * Any))
         (dotted-list-third ,dotted-list-third_ (-> Any * Any))
         (dotted-list? ,dotted-list?_ (-> Any * Any))
         (dotted-pair-car ,car_ (-> Any * Any))
         (dotted-pair-cdr ,dotted-pair-cdr_ (-> Any * Any))
         (dotted-pair-p ,dotted-pair?_ (-> Any * Any))
         (dotted-pair? ,dotted-pair?_ (-> Any * Any))
         (dotted-proper-list? ,dotted-proper-list?_ (-> Any * Any))
         (drop ,drop_ (-> Any * Any))
         (drop-right ,drop-right_ (-> Any * Any))
         (eighth ,eighth_ (-> Any * Any))
         (el/listp ,pair-or-list?_ (-> Any * Any))
         (eq ,eq?_ (-> Any * Any))
         (eq? ,eq?_ (-> Any * Any))
         (eql ,eqv?_ (-> Any * Any))
         (eql? ,eqv?_ (-> Any * Any))
         (equal ,equal?_ (-> Any * Any))
         (equal? ,equal?_ (-> Any * Any))
         (eqv ,eqv?_ (-> Any * Any))
         (eqv? ,eqv?_ (-> Any * Any))
         (error ,error_ (-> Any * Any))
         (even? ,even?_ (-> Any * Any))
         (extend-environment ,extend-environment (-> Any * Any))
         (false? ,false?_ (-> Any * Any))
         (falsep ,false?_ (-> Any * Any))
         (fexpr? ,fexpr?_ (-> Any * Any))
         (fexprp ,fexpr?_ (-> Any * Any))
         (field-names ,field-names_ (-> Any * Any))
         (fifth ,fifth_ (-> Any * Any))
         (filter ,filter_ (-> Any * Any))
         (findf ,findf_ (-> Any * Any))
         (findf-index ,findf-index_ (-> Any * Any))
         (first ,first_ (-> Any * Any))
         (flatten ,flatten_ (-> Any * Any))
         (foldl ,foldl_ (-> Any * Any))
         (foldr ,foldr_ (-> Any * Any))
         (for-each ,for-each_ (-> Any * Any))
         (force ,force_ (-> Any * Any))
         (fourth ,fourth_ (-> Any * Any))
         (funcall ,funcall_ (-> Any * Any))
         (function-object? ,js/function-object?_ (-> Any * Any))
         (function-type? ,js/function-type?_ (-> Any * Any))
         (function? ,procedure?_ (-> Any * Any))
         (functionp ,procedure?_ (-> Any * Any))
         (gensym ,gensym_ (-> Any * Any))
         (gensym? ,gensym?_ (-> Any * Any))
         (get ,list-ref_ (-> Any * Any))
         (hash ,make-hash_ (-> Any * Any))
         (hash->list ,hash->list_ (-> Any * Any))
         (hash-clear ,hash-clear_ (-> Any * Any))
         (hash-clear! ,hash-clear!_ (-> Any * Any))
         (hash-copy ,hash-copy_ (-> Any * Any))
         (hash-entries ,hash-entries_ (-> Any * Any))
         (hash-has-key? ,hash-has-key?_ (-> Any * Any))
         (hash-keys ,hash-keys_ (-> Any * Any))
         (hash-ref ,hash-ref_ (-> Any * Any))
         (hash-remove ,hash-remove_ (-> Any * Any))
         (hash-remove! ,hash-remove!_ (-> Any * Any))
         (hash-set ,hash-set!_ (-> Any * Any))
         (hash-set! ,hash-set!_ (-> Any * Any))
         (hash-size ,hash-size_ (-> Any * Any))
         (hash-values ,hash-values_ (-> Any * Any))
         (hash? ,hash?_ (-> Any * Any))
         (head ,car_ (-> Any * Any))
         (id ,identity_ (-> Any * Any))
         (identity ,identity_ (-> Any * Any))
         (improper-list-p ,improper-list?_ (-> Any * Any))
         (improper-list? ,improper-list?_ (-> Any * Any))
         (in-range ,range_ (-> Any * Any))
         (index-of ,index-of_ (-> Any * Any))
         (index-where ,index-where_ (-> Any * Any))
         (instance-of ,is-a?_ (-> Any * Any))
         (instance-of? ,is-a?_ (-> Any * Any))
         (instanceof ,is-a?_ (-> Any * Any))
         (instanceof? ,is-a?_ (-> Any * Any))
         (intern ,string->symbol_ (-> Any * Any))
         (intersection ,intersection_ (-> Any * Any))
         (is-a? ,is-a?_ (-> Any * Any))
         (js ,js/raw_ (-> Any * Any))
         (js-field ,list-ref_ (-> Any * Any))
         (js-keys ,js/keys_ (-> Any * Any))
         (js-obj ,js/obj_ (-> Any * Any))
         (js-obj-append ,js/obj-append_ (-> Any * Any))
         (js-obj-keys ,js/keys_ (-> Any * Any))
         (js-obj? ,js/obj?_ (-> Any * Any))
         (js/! ,js/not_ (-> Any * Any))
         (js/% ,js/mod_ (-> Any * Any))
         (js/& ,js/bitwise-and_ (-> Any * Any))
         (js/&& ,js/and_ (-> Any * Any))
         (js/* ,mul_ (-> Any * Any))
         (js/+ ,add_ (-> Any * Any))
         (js/+ ,js/plus_ (-> Any * Any))
         (js/- ,sub_ (-> Any * Any))
         (js/. ,js/dot_ (-> Any * Any))
         (js// ,div_ (-> Any * Any))
         (js/< ,js/lt_ (-> Any * Any))
         (js/<< ,js/bitwise-shift-left_ (-> Any * Any))
         (js/<= ,js/lte_ (-> Any * Any))
         (js/== ,js/loosely-equal?_ (-> Any * Any))
         (js/=== ,js/strictly-equal?_ (-> Any * Any))
         (js/===? ,js/strictly-equal?_ (-> Any * Any))
         (js/==? ,js/loosely-equal?_ (-> Any * Any))
         (js/> ,js/gt_ (-> Any * Any))
         (js/>= ,js/gte_ (-> Any * Any))
         (js/>> ,js/bitwise-shift-right_ (-> Any * Any))
         (js/>>> ,js/unsigned-bitwise-shift-right_ (-> Any * Any))
         (js/?. ,js/optional-chaining_ (-> Any * Any))
         (js/\(\) ,funcall_ (-> Any * Any))
         (js/\[\] ,list-ref_ (-> Any * Any))
         (js/\| ,js/bitwise-or_ (-> Any * Any))
         (js/\|\| ,js/or_ (-> Any * Any))
         (js/^ ,js/bitwise-xor_ (-> Any * Any))
         (js/abs ,js/abs_ (-> Any * Any))
         (js/append ,js/plus_ (-> Any * Any))
         (js/array? ,js/array?_ (-> Any * Any))
         (js/arrow? ,js/arrow?_ (-> Any * Any))
         (js/console.log ,(get-field log console) (-> Any * Any))
         (js/delete ,js/delete_ (-> Any * Any))
         (js/eighth ,array-eighth_ (-> Any * Any))
         (js/field ,list-ref_ (-> Any * Any))
         (js/fifth ,array-fifth_ (-> Any * Any))
         (js/find-index ,js/find-index_ (-> Any * Any))
         (js/findf-index ,js/find-index_ (-> Any * Any))
         (js/first ,array-first_ (-> Any * Any))
         (js/fourth ,array-fourth_ (-> Any * Any))
         (js/function-object? ,js/function-object?_ (-> Any * Any))
         (js/function-type? ,js/function-type?_ (-> Any * Any))
         (js/function? ,js/function?_ (-> Any * Any))
         (js/get ,js/get_ (-> Any * Any))
         (js/in ,js/in_ (-> Any * Any))
         (js/instance-of ,js/instanceof_ (-> Any * Any))
         (js/instance-of? ,js/instanceof_ (-> Any * Any))
         (js/instanceof ,js/instanceof_ (-> Any * Any))
         (js/instanceof? ,js/instanceof_ (-> Any * Any))
         (js/is-loosely-equal? ,js/loosely-equal?_ (-> Any * Any))
         (js/is-strictly-equal? ,js/strictly-equal?_ (-> Any * Any))
         (js/js-obj ,js/obj_ (-> Any * Any))
         (js/js-obj-append ,js/obj-append_ (-> Any * Any))
         (js/js-obj? ,js/obj?_ (-> Any * Any))
         (js/keys ,js/keys_ (-> Any * Any))
         (js/last ,array-last_ (-> Any * Any))
         (js/length ,js/length_ (-> Any * Any))
         (js/nan? ,js/nan?_ (-> Any * Any))
         (js/new ,js/new_ (-> Any * Any))
         (js/ninth ,array-ninth_ (-> Any * Any))
         (js/nth ,nth_ (-> Any * Any))
         (js/null? ,js/null?_ (-> Any * Any))
         (js/obj ,js/obj_ (-> Any * Any))
         (js/obj-append ,js/obj-append_ (-> Any * Any))
         (js/obj-keys ,js/keys_ (-> Any * Any))
         (js/obj-spread ,js/obj-spread_ (-> Any * Any))
         (js/obj? ,js/obj?_ (-> Any * Any))
         (js/object ,js/obj_ (-> Any * Any))
         (js/object-type? ,js/object-type?_ (-> Any * Any))
         (js/object? ,js/object-type?_ (-> Any * Any))
         (js/promise ,js/promise_ (-> Any * Any))
         (js/promise? ,js/promise?_ (-> Any * Any))
         (js/raw ,js/raw_ (-> Any * Any))
         (js/reduce ,js/reduce_ (-> Any * Any))
         (js/reduce-right ,js/reduce-right_ (-> Any * Any))
         (js/regexp ,js/regexp_ (-> Any * Any))
         (js/regexp-match ,js/regexp-match_ (-> Any * Any))
         (js/regexp-quote ,regexp-quote_ (-> Any * Any))
         (js/regexp-replace ,js/regexp-replace_ (-> Any * Any))
         (js/regexp? ,js/regexp?_ (-> Any * Any))
         (js/rest ,array-rest_ (-> Any * Any))
         (js/return ,js/return_ (-> Any * Any))
         (js/reverse ,array-reverse_ (-> Any * Any))
         (js/same-value-zero? ,js/same-value-zero?_ (-> Any * Any))
         (js/same-value? ,js/same-value?_ (-> Any * Any))
         (js/second ,array-second_ (-> Any * Any))
         (js/seventh ,array-seventh_ (-> Any * Any))
         (js/sixth ,array-sixth_ (-> Any * Any))
         (js/slice ,js/slice_ (-> Any * Any))
         (js/source ,js/source_ (-> Any * Any))
         (js/string-append ,js/string-concat_ (-> Any * Any))
         (js/string-concat ,js/string-concat_ (-> Any * Any))
         (js/string-literal? ,js/string-literal?_ (-> Any * Any))
         (js/string-object? ,js/string-object?_ (-> Any * Any))
         (js/string? ,js/string?_ (-> Any * Any))
         (js/tag ,js/tagged-template_ (-> Any * Any))
         (js/tagged-template ,js/tagged-template_ (-> Any * Any))
         (js/take ,array-take_ (-> Any * Any))
         (js/tenth ,array-tenth_ (-> Any * Any))
         (js/third ,array-third_ (-> Any * Any))
         (js/to-string ,js/to-string_ (-> Any * Any))
         (js/type-of ,js/typeof_ (-> Any * Any))
         (js/typeof ,js/typeof_ (-> Any * Any))
         (js/yield ,yield_ (-> Any * Any))
         (js/~ ,js/bitwise-not_ (-> Any * Any))
         (keyword? ,keyword?_ (-> Any * Any))
         (keywordp ,keyword?_ (-> Any * Any))
         (last ,last_ (-> Any * Any))
         (last-cdr ,last-cdr_ (-> Any * Any))
         (last-cons ,last-pair_ (-> Any * Any))
         (last-pair ,last-pair_ (-> Any * Any))
         (last-pair ,last-pair_ (-> Any * Any))
         (length ,length_ (-> Any * Any))
         (length* ,length_ (-> Any * Any))
         (list ,list_ (-> Any * Any))
         (list* ,list-star_ (-> Any * Any))
         (list->dotted-list_ ,list->dotted-list_ (-> Any * Any))
         (list-or-cons? ,pair-or-list?_ (-> Any * Any))
         (list-or-pair? ,pair-or-list?_ (-> Any * Any))
         (list-ref ,list-ref_ (-> Any * Any))
         (list-set ,list-set_ (-> Any * Any))
         (list-set! ,list-set!_ (-> Any * Any))
         (list-star ,list-star_ (-> Any * Any))
         (list-tail ,list-tail_ (-> Any * Any))
         (list? ,list?_ (-> Any * Any))
         (listlike? ,pair-or-list?_ (-> Any * Any))
         (listp ,list?_ (-> Any * Any))
         (log ,(get-field log console) (-> Any * Any))
         (macro? ,macro?_ (-> Any * Any))
         (macroexpand ,macroexpand (-> Any * Any))
         (macroexpand* ,macroexpand* (-> Any * Any))
         (macroexpand*-1 ,macroexpand*-1 (-> Any * Any))
         (macroexpand-1 ,macroexpand-1 (-> Any * Any))
         (make ,js/new_ (-> Any * Any))
         (make-hash ,make-hash_ (-> Any * Any))
         (make-list ,make-list_ (-> Any * Any))
         (make-object ,js/new_ (-> Any * Any))
         (map ,map_ (-> Any * Any))
         (mapcar ,map_ (-> Any * Any))
         (member ,member_ (-> Any * Any))
         (member-p ,member?_ (-> Any * Any))
         (member? ,member?_ (-> Any * Any))
         (memberp ,member?_ (-> Any * Any))
         (memf ,memf_ (-> Any * Any))
         (memf? ,memf?_ (-> Any * Any))
         (memq ,memq_ (-> Any * Any))
         (memq? ,memq?_ (-> Any * Any))
         (mod ,modulo_ (-> Any * Any))
         (modulo ,modulo_ (-> Any * Any))
         (mul ,mul_ (-> Any * Any))
         (new ,js/new_ (-> Any * Any))
         (new* ,js/new_ (-> Any * Any))
         (ninth ,ninth_ (-> Any * Any))
         (not ,not_ (-> Any * Any))
         (nth ,nth_ (-> Any * Any))
         (nthcdr ,nthcdr_ (-> Any * Any))
         (null? ,null?_ (-> Any * Any))
         (nullp ,null?_ (-> Any * Any))
         (number->string ,number->string_ (-> Any * Any))
         (number? ,number?_ (-> Any * Any))
         (numberp ,number?_ (-> Any * Any))
         (object? ,js/obj?_ (-> Any * Any))
         (objectp ,js/obj?_ (-> Any * Any))
         (odd? ,odd?_ (-> Any * Any))
         (oget ,object-ref_ (-> Any * Any))
         (one? ,one?_ (-> Any * Any))
         (onep ,one?_ (-> Any * Any))
         (oref ,list-ref_ (-> Any * Any))
         (oset ,object-set!_ (-> Any * Any))
         (oset! ,object-set!_ (-> Any * Any))
         (pair-or-list? ,pair-or-list?_ (-> Any * Any))
         (pair? ,pair?_ (-> Any * Any))
         (parse ,read (-> Any * Any))
         (plist->alist ,plist->alist_ (-> Any * Any))
         (plist->object ,plist->object_ (-> Any * Any))
         (plist-copy ,plist-copy_ (-> Any * Any))
         (plist-get ,plist-get_ (-> Any * Any))
         (plist-has ,plist-has?_ (-> Any * Any))
         (plist-has? ,plist-has?_ (-> Any * Any))
         (plist-ref ,plist-get_ (-> Any * Any))
         (plist-set ,plist-set!_ (-> Any * Any))
         (plist-set! ,plist-set!_ (-> Any * Any))
         (plist? ,plist?_ (-> Any * Any))
         (pop ,pop-left!_ (-> Any * Any))
         (pop! ,pop-left!_ (-> Any * Any))
         (pop-left ,pop-left!_ (-> Any * Any))
         (pop-left! ,pop-left!_ (-> Any * Any))
         (pop-right ,pop-right!_ (-> Any * Any))
         (pop-right! ,pop-right!_ (-> Any * Any))
         (print ,print (-> Any * Any))
         (print-estree ,print-estree (-> Any * Any))
         (procedure? ,procedure?_ (-> Any * Any))
         (promise-forced? ,promise-forced?_ (-> Any * Any))
         (promise-running? ,promise-running?_ (-> Any * Any))
         ;; (promise-type? ,js/function-type?_ (-> Any * Any))
         (promise? ,promise?_ (-> Any * Any))
         (proper-list->dotted-list ,list->dotted-list_ (-> Any * Any))
         (proper-list-p ,proper-list?_ (-> Any * Any))
         (proper-list? ,proper-list?_ (-> Any * Any))
         (push ,push-left!_ (-> Any * Any))
         (push! ,push-left!_ (-> Any * Any))
         (push-left ,push-left!_ (-> Any * Any))
         (push-left! ,push-left!_ (-> Any * Any))
         (push-right ,push-right!_ (-> Any * Any))
         (push-right! ,push-right!_ (-> Any * Any))
         (range ,range_ (-> Any * Any))
         (re ,js/regexp_ (-> Any * Any))
         (re-pattern ,js/regexp_ (-> Any * Any))
         (read ,read (-> Any * Any))
         (read-syntax ,read-syntax (-> Any * Any))
         (regexp ,js/regexp_ (-> Any * Any))
         (regexp-match ,regexp-match_ (-> Any * Any))
         (regexp-match? ,regexp-match?_ (-> Any * Any))
         (regexp-quote ,regexp-quote_ (-> Any * Any))
         (regexp-replace ,regexp-replace_ (-> Any * Any))
         (regexp? ,regexp?_ (-> Any * Any))
         (rest ,rest_ (-> Any * Any))
         (reverse ,reverse_ (-> Any * Any))
         (reverse! ,reverse!_ (-> Any * Any))
         (rx ,js/regexp_ (-> Any * Any))
         (scm/new ,js/new_ (-> Any * Any))
         (second ,second_ (-> Any * Any))
         (self-evaluating? ,self-evaluating?_ (-> Any * Any))
         (set-car! ,set-car!_ (-> Any * Any))
         (set-cdr! ,set-cdr!_ (-> Any * Any))
         (set-mcar! ,set-car!_ (-> Any * Any))
         (set-mcdr! ,set-cdr!_ (-> Any * Any))
         (set-nth ,list-set_ (-> Any * Any))
         (set-nth! ,list-set!_ (-> Any * Any))
         (seventh ,seventh_ (-> Any * Any))
         (sixth ,sixth_ (-> Any * Any))
         (source ,source (-> Any * Any))
         (string->number ,string->number_ (-> Any * Any))
         (string->symbol ,string->symbol_ (-> Any * Any))
         (string-append ,string-append_ (-> Any * Any))
         (string-downcase ,string-downcase_ (-> Any * Any))
         (string-join ,string-join_ (-> Any * Any))
         (string-length ,string-length_ (-> Any * Any))
         (string-literal? ,js/string-literal?_ (-> Any * Any))
         (string-object? ,js/string-object?_ (-> Any * Any))
         (string-primitive? ,js/string-literal?_ (-> Any * Any))
         (string-ref ,string-ref_ (-> Any * Any))
         (string-repeat ,string-repeat_ (-> Any * Any))
         (string-replace ,string-replace_ (-> Any * Any))
         (string-split ,string-split_ (-> Any * Any))
         (string-to-symbol ,string->symbol_ (-> Any * Any))
         (string-trim ,string-trim_ (-> Any * Any))
         (string-upcase ,string-upcase_ (-> Any * Any))
         (string? ,string?_ (-> Any * Any))
         (stringp ,string?_ (-> Any * Any))
         (sub ,sub_ (-> Any * Any))
         (sub1 ,sub1_ (-> Any * Any))
         (substring ,substring_ (-> Any * Any))
         (symbol->string ,symbol->string_ (-> Any * Any))
         (symbol-to-string ,symbol->string_ (-> Any * Any))
         (symbol? ,symbol?_ (-> Any * Any))
         (symbolp ,symbol?_ (-> Any * Any))
         (syntax->datum ,syntax->datum (-> Any * Any))
         (syntax->list ,syntax->list (-> Any * Any))
         (syntax-e ,syntax-e (-> Any * Any))
         (syntax? ,syntax? (-> Any * Any))
         (tail ,cdr_ (-> Any * Any))
         (take ,take_ (-> Any * Any))
         (tenth ,tenth_ (-> Any * Any))
         (third ,third_ (-> Any * Any))
         (thunk? ,thunk?_ (-> Any * Any))
         (true? ,true?_ (-> Any * Any))
         (truep ,true?_ (-> Any * Any))
         (ts/raw ,js/raw_ (-> Any * Any))
         (type-of ,type-of_ (-> Any * Any))
         (typeof ,type-of_ (-> Any * Any))
         (undefined? ,undefined?_ (-> Any * Any))
         (union ,union_ (-> Any * Any))
         (unsigned-bit-shift-right ,js/unsigned-bitwise-shift-right_ (-> Any * Any))
         (unsigned-bitwise-shift-right ,js/unsigned-bitwise-shift-right_ (-> Any * Any))
         (values ,values_ (-> Any * Any))
         (vector ,list_ (-> Any * Any))
         (vector-ref ,nth_ (-> Any * Any))
         (vector-set ,list-set_ (-> Any * Any))
         (vector-set! ,list-set!_ (-> Any * Any))
         (vector? ,array?_ (-> Any * Any))
         (write-to-string ,write-to-string (-> Any * Any))
         (zero? ,zero?_ (-> Any * Any))
         (zerop ,zero?_ (-> Any * Any))
         ;; Macros.
         (,(string->symbol ".") ,dot_ (macro-> Any * Any))
         (,(string->symbol ":") ,colon_ (macro-> Any * Any))
         (,quasiquote-sym_ ,quasiquote_ (macro-> Any * Any))
         (,quote-sym_ ,quote_ (macro-> Any * Any))
         (-> ,thread-first_ (macro-> Any * Any))
         (->> ,thread-last_ (macro-> Any * Any))
         (~> ,thread-first_ (macro-> Any * Any))
         (~>> ,thread-last_ (macro-> Any * Any))
         (and ,and_ (macro-> Any * Any))
         (ann ,ann_ (macro-> Any * Any))
         (as-> ,thread-as_ (macro-> Any * Any))
         (async ,js/async_ (macro-> Any * Any))
         (as~> ,thread-as_ (macro-> Any * Any))
         (await ,js/await_ (macro-> Any * Any))
         (begin ,begin_ (macro-> Any * Any))
         (begin0 ,begin0_ (macro-> Any * Any))
         (block ,js/block_ (macro-> Any * Any))
         (break ,break_ (macro-> Any * Any))
         (call-method ,send_ (macro-> Any * Any))
         (case ,case_ (macro-> Any * Any))
         (case/eq ,case-eq_ (macro-> Any * Any))
         (cl/loop ,cl/loop_ (macro-> Any * Any))
         (class ,class_ (macro-> Any * Any))
         (clj/try ,clj/try_ (macro-> Any * Any))
         (cond ,cond_ (macro-> Syntax Syntax))
         (continue ,continue_ (macro-> Any * Any))
         (declare ,declare_ (macro-> Any * Any))
         (declare-fexpr ,declare-fexpr_ (macro-> Any * Any))
         (declare-macro ,declare-macro_ (macro-> Any * Any))
         (defclass ,defclass_ (macro-> Any * Any))
         (define ,define_ (macro-> Any * Any))
         (define-class ,define-class_ (macro-> Any * Any))
         (define-fexpr ,define-fexpr_ (macro-> Any * Any))
         (define-fields ,define-fields_ (macro-> Any * Any))
         (define-js/obj ,define-fields_ (macro-> Any * Any))
         (define-macro ,define-macro_ (macro-> Any * Any))
         (define-syntax ,define-syntax_ (macro-> Any * Any))
         (define-type ,define-type_ (macro-> Any * Any))
         (define-values ,define-values_ (macro-> Any * Any))
         (define/async ,define-async_ (macro-> Any * Any))
         (define/generator ,define-generator_ (macro-> Any * Any))
         (define/private ,define-private_ (macro-> Any * Any))
         (define/public ,define-public_ (macro-> Any * Any))
         (defmacro ,defmacro_ (macro-> Any * Any))
         (defun ,defun_ (macro-> Any * Any))
         (delay ,delay_ (macro-> Any * Any))
         (destructuring-bind ,multiple-value-bind_ (macro-> Any * Any))
         (do ,do_ (macro-> Any * Any))
         (el/if ,el/if_ (macro-> Any * Any))
         (field-bound? ,field-bound?_ (macro-> Any * Any))
         (fn ,lambda_ (macro-> Any * Any))
         (for ,for_ (macro-> Any * Any))
         (fset ,set_ (macro-> Any * Any))
         (get-field ,get-field_ (macro-> Any * Any))
         (if ,if_ (macro-> Any * Any))
         (js/= ,js/assignment_ (macro-> Any * Any))
         (js/=> ,js/arrow_ (macro-> Any * Any))
         (js/? ,js/ternary-operator_ (macro-> Any * Any))
         (js/\, ,js/sequence_ (macro-> Any * Any))
         (js/\; ,begin_ (macro-> Any * Any))
         (js/\{\} ,js/block_ (macro-> Any * Any))
         (js/arrow ,js/arrow_ (macro-> Any * Any))
         (js/async ,js/async_ (macro-> Any * Any))
         (js/await ,js/await_ (macro-> Any * Any))
         (js/block ,js/block_ (macro-> Any * Any))
         (js/const ,js/const_ (macro-> Any * Any))
         (js/define ,js/let_ (macro-> Any * Any))
         (js/do-while ,js/do-while_ (macro-> Any * Any))
         (js/for ,js/for_ (macro-> Any * Any))
         (js/for-in ,js/for-in_ (macro-> Any * Any))
         (js/for-of ,js/for-of_ (macro-> Any * Any))
         (js/function ,js/function_ (macro-> Any * Any))
         (js/if ,js/if_ (macro-> Any * Any))
         (js/iife ,js/iife_ (macro-> Any * Any))
         (js/let ,js/let_ (macro-> Any * Any))
         (js/op ,js/op_ (macro-> Any * Any))
         (js/op/apply ,js/op/apply_ (macro-> Any * Any))
         (js/operator ,js/op_ (macro-> Any * Any))
         (js/rename ,js/rename_ (macro-> Any * Any))
         (js/sequence ,js/sequence_ (macro-> Any * Any))
         (js/statement-or-expression ,js/statement-or-expression_ (macro-> Any * Any))
         (js/switch ,js/switch_ (macro-> Any * Any))
         (js/try ,js/try_ (macro-> Any * Any))
         (js/var ,js/var_ (macro-> Any * Any))
         (js/while ,js/while_ (macro-> Any * Any))
         (λ ,lambda_ (macro-> Any * Any))
         (lambda ,lambda_ (macro-> Any * Any))
         (lazy ,lazy_ (macro-> Any * Any))
         (let ,let-star_ (macro-> Any * Any))
         (let* ,let-star_ (macro-> Any * Any))
         (let*-values ,let-values_ (macro-> Any * Any))
         (let-env ,let-env_ (macro-> Any * Any))
         (let-fields ,let-fields_ (macro-> Any * Any))
         (let-js/obj ,let-fields_ (macro-> Any * Any))
         (let-values ,let-values_ (macro-> Any * Any))
         (letrec ,let-star_ (macro-> Any * Any))
         (letrec-values ,let-values_ (macro-> Any * Any))
         (match ,match_ (macro-> Any * Any))
         (module ,module_ (macro-> Any * Any))
         (multiple-value-bind ,multiple-value-bind_ (macro-> Any * Any))
         (multiple-values-bind ,multiple-value-bind_ (macro-> Any * Any))
         (new/apply ,new/apply_ (macro-> Any * Any))
         (once-only ,once-only_ (macro-> Any * Any))
         (or ,or_ (macro-> Any * Any))
         (prog1 ,begin0_ (macro-> Any * Any))
         (progn ,begin_ (macro-> Any * Any))
         (provide ,provide_ (macro-> Any * Any))
         (quasisyntax ,quasisyntax_ (macro-> Any * Any))
         (require ,require_ (macro-> Any * Any))
         (return ,return_ (macro-> Any * Any))
         (rkt/new ,rkt/new_ (macro-> Any * Any))
         (send ,send_ (macro-> Any * Any))
         (send/apply ,send/apply_ (macro-> Any * Any))
         (set ,set_ (macro-> Any * Any))
         (set! ,set!_ (macro-> Any * Any))
         (set!-fields ,set-fields_ (macro-> Any * Any))
         (set!-js/obj ,set-fields_ (macro-> Any * Any))
         (set!-values ,set-values_ (macro-> Any * Any))
         (set-field! ,set-field_ (macro-> Any * Any))
         (setq ,setq_ (macro-> Any * Any))
         (syntax ,syntax_ (macro-> Any * Any))
         (throw ,throw_ (macro-> Any * Any))
         (thunk ,thunk_ (macro-> Any * Any))
         (try ,try_ (macro-> Any * Any))
         (unless ,unless_ (macro-> Any * Any))
         (unwind-protect ,unwind-protect_ (macro-> Any * Any))
         (when ,when_ (macro-> Any * Any))
         (while ,while_ (macro-> Any * Any))
         (with-gensyms ,with-gensyms_ (macro-> Any * Any))
         (yield ,yield_ (macro-> Any * Any)))))

;;; Evaluation environment.
(define eval-environment
  (new LispEnvironment
       `((eval ,interpret (-> Any * Any))
         (interpret ,interpret (-> Any * Any))
         (js/eval ,js/eval_ (-> Any * Any))
         (scm/eval ,interpret (-> Any * Any))
         (seval ,eval_ (-> Any * Any)))))

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
  (js/obj :language-environment
          lang-environment
          :compilation-mapping-environment
          compilation-mapping-env
          :finline-functions
          #t
          :gensym-map
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
  (all-from-out "./thunk")
  (rename-out (and_ and))
  (rename-out (ann_ ann))
  (rename-out (begin_ begin))
  (rename-out (call-with-current-continuation_ call-with-current-continuation))
  (rename-out (call-with-current-continuation_ call/cc))
  (rename-out (colon_ colon))
  (rename-out (compile-syntax compile-rose))
  (rename-out (compile-with-environment compile-lisp))
  (rename-out (compile-with-environment compile-lisp-to-javascript))
  (rename-out (cond_ cond))
  (rename-out (define-async_ define/async))
  (rename-out (define-class_ define-class))
  (rename-out (define-fields_ define-fields))
  (rename-out (define-fields_ define-js/obj))
  (rename-out (define-generator_ define/generator))
  (rename-out (define-macro_ define-macro))
  (rename-out (define-public_ define/public))
  (rename-out (define-type_ define-type))
  (rename-out (define-values_ define-values))
  (rename-out (define_ define))
  (rename-out (dot_ dot))
  (rename-out (get-field_ get-field))
  (rename-out (js/async_ async))
  (rename-out (js/async_ async_))
  (rename-out (js/async_ js-async))
  (rename-out (js/await_ await))
  (rename-out (js/await_ await_))
  (rename-out (js/await_ js-await))
  (rename-out (js/block_ block))
  (rename-out (js/block_ block_))
  (rename-out (js/raw_ js))
  (rename-out (js/raw_ js/raw))
  (rename-out (js/raw_ js_))
  (rename-out (lambda_ compile-function))
  (rename-out (lambda_ fn))
  (rename-out (lambda_ lambda))
  (rename-out (let-fields_ let-fields))
  (rename-out (let-fields_ let-js/obj))
  (rename-out (let-star_ let*))
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
  (rename-out (optimize-syntax optimize-rose))
  (rename-out (or_ or))
  (rename-out (provide_ provide))
  (rename-out (quasiquote_ quasiquote))
  (rename-out (quote_ quote))
  (rename-out (read-syntax read-rose))
  (rename-out (require_ require))
  (rename-out (send/apply_ send/apply))
  (rename-out (send_ call-method))
  (rename-out (send_ send))
  (rename-out (set!_ set!))
  (rename-out (set-field_ set-field!))
  (rename-out (set-field_ set-field))
  (rename-out (set-fields_ set!-fields))
  (rename-out (set-fields_ set!-js/obj))
  (rename-out (set-fields_ set-fields!))
  (rename-out (set-fields_ set-fields))
  (rename-out (set-values_ set!-values))
  (rename-out (set-values_ set-values))
  (rename-out (setq_ setq))
  (rename-out (sexp read-from-string))
  Module
  and_
  ann_
  apply-optimizations
  begin_
  break_
  class_
  clj/try_
  colon_
  compilation-environment
  compile
  compile-file!
  compile-files!
  compile-module-map
  compile-modules
  compile-with-environment
  cond_
  continue_
  decompile
  define->define-class
  define-async_
  define-fields_
  define-generator_
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
  js/async_
  js/await_
  js/raw_
  lambda_
  lang-environment
  let-fields_
  let-star_
  let-values_
  let-vars->const-vars
  lisp
  lisp-environment
  load_
  macroexpand
  macroexpand*
  macroexpand*-1
  macroexpand*-n
  macroexpand-1
  macroexpand-all
  macroexpand-all-until
  macroexpand-n
  macroexpand-until
  make-lisp
  make-module-map
  map-sexp
  map-syntax
  map-visit-rose
  module-expression->module-object
  module_
  new_
  nop_
  optimizations
  optimize-estree
  optimize-module
  optimize-sexp
  optimize-syntax
  or_
  provide_
  quasiquote_
  quote?
  quote_
  read
  read-sexp
  read-syntax
  require_
  return_
  s
  send-method
  send/apply_
  send_
  set!_
  set-field_
  set-fields_
  set-values_
  setq_
  sexp
  source
  source?
  split-comments
  throw_
  tokenize
  traverse-estree
  try_
  type-of_
  yield_)
