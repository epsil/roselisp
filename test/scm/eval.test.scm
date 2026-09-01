;;; # Evaluation tests

(require (only-in "../../src/ts/estree"
                  ArrayExpression
                  ArrayPattern
                  ArrowFunctionExpression
                  AssignmentExpression
                  BinaryExpression
                  BlockStatement
                  BreakStatement
                  CallExpression
                  CatchClause
                  ClassBody
                  ClassDeclaration
                  ClassExpression
                  ConditionalExpression
                  ContinueStatement
                  ExpressionStatement
                  ForOfStatement
                  ForStatement
                  FunctionDeclaration
                  FunctionExpression
                  Identifier
                  IfStatement
                  Literal
                  LogicalExpression
                  MemberExpression
                  MethodDefinition
                  NewExpression
                  ObjectExpression
                  Property
                  PropertyDefinition
                  RestElement
                  ReturnStatement
                  SpreadElement
                  ThisExpression
                  ThrowStatement
                  TryStatement
                  UnaryExpression
                  VariableDeclaration
                  VariableDeclarator
                  WhileStatement))
(require (only-in "../../src/ts/eval"
                  eval_
                  eval-estree
                  eval-syntax))
(require (only-in "../../src/ts/language"
                  __
                  LispEnvironment
                  lang-environment
                  lisp-1-environment
                  eval-lisp))
(require (only-in "../../src/ts/rose"
                  datum->syntax))
(require (only-in "../../src/ts/sexp"
                  sexp))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 :describe "eval_"
 > (eval_ #t lisp-1-environment)
 #t
 > (eval_ (js/tag sexp "true") lisp-1-environment)
 #t
 > (eval_ (js/tag sexp "t") lisp-1-environment)
 #t
 > (eval_ (js/tag sexp "#t") lisp-1-environment)
 #t
 > (eval_ #f lisp-1-environment)
 #f
 > (eval_ (js/tag sexp "false") lisp-1-environment)
 #f
 > (eval_ (js/tag sexp "#f") lisp-1-environment)
 #f

 :describe "eval-syntax"
 > (eval-syntax (datum->syntax #f #t)
                lisp-1-environment)
 #t
 > (eval-syntax (datum->syntax #f #f)
                lisp-1-environment)
 #f

 :describe "eval-estree"
 > (it "#t"
       (eval-estree
        (new Literal #t)
        lisp-1-environment))
 #t
 > (it "#f"
       (eval-estree
        (new Literal #f)
        lisp-1-environment))
 #f
 > (it "0"
       (eval-estree
        (new Literal 0)
        lisp-1-environment))
 0
 > (it "1"
       (eval-estree
        (new Literal 1)
        lisp-1-environment))
 1
 > (it "2"
       (eval-estree
        (new Literal 2)
        lisp-1-environment))
 2
 > (it "foo"
       (eval-estree
        (new Identifier "foo")
        lisp-1-environment))
 #u
 > (it "{}"
       (eval-estree
        (new ObjectExpression)
        lisp-1-environment))
 (js/obj)
 > (it "{ foo: 'bar' }"
       (eval-estree
        (new ObjectExpression
             (list
              (new Property
                   (new Identifier "foo")
                   (new Literal "bar"))))
        lisp-1-environment))
 (js/obj "foo" "bar")
 > (it "{ const foo = { bar: 'baz' }; {...foo}; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclarator
                   (new Identifier "foo")
                   (new ObjectExpression
                        (list
                         (new Property
                              (new Identifier "bar")
                              (new Literal "baz")))))
              (new ObjectExpression
                   (list
                    (new SpreadElement
                         (new Identifier "foo"))))))
        lisp-1-environment))
 (js/obj "bar" "baz")
 > (it "!false"
       (eval-estree
        (new UnaryExpression
             "!"
             #f
             (new Literal #f))
        lisp-1-environment))
 #t
 > (it "{ let x = 0; ++x; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclarator
                   (new Identifier "x")
                   (new Literal 0))
              (new UnaryExpression
                   "++"
                   #t
                   (new Identifier "x"))))
        lisp-1-environment))
 1
 > (it "{ let x = 0; x++; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclarator
                   (new Identifier "x")
                   (new Literal 0))
              (new UnaryExpression
                   "++"
                   #f
                   (new Identifier "x"))))
        lisp-1-environment))
 0
 > (it "{ let x = 0; --x; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclarator
                   (new Identifier "x")
                   (new Literal 0))
              (new UnaryExpression
                   "--"
                   #t
                   (new Identifier "x"))))
        lisp-1-environment))
 -1
 > (it "{ let x = 0; x--; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclarator
                   (new Identifier "x")
                   (new Literal 0))
              (new UnaryExpression
                   "--"
                   #f
                   (new Identifier "x"))))
        lisp-1-environment))
 0
 > (it "1 < 2"
       (eval-estree
        (new BinaryExpression
             "<"
             (new Literal 1)
             (new Literal 2))
        lisp-1-environment))
 #t
 > (it "1 > 2"
       (eval-estree
        (new BinaryExpression
             ">"
             (new Literal 1)
             (new Literal 2))
        lisp-1-environment))
 #f
 > (it "1 + 2"
       (eval-estree
        (new BinaryExpression
             "+"
             (new Literal 1)
             (new Literal 2))
        lisp-1-environment))
 3
 > (it "1 - 2"
       (eval-estree
        (new BinaryExpression
             "-"
             (new Literal 1)
             (new Literal 2))
        lisp-1-environment))
 -1
 > (it "1 * 2"
       (eval-estree
        (new BinaryExpression
             "*"
             (new Literal 1)
             (new Literal 2))
        lisp-1-environment))
 2
 > (it "4 / 2"
       (eval-estree
        (new BinaryExpression
             "/"
             (new Literal 4)
             (new Literal 2))
        lisp-1-environment))
 2
 > (it "false && false"
       (eval-estree
        (new LogicalExpression
             "&&"
             (new Literal #f)
             (new Literal #f))
        lisp-1-environment))
 #f
 > (it "false && true"
       (eval-estree
        (new LogicalExpression
             "&&"
             (new Literal #f)
             (new Literal #t))
        lisp-1-environment))
 #f
 > (it "true && false"
       (eval-estree
        (new LogicalExpression
             "&&"
             (new Literal #t)
             (new Literal #f))
        lisp-1-environment))
 #f
 > (it "true && true"
       (eval-estree
        (new LogicalExpression
             "&&"
             (new Literal #t)
             (new Literal #t))
        lisp-1-environment))
 #t
 > (it "false || false"
       (eval-estree
        (new LogicalExpression
             "||"
             (new Literal #f)
             (new Literal #f))
        lisp-1-environment))
 #f
 > (it "false || true"
       (eval-estree
        (new LogicalExpression
             "||"
             (new Literal #f)
             (new Literal #t))
        lisp-1-environment))
 #t
 > (it "true || false"
       (eval-estree
        (new LogicalExpression
             "||"
             (new Literal #t)
             (new Literal #f))
        lisp-1-environment))
 #t
 > (it "true || true"
       (eval-estree
        (new LogicalExpression
             "||"
             (new Literal #t)
             (new Literal #t))
        lisp-1-environment))
 #t
 > (it "{ const x = 1; x; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "x")
                         (new Literal 1)))
                   "const")
              (new Identifier "x")))
        lisp-1-environment))
 1
 > (it "{ x = 1; x; }"
       (eval-estree
        (new BlockStatement
             (list
              (new ExpressionStatement
                   (new AssignmentExpression
                        "="
                        (new Identifier "x")
                        (new Literal 1)))
              (new Identifier "x")))
        lisp-1-environment))
 1
 > (it "{ ([x] = [1]); x; }"
       (eval-estree
        (new BlockStatement
             (list
              (new ExpressionStatement
                   (new AssignmentExpression
                        "="
                        (new ArrayPattern
                             (list
                              (new Identifier
                                   "x")))
                        (new ArrayExpression
                             (list
                              (new Literal
                                   1)))))
              (new Identifier "x")))
        lisp-1-environment))
 1
 > (it "{ const x = {}; x.foo = 'bar'; x.foo; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclarator
                   (new Identifier "x")
                   (new ObjectExpression))
              (new ExpressionStatement
                   (new AssignmentExpression
                        "="
                        (new MemberExpression
                             (new Identifier "x")
                             (new Identifier "foo"))
                        (new Literal "bar")))
              (new MemberExpression
                   (new Identifier "x")
                   (new Identifier "foo"))))
        lisp-1-environment))
 "bar"
 > (it "{ 1; }"
       (eval-estree
        (new BlockStatement
             (list
              (new Literal 1)))
        lisp-1-environment))
 1
 > (it "{ let x = true; if (x) { x = false; } x; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclarator
                   (new Identifier "x")
                   (new Literal #t))
              (new IfStatement
                   (new Identifier "x")
                   (new BlockStatement
                        (list
                         (new AssignmentExpression
                              "="
                              (new Identifier "x")
                              (new Literal #f)))))
              (new Identifier "x")))
        lisp-1-environment))
 #f
 > (it "true ? 1 : 2"
       (eval-estree
        (new ConditionalExpression
             (new Literal #t)
             (new Literal 1)
             (new Literal 2))
        lisp-1-environment))
 1
 > (it "false ? 1 : 2"
       (eval-estree
        (new ConditionalExpression
             (new Literal #f)
             (new Literal 1)
             (new Literal 2))
        lisp-1-environment))
 2
 ;; TODO: Test `continue`.
 > (it "{ let x = true; while (x) { x = false; } x; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "x")
                         (new Literal #t))))
              (new WhileStatement
                   (new Identifier "x")
                   (new BlockStatement
                        (list
                         (new ExpressionStatement
                              (new AssignmentExpression
                                   "="
                                   (new Identifier "x")
                                   (new Literal #f))))))
              (new Identifier "x")))
        lisp-1-environment))
 #f
 > (it "{ let x = true; while (x) { break; } x; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "x")
                         (new Literal #t))))
              (new WhileStatement
                   (new Identifier "x")
                   (new BlockStatement
                        (list
                         (new BreakStatement))))
              (new Identifier "x")))
        lisp-1-environment))
 #t
 ;; TODO: Test `continue`.
 > (it "{ let x = 0; let i = 0; for (i = 0; i < 10; i = i + 1) { x = i; } x; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "x")
                         (new Literal 0))
                    (new VariableDeclarator
                         (new Identifier "i")
                         (new Literal 0)))
                   "const")
              (new ForStatement
                   (new AssignmentExpression
                        "="
                        (new Identifier "i")
                        (new Literal 0))
                   (new BinaryExpression
                        "<"
                        (new Identifier "i")
                        (new Literal 10))
                   (new AssignmentExpression
                        "="
                        (new Identifier "i")
                        (new BinaryExpression
                             "+"
                             (new Identifier "i")
                             (new Literal 1)))
                   (new BlockStatement
                        (list
                         (new ExpressionStatement
                              (new AssignmentExpression
                                   "="
                                   (new Identifier "x")
                                   (new Identifier "i"))))))
              (new Identifier "x")))
        lisp-1-environment))
 9
 > (it "{ let x = 0; let i = 0; for (i = 0; i < 10; i = i + 1) { if (i === 5) { break; } x = i; } x; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "x")
                         (new Literal 0))
                    (new VariableDeclarator
                         (new Identifier "i")
                         (new Literal 0)))
                   "const")
              (new ForStatement
                   (new AssignmentExpression
                        "="
                        (new Identifier "i")
                        (new Literal 0))
                   (new BinaryExpression
                        "<"
                        (new Identifier "i")
                        (new Literal 10))
                   (new AssignmentExpression
                        "="
                        (new Identifier "i")
                        (new BinaryExpression
                             "+"
                             (new Identifier "i")
                             (new Literal 1)))
                   (new BlockStatement
                        (list
                         (new IfStatement
                              (new BinaryExpression
                                   "==="
                                   (new Identifier "i")
                                   (new Literal 5))
                              (new BlockStatement
                                   (list
                                    (new BreakStatement))))
                         (new ExpressionStatement
                              (new AssignmentExpression
                                   "="
                                   (new Identifier "x")
                                   (new Identifier "i"))))))
              (new Identifier "x")))
        lisp-1-environment))
 4
 > (it "{ let x = 0; let i = 0; for (i = 0; i < 10; i = i + 1) { if (i === 9) { continue; } x = i; } x; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "x")
                         (new Literal 0))
                    (new VariableDeclarator
                         (new Identifier "i")
                         (new Literal 0)))
                   "const")
              (new ForStatement
                   (new AssignmentExpression
                        "="
                        (new Identifier "i")
                        (new Literal 0))
                   (new BinaryExpression
                        "<"
                        (new Identifier "i")
                        (new Literal 10))
                   (new AssignmentExpression
                        "="
                        (new Identifier "i")
                        (new BinaryExpression
                             "+"
                             (new Identifier "i")
                             (new Literal 1)))
                   (new BlockStatement
                        (list
                         (new IfStatement
                              (new BinaryExpression
                                   "==="
                                   (new Identifier "i")
                                   (new Literal 9))
                              (new BlockStatement
                                   (list
                                    (new ContinueStatement))))
                         (new ExpressionStatement
                              (new AssignmentExpression
                                   "="
                                   (new Identifier "x")
                                   (new Identifier "i"))))))
              (new Identifier "x")))
        lisp-1-environment))
 8
 > (it "{ let lst = []; for (let x of [1, 2, 3]) { lst.push(x); } lst; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "lst")
                         (new ArrayExpression)))
                   "let")
              (new ForOfStatement
                   (new VariableDeclaration
                        (list
                         (new VariableDeclarator
                              (new Identifier "x")))
                        "let")
                   (new ArrayExpression
                        (list
                         (new Literal 1)
                         (new Literal 2)
                         (new Literal 3)))
                   (new BlockStatement
                        (list
                         (new CallExpression
                              (new MemberExpression
                                   (new Identifier "lst")
                                   (new Identifier "push"))
                              (list
                               (new Identifier "x"))))))
              (new Identifier "lst")))
        lisp-1-environment))
 '(1 2 3)
 > (it "{ let lst = []; for (let x of [1, 2, 3]) { if (x === 2) { continue; } lst.push(x); } lst; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "lst")
                         (new ArrayExpression)))
                   "let")
              (new ForOfStatement
                   (new VariableDeclaration
                        (list
                         (new VariableDeclarator
                              (new Identifier "x")))
                        "let")
                   (new ArrayExpression
                        (list
                         (new Literal 1)
                         (new Literal 2)
                         (new Literal 3)))
                   (new BlockStatement
                        (list
                         (new IfStatement
                              (new BinaryExpression
                                   "==="
                                   (new Identifier "x")
                                   (new Literal 2))
                              (new BlockStatement
                                   (list
                                    (new ContinueStatement))))
                         (new CallExpression
                              (new MemberExpression
                                   (new Identifier "lst")
                                   (new Identifier "push"))
                              (list
                               (new Identifier "x"))))))
              (new Identifier "lst")))
        lisp-1-environment))
 '(1 3)
 > (it "I(1)"
       (eval-estree
        (new CallExpression
             (new Identifier "I")
             (list (new Literal 1)))
        (new LispEnvironment
             (list
              (list
               'I
               (lambda (x)
                 x)
               "function")))))
 1
 > (it "(<fn> 1)"
       ((eval-estree
         (new FunctionExpression
              (list
               (new Identifier "x"))
              (new BlockStatement
                   (list
                    (new ReturnStatement
                         (new Identifier "x")))))
         lisp-1-environment)
        1))
 1
 > (it "(<fn-with-rest-element> 1)"
       ((eval-estree
         (new FunctionExpression
              (list
               (new RestElement
                    (new Identifier "x")))
              (new BlockStatement
                   (list
                    (new ReturnStatement
                         (new Identifier "x")))))
         lisp-1-environment)
        1))
 (list 1)
 > (it "(<arrow-fn> 1)"
       ((eval-estree
         (new ArrowFunctionExpression
              (list
               (new Identifier "x"))
              (new BlockStatement
                   (list
                    (new ReturnStatement
                         (new Identifier "x")))))
         lisp-1-environment)
        1))
 1
 > (it "{ function I(x) { ... } I(1); }"
       (eval-estree
        (new BlockStatement
             (list
              (new FunctionDeclaration
                   (new Identifier "I")
                   (list
                    (new Identifier "x"))
                   (new BlockStatement
                        (list
                         (new ReturnStatement
                              (new Identifier "x")))))
              (new CallExpression
                   (new Identifier "I")
                   (list
                    (new Literal 1)))))
        lisp-1-environment))
 1
 > (it "{ try { throw new Error(); } catch (err) { } 1; }"
       (eval-estree
        (new BlockStatement
             (list
              (new TryStatement
                   (new BlockStatement
                        (list
                         (new ThrowStatement
                              (new NewExpression
                                   (new Identifier "Error")))))
                   (new CatchClause
                        (new Identifier "err")
                        (new BlockStatement
                             (list))))
              (new Literal 1)))
        lang-environment))
 1
 > (it "{ const Foo = class {}; const bar = new Foo(); bar instanceof Foo; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "Foo")
                         (new ClassExpression)))
                   "const")
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "bar")
                         (new NewExpression
                              (new Identifier "Foo"))))
                   "const")
              (new BinaryExpression
                   "instanceof"
                   (new Identifier "bar")
                   (new Identifier "Foo"))))
        lang-environment))
 #t
 > (it "{ const Foo = class {}; const Bar = class extends Foo {}; const baz = new Bar(); baz instanceof Foo; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "Foo")
                         (new ClassExpression)))
                   "const")
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "Bar")
                         (new ClassExpression
                              (new ClassBody)
                              (new Identifier "Foo"))))
                   "const")
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "baz")
                         (new NewExpression
                              (new Identifier "Bar"))))
                   "const")
              (new BinaryExpression
                   "instanceof"
                   (new Identifier "baz")
                   (new Identifier "Foo"))))
        lang-environment))
 #t
 > (it "{ const Foo = class { bar = 'baz'; }; const quux = new Foo(); quux.bar; }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "Foo")
                         (new ClassExpression
                              (new ClassBody
                                   (list
                                    (new PropertyDefinition
                                         (new Identifier "bar")
                                         (new Literal "baz")))))))
                   "const")
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "quux")
                         (new NewExpression
                              (new Identifier "Foo"))))
                   "const")
              (new MemberExpression
                   (new Identifier "quux")
                   (new Identifier "bar"))))
        lang-environment))
 "baz"
 > (it "{ const Foo = class { bar() { return 'baz'; } }; const quux = new Foo(); quux.bar(); }"
       (eval-estree
        (new BlockStatement
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "Foo")
                         (new ClassExpression
                              (new ClassBody
                                   (list
                                    (new MethodDefinition
                                         (new Identifier "bar")
                                         (new FunctionExpression
                                              (list)
                                              (new BlockStatement
                                                   (list
                                                    (new ReturnStatement
                                                         (new Literal "baz")))))))))))
                   "const")
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "quux")
                         (new NewExpression
                              (new Identifier "Foo"))))
                   "const")
              (new CallExpression
                   (new MemberExpression
                        (new Identifier "quux")
                        (new Identifier "bar"))
                   (list))))
        lang-environment))
 "baz"
 > (it "{ class Foo {} const bar = new Foo(); bar instanceof Foo; }"
       (eval-estree
        (new BlockStatement
             (list
              (new ClassDeclaration
                   (new Identifier "Foo"))
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "bar")
                         (new NewExpression
                              (new Identifier "Foo"))))
                   "const")
              (new BinaryExpression
                   "instanceof"
                   (new Identifier "bar")
                   (new Identifier "Foo"))))
        lang-environment))
 #t
 > (it "{ class Foo { bar = 'bar'; baz() { return this.bar; } } const quux = new Foo(); quux.baz(); }"
       (eval-estree
        (new BlockStatement
             (list
              (new ClassDeclaration
                   (new Identifier "Foo")
                   (new ClassBody
                        (list
                         (new PropertyDefinition
                              (new Identifier "bar")
                              (new Literal "baz"))
                         (new MethodDefinition
                              (new Identifier "baz")
                              (new FunctionExpression
                                   (list)
                                   (new BlockStatement
                                        (list
                                         (new ReturnStatement
                                              (new MemberExpression
                                                   (new ThisExpression)
                                                   (new Identifier "bar"))))))))))
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "quux")
                         (new NewExpression
                              (new Identifier "Foo"))))
                   "const")
              (new CallExpression
                   (new MemberExpression
                        (new Identifier "quux")
                        (new Identifier "baz"))
                   (list))))
        lang-environment))
 "baz"
 > (it "{ class Foo { bar; constructor(bar) { this.bar = bar; } } const quux = new Foo('bar'); quux.bar; }"
       (eval-estree
        (new BlockStatement
             (list
              (new ClassDeclaration
                   (new Identifier "Foo")
                   (new ClassBody
                        (list
                         (new PropertyDefinition
                              (new Identifier "bar"))
                         (new MethodDefinition
                              (new Identifier "constructor")
                              (new FunctionExpression
                                   (list
                                    (new Identifier "bar"))
                                   (new BlockStatement
                                        (list
                                         (new ExpressionStatement
                                              (new AssignmentExpression
                                                   "="
                                                   (new MemberExpression
                                                        (new ThisExpression)
                                                        (new Identifier "bar"))
                                                   (new Identifier "bar"))))))))))
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "quux")
                         (new NewExpression
                              (new Identifier "Foo")
                              (list
                               (new Literal "bar")))))
                   "const")
              (new MemberExpression
                   (new Identifier "quux")
                   (new Identifier "bar"))))
        lang-environment))
 "bar")
