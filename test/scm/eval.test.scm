;;; # Evaluation tests
;;;
;;; ## To do
;;;
;;; ...

(require chai "chai")
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
                  eval-rose))
(require (only-in "../../src/ts/language"
                  __
                  LispEnvironment
                  lang-environment
                  lisp-1-environment
                  eval-lisp))
(require (only-in "../../src/ts/rose"
                  wrap-sexp-in-rose))
(require (only-in "../../src/ts/sexp"
                  sexp))
(require (only-in "./test-util"
                  assert-equal))

(describe "eval_"
  (fn ()
    (it "true"
        (fn ()
          (assert-equal
           (eval_ #t lisp-1-environment)
           #t)
          (assert-equal
           (eval_ (js/tag sexp "true") lisp-1-environment)
           #t)
          (assert-equal
           (eval_ (js/tag sexp "t") lisp-1-environment)
           #t)
          (assert-equal
           (eval_ (js/tag sexp "#t") lisp-1-environment)
           #t)))
    (it "false"
        (fn ()
          (assert-equal
           (eval_ #f lisp-1-environment)
           #f)
          (assert-equal
           (eval_ (js/tag sexp "false") lisp-1-environment)
           #f)
          (assert-equal
           (eval_ (js/tag sexp "#f") lisp-1-environment)
           #f)))))

(describe "eval-rose"
  (fn ()
    (it "true"
        (fn ()
          (assert-equal
           (eval-rose (wrap-sexp-in-rose #t)
                      lisp-1-environment)
           #t)))
    (it "false"
        (fn ()
          (assert-equal
           (eval-rose (wrap-sexp-in-rose #f)
                      lisp-1-environment)
           #f)))))

(describe "eval-estree"
  (fn ()
    (describe "boolean values"
      (fn ()
        (it "#t"
            (fn ()
              (assert-equal
               (eval-estree
                (new Literal #t)
                lisp-1-environment)
               #t)))
        (it "#f"
            (fn ()
              (assert-equal
               (eval-estree
                (new Literal #f)
                lisp-1-environment)
               #f)))))
    (describe "numbers"
      (fn ()
        (it "0"
            (fn ()
              (assert-equal
               (eval-estree
                (new Literal 0)
                lisp-1-environment)
               0)))
        (it "1"
            (fn ()
              (assert-equal
               (eval-estree
                (new Literal 1)
                lisp-1-environment)
               1)))
        (it "2"
            (fn ()
              (assert-equal
               (eval-estree
                (new Literal 2)
                lisp-1-environment)
               2)))))
    (describe "variables"
      (fn ()
        (it "foo"
            (fn ()
              (assert-equal
               (eval-estree
                (new Identifier "foo")
                lisp-1-environment)
               undefined)))))
    (describe "object expressions"
      (fn ()
        (it "{}"
            (fn ()
              (assert-equal
               (eval-estree
                (new ObjectExpression)
                lisp-1-environment)
               (js-obj))))
        (it "{ foo: 'bar' }"
            (fn ()
              (assert-equal
               (eval-estree
                (new ObjectExpression
                     (list
                      (new Property
                           (new Identifier "foo")
                           (new Literal "bar"))))
                lisp-1-environment)
               (js-obj "foo" "bar"))))
        (it "{ const foo = { bar: 'baz' }; {...foo}; }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               (js-obj "bar" "baz"))))))
    (describe "unary expressions"
      (fn ()
        (it "!false"
            (fn ()
              (assert-equal
               (eval-estree
                (new UnaryExpression
                     "!"
                     #f
                     (new Literal #f))
                lisp-1-environment)
               #t)))
        (it "{ let x = 0; ++x; }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               1)))
        (it "{ let x = 0; x++; }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               0)))
        (it "{ let x = 0; --x; }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               -1)))
        (it "{ let x = 0; x--; }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               0)))))
    (describe "binary expressions"
      (fn ()
        (it "1 < 2"
            (fn ()
              (assert-equal
               (eval-estree
                (new BinaryExpression
                     "<"
                     (new Literal 1)
                     (new Literal 2))
                lisp-1-environment)
               #t)))
        (it "1 > 2"
            (fn ()
              (assert-equal
               (eval-estree
                (new BinaryExpression
                     ">"
                     (new Literal 1)
                     (new Literal 2))
                lisp-1-environment)
               #f)))
        (it "1 + 2"
            (fn ()
              (assert-equal
               (eval-estree
                (new BinaryExpression
                     "+"
                     (new Literal 1)
                     (new Literal 2))
                lisp-1-environment)
               3)))
        (it "1 - 2"
            (fn ()
              (assert-equal
               (eval-estree
                (new BinaryExpression
                     "-"
                     (new Literal 1)
                     (new Literal 2))
                lisp-1-environment)
               -1)))
        (it "1 * 2"
            (fn ()
              (assert-equal
               (eval-estree
                (new BinaryExpression
                     "*"
                     (new Literal 1)
                     (new Literal 2))
                lisp-1-environment)
               2)))
        (it "4 / 2"
            (fn ()
              (assert-equal
               (eval-estree
                (new BinaryExpression
                     "/"
                     (new Literal 4)
                     (new Literal 2))
                lisp-1-environment)
               2)))))
    (describe "logical expressions"
      (fn ()
        (it "false && false"
            (fn ()
              (assert-equal
               (eval-estree
                (new LogicalExpression
                     "&&"
                     (new Literal #f)
                     (new Literal #f))
                lisp-1-environment)
               #f)))
        (it "false && true"
            (fn ()
              (assert-equal
               (eval-estree
                (new LogicalExpression
                     "&&"
                     (new Literal #f)
                     (new Literal #t))
                lisp-1-environment)
               #f)))
        (it "true && false"
            (fn ()
              (assert-equal
               (eval-estree
                (new LogicalExpression
                     "&&"
                     (new Literal #t)
                     (new Literal #f))
                lisp-1-environment)
               #f)))
        (it "true && true"
            (fn ()
              (assert-equal
               (eval-estree
                (new LogicalExpression
                     "&&"
                     (new Literal #t)
                     (new Literal #t))
                lisp-1-environment)
               #t)))
        (it "false || false"
            (fn ()
              (assert-equal
               (eval-estree
                (new LogicalExpression
                     "||"
                     (new Literal #f)
                     (new Literal #f))
                lisp-1-environment)
               #f)))
        (it "false || true"
            (fn ()
              (assert-equal
               (eval-estree
                (new LogicalExpression
                     "||"
                     (new Literal #f)
                     (new Literal #t))
                lisp-1-environment)
               #t)))
        (it "true || false"
            (fn ()
              (assert-equal
               (eval-estree
                (new LogicalExpression
                     "||"
                     (new Literal #t)
                     (new Literal #f))
                lisp-1-environment)
               #t)))
        (it "true || true"
            (fn ()
              (assert-equal
               (eval-estree
                (new LogicalExpression
                     "||"
                     (new Literal #t)
                     (new Literal #t))
                lisp-1-environment)
               #t)))))
    (describe "assignment"
      (fn ()
        (it "{ const x = 1; x; }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               1)))
        (it "{ x = 1; x; }"
            (fn ()
              (assert-equal
               (eval-estree
                (new BlockStatement
                     (list
                      (new ExpressionStatement
                           (new AssignmentExpression
                                "="
                                (new Identifier "x")
                                (new Literal 1)))
                      (new Identifier "x")))
                lisp-1-environment)
               1)))
        (it "{ ([x] = [1]); x; }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               1)))
        (it "{ const x = {}; x.foo = 'bar'; x.foo; }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               "bar")))))
    (describe "blocks"
      (fn ()
        (it "{ 1; }"
            (fn ()
              (assert-equal
               (eval-estree
                (new BlockStatement
                     (list
                      (new Literal 1)))
                lisp-1-environment)
               1)))))
    (describe "if statements"
      (fn ()
        (it "{ let x = true; if (x) { x = false; } x; }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               #f)))))
    (describe "conditional expressions"
      (fn ()
        (it "true ? 1 : 2"
            (fn ()
              (assert-equal
               (eval-estree
                (new ConditionalExpression
                     (new Literal #t)
                     (new Literal 1)
                     (new Literal 2))
                lisp-1-environment)
               1)))
        (it "false ? 1 : 2"
            (fn ()
              (assert-equal
               (eval-estree
                (new ConditionalExpression
                     (new Literal #f)
                     (new Literal 1)
                     (new Literal 2))
                lisp-1-environment)
               2)))))
    (describe "while loops"
      (fn ()
        ;; TODO: Test `continue`.
        (it "{ let x = true; while (x) { x = false; } x; }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               #f)))
        (it "{ let x = true; while (x) { break; } x; }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               #t)))))
    (describe "for loops"
      (fn ()
        ;; TODO: Test `continue`.
        (it "{ let x = 0; let i = 0; for (i = 0; i < 10; i = i + 1) { x = i; } x; }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               9)))
        (it "{ let x = 0; let i = 0; for (i = 0; i < 10; i = i + 1) { if (i === 5) { break; } x = i; } x; }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               4)))
        (it "{ let x = 0; let i = 0; for (i = 0; i < 10; i = i + 1) { if (i === 9) { continue; } x = i; } x; }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               8)))))
    (describe "for...of loops"
      (fn ()
        (it "{ let lst = []; for (let x of [1, 2, 3]) { lst.push(x); } lst; }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               '(1 2 3))))
        (it "{ let lst = []; for (let x of [1, 2, 3]) { if (x === 2) { continue; } lst.push(x); } lst; }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               '(1 3))))))
    (describe "function calls"
      (fn ()
        (it "I(1)"
            (fn ()
              (assert-equal
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
                       "function"))))
               1)))))
    (describe "function expressions"
      (fn ()
        (it "(<fn> 1)"
            (fn ()
              (assert-equal
               ((eval-estree
                 (new FunctionExpression
                      (list
                       (new Identifier "x"))
                      (new BlockStatement
                           (list
                            (new ReturnStatement
                                 (new Identifier "x")))))
                 lisp-1-environment)
                1)
               1)))
        (it "(<fn-with-rest-element> 1)"
            (fn ()
              (assert-equal
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
                1)
               (list 1))))
        (it "(<arrow-fn> 1)"
            (fn ()
              (assert-equal
               ((eval-estree
                 (new ArrowFunctionExpression
                      (list
                       (new Identifier "x"))
                      (new BlockStatement
                           (list
                            (new ReturnStatement
                                 (new Identifier "x")))))
                 lisp-1-environment)
                1)
               1)))))
    (describe "function definitions"
      (fn ()
        (it "{ function I(x) { ... } I(1); }"
            (fn ()
              (assert-equal
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
                lisp-1-environment)
               1)))))
    (describe "try...catch"
      (fn ()
        (it "{ try { throw new Error(); } catch (err) { } 1; }"
            (fn ()
              (assert-equal
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
                lang-environment)
               1)))))
    (describe "class expressions"
      (fn ()
        (it "{ const Foo = class {}; const bar = new Foo(); bar instanceof Foo; }"
            (fn ()
              (assert-equal
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
                lang-environment)
               #t)))
        (it "{ const Foo = class {}; const Bar = class extends Foo {}; const baz = new Bar(); baz instanceof Foo; }"
            (fn ()
              (assert-equal
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
                lang-environment)
               #t)))
        (it "{ const Foo = class { bar = 'baz'; }; const quux = new Foo(); quux.bar; }"
            (fn ()
              (assert-equal
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
                lang-environment)
               "baz")))
        (it "{ const Foo = class { bar() { return 'baz'; } }; const quux = new Foo(); quux.bar(); }"
            (fn ()
              (assert-equal
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
                lang-environment)
               "baz")))))
    (describe "class declarations"
      (fn ()
        (it "{ class Foo {} const bar = new Foo(); bar instanceof Foo; }"
            (fn ()
              (assert-equal
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
                lang-environment)
               #t)))
        (it "{ class Foo { bar = 'bar'; baz() { return this.bar; } } const quux = new Foo(); quux.baz(); }"
            (fn ()
              (assert-equal
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
                lang-environment)
               "baz")))
        (it "{ class Foo { bar; constructor(bar) { this.bar = bar; } } const quux = new Foo('bar'); quux.bar; }"
            (fn ()
              (assert-equal
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
                lang-environment)
               "bar")))))))
