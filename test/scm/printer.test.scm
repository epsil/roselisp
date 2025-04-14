(require chai "chai")
(require (only-in "../../src/ts/estree"
                  ArrowFunctionExpression
                  AssignmentExpression
                  AssignmentPattern
                  BinaryExpression
                  BlockStatement
                  CallExpression
                  ExportAllDeclaration
                  ExpressionStatement
                  ForStatement
                  FunctionExpression
                  Identifier
                  IfStatement
                  LeadingComment
                  Literal
                  LogicalExpression
                  MemberExpression
                  ObjectExpression
                  ReturnStatement
                  TSAnyKeyword
                  TSAsExpression
                  TSFunctionType
                  TSNumberKeyword
                  TSTypeAliasDeclaration
                  TSTypeAnnotation
                  TSTypeParameterInstantiation
                  TSTypeReference
                  TaggedTemplateExpression
                  TemplateElement
                  TemplateLiteral
                  TrailingComment
                  VariableDeclaration
                  VariableDeclarator
                  WhileStatement))
(require (only-in "../../src/ts/printer"
                  print-estree
                  write-to-string))
(require (only-in "../../src/ts/sexp"
                  s
                  sexp))
(require (only-in "./test-util"
                  assert-equal))

(describe "print-estree"
  (fn ()
    (describe "ESTree"
      (fn ()
        (describe "Identifier"
          (fn ()
            (it "foo"
                (fn ()
                  (assert-equal
                   (print-estree (new Identifier "foo"))
                   "foo")))
            (it "foo, leading comment"
                (fn ()
                  (assert-equal
                   (print-estree
                    (~> (new Identifier "foo")
                        (send addComment
                              (new LeadingComment
                                   "comment")))
                    (js-obj "comments" #t))
                   "// comment
foo")))
            (it "foo, multi-line comment"
                (fn ()
                  (assert-equal
                   (print-estree
                    (~> (new Identifier "foo")
                        (send addComment
                              (new LeadingComment
                                   "multi-line
comment")))
                    (js-obj "comments" #t))
                   "// multi-line
// comment
foo")))))
        (describe "FunctionExpression"
          (fn ()
            (it "function (x) { return x; }"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new FunctionExpression
                         (list
                          (new Identifier "x"))
                         (new BlockStatement
                              (list
                               (new ReturnStatement
                                    (new Identifier "x"))))))
                   "function (x) {
  return x;
}")))))
        (describe "CallExpression"
          (fn ()
            (it "foo()"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new CallExpression
                         (new Identifier "foo")))
                   "foo()")))
            (it "foo(1)"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new CallExpression
                         (new Identifier "foo")
                         (list
                          (new Literal 1))))
                   "foo(1)")))
            (it "foo(1, 2)"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new CallExpression
                         (new Identifier "foo")
                         (list
                          (new Literal 1)
                          (new Literal 2))))
                   "foo(1, 2)")))
            (it "foo.bar()"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new CallExpression
                         (new MemberExpression
                              (new Identifier "foo")
                              (new Identifier "bar"))
                         (list)))
                   "foo.bar()")))
            (it "({}).foo()"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new CallExpression
                         (new MemberExpression
                              (new ObjectExpression)
                              (new Identifier "foo"))
                         (list)))
                   "({}).foo()")))))
        (describe "BinaryExpression"
          (fn ()
            (it "a + b"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new BinaryExpression
                         "+"
                         (new Identifier "a")
                         (new Identifier "b")))
                   "a + b")))
            (it "a + b, leading comment"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new BinaryExpression
                         "+"
                         (~> (new Identifier "a")
                             (send addComment
                                   (new LeadingComment
                                        "comment")))
                         (new Identifier "b"))
                    (js-obj "comments" #t))
                   "(
 // comment
 a +
 b
)")))
            (it "(a + b) + c, leading comment"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new BinaryExpression
                         "+"
                         (~> (new BinaryExpression
                                  "+"
                                  (new Identifier "a")
                                  (new Identifier "b"))
                             (send addComment
                                   (new LeadingComment
                                        "comment")))
                         (new Identifier "c"))
                    (js-obj "comments" #t))
                   "(
 // comment
 a + b +
 c
)")))
            (it "(a + b) + c, trailing comment"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new BinaryExpression
                         "+"
                         (~> (new BinaryExpression
                                  "+"
                                  (new Identifier "a")
                                  (new Identifier "b"))
                             (send addComment
                                   (new TrailingComment
                                        "comment")))
                         (new Identifier "c"))
                    (js-obj "comments" #t))
                   "(
 a + b // comment
 +
 c
)")))
            (it "a + b + c"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new BinaryExpression
                         "+"
                         (new BinaryExpression
                              "+"
                              (new Identifier "a")
                              (new Identifier "b"))
                         (new Identifier "c")))
                   "a + b + c")))
            (xit "a + b + c + d, leading comments"
                 (fn ()
                   (assert-equal
                    (print-estree
                     (new BinaryExpression
                          "+"
                          (new BinaryExpression
                               "+"
                               (new BinaryExpression
                                    "+"
                                    (new Identifier "a")
                                    (new Identifier "b"))
                               (new Identifier "c"))
                          (new Identifier "d")))
                    "a + b + c")))
            (it "a < b"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new BinaryExpression
                         "<"
                         (new Identifier "a")
                         (new Identifier "b")))
                   "a < b")))
            (it "a < b < c"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new BinaryExpression
                         "<"
                         (new BinaryExpression
                              "<"
                              (new Identifier "a")
                              (new Identifier "b"))
                         (new Identifier "c")))
                   "a < b < c")))))
        (describe "LogicalExpression"
          (fn ()
            (it "a && b"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new LogicalExpression
                         "&&"
                         (new Identifier "a")
                         (new Identifier "b")))
                   "a && b")))
            (it "a || b"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new LogicalExpression
                         "||"
                         (new Identifier "a")
                         (new Identifier "b")))
                   "a || b")))))
        (describe "IfStatement"
          (fn ()
            (it "if (x) { x = 1; }"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new IfStatement
                         (new Identifier "x")
                         (new BlockStatement
                              (list
                               (new ExpressionStatement
                                    (new AssignmentExpression
                                         "="
                                         (new Identifier "x")
                                         (new Literal 1)))))))
                   "if (x) {
  x = 1;
}")))
            (it "if ((x = 1)) { x = 1; }"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new IfStatement
                         (new AssignmentExpression
                              "="
                              (new Identifier "x")
                              (new Literal 1))
                         (new BlockStatement
                              (list
                               (new ExpressionStatement
                                    (new AssignmentExpression
                                         "="
                                         (new Identifier "x")
                                         (new Literal 1)))))))
                   "if ((x = 1)) {
  x = 1;
}")))))
        (describe "WhileStatement"
          (fn ()
            (it "while (x) { x = 1; }"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new WhileStatement
                         (new Identifier "x")
                         (new BlockStatement
                              (list
                               (new ExpressionStatement
                                    (new AssignmentExpression
                                         "="
                                         (new Identifier "x")
                                         (new Literal 1)))))))
                   "while (x) {
  x = 1;
}")))
            (it "while ((x = 1)) { x = 1; }"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new WhileStatement
                         (new AssignmentExpression
                              "="
                              (new Identifier "x")
                              (new Literal 1))
                         (new BlockStatement
                              (list
                               (new ExpressionStatement
                                    (new AssignmentExpression
                                         "="
                                         (new Identifier "x")
                                         (new Literal 1)))))))
                   "while ((x = 1)) {
  x = 1;
}")))))
        (describe "ForStatement"
          (fn ()
            (it "for (i = 0; i < 10; i = i + 1) { x = 1; }"
                (fn ()
                  (assert-equal
                   (print-estree
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
                                         (new Literal 1)))))))
                   "for (i = 0; i < 10; i = i + 1) {
  x = 1;
}")))))
        (describe "ReturnStatement"
          (fn ()
            (it "return 0;"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new ReturnStatement
                         (new Literal 0)))
                   "return 0;")))
            (it "return ( ... );"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new ReturnStatement
                         (~> (new Literal 0)
                             (send addComment
                                   (new LeadingComment
                                        "comment"))))
                    (js-obj "comments" #t))
                   "return (
  // comment
  0
);")))))))
    (describe "TSESTree"
      (fn ()
        (describe "TSTypeAliasDeclaration"
          (fn ()
            (it "const x: number = 1;"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new TSTypeAliasDeclaration
                         (new Identifier "X")
                         (new TSNumberKeyword))
                    (js-obj "language" "TypeScript"))
                   "type X = number;")))))
        (describe "TSAsExpression"
          (fn ()
            (it "1 as number"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new TSAsExpression
                         (new Literal 1)
                         (new TSNumberKeyword))
                    (js-obj "language" "TypeScript"))
                   "1 as number")))
            (it "x as any"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new TSAsExpression
                         (new Identifier "x")
                         (new TSAnyKeyword))
                    (js-obj "language" "TypeScript"))
                   "x as any")))
            (it "x as Foo"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new TSAsExpression
                         (new Identifier "x")
                         (new TSTypeReference
                              (new Identifier "Foo")))
                    (js-obj "language" "TypeScript"))
                   "x as Foo")))
            (it "x as Promise<any>"
                (fn ()
                  (assert-equal
                   (print-estree
                    (new TSAsExpression
                         (new Identifier "x")
                         (new TSTypeReference
                              (new Identifier "Promise")
                              (new TSTypeParameterInstantiation
                                   (list
                                    (new TSAnyKeyword)))))
                    (js-obj "language" "TypeScript"))
                   "x as Promise<any>")))))))
    (describe "VariableDeclaration"
      (fn ()
        (it "const x: number = 1;"
            (fn ()
              (assert-equal
               (print-estree
                (new VariableDeclaration
                     (list
                      (new VariableDeclarator
                           (~> (new Identifier "x")
                               (send setType
                                     (new TSNumberKeyword)))
                           (new Literal 1)))
                     "const")
                (js-obj "language" "TypeScript"))
               "const x: number = 1;")))))
    (describe "FunctionExpression"
      (fn ()
        (it "function (x: number): number { return x; }"
            (fn ()
              (assert-equal
               (print-estree
                (~> (new FunctionExpression
                         (list
                          (~> (new Identifier "x")
                              (send setType
                                    (new TSNumberKeyword))))
                         (new BlockStatement
                              (list
                               (new ReturnStatement
                                    (new Identifier "x")))))
                    (send setType
                          (new TSNumberKeyword)))
                (js-obj "language" "TypeScript"))
               "function (x: number): number {
  return x;
}")))
        (it "function (x: number = 1): number { return x; }"
            (fn ()
              (assert-equal
               (print-estree
                (~> (new FunctionExpression
                         (list
                          (new AssignmentPattern
                               (~> (new Identifier "x")
                                   (send setType
                                         (new TSNumberKeyword)))
                               (new Literal 1)))
                         (new BlockStatement
                              (list
                               (new ReturnStatement
                                    (new Identifier "x")))))
                    (send setType
                          (new TSNumberKeyword)))
                (js-obj "language" "TypeScript"))
               "function (x: number = 1): number {
  return x;
}")))
        (it "function (x: number = y): number { return x; }"
            (fn ()
              (assert-equal
               (print-estree
                (~> (new FunctionExpression
                         (list
                          (new AssignmentPattern
                               (~>
                                (new Identifier "x")
                                (send setType
                                      (new TSNumberKeyword)))
                               (new Identifier "y")))
                         (new BlockStatement
                              (list
                               (new ReturnStatement
                                    (new Identifier "x")))))
                    (send setType
                          (new TSNumberKeyword)))
                (js-obj "language" "TypeScript"))
               "function (x: number = y): number {
  return x;
}")))))
    (describe "ArrowFunctionExpression"
      (fn ()
        (it "function (x: number): number { return x; }"
            (fn ()
              (assert-equal
               (print-estree
                (~> (new ArrowFunctionExpression
                         (list
                          (~> (new Identifier "x")
                              (send setType
                                    (new TSNumberKeyword))))
                         (new BlockStatement
                              (list
                               (new ReturnStatement
                                    (new Identifier "x")))))
                    (send setType
                          (new TSNumberKeyword)))
                (js-obj "language" "TypeScript"))
               "(x: number): number => {
  return x;
}")))
        (it "const f: (a: any) => any = (x: any): any => { return x; };"
            (fn ()
              (assert-equal
               (print-estree
                (new VariableDeclaration
                     (list
                      (new VariableDeclarator
                           (~> (new Identifier "f")
                               (send setType
                                     (new TSFunctionType
                                          (list
                                           (~> (new Identifier "a")
                                               (send setType
                                                     (new TSAnyKeyword))))
                                          (new TSTypeAnnotation
                                               (new TSAnyKeyword)))))
                           (new ArrowFunctionExpression
                                (list
                                 (new Identifier "x"))
                                (new BlockStatement
                                     (list
                                      (new ReturnStatement
                                           (new Identifier "x")))))))
                     "const")
                (js-obj "language" "TypeScript"))
               "const f: (a: any) => any = (x: any): any => {
  return x;
};")))))
    (describe "TemplateLiteral"
      (fn ()
        (it "`foo`"
            (fn ()
              (assert-equal
               (print-estree
                (new TemplateLiteral
                     (list
                      (new TemplateElement #t "foo")))
                (js-obj "language" "TypeScript"))
               "`foo`")))
        (it "`foo
bar`"
            (fn ()
              (assert-equal
               (print-estree
                (new TemplateLiteral
                     (list
                      (new TemplateElement #t "foo
bar")))
                (js-obj "language" "TypeScript"))
               "`foo
bar`")))
        (it "`foo
\\`bar`"
            (fn ()
              (assert-equal
               (print-estree
                (new TemplateLiteral
                     (list
                      (new TemplateElement #t "foo
`bar")))
                (js-obj "language" "TypeScript"))
               "`foo
\\`bar`")))
        (it "function (): any { return `foo
bar`; }"
            (fn ()
              (assert-equal
               (print-estree
                (~> (new FunctionExpression
                         '()
                         (new BlockStatement
                              (list
                               (new ReturnStatement
                                    (new TemplateLiteral
                                         (list
                                          (new TemplateElement
                                               #t
                                               "foo
bar")))))))
                    (send setType
                          (new TSAnyKeyword)))
                (js-obj "language" "TypeScript"))
               "function (): any {
  return `foo
bar`;
}")))))
    (describe "TaggedTemplateExpression"
      (fn ()
        (it "foo`bar`"
            (fn ()
              (assert-equal
               (print-estree
                (new TaggedTemplateExpression
                     (new Identifier "foo")
                     (new TemplateLiteral
                          (list
                           (new TemplateElement
                                #t
                                "bar"))))
                (js-obj "language" "TypeScript"))
               "foo`bar`")))
        (it "foo`bar
baz`"
            (fn ()
              (assert-equal
               (print-estree
                (new TaggedTemplateExpression
                     (new Identifier "foo")
                     (new TemplateLiteral
                          (list
                           (new TemplateElement
                                #t
                                "bar
baz"))))
                (js-obj "language" "TypeScript"))
               "foo`bar
baz`")))
        (it "function (): any { return foo`bar
baz`; }"
            (fn ()
              (assert-equal
               (print-estree
                (~> (new FunctionExpression
                         '()
                         (new BlockStatement
                              (list
                               (new ReturnStatement
                                    (new TaggedTemplateExpression
                                         (new Identifier "foo")
                                         (new TemplateLiteral
                                              (list
                                               (new TemplateElement
                                                    #t
                                                    "bar
baz"))))))))
                    (send setType
                          (new TSAnyKeyword)))
                (js-obj "language" "TypeScript"))
               "function (): any {
  return foo`bar
baz`;
}")))
        (it "function (): any { return function (): any { return foo`bar
baz`; }; }"
            (fn ()
              (assert-equal
               (print-estree
                (~> (new FunctionExpression
                         '()
                         (new BlockStatement
                              (list
                               (new ReturnStatement
                                    (~> (new FunctionExpression
                                             '()
                                             (new
                                              BlockStatement
                                              (list
                                               (new
                                                ReturnStatement
                                                (new
                                                 TaggedTemplateExpression
                                                 (new Identifier "foo")
                                                 (new
                                                  TemplateLiteral
                                                  (list
                                                   (new TemplateElement
                                                        #t
                                                        "bar
baz"))))))))
                                        (send setType
                                              (new TSAnyKeyword)))))))
                    (send setType
                          (new TSAnyKeyword)))
                (js-obj "language" "TypeScript"))
               "function (): any {
  return function (): any {
    return foo`bar
baz`;
  };
}")))))
    (describe "ExportAllDeclaration"
      (fn ()
        (it "export * from \"foo\";"
            (fn ()
              (assert-equal
               (print-estree
                (new ExportAllDeclaration
                     (new Literal "foo"))
                (js-obj "language" "JavaScript"))
               "export * from 'foo';")))))))

(describe "write-to-string"
  (fn ()
    (describe "symbol"
      (fn ()
        (it "foo"
            (fn ()
              (assert-equal
               (write-to-string 'foo)
               "foo")))))
    (describe "string"
      (fn ()
        (it "\"foo\""
            (fn ()
              (assert-equal
               (write-to-string "foo")
               "\"foo\"")))
        (it "\"foo
bar\""
            (fn ()
              (assert-equal
               (write-to-string
                '(begin
                   "foo
bar")
                (js-obj "pretty" #t))
               "(begin
  \"foo
bar\")")))
        (it "\"\\\"foo bar\\\"\""
            (fn ()
              (assert-equal
               (write-to-string
                '(begin
                   "\"foo bar\"")
                (js-obj "pretty" #t))
               "(begin
  \"\\\"foo bar\\\"\")")))))
    (describe "number"
      (fn ()
        (it "1"
            (fn ()
              (assert-equal
               (write-to-string 1)
               "1")))))
    (describe "function call"
      (fn ()
        (it "(foo)"
            (fn ()
              (assert-equal
               (write-to-string '(foo))
               "(foo)")))
        (it "(foo bar)"
            (fn ()
              (assert-equal
               (write-to-string
                '(foo
                  bar))
               "(foo bar)")))
        (it "(foo \"bar\")"
            (fn ()
              (assert-equal
               (write-to-string
                '(foo
                  "bar"))
               "(foo \"bar\")")))
        (it "(\"foo\" \"bar\")"
            (fn ()
              (assert-equal
               (write-to-string
                '("foo"
                  "bar"))
               "(\"foo\" \"bar\")")))
        (it "(foo (bar))"
            (fn ()
              (assert-equal
               (write-to-string
                '(foo
                  (bar)))
               "(foo (bar))")))
        (it "(foo (bar (baz)))"
            (fn ()
              (assert-equal
               (write-to-string
                '(foo
                  (bar
                   (baz))))
               "(foo (bar (baz)))")))))
    (describe "begin"
      (fn ()
        (it "(begin (foo) (bar)), pretty"
            (fn ()
              (assert-equal
               (write-to-string
                '(begin
                   (foo)
                   (bar))
                (js-obj "pretty" #t))
               "(begin
  (foo)
  (bar))")))
        (it "(begin (foo (bar)) (bar (baz))), pretty"
            (fn ()
              (assert-equal
               (write-to-string
                '(begin
                   (foo (bar))
                   (bar (baz)))
                (js-obj "pretty" #t))
               "(begin
  (foo (bar))
  (bar (baz)))")))))
    (describe "cond"
      (fn ()
        (it "(cond (foo (bar)) (bar (baz))), pretty"
            (fn ()
              (assert-equal
               (write-to-string
                '(cond
                  (foo
                   (bar))
                  (bar
                   (baz)))
                (js-obj "pretty" #t))
               "(cond
 (foo
  (bar))
 (bar
  (baz)))")))))
    (describe "if"
      (fn ()
        (it "(if foo bar baz), pretty"
            (fn ()
              (assert-equal
               (write-to-string
                '(if foo
                     bar
                     baz)
                (js-obj "pretty" #t))
               "(if foo
    bar
    baz)")))))
    (describe "when"
      (fn ()
        (it "(when foo bar), pretty"
            (fn ()
              (assert-equal
               (write-to-string
                '(when foo
                   bar)
                (js-obj "pretty" #t))
               "(when foo
  bar)")))))
    (describe "unless"
      (fn ()
        (it "(unless foo bar), pretty"
            (fn ()
              (assert-equal
               (write-to-string
                '(unless foo
                   bar)
                (js-obj "pretty" #t))
               "(unless foo
  bar)")))))
    (describe "define"
      (fn ()
        (it "(define (foo x) x), pretty"
            (fn ()
              (assert-equal
               (write-to-string
                '(define (foo x)
                   x)
                (js-obj "pretty" #t))
               "(define (foo x)
  x)")))))
    (describe "module"
      (fn ()
        (it "(module m scheme ... (define ...) ...), pretty"
            (fn ()
              (assert-equal
               (write-to-string
                '(module m scheme
                   (define (foo x)
                     x)

                   (define (bar y)
                     y))
                (js-obj "pretty" #t))
               "(module m scheme
  (define (foo x)
    x)

  (define (bar y)
    y))"))))
      (it "(module m scheme ... (define ...) ...), pretty"
          (fn ()
            (assert-equal
             (write-to-string
              '(module m scheme
                 (define (foo x)
                   x)

                 (define (bar y)
                   y))
              (js-obj "noModuleForm" #t "pretty" #t))
             "(define (foo x)
  x)

(define (bar y)
  y)"))))
    (describe "unsorted"
      (fn ()
        (it "number 1"
            (fn ()
              (assert-equal
               (write-to-string 1)
               "1")))
        (it "symbol foo"
            (fn ()
              (assert-equal
               (write-to-string 'foo)
               "foo")))
        (it "string \"foo\""
            (fn ()
              (assert-equal
               (write-to-string "foo")
               "\"foo\"")))
        (it "string \"foo\\bar\""
            (fn ()
              (assert-equal
               (write-to-string "foo\\bar")
               "\"foo\\\\bar\"")))
        (it "string \"\\\""
            (fn ()
              (assert-equal
               (write-to-string "\\")
               "\"\\\\\"")))
        (it "string \"foo\\\"bar\""
            (fn ()
              (assert-equal
               (write-to-string "foo\"bar")
               "\"foo\\\"bar\"")))
        (it "list ()"
            (fn ()
              (assert-equal
               (write-to-string '())
               "()")))
        (it "cons cell (1 . 2)"
            (fn ()
              (assert-equal
               (write-to-string '(1 . 2))
               "(1 . 2)")))))))
