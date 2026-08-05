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
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; `print-estree`
 > (describe "print-estree")
 _
 > (it "foo"
       (print-estree (new Identifier "foo")))
 "foo"
 > (it "foo, leading comment"
       (print-estree
        (~> (new Identifier "foo")
            (send add-comment
                  (new LeadingComment
                       "comment")))
        (js/obj :comments #t)))
 "// comment
foo"
 > (it "foo, multi-line comment"
       (print-estree
        (~> (new Identifier "foo")
            (send add-comment
                  (new LeadingComment
                       "multi-line
comment")))
        (js/obj :comments #t)))
 "// multi-line
// comment
foo"
 > (it "function (x) { return x; }"
       (print-estree
        (new FunctionExpression
             (list
              (new Identifier "x"))
             (new BlockStatement
                  (list
                   (new ReturnStatement
                        (new Identifier "x")))))))
 "function (x) {
  return x;
}"
 > (it "foo()"
       (print-estree
        (new CallExpression
             (new Identifier "foo"))))
 "foo()"
 > (it "foo(1)"
       (print-estree
        (new CallExpression
             (new Identifier "foo")
             (list
              (new Literal 1)))))
 "foo(1)"
 > (it "foo(1, 2)"
       (print-estree
        (new CallExpression
             (new Identifier "foo")
             (list
              (new Literal 1)
              (new Literal 2)))))
 "foo(1, 2)"
 > (it "foo.bar()"
       (print-estree
        (new CallExpression
             (new MemberExpression
                  (new Identifier "foo")
                  (new Identifier "bar"))
             (list))))
 "foo.bar()"
 > (it "({}).foo()"
       (print-estree
        (new CallExpression
             (new MemberExpression
                  (new ObjectExpression)
                  (new Identifier "foo"))
             (list))))
 "({}).foo()"
 > (it "a + b"
       (print-estree
        (new BinaryExpression
             "+"
             (new Identifier "a")
             (new Identifier "b"))))
 "a + b"
 > (it "a + b, leading comment"
       (print-estree
        (new BinaryExpression
             "+"
             (~> (new Identifier "a")
                 (send add-comment
                       (new LeadingComment
                            "comment")))
             (new Identifier "b"))
        (js/obj :comments #t)))
 "(
 // comment
 a +
 b
)"
 > (it "(a + b) + c, leading comment"
       (print-estree
        (new BinaryExpression
             "+"
             (~> (new BinaryExpression
                      "+"
                      (new Identifier "a")
                      (new Identifier "b"))
                 (send add-comment
                       (new LeadingComment
                            "comment")))
             (new Identifier "c"))
        (js/obj :comments #t)))
 "(
 // comment
 a + b +
 c
)"
 > (it "(a + b) + c, trailing comment"
       (print-estree
        (new BinaryExpression
             "+"
             (~> (new BinaryExpression
                      "+"
                      (new Identifier "a")
                      (new Identifier "b"))
                 (send add-comment
                       (new TrailingComment
                            "comment")))
             (new Identifier "c"))
        (js/obj :comments #t)))
 "(
 a + b // comment
 +
 c
)"
 > (it "a + b + c"
       (print-estree
        (new BinaryExpression
             "+"
             (new BinaryExpression
                  "+"
                  (new Identifier "a")
                  (new Identifier "b"))
             (new Identifier "c"))))
 "a + b + c"
 xit> (it "a + b + c + d, leading comments"
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
                (new Identifier "d"))))
 "a + b + c"
 > (it "a < b"
       (print-estree
        (new BinaryExpression
             "<"
             (new Identifier "a")
             (new Identifier "b"))))
 "a < b"
 > (it "a < b < c"
       (print-estree
        (new BinaryExpression
             "<"
             (new BinaryExpression
                  "<"
                  (new Identifier "a")
                  (new Identifier "b"))
             (new Identifier "c"))))
 "a < b < c"
 > (it "a && b"
       (print-estree
        (new LogicalExpression
             "&&"
             (new Identifier "a")
             (new Identifier "b"))))
 "a && b"
 > (it "a || b"
       (print-estree
        (new LogicalExpression
             "||"
             (new Identifier "a")
             (new Identifier "b"))))
 "a || b"
 > (it "if (x) { x = 1; }"
       (print-estree
        (new IfStatement
             (new Identifier "x")
             (new BlockStatement
                  (list
                   (new ExpressionStatement
                        (new AssignmentExpression
                             "="
                             (new Identifier "x")
                             (new Literal 1))))))))
 "if (x) {
  x = 1;
}"
 > (it "if ((x = 1)) { x = 1; }"
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
                             (new Literal 1))))))))
 "if ((x = 1)) {
  x = 1;
}"
 > (it "while (x) { x = 1; }"
       (print-estree
        (new WhileStatement
             (new Identifier "x")
             (new BlockStatement
                  (list
                   (new ExpressionStatement
                        (new AssignmentExpression
                             "="
                             (new Identifier "x")
                             (new Literal 1))))))))
 "while (x) {
  x = 1;
}"
 > (it "while ((x = 1)) { x = 1; }"
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
                             (new Literal 1))))))))
 "while ((x = 1)) {
  x = 1;
}"
 > (it "for (i = 0; i < 10; i = i + 1) { x = 1; }"
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
                             (new Literal 1))))))))
 "for (i = 0; i < 10; i = i + 1) {
  x = 1;
}"
 > (print-estree
    (new ReturnStatement
         (new Literal 0)))
 "return 0;"
 > (it "return ( ... );"
       (print-estree
        (new ReturnStatement
             (~> (new Literal 0)
                 (send add-comment
                       (new LeadingComment
                            "comment"))))
        (js/obj :comments #t)))
 "return (
  // comment
  0
);"
 > (it "const x: number = 1;"
       (print-estree
        (new TSTypeAliasDeclaration
             (new Identifier "X")
             (new TSNumberKeyword))
        (js/obj :language "typescript")))
 "type X = number;"
 > (it "1 as number"
       (print-estree
        (new TSAsExpression
             (new Literal 1)
             (new TSNumberKeyword))
        (js/obj :language "typescript")))
 "1 as number"
 > (it "x as any"
       (print-estree
        (new TSAsExpression
             (new Identifier "x")
             (new TSAnyKeyword))
        (js/obj :language "typescript")))
 "x as any"
 > (it "x as Foo"
       (print-estree
        (new TSAsExpression
             (new Identifier "x")
             (new TSTypeReference
                  (new Identifier "Foo")))
        (js/obj :language "typescript")))
 "x as Foo"
 > (it "x as Promise<any>"
       (print-estree
        (new TSAsExpression
             (new Identifier "x")
             (new TSTypeReference
                  (new Identifier "Promise")
                  (new TSTypeParameterInstantiation
                       (list
                        (new TSAnyKeyword)))))
        (js/obj :language "typescript")))
 "x as Promise<any>"
 > (it "const x: number = 1;"
       (print-estree
        (new VariableDeclaration
             (list
              (new VariableDeclarator
                   (~> (new Identifier "x")
                       (send set-type
                             (new TSNumberKeyword)))
                   (new Literal 1)))
             "const")
        (js/obj :language "typescript")))
 "const x: number = 1;"
 > (it "function (x: number): number { return x; }"
       (print-estree
        (~> (new FunctionExpression
                 (list
                  (~> (new Identifier "x")
                      (send set-type
                            (new TSNumberKeyword))))
                 (new BlockStatement
                      (list
                       (new ReturnStatement
                            (new Identifier "x")))))
            (send set-type
                  (new TSNumberKeyword)))
        (js/obj :language "typescript")))
 "function (x: number): number {
  return x;
}"
 > (it "function (x: number = 1): number { return x; }"
       (print-estree
        (~> (new FunctionExpression
                 (list
                  (new AssignmentPattern
                       (~> (new Identifier "x")
                           (send set-type
                                 (new TSNumberKeyword)))
                       (new Literal 1)))
                 (new BlockStatement
                      (list
                       (new ReturnStatement
                            (new Identifier "x")))))
            (send set-type
                  (new TSNumberKeyword)))
        (js/obj :language "typescript")))
 "function (x: number = 1): number {
  return x;
}"
 > (it "function (x: number = y): number { return x; }"
       (print-estree
        (~> (new FunctionExpression
                 (list
                  (new AssignmentPattern
                       (~>
                        (new Identifier "x")
                        (send set-type
                              (new TSNumberKeyword)))
                       (new Identifier "y")))
                 (new BlockStatement
                      (list
                       (new ReturnStatement
                            (new Identifier "x")))))
            (send set-type
                  (new TSNumberKeyword)))
        (js/obj :language "typescript")))
 "function (x: number = y): number {
  return x;
}"
 > (it "function (x: number): number { return x; }"
       (print-estree
        (~> (new ArrowFunctionExpression
                 (list
                  (~> (new Identifier "x")
                      (send set-type
                            (new TSNumberKeyword))))
                 (new BlockStatement
                      (list
                       (new ReturnStatement
                            (new Identifier "x")))))
            (send set-type
                  (new TSNumberKeyword)))
        (js/obj :language "typescript")))
 "(x: number): number => {
  return x;
}"
 > (it "const f: (a: any) => any = (x: any): any => { return x; };"
       (print-estree
        (new VariableDeclaration
             (list
              (new VariableDeclarator
                   (~> (new Identifier "f")
                       (send set-type
                             (new TSFunctionType
                                  (list
                                   (~> (new Identifier "a")
                                       (send set-type
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
        (js/obj :language "typescript")))
 "const f: (a: any) => any = (x: any): any => {
  return x;
};"
 > (it "`foo`"
       (print-estree
        (new TemplateLiteral
             (list
              (new TemplateElement #t "foo")))
        (js/obj :language "typescript")))
 "`foo`"
 > (it "`foo
bar`"
       (print-estree
        (new TemplateLiteral
             (list
              (new TemplateElement #t "foo
bar")))
        (js/obj :language "typescript")))
 "`foo
bar`"
 > (it "`foo
\\`bar`"
       (print-estree
        (new TemplateLiteral
             (list
              (new TemplateElement #t "foo
`bar")))
        (js/obj :language "typescript")))
 "`foo
\\`bar`"
 > (it "function (): any { return `foo
bar`; }"
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
            (send set-type
                  (new TSAnyKeyword)))
        (js/obj :language "typescript")))
 "function (): any {
  return `foo
bar`;
}"
 > (it "foo`bar`"
       (print-estree
        (new TaggedTemplateExpression
             (new Identifier "foo")
             (new TemplateLiteral
                  (list
                   (new TemplateElement
                        #t
                        "bar"))))
        (js/obj :language "typescript")))
 "foo`bar`"
 > (it "foo`bar
baz`"
       (print-estree
        (new TaggedTemplateExpression
             (new Identifier "foo")
             (new TemplateLiteral
                  (list
                   (new TemplateElement
                        #t
                        "bar
baz"))))
        (js/obj :language "typescript")))
 "foo`bar
baz`"
 > (it "function (): any { return foo`bar
baz`; }"
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
            (send set-type
                  (new TSAnyKeyword)))
        (js/obj :language "typescript")))
 "function (): any {
  return foo`bar
baz`;
}"
 > (it "function (): any { return function (): any { return foo`bar
baz`; }; }"
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
                                (send set-type
                                      (new TSAnyKeyword)))))))
            (send set-type
                  (new TSAnyKeyword)))
        (js/obj :language "typescript")))
 "function (): any {
  return function (): any {
    return foo`bar
baz`;
  };
}"
 > (it "export * from \"foo\";"
       (print-estree
        (new ExportAllDeclaration
             (new Literal "foo"))
        (js/obj :language "javascript")))
 "export * from 'foo';"

 ;; `write-to-string`
 > (describe "write-to-string")
 _
 > (write-to-string 1)
 "1"
 > (write-to-string 'foo)
 "foo"
 > (write-to-string "foo")
 "\"foo\""
 > (write-to-string "foo")
 "\"foo\""
 > (write-to-string "foo\\bar")
 "\"foo\\\\bar\""
 > (write-to-string "\\")
 "\"\\\\\""
 > (write-to-string "foo\"bar")
 "\"foo\\\"bar\""
 > (write-to-string '())
 "()"
 > (write-to-string '(1 . 2))
 "(1 . 2)"
 > (write-to-string
    '(begin
       "foo
bar")
    (js/obj :pretty #t))
 "(begin
  \"foo
bar\")"
 > (write-to-string
    '(begin
       "\"foo bar\"")
    (js/obj :pretty #t))
 "(begin
  \"\\\"foo bar\\\"\")"
 > (write-to-string 1)
 "1"
 > (write-to-string '(foo))
 "(foo)"
 > (write-to-string
    '(foo
      bar))
 "(foo bar)"
 > (write-to-string
    '(foo
      "bar"))
 "(foo \"bar\")"
 > (write-to-string
    '("foo"
      "bar"))
 "(\"foo\" \"bar\")"
 > (write-to-string
    '(foo
      (bar)))
 "(foo (bar))"
 > (write-to-string
    '(foo
      (bar
       (baz))))
 "(foo (bar (baz)))"
 > (write-to-string
    '(begin
       (foo)
       (bar))
    (js/obj :pretty #t))
 "(begin
  (foo)
  (bar))"
 > (write-to-string
    '(begin
       (foo (bar))
       (bar (baz)))
    (js/obj :pretty #t))
 "(begin
  (foo (bar))
  (bar (baz)))"
 > (write-to-string
    '(cond
      (foo
       (bar))
      (bar
       (baz)))
    (js/obj :pretty #t))
 "(cond
 (foo
  (bar))
 (bar
  (baz)))"
 > (write-to-string
    '(if foo
         bar
         baz)
    (js/obj :pretty #t))
 "(if foo
    bar
    baz)"
 > (write-to-string
    '(when foo
       bar)
    (js/obj :pretty #t))
 "(when foo
  bar)"
 > (write-to-string
    '(unless foo
       bar)
    (js/obj :pretty #t))
 "(unless foo
  bar)"
 > (write-to-string
    '(define (foo x)
       x)
    (js/obj :pretty #t))
 "(define (foo x)
  x)"
 > (write-to-string
    '(module m scheme
       (define (foo x)
         x)

       (define (bar y)
         y))
    (js/obj :pretty #t))
 "(module m scheme
  (define (foo x)
    x)

  (define (bar y)
    y))"
 > (write-to-string
    '(module m scheme
       (define (foo x)
         x)

       (define (bar y)
         y))
    (js/obj :no-module-form #t
            :pretty #t))
 "(define (foo x)
  x)

(define (bar y)
  y)")
