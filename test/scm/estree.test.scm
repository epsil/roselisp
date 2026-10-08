(require (only-in "@typescript-eslint/typescript-estree"
                  parse))
(require (only-in "../../src/ts/estree"
                  BinaryExpression))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 :describe "ESTree"
 xit> (it "const hello: string = 'world';"
          (let* ((code "const hello: string = 'world';")
                 (ast (parse code (object :loc #t
                                          :range #t))))
            ast))
 (object)
 xit> (it "foo();"
          (let* ((code "foo();")
                 (ast (parse code (object :loc #t
                                          :range #t))))
            ast))
 (object)
 xit> (it "foo.bar();"
          (let* ((code "foo.bar();")
                 (ast (parse code (object :loc #t
                                          :range #t))))
            ast))
 (object)
 xit> (it "let foo = function () { return undefined };"
          (let* ((code "let foo = function () { return undefined };")
                 (ast (parse code (object :loc #t
                                          :range #t))))
            ast))
 (object))
