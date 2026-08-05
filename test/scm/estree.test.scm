(require (only-in "@typescript-eslint/typescript-estree"
                  parse))
(require (only-in "../../src/ts/estree"
                  BinaryExpression))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; ESTree
 > (describe "ESTree")
 _
 xit> (it "const hello: string = 'world';"
          (let* ((code "const hello: string = 'world';")
                 (ast (parse code (js/obj :loc #t
                                          :range #t))))
            ast))
 (js/obj)
 xit> (it "foo();"
          (let* ((code "foo();")
                 (ast (parse code (js/obj :loc #t
                                          :range #t))))
            ast))
 (js/obj)
 xit> (it "foo.bar();"
          (let* ((code "foo.bar();")
                 (ast (parse code (js/obj :loc #t
                                          :range #t))))
            ast))
 (js/obj)
 xit> (it "let foo = function () { return undefined };"
          (let* ((code "let foo = function () { return undefined };")
                 (ast (parse code (js/obj :loc #t
                                          :range #t))))
            ast))
 (js/obj))
