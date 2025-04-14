(require chai "chai")
(require (only-in "@typescript-eslint/typescript-estree"
                  parse))
(require (only-in "../../src/ts/estree"
                  BinaryExpression))
(require (only-in "./test-util"
                  assert-equal))

(describe "ESTree"
  (fn ()
    (describe "parsing"
      (fn ()
        (xit "const hello: string = 'world';"
             (fn ()
               (define code
                 "const hello: string = 'world';")
               (define ast
                 (parse code (js-obj "loc" #t "range" #t)))
               (assert-equal
                ast
                (js-obj))))
        (xit "foo();"
             (fn ()
               (define code
                 "foo();")
               (define ast
                 (parse code (js-obj "loc" #t "range" #t)))
               (assert-equal
                ast
                (js-obj))))
        (xit "foo.bar();"
             (fn ()
               (define code
                 "foo.bar();")
               (define ast
                 (parse code (js-obj "loc" #t "range" #t)))
               (assert-equal
                ast
                (js-obj))))
        (xit "let foo = function () { return undefined };"
             (fn ()
               (define code
                 "let foo = function () { return undefined };")
               (define ast
                 (parse code (js-obj "loc" #t "range" #t)))
               (assert-equal
                ast
                (js-obj))))))))
