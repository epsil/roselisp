(require (only-in "../../src/ts/estree"
                  ArrayExpression
                  ArrayPattern
                  AssignmentExpression
                  ExpressionStatement
                  Identifier
                  Literal
                  Program
                  UpdateExpression
                  VariableDeclaration
                  VariableDeclarator))
(require (only-in "../../src/ts/language"
                  let-vars-to-const-vars
                  lisp-environment
                  optimize-sexp
                  sexp))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 :describe "optimize-sexp"
 > (optimize-sexp '() lisp-environment)
 '()
 xit> (optimize-sexp
       '(let ((x 1))
          x)
       lisp-environment)
 '(begin
    (define x 1)
    x)
 xit> (optimize-sexp
       '(let-values (((y) (foo)))
          y)
       lisp-environment)
 '(begin
    (define-values (y) (foo))
    y)
 xit> (optimize-sexp
       '(define (f x)
          (let-values (((y) (foo)))
            y))
       lisp-environment)
 '(define (f x)
    (begin
      (define-values (y) (foo))
      y))
 xit> (optimize-sexp
       '(define (f x)
          `(let-values (((y) (foo)))
             y))
       lisp-environment)
 '(define (f x)
    `(let-values (((y) (foo)))
       y))
 xit> (optimize-sexp
       '(define (make-macro-function-form exp)
          (let* ((name (second exp))
                 (args (third exp))
                 (body (drop exp 3)))
            (when (list? name)
              (set! args (rest name))
              (set! name (first name))
              (set! body (drop exp 2)))
            `(lambda (exp env)
               (let-values ((,args (rest exp)))
                 ,@body))))
       lisp-environment)
 '(define (make-macro-function-form exp)
    (begin
      (define name (second exp))
      (define args (third exp))
      (define body (drop exp 3))
      (when (list? name)
        (set! args (rest name))
        (set! name (first name))
        (set! body (drop exp 2)))
      `(lambda (exp env)
         (let-values ((,args (rest exp)))
           ,@body))))

 :describe "let-vars-to-const-vars"
 > (it "empty program"
       (let-vars-to-const-vars
        (new Program)))
 (new Program)
 > (it "single uninitialized let variable"
       (let-vars-to-const-vars
        (new Program
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "foo")))
                   "let")))))
 (new Program
      (list
       (new VariableDeclaration
            (list
             (new VariableDeclarator
                  (new Identifier "foo")))
            "let")))
 > (it "single let variable"
       (let-vars-to-const-vars
        (new Program
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "foo")
                         (new Literal 1)))
                   "let")))))
 (new Program
      (list
       (new VariableDeclaration
            (list
             (new VariableDeclarator
                  (new Identifier "foo")
                  (new Literal 1)))
            "const")))
 > (it "single let variable with assignment"
       (let-vars-to-const-vars
        (new Program
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "foo")
                         (new Literal 1)))
                   "let")
              (new ExpressionStatement
                   (new AssignmentExpression
                        "="
                        (new Identifier "foo")
                        (new Literal 2)))))))
 (new Program
      (list
       (new VariableDeclaration
            (list
             (new VariableDeclarator
                  (new Identifier "foo")
                  (new Literal 1)))
            "let")
       (new ExpressionStatement
            (new AssignmentExpression
                 "="
                 (new Identifier "foo")
                 (new Literal 2)))))
 > (it "single let variable with destructuring initialization"
       (let-vars-to-const-vars
        (new Program
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new ArrayPattern
                              (list
                               (new Identifier "foo")))
                         (new ArrayExpression
                              (list
                               (new Literal 1)))))
                   "let")))))
 (new Program
      (list
       (new VariableDeclaration
            (list
             (new VariableDeclarator
                  (new ArrayPattern
                       (list
                        (new Identifier "foo")))
                  (new ArrayExpression
                       (list
                        (new Literal 1)))))
            "const")))
 > (it "single let variable with destructuring initialization and subsequent assignment"
       (let-vars-to-const-vars
        (new Program
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new ArrayPattern
                              (list
                               (new Identifier "foo")))
                         (new ArrayExpression
                              (list
                               (new Literal 1)))))
                   "let")
              (new ExpressionStatement
                   (new AssignmentExpression
                        "="
                        (new Identifier "foo")
                        (new Literal 2)))))))
 (new Program
      (list
       (new VariableDeclaration
            (list
             (new VariableDeclarator
                  (new ArrayPattern
                       (list
                        (new Identifier "foo")))
                  (new ArrayExpression
                       (list
                        (new Literal 1)))))
            "let")
       (new ExpressionStatement
            (new AssignmentExpression
                 "="
                 (new Identifier "foo")
                 (new Literal 2)))))
 > (it "single let variable with assignment by update expression"
       (let-vars-to-const-vars
        (new Program
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "foo")
                         (new Literal 1)))
                   "let")
              (new ExpressionStatement
                   (new UpdateExpression
                        "++"
                        (new Identifier "foo")
                        #f))))))
 (new Program
      (list
       (new VariableDeclaration
            (list
             (new VariableDeclarator
                  (new Identifier "foo")
                  (new Literal 1)))
            "let")
       (new ExpressionStatement
            (new UpdateExpression
                 "++"
                 (new Identifier "foo")
                 #f))))
 > (it "single let variable with destructuring assignment"
       (let-vars-to-const-vars
        (new Program
             (list
              (new VariableDeclaration
                   (list
                    (new VariableDeclarator
                         (new Identifier "foo")
                         (new Literal 1)))
                   "let")
              (new ExpressionStatement
                   (new AssignmentExpression
                        "="
                        (new ArrayPattern
                             (list
                              (new Identifier "foo")))
                        (new ArrayExpression
                             (list
                              (new Literal 2)))))))))
 (new Program
      (list
       (new VariableDeclaration
            (list
             (new VariableDeclarator
                  (new Identifier "foo")
                  (new Literal 1)))
            "let")
       (new ExpressionStatement
            (new AssignmentExpression
                 "="
                 (new ArrayPattern
                      (list
                       (new Identifier "foo")))
                 (new
                  ArrayExpression
                  (list
                   (new Literal 2))))))))
