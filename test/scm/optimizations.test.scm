(require chai "chai")
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
(require (only-in "../../src/ts/rose"
                  make-rose))
(require (only-in "./test-util"
                  assert-equal))

(describe "optimize-sexp"
  (fn ()
    (it "()"
        (fn ()
          (assert-equal
           (optimize-sexp '() lisp-environment)
           '())))
    (xit "(let ((x 1)) x)"
         (fn ()
           (assert-equal
            (optimize-sexp
             '(let ((x 1))
                x)
             lisp-environment)
            '(begin
               (define x 1)
               x))))
    (xit "(let-values (((y) (foo))) y)"
         (fn ()
           (assert-equal
            (optimize-sexp
             '(let-values (((y) (foo)))
                y)
             lisp-environment)
            '(begin
               (define-values (y) (foo))
               y))))
    (xit "(define (f x) (let-values (((y) (foo))) y))"
         (fn ()
           (assert-equal
            (optimize-sexp
             '(define (f x)
                (let-values (((y) (foo)))
                  y))
             lisp-environment)
            '(define (f x)
               (begin
                 (define-values (y) (foo))
                 y)))))
    (xit "(define (f x) `(let-values (((y) (foo))) y))"
         (fn ()
           (assert-equal
            (optimize-sexp
             '(define (f x)
                `(let-values (((y) (foo)))
                   y))
             lisp-environment)
            '(define (f x)
               `(let-values (((y) (foo)))
                  y)))))
    (xit "(define (make-macro-function-form exp) ...)"
         (fn ()
           (assert-equal
            (optimize-sexp
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
                      ,@body)))))))))

;; (describe "rewrite-define-to-define-class"
;;   (fn ()
;;     (xit "()"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-define-to-define-class
;;              (make-rose
;;               '())
;;              lisp-environment)
;;             (make-rose
;;              '()))))
;;     (xit "(define Foo (class ...))"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-define-to-define-class
;;              (make-rose
;;               '(define Foo
;;                  (class object%
;;                    (define/public (foo)
;;                      this))))
;;              lisp-environment)
;;             (make-rose
;;              '(define-class Foo ()
;;                 (define/public (foo)
;;                   this))))))))

;; (describe "rewrite-let-to-begin-define"
;;   (fn ()
;;     (xit "()"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-let-to-begin-define
;;              (make-rose '())
;;              lisp-environment)
;;             (make-rose '()))))
;;     (xit "(let (x) x)"
;;         (fn ()
;;           (assert-equal
;;            (rewrite-let-to-begin-define
;;             (make-rose
;;              '(let (x)
;;                 x))
;;             lisp-environment)
;;            (make-rose
;;             '(begin
;;                (define x)
;;                x)))))
;;     (xit "(let ((x 1)) x)"
;;         (fn ()
;;           (assert-equal
;;            (rewrite-let-to-begin-define
;;             (make-rose
;;              '(let ((x 1))
;;                 x))
;;             lisp-environment)
;;            (make-rose
;;             '(begin
;;                (define x 1)
;;                x)))))
;;     (xit "(lambda (x) (let ((y 1)) y))"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-let-to-begin-define
;;              (make-rose
;;               '(lambda (x)
;;                  (let ((y 1))
;;                    y)))
;;              lisp-environment)
;;             (make-rose
;;              '(lambda (x)
;;                 (begin
;;                   (define y 1)
;;                   y))))))
;;     (xit "(lambda (x) (let ((x 1)) x))"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-let-to-begin-define
;;              (make-rose
;;               '(lambda (x)
;;                  (let ((x 1))
;;                    x)))
;;              lisp-environment)
;;             (make-rose
;;              '(lambda (x)
;;                 (let ((x 1))
;;                   x))))))
;;     (xit "(define (f x) (let ((x 1)) x))"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-let-to-begin-define
;;              (make-rose
;;               '(define (f x)
;;                  (let ((x 1))
;;                    x)))
;;              lisp-environment)
;;             (make-rose
;;              '(define (f x)
;;                 (let ((x 1))
;;                   x))))))
;;     (xit "(let ((x 1)) (let ((x 2)) x))"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-let-to-begin-define
;;              (make-rose
;;               '(let ((x 1))
;;                  (let ((x 2))
;;                    x)))
;;              lisp-environment)
;;             (make-rose
;;              '(begin
;;                 (define x 1)
;;                 (let ((x 2))
;;                   x))))))
;;     (xit "(for ((x xs)) (let ((x 2)) x))"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-let-to-begin-define
;;              (make-rose
;;               '(for ((x xs))
;;                  (let ((x 1))
;;                    x)))
;;              lisp-environment)
;;             (make-rose
;;              '(for ((x xs))
;;                 (let ((x 1))
;;                   x))))))
;;     (xit "(for ((x xs)) (let ((x 2)) x))"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-let-to-begin-define
;;              (make-rose
;;               '(for ((x xs))
;;                  (let ((y 1))
;;                    y)))
;;              lisp-environment)
;;             (make-rose
;;              '(for ((x xs))
;;                 (begin
;;                   (define y 1)
;;                   y))))))))
;;
;; (describe "rewrite-let-values-to-begin-define-values"
;;   (fn ()
;;     (xit "()"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-let-values-to-begin-define-values
;;              (make-rose
;;               '())
;;              lisp-environment)
;;             (make-rose
;;              '()))))
;;     (xit "(let-values (((x) (foo))) x)"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-let-values-to-begin-define-values
;;              (make-rose
;;               '(let-values (((x) (foo)))
;;                  x))
;;              lisp-environment)
;;             (make-rose
;;              '(begin
;;                 (define-values (x) (foo))
;;                 x)))))
;;     (xit "(lambda (x) (let-values ...))"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-let-values-to-begin-define-values
;;              (make-rose
;;               '(lambda (x)
;;                  (let-values (((x) (foo)))
;;                    x)))
;;              lisp-environment)
;;             (make-rose
;;              '(lambda (x)
;;                 (let-values (((x) (foo)))
;;                   x))))))
;;     (xit "(define (f x) (let-values ...))"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-let-values-to-begin-define-values
;;              (make-rose
;;               '(define (f x)
;;                  (let-values (((x) (foo)))
;;                    x)))
;;              lisp-environment)
;;             (make-rose
;;              '(define (f x)
;;                 (let-values (((x) (foo)))
;;                   x))))))
;;     (xit "(let ((x 1)) (let-values (((x) ...)) ...))"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-let-values-to-begin-define-values
;;              (make-rose
;;               '(let ((x 1))
;;                  (let-values (((x) (foo)))
;;                    x)))
;;              lisp-environment)
;;             (make-rose
;;              '(let ((x 1))
;;                 (let-values (((x) (foo)))
;;                   x))))))
;;     (xit "(let ((x 1)) (let-values (((y) ...)) ...))"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-let-values-to-begin-define-values
;;              (make-rose
;;               '(let ((x 1))
;;                  (let-values (((y) (foo)))
;;                    y)))
;;              lisp-environment)
;;             (make-rose
;;              '(let ((x 1))
;;                 (begin
;;                   (define-values (y) (foo))
;;                   y))))))
;;     (xit "(for ((x xs)) (let-values (((x) ...)) ...))"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-let-values-to-begin-define-values
;;              (make-rose
;;               '(for ((x xs))
;;                  (let-values (((x) (foo)))
;;                    x)))
;;              lisp-environment)
;;             (make-rose
;;              '(for ((x xs))
;;                 (let-values (((x) (foo)))
;;                   x))))))
;;     (xit "(for ((x xs)) (let-values (((y) ...)) ...))"
;;          (fn ()
;;            (assert-equal
;;             (rewrite-let-values-to-begin-define-values
;;              (make-rose
;;               '(for ((x xs))
;;                  (let-values (((y) (foo)))
;;                    y)))
;;              lisp-environment)
;;             (make-rose
;;              '(for ((x xs))
;;                 (begin
;;                   (define-values (y) (foo))
;;                   y))))))))

(describe "let-vars-to-const-vars"
  (fn ()
    (it "empty program"
        (fn ()
          (assert-equal
           (let-vars-to-const-vars
            (new Program))
           (new Program))))
    (it "single uninitialized let variable"
        (fn ()
          (assert-equal
           (let-vars-to-const-vars
            (new Program
                 (list
                  (new VariableDeclaration
                       (list
                        (new VariableDeclarator
                             (new Identifier "foo")))
                       "let"))))
           (new Program
                (list
                 (new VariableDeclaration
                      (list
                       (new VariableDeclarator
                            (new Identifier "foo")))
                      "let"))))))
    (it "single let variable"
        (fn ()
          (assert-equal
           (let-vars-to-const-vars
            (new Program
                 (list
                  (new VariableDeclaration
                       (list
                        (new VariableDeclarator
                             (new Identifier "foo")
                             (new Literal 1)))
                       "let"))))
           (new Program
                (list
                 (new VariableDeclaration
                      (list
                       (new VariableDeclarator
                            (new Identifier "foo")
                            (new Literal 1)))
                      "const"))))))
    (it "single let variable with assignment"
        (fn ()
          (assert-equal
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
                            (new Literal 2))))))
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
                           (new Literal 2))))))))
    (it "single let variable with destructuring initialization"
        (fn ()
          (assert-equal
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
                       "let"))))
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
                      "const"))))))
    (it "single let variable with destructuring initialization and subsequent assignment"
        (fn ()
          (assert-equal
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
                            (new Literal 2))))))
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
                           (new Literal 2))))))))
    (it "single let variable with assignment by update expression"
        (fn ()
          (assert-equal
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
                            #f)))))
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
                           #f)))))))
    (it "single let variable with destructuring assignment"
        (fn ()
          (assert-equal
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
                                  (new Literal 2))))))))
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
                             (new Literal 2))))))))))))
