;;; # Interpreter tests

(require chai "chai")
(require (only-in "../../src/ts/language"
                  __
                  LispEnvironment
                  functionp
                  interpret
                  lisp
                  write-to-string))
(require (only-in "../../src/ts/sexp"
                  s
                  sexp))
(require (only-in "./test-util"
                  assert-equal
                  assert-throws
                  test-lisp
                  test-repl))

(describe "boolean values"
  (fn ()
    (it "true"
        (fn ()
          (test-lisp
           #t
           #t)
          (test-lisp
           (js/tag sexp "true")
           #t)
          (test-lisp
           (js/tag sexp "t")
           #t)
          (test-lisp
           (js/tag sexp "#t")
           #t)))
    (it "false"
        (fn ()
          (test-lisp
           #f
           #f)
          (test-lisp
           (js/tag sexp "false")
           #f)
          (test-lisp
           (js/tag sexp "#f")
           #f)))))

(describe "truep"
  (fn ()
    (it "(truep true)"
        (fn ()
          (test-lisp
           '(truep true)
           #t)))
    (it "(truep false)"
        (fn ()
          (test-lisp
           '(truep false)
           #f)))
    (it "(truep undefined)"
        (fn ()
          (test-lisp
           (list (js/tag s "truep") undefined)
           #f)))
    (it "(truep true)"
        (fn ()
          (test-lisp
           (list (js/tag s "truep")
                 (js/tag s "true"))
           #t)))
    (it "(truep (quote ()))"
        (fn ()
          (test-lisp
           '(truep (quote ()))
           #f)))
    (it "(truep '())"
        (fn ()
          (test-lisp
           '(truep '())
           #f)))))

(describe "falsep"
  (fn ()
    (it "(falsep true)"
        (fn ()
          (assert-equal
           (interpret '(falsep true))
           #f)))
    (it "(falsep false)"
        (fn ()
          (assert-equal
           (interpret '(falsep false))
           #t)))
    (it "(falsep undefined)"
        (fn ()
          (assert-equal
           (interpret '(falsep undefined))
           #t)))))

(describe "nil"
  (fn ()
    (xit "nil"
         (fn ()
           (assert-equal
            (interpret (js/tag sexp "nil"))
            (quote ()))))
    (xit "(listp nil)"
         (fn ()
           (assert-equal
            (interpret (js/tag sexp "(listp nil)"))
            #t)))
    (xit "(length nil)"
         (fn ()
           (assert-equal
            (interpret (js/tag sexp "(length nil)"))
            0)))))

(describe "null"
  (fn ()
    (it "null (JS)"
        (fn ()
          (test-lisp
           js/null
           js/null)))
    (it "null"
        (fn ()
          (test-lisp
           (js/tag sexp "null")
           '())))
    (it "(listp null)"
        (fn ()
          (test-lisp
           '(listp null)
           #t)))
    (it "(length null)"
        (fn ()
          (test-lisp
           '(length null)
           0)))))

(describe "js-null"
  (fn ()
    (it "js-null"
        (fn ()
          (test-lisp
           (js/tag sexp "js-null")
           js/null)))
    (it "js/null"
        (fn ()
          (test-lisp
           (js/tag sexp "js/null")
           js/null)))))

(describe "undefined"
  (fn ()
    (it "undefined"
        (fn ()
          (test-lisp
           undefined
           undefined)))
    (xit "(undefined 0)"
         (fn ()
           (test-lisp
            '(undefined 0)
            undefined)))
    (xit "((undefined 0) 0)"
         (fn ()
           (test-lisp
            '((undefined 0) 0)
            undefined)))))

(describe "numbers"
  (fn ()
    (it "0"
        (fn ()
          (test-lisp
           (js/tag sexp "0")
           0)))
    (it "1"
        (fn ()
          (test-lisp
           (js/tag sexp "1")
           1)))
    (it "2"
        (fn ()
          (test-lisp
           (js/tag sexp "2")
           2)))))

(describe "strings"
  (fn ()
    (describe "quotation marks"
      (fn ()
        (it "\"foo\""
            (fn ()
              (test-lisp
               "foo"
               "foo")))))
    (describe "tabs"
      (fn ()
        (it "> (eq? \"\\t\" ...)"
            (fn ()
              (test-repl
               '(roselisp
                 > (eq? "\t" "	")
                 #t))))))))
(describe "keywords"
  (fn ()
    (it ":foo"
        (fn ()
          (test-lisp
           ':foo
           ':foo)))))

(describe "cons cells"
  (fn ()
    (it "(cons 1 2)"
        (fn ()
          (test-lisp
           '(cons 1 2)
           '(1 . 2))))
    (it "(cons 1 (cons 2 3))"
        (fn ()
          (test-lisp
           '(cons 1 (cons 2 3))
           '(1 2 . 3))))
    (it "(cons 1 '())"
        (fn ()
          (test-lisp
           '(cons 1 '())
           (js/tag sexp "(1)"))))
    (it "(cons 1 '(2))"
        (fn ()
          (test-lisp
           '(cons 1 '(2))
           '(1 2))))
    (it "(car '(1 . 2))"
        (fn ()
          (test-lisp
           '(car '(1 . 2))
           1)))
    (it "(cdr '(1 . 2))"
        (fn ()
          (test-lisp
           '(cdr '(1 . 2))
           2)))
    (xit "(car (cons 1 2))"
         (fn ()
           (test-lisp
            '(car (cons 1 2))
            1)))
    (xit "(cdr (cons 1 2))"
         (fn ()
           (test-lisp
            '(cdr (cons 1 2))
            2)))))

(describe "lists"
  (fn ()
    (it "(list 1 2)"
        (fn ()
          (test-lisp
           '(list 1 2)
           '(1 2))))
    (it "(aget '(1 2) 0)"
        (fn ()
          (test-lisp
           '(aget '(1 2) 0)
           1)))
    (it "(aget '((1 2) (3 4)) 0 1)"
        (fn ()
          (test-lisp
           '(aget '((1 2) (3 4)) 0 1)
           2)))
    (it "(aref '(1 2) 0)"
        (fn ()
          (test-lisp
           '(aref '(1 2) 0)
           1)))
    (it "(aset '(1 2) 0 3)"
        (fn ()
          (test-lisp
           '(aset '(1 2) 0 3)
           3)))
    (it "(let ((x '(1 2))) (aset x 0 3) x)"
        (fn ()
          (test-lisp
           '(let ((x '(1 2)))
              (aset x 0 3)
              x)
           '(3 2))))
    (it "(let ((x '(1 2))) (set! (aref x 0) 3) x)"
        (fn ()
          (test-lisp
           '(let ((x '(1 2)))
              (set! (aref x 0) 3)
              x)
           '(3 2))))))

(describe "quote"
  (fn ()
    (it "'foo"
        (fn ()
          (test-lisp
           '(quote foo)
           'foo)))
    (it "'(1)"
        (fn ()
          (test-lisp
           '(quote (1))
           '(1))))
    (it "'(1 2)"
        (fn ()
          (test-lisp
           '(quote (1 2))
           '(1 2))))
    (it "'((1 2) (3 4))"
        (fn ()
          (test-lisp
           '(quote ((1 2) (3 4)))
           '((1 2) (3 4)))))
    (it "(quote foo)"
        (fn ()
          (test-lisp
           '(quote foo)
           'foo)))
    (it "(quote (1))"
        (fn ()
          (test-lisp
           '(quote (1))
           '(1))))))

(describe "quasiquote"
  (fn ()
    (it "`foo"
        (fn ()
          (test-lisp
           (js/tag sexp "`foo")
           'foo)))
    (it "(quasiquote foo)"
        (fn ()
          (test-lisp
           '(quasiquote foo)
           'foo)))
    (it "`(,1)"
        (fn ()
          (test-lisp
           '(quasiquote (,1))
           '(1))))
    (it "`((,1))"
        (fn ()
          (test-lisp
           '(quasiquote ((,1)))
           '((1)))))
    (it "`(,@(list 1 2 3))"
        (fn ()
          (test-lisp
           '(quasiquote (,@(list 1 2 3)))
           '(1 2 3))))))

(describe "variables"
  (fn ()
    (it "x"
        (fn ()
          (test-lisp
           'x
           2
           (js-obj "env"
                   (new LispEnvironment
                        '((x 2 "variable")))))))
    (it "y"
        (fn ()
          (test-lisp
           'y
           undefined
           (js-obj "compile"
                   #f
                   "env"
                   (new LispEnvironment
                        '((x 2 "variable")))))))
    (it "(begin (setq x 2) x)"
        (fn ()
          (test-lisp
           '(begin
              (setq x 2)
              x)
           2
           (js-obj "compile" #f))))
    (it "(begin (define x 2) x)"
        (fn ()
          (test-lisp
           '(begin
              (define x 2)
              x)
           2)))
    (it "(begin (set! x 2) x)"
        (fn ()
          (test-lisp
           '(begin
              (set! x 2)
              x)
           2
           (js-obj "compile" #f))))
    (it "(begin (set! x 2) (set! y 3) (+ x y))"
        (fn ()
          (test-lisp
           '(begin
              (set! x 2)
              (set! y 3)
              (+ x y))
           5
           (js-obj "compile"
                   #f
                   "env"
                   (new LispEnvironment
                        `((+
                           ,(lambda (x y)
                              (+ x y))
                           "function")))))))
    (xit "(setq a 1 b 2 c 3)"
         (fn ()
           (test-lisp
            '(begin
               (setq a 1 b 2 c 3)
               (list a b c))
            '(1 2 3)
            (js-obj "compile" #f))))))

(describe "function calls"
  (fn ()
    (it "(identity \"foo\")"
        (fn ()
          (test-lisp
           '(identity "foo")
           "foo"
           (js-obj "env"
                   (new LispEnvironment
                        `((identity
                           ,(lambda (x)
                              x)
                           "function")))))))
    (xit "((add _ 2 3) 1)"
         (fn ()
           (test-lisp
            '(begin
               (define ((((my-add) x) y) z)
                 (+ x y z))
               ((my-add _ 2 3) 1))
            6)))
    (xit "((my-add _ _ _) 1 2 3)"
         (fn ()
           (test-lisp
            '(begin
               (define ((((my-add) x) y) z)
                 (+ x y z))
               ((my-add _ _ _) 1 2 3))
            6)))
    (xit "(((add _ _ 3) 1) 2)"
         (fn ()
           (test-lisp
            '(begin
               (define ((((my-add) x) y) z)
                 (+ x y z))
               (((my-add _ _ 3) 1) 2))
            6)))
    (xit "((add _ _ 3) 1 2)"
         (fn ()
           (test-lisp
            '(begin
               (define ((((my-add) x) y) z)
                 (+ x y z))
               ((my-add _ _ 3) 1 2))
            6)))))

(describe "define"
  (fn ()
    (it "(begin (define (add x y) (+ x y)) (add 2 3))"
        (fn ()
          (test-lisp
           '(begin
              (define (add x y)
                (+ x y))
              (add 2 3))
           5
           (js-obj "env"
                   (new LispEnvironment
                        `((+
                           ,(lambda (x y)
                              (+ x y))
                           "function")))))))
    (it "(begin (define (add x y) (+ x y)) (add 2 3))"
        (fn ()
          (test-lisp
           '(begin
              (define (add x y)
                (+ x y))
              (add 2 3))
           5
           (js-obj "env"
                   (new LispEnvironment
                        `((+
                           ,(lambda (x y)
                              (+ x y))
                           "function")))))))
    (it "(define ((my-add x) y) ...)"
        (fn ()
          (test-lisp
           '(begin
              (define ((my-add x) y)
                (+ x y))
              (my-add 2 3))
           5)))
    (xit "(define ((my-add x) y) ...)"
         (fn ()
           (test-lisp
            '(begin
               (define ((my-add x) y)
                 (+ x y))
               ((my-add 2) 3))
            5)))
    (xit "(define (((add) x) y) ...)"
         (fn ()
           (test-lisp
            '(begin
               (define (((my-add) x) y)
                 (+ x y))
               (((my-add) 2) 3))
            5)))
    (it "(define ((add x y) z) ...)"
        (fn ()
          (test-lisp
           '(begin
              (define ((my-add x y) z)
                (+ x y z))
              (my-add 1 2 3))
           6)
          ;; (test-lisp
          ;;  '(begin
          ;;     (define ((my-add x y) z)
          ;;       (+ x y z))
          ;;     ((my-add 1 2) 3))
          ;;  6)
          ;; (test-lisp
          ;;  '(begin
          ;;     (define ((my-add x y) z)
          ;;       (+ x y z))
          ;;     ((my-add 1) 2 3))
          ;;  6)
          ;; (test-lisp
          ;;  '(begin
          ;;     (define ((my-add x y) z)
          ;;       (+ x y z))
          ;;     (((my-add 1) 2) 3))
          ;;  6)
          ;; (test-lisp
          ;;  '(begin
          ;;     (define ((my-add x y) z)
          ;;       (+ x y z))
          ;;     ((((my-add) 1) 2) 3))
          ;;  6)
          ;; (test-lisp
          ;;  '(begin
          ;;     (define ((((my-add) x) y) z)
          ;;       (+ x y z))
          ;;     ((((my-add) 1) 2) 3))
          ;;  6)
          ))
    (it "> (define x 1)"
        (fn ()
          (test-repl
           '(roselisp
             > (define x 1)
             undefined
             > x
             1))))))

(describe "defun"
  (fn ()
    (xit "(begin (defun add (x y) (+ x y)) (add 2 3))"
         (fn ()
           (test-lisp
            '(begin
               (defun add (x y)
                 (+ x y))
               (add 2 3))
            5
            (js-obj "env"
                    (new LispEnvironment
                         `((+
                            ,(lambda (x y)
                               (+ x y))
                            "function"))))
            (js-obj "verbose" #t))))
    (xit "(begin (defun add (x y) (+ x y)) (add 2 3))"
         (fn ()
           (test-lisp
            '(begin
               (defun add (x y)
                 (+ x y))
               (add 2 3))
            5
            (js-obj "env"
                    (new LispEnvironment
                         `((+
                            ,(lambda (x y)
                               (+ x y))
                            "function")))))))))

(describe "defmacro"
  (fn ()
    (xit "(defmacro my-macro (x) ...)"
         (fn ()
           (test-lisp
            '(begin
               (defmacro my-macro (x)
                 `(begin ,x))
               (my-macro 1))
            1
            (js-obj "compile" #f))))
    (xit "(defmacro my-macro (&environment env) ...)"
         (fn ()
           (test-lisp
            '(begin
               (defmacro my-macro (&environment env)
                 (send env has '+))
               (my-macro))
            #t
            (js-obj "compile"
                    #f
                    "env"
                    (new LispEnvironment
                         `((+
                            ,(lambda (x y)
                               (+ x y))
                            "function")))))))
    (xit "(defmacro my-macro (&environment env-arg) ...)"
         (fn ()
           (test-lisp
            '(begin
               (defmacro my-macro (&environment env-arg)
                 (send env-arg has '+))
               (my-macro))
            #t
            (js-obj "compile"
                    #f
                    "env"
                    (new LispEnvironment
                         `((+
                            ,(lambda (x y)
                               (+ x y))
                            "function")))))))
    (xit "(defmacro (my-macro x) ...)"
         (fn ()
           (test-lisp
            '(begin
               (defmacro (my-macro x)
                 `(begin ,x))
               (my-macro 1))
            1
            (js-obj "compile" #f))))))

(describe "define-macro"
  (fn ()
    (xit "(define-macro (my-macro x) ...)"
         (fn ()
           (test-lisp
            '(begin
               (define-macro (my-macro x)
                 `(begin ,x))
               (my-macro 1))
            1
            (js-obj "compile" #f))))
    (xit "(define-macro my-macro (x) ...)"
         (fn ()
           (test-lisp
            '(begin
               (define-macro my-macro (x)
                 `(begin ,x))
               (my-macro 1))
            1
            (js-obj "compile" #f))))))

(describe "let"
  (fn ()
    (it "(let ((x 0)) x)"
        (fn ()
          (test-lisp
           '(let ((x 0))
              x)
           0)))
    (it "(let ((x 1)) x)"
        (fn ()
          (test-lisp
           '(let ((x 1))
              x)
           1)))
    (it "(let ((x 1)) (let ((y 2)) x))"
        (fn ()
          (test-lisp
           '(let ((x 1))
              (let ((y 2))
                x))
           1)))
    (it "(let ((x '((1 2) (3 4)))) x)"
        (fn ()
          (test-lisp
           '(let ((x '((1 2) (3 4))))
              x)
           '((1 2) (3 4)))))
    (it "(let (x) (set! x 1) x)"
        (fn ()
          (test-lisp
           '(let (x)
              (set! x 1)
              x)
           1)))
    (it "(let (x) (set! x 1) (set! x 2) x)"
        (fn ()
          (test-lisp
           '(let (x)
              (set! x 1)
              (set! x 2)
              x)
           2)))
    (it "(let (x))"
        (fn ()
          (test-lisp
           '(let (x))
           undefined)))
    (it "(let ((a 1)) (+ (let ((a 2)) a) a))"
        (fn ()
          (test-lisp
           '(let ((a 1))
              (+ (let ((a 2))
                   a)
                 a))
           3)))
    (it "(let* ((x 1)) x)"
        (fn ()
          (test-lisp
           '(let* ((x 1))
              x)
           1)))
    (it "(let ((compose ...) ...) ...)"
        (fn ()
          (test-lisp
           '(let ((compose (lambda (f g)
                             (lambda (x)
                               (f (g x)))))
                  (square (lambda (x) (* x x)))
                  (add1 (lambda (x) (+ x 1))))
              ((compose square add1) (add1 4)))
           36)))))

(describe "lambda"
  (fn ()
    (it "((lambda (x) x) 1)"
        (fn ()
          (test-lisp
           '((lambda (x) x) 1)
           1)))
    (it "((lambda (x) x) \"Lisp\")"
        (fn ()
          (test-lisp
           '((lambda (x)
               x)
             "Lisp")
           "Lisp")))
    (it "((lambda x x) \"Lisp\")"
        (fn ()
          (test-lisp
           '((lambda x
               x)
             "Lisp")
           '("Lisp"))))
    (it "((fn (x) x) 1)"
        (fn ()
          (test-lisp
           '((fn (x)
               x)
             1)
           1)))
    (it "((λ (x) x) 1)"
        (fn ()
          (test-lisp
           '((λ (x)
               x)
             1)
           1)))
    (it "(fn (x) x))"
        (fn ()
          (assert-equal
           (functionp (interpret '(fn (x) x)))
           #t)))
    (it "(<fn> 1 1)"
        (fn ()
          (test-lisp
           `(,(lambda (x y)
                (+ x y))
             1
             1)
           2
           (js-obj "wrapParens" #t))))))

(describe "nlambda"
  (fn ()
    (xit "(define f (nlambda ...))"
         (fn ()
           (test-lisp
            '(begin
               (setq a 1 b 2 c 3)
               (define f
                 (nlambda (x y z)
                          (list x y z)))
               (f a b c))
            '(a b c)
            (js-obj "compile" #f))))
    (xit "((nlambda ...) ...)"
         (fn ()
           (test-lisp
            '(begin
               (setq a 1 b 2 c 3)
               ((nlambda (x y z) (list x y z)) a b c))
            '(a b c)
            (js-obj "compile" #f))))))

(describe "lexical scope"
  (fn ()
    (it "lexical scope 1"
        (fn ()
          (test-lisp
           '(begin
              (define (K x)
                (lambda () x))
              ((K 42)))
           42)))
    (it "lexical scope 2"
        (fn ()
          (test-lisp
           '(begin
              (define incrementer undefined)
              (let ((x 1))
                (set! incrementer
                      (lambda ()
                        (set! x (+ x 1))
                        x)))
              (incrementer))
           2)))
    (it "lexical scope 3"
        (fn ()
          (test-lisp
           '(let ((x 100)
                  incrementer)
              (let ((x 1))
                (set! incrementer
                      (lambda ()
                        (set! x (+ x 1))
                        x)))
              (incrementer)
              x)
           100)))))

(describe "begin"
  (fn ()
    (it "(begin)"
        (fn ()
          (test-lisp
           '(begin)
           undefined)))))

(describe "begin0"
  (fn ()
    (it "(begin0 1 2)"
        (fn ()
          (test-lisp
           '(begin0 1
              2)
           1)))))

(describe "if"
  (fn ()
    (it "(if true 1 2)"
        (fn ()
          (test-lisp
           '(if true
                1
                2)
           1)))
    (it "(if (< 1 2) 1 2)"
        (fn ()
          (test-lisp
           '(if (< 1 2)
                1
                2)
           1)))
    (it "(if (> 2 1) 1 2)"
        (fn ()
          (test-lisp
           '(if (> 2 1)
                1
                2)
           1)))
    (it "(if false 1 2)"
        (fn ()
          (test-lisp
           '(if false
                1
                2)
           2)))
    (xit "(if \"\" 1 2)"
         (fn ()
           (test-lisp
            '(if ""
                 1
                 2)
            1)))))

(describe "when"
  (fn ()
    (it "(when (< 1 2) 1 2)"
        (fn ()
          (test-lisp
           '(when (< 1 2)
              1 2)
           2)))
    (it "(when (> 1 2) 1 2)"
        (fn ()
          (test-lisp
           '(when (> 1 2)
              1 2)
           undefined)))))

(describe "unless"
  (fn ()
    (it "(unless (< 1 2) 1 2)"
        (fn ()
          (test-lisp
           '(unless (< 1 2)
              1 2)
           undefined)))
    (it "(unless (> 1 2) 1 2)"
        (fn ()
          (test-lisp
           '(unless (> 1 2)
              1 2)
           2)))))

(describe "cond"
  (fn ()
    (it "(cond (#f 1) (else 2))"
        (fn ()
          (test-lisp
           '(cond
             (#f
              1)
             (else
              2))
           2)))
    (it "(cond (true 1) (false 2))"
        (fn ()
          (test-lisp
           '(cond
             (true
              1)
             (false
              2))
           1)))
    (it "(cond (false 1) (true 2))"
        (fn ()
          (test-lisp
           '(cond
             (false
              1)
             (true
              2))
           2)))
    (it "(cond (false 1) (t 2))"
        (fn ()
          (test-lisp
           '(cond
             (false
              1)
             (t
              2))
           2)))))

(describe "js/switch"
  (fn ()
    (it "(js/switch x (case \"foo\" ...) (default ...))"
        (fn ()
          (test-lisp
           '(begin
              (define x "foo")
              (define y "bar")
              (js/switch x
                         (case "foo"
                           (set! y "baz")
                           (break))
                         (default
                           (set! y "quux")))
              y)
           "baz")))))

(describe "eq?"
  (fn ()
    (it "(eq (my-unit 'foo) 'foo)"
        (fn ()
          (test-lisp
           '(begin
              (define (my-unit x) x)
              (my-unit 'foo))
           'foo)))
    (it "(eq (my-curried-unit '_) '_)"
        (fn ()
          (test-lisp
           '(begin
              (define ((my-curried-unit) x) x)
              (my-curried-unit '_))
           '_)))
    (it "(eq '_ '_)"
        (fn ()
          (test-lisp
           '(eq '_ '_)
           #t)))
    (xit "(assert (eq '_ '_))"
         (fn ()
           (test-lisp
            '(assert (eq '_ '_))
            #t)))
    (xit "(assert (not (eq _ '_)))"
         (fn ()
           (test-lisp
            '(assert (not (eq _ '_)))
            #t)))))

(describe "equal?"
  (fn ()
    (xit "(assert (not (equal _ '_)))"
         (fn ()
           (test-lisp
            '(assert (not (equal _ '_)))
            #t)))))

(describe "and"
  (fn ()
    (it "(and)"
        (fn ()
          (test-lisp
           '(and)
           #t)))
    (it "(and false false)"
        (fn ()
          (test-lisp
           '(and false false)
           #f)))
    (it "(and false true)"
        (fn ()
          (test-lisp
           '(and false true)
           #f)))
    (it "(and true true)"
        (fn ()
          (test-lisp
           '(and false true)
           #f)))))

(describe "or"
  (fn ()
    (it "(or)"
        (fn ()
          (test-lisp
           '(or)
           #f)))
    (it "(or false false)"
        (fn ()
          (test-lisp
           '(or false false)
           #f)))
    (it "(or false true)"
        (fn ()
          (test-lisp
           '(or false true)
           #t)))
    (it "(or 1 2)"
        (fn ()
          (test-lisp
           '(or 1 2)
           1)))
    (it "(or undefined 2)"
        (fn ()
          (test-lisp
           '(or undefined 2)
           2)
          (test-lisp
           (list 'or undefined 2)
           2)))))

(describe "while"
  (fn ()
    (it "(let ... (while ...) ...)"
        (fn ()
          (test-lisp
           '(let ((result '()))
              (while (< (length result) 3)
                (set! result (cons 1 result)))
              result)
           '(1 1 1))))))

(describe "for"
  (fn ()
    (it "(let ... (for ...) ...)"
        (fn ()
          (test-lisp
           '(let ((result '()))
              (for ((x '(1 2 3)))
                (set! result (cons x result)))
              result)
           '(3 2 1))))
    (xit "(let ... (for ((x ...) (y ...)) ...) ...)"
         (fn ()
           (test-lisp
            '(let ((result '()))
               (for ((x '(1 2 3))
                     (y '(4 5 6)))
                 (set! result (cons x result))
                 (set! result (cons y result)))
               result)
            '(6 3 5 2 4 1))))
    (it "(for ((i (range 0 len))) ...)"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo
                 '(1 2 3 4))
             undefined
             > (define len
                 (length foo))
             undefined
             > (for ((i (range 0 len)))
                 (pop-right! foo))
             undefined
             > foo
             '()))))
    (it "(for ((i (range 0 (length foo)))) ...)"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo
                 '(1 2 3 4))
             undefined
             > (for ((i (range 0 (length foo))))
                 (pop-right! foo))
             undefined
             > foo
             '()))))))

(describe "break"
  (fn ()
    (it "(begin (while #t (break) 1))"
        (fn ()
          (test-lisp
           '(begin
              (while #t
                (break))
              1)
           1
           (js-obj "compile" #t))))
    (it "(let ... (for ...) ...)"
        (fn ()
          (test-lisp
           '(let ((result (list)))
              (for ((i (range 0 10)))
                (break)
                (push-right! result i))
              result)
           '()
           (js-obj "compile" #t))))))

(describe "continue"
  (fn ()
    (it "(begin (while #t (break) 1))"
        (fn ()
          (test-lisp
           '(let ((result (list))
                  (i 0))
              (while (< i 10)
                (set! i (+ i 1))
                (when (< i 5)
                  (continue))
                (push-right! result i))
              result)
           '(5 6 7 8 9 10))))
    (it "(let ... (for ...) ...)"
        (fn ()
          (test-lisp
           '(let ((result (list)))
              (for ((i (range 0 11)))
                (when (< i 5)
                  (continue))
                (push-right! result i))
              result)
           '(5 6 7 8 9 10))))))

(describe "return"
  (fn ()
    (it "((lambda () (return 1) 2))"
        (fn ()
          (test-lisp
           '((lambda ()
               (return 1)
               2))
           1)))
    (it "((js/function () (return 1) 2))"
        (fn ()
          (test-lisp
           '((js/function ()
               (return 1)
               2))
           1)))
    (it "((js/arrow () (return 1) 2))"
        (fn ()
          (test-lisp
           '((js/arrow ()
               (return 1)
               2))
           1)))))

(describe "get-field"
  (fn ()
    (it "(get-field foo obj)"
        (fn ()
          (test-lisp
           '(let ((obj (js-obj "foo" "bar")))
              (get-field foo obj))
           "bar")))))

(describe "set-field!"
  (fn ()
    (it "(set-field! foo obj \"bar)"
        (fn ()
          (test-lisp
           '(let ((obj (js-obj)))
              (set-field! foo obj "bar")
              obj)
           (js-obj "foo" "bar"))))))

(describe "field-bound?"
  (fn ()
    (it "(field-bound? foo obj)"
        (fn ()
          (test-lisp
           '(let ((obj (js-obj "foo" "bar")))
              (field-bound? foo obj))
           #t)))))

(describe "oget"
  (fn ()
    (it "(oget obj \"prop\")"
        (fn ()
          (test-lisp
           '(oget obj "prop")
           "foo"
           (js-obj "env"
                   (new LispEnvironment
                        `((obj
                           ,(js-obj "prop" "foo")
                           "variable")))))))
    (it "(oget _ \"@@functional/placeholder\")"
        (fn ()
          (test-lisp
           '(oget _ "@@functional/placeholder")
           #t)))))

(describe "send"
  (fn ()
    (xit "(send obj 'add 1 1)"
         (fn ()
           (test-lisp
            '(send obj 'add 1 1)
            2
            (js-obj "compile"
                    #f
                    "env"
                    (new LispEnvironment
                         `((obj
                            ,(js-obj "add"
                                     (lambda (x y)
                                       (+ x y)))
                            "variable")))))))
    (it "(send obj add 1 1)"
        (fn ()
          (test-lisp
           '(send obj add 1 1)
           2
           (js-obj "compile"
                   #f
                   "env"
                   (new LispEnvironment
                        `((obj
                           ,(js-obj "add"
                                    (lambda (x y)
                                      (+ x y)))
                           "variable")))))))
    (it "(send (make-hash '((\"foo\" . \"foo\"))) has \"foo\")"
        (fn ()
          (test-lisp
           '(send (make-hash
                   '(("foo" . "foo")))
                  has
                  "foo")
           #t)))
    (it "(send (make-hash '((\"foo\" . \"foo\"))) has '(\"foo\"))"
        (fn ()
          (test-lisp
           '(send (make-hash
                   '(("foo" . "foo")))
                  has
                  '("foo"))
           #f)))))

(describe "send/apply"
  (fn ()
    (it "(send/apply (make-hash '((\"foo\" . \"foo\"))) has '(\"foo\"))"
        (fn ()
          (test-lisp
           '(send/apply (make-hash
                         '(("foo" . "foo")))
                        has
                        '("foo"))
           #t)))))

(describe "dot"
  (fn ()
    (it "(. obj add1 1)"
        (fn ()
          (test-lisp
           '(. obj add1 1)
           2
           (js-obj "compile"
                   #f
                   "env"
                   (new LispEnvironment
                        `((obj
                           ,(js-obj "add1"
                                    (lambda (x)
                                      (+ x 1)))
                           "variable")))))))
    (it "(.add1 obj 1)"
        (fn ()
          (test-lisp
           '(.add1 obj 1)
           2
           (js-obj "compile"
                   #f
                   "env"
                   (new LispEnvironment
                        `((obj
                           ,(js-obj "add1"
                                    (lambda (x)
                                      (+ x 1)))
                           "variable")))))))
    (it "(.add obj 1 1)"
        (fn ()
          (test-lisp
           '(.add obj 1 1)
           2
           (js-obj "compile"
                   #f
                   "env"
                   (new LispEnvironment
                        `((obj
                           ,(js-obj "add"
                                    (lambda (x y)
                                      (+ x y)))
                           "variable")))))))
    (it "(.-prop obj)"
        (fn ()
          (test-lisp
           '(let ((obj (js-obj)))
              (set! (.-prop obj) "bar")
              (.-prop obj))
           "bar")))
    (it "(.-prop obj) 2"
        (fn ()
          (test-lisp
           '(.-prop obj)
           "foo"
           (js-obj "compile"
                   #f
                   "env"
                   (new LispEnvironment
                        `((obj
                           ,(js-obj "prop" "foo")
                           "variable")))))))))

(describe "new"
  (fn ()
    (it "(new (class ...))"
        (fn ()
          (test-lisp
           '(let (quux)
              (set! quux
                    (new (class ()
                           (define/public (bar)
                             "baz"))))
              (.bar quux))
           "baz")))
    (it "(new (class ...)) with constructor"
        (fn ()
          (test-lisp
           '(let (quux)
              (set! quux
                    (new (class ()
                           (define/public val 1)
                           (define (constructor x)
                             ;; (set! (.-val this) x)
                             (set-field! val this x))
                           (define/public (bar)
                             ;; (.-val this)
                             (get-field val this)))
                         2))
              ;; (.bar quux)
              (send quux bar))
           2
           (js-obj "compile" #f))))
    (it "(new (class ...)) extending Object"
        (fn ()
          (test-lisp
           '(let (quux)
              (set! quux
                    (new (class (Object)
                           (define/public val 1)
                           (define (constructor x)
                             (set! (.-val this) x))
                           (define/public (bar)
                             (.-val this)))
                         2))
              (.bar quux))
           2
           (js-obj "compile"
                   #f
                   "env"
                   (new LispEnvironment
                        `((Object ,Object "function")))))))))

(describe "class"
  (fn ()
    (it "(define Foo (class ...))"
        (fn ()
          (test-lisp
           '(begin
              (define Foo
                (class object%
                  (define/public (bar)
                    "baz")))
              (set! quux (new Foo))
              (.bar quux))
           "baz"
           (js-obj "compile" #f))))
    (it "(defclass Foo ...)"
        (fn ()
          (test-lisp
           '(begin
              (defclass Foo ()
                (define/public (bar)
                  "baz"))
              (set! quux (new Foo))
              (.bar quux))
           "baz"
           (js-obj "compile" #f))))
    (it "(defclass Foo ...) with property"
        (fn ()
          (test-lisp
           '(begin
              (defclass Foo ()
                (define bar "baz"))
              (set! quux (new Foo))
              (.-bar quux))
           "baz"
           (js-obj "compile" #f))))
    (it "(defclass Foo ...) with constructor"
        (fn ()
          (test-lisp
           '(begin
              (defclass Foo ()
                (define x)
                (define (constructor x)
                  (set! (.-x this) x))
                (define (bar)
                  (.-x this)))
              (set! quux (new Foo "xyzzy"))
              (.bar quux))
           "xyzzy"
           (js-obj "compile" #f))))
    (xit "(defclass Foo ...) with constructor and new"
         (fn ()
           (test-lisp
            '(begin
               (defclass Foo ()
                 (define x)
                 (define (constructor x)
                   (set! (.-x this) x))
                 (define (bar)
                   (.-x this)))
               (set! quux (new Foo (x "xyzzy")))
               (.bar quux))
            "xyzzy"
            (js-obj "compile" #f))))
    (xit "(defclass Foo ...) with no arguments"
         (fn ()
           (test-lisp
            '(begin
               (defclass Foo ()
                 (define x "wobble")
                 (define (constructor x)
                   (set! (.-x this) x))
                 (define (bar)
                   (.-x this)))
               (set! quux (new Foo))
               (.bar quux))
            "wobble"
            (js-obj "compile" #f))))
    (it "(defclass Foo (Object) ...)"
        (fn ()
          (test-lisp
           '(begin
              (defclass Foo (Object)
                (define (bar)
                  "baz"))
              (set! quux (new Foo))
              (.bar quux))
           "baz"
           (js-obj "compile"
                   #f
                   "env"
                   (new LispEnvironment
                        `((Object ,Object "function")))))))))

(describe "js-obj"
  (fn ()
    (it "(js-obj)"
        (fn ()
          (test-lisp
           '(js-obj)
           (js-obj)
           (js-obj "wrapParens" #t))))
    (it "(js-obj \"foo\" \"bar\")"
        (fn ()
          (test-lisp
           '(js-obj "foo" "bar")
           (js-obj "foo" "bar")
           (js-obj "wrapParens" #t))))
    (it "(js-obj \"foo\" 1 \"bar\" 2)"
        (fn ()
          (test-lisp
           '(js-obj "foo" 1 "bar" 2)
           (js-obj "foo" 1 "bar" 2)
           (js-obj "wrapParens" #t))))))

(describe "js/in"
  (fn ()
    (it "(js/in \"foo\" (js-obj \"foo\" \"bar\"))"
        (fn ()
          (test-lisp
           '(let ((obj (js-obj "foo" "bar")))
              (js/in "foo" obj))
           #t)))))

(describe "module"
  (fn ()
    (it "(module foo bar (+ 1 1))"
        (fn ()
          (test-lisp
           '(module foo bar
              (+ 1 1))
           2)))))

(describe "error"
  (fn ()
    (it "(error)"
        (fn ()
          (assert-throws
           (lambda ()
             (interpret
              '(error)
              (new LispEnvironment))))))
    (it "(error \"foo\")"
        (fn ()
          (assert-throws
           (lambda ()
             (interpret
              '(error "foo")
              (new LispEnvironment))))))))

(describe "clj/try"
  (fn ()
    (it "(/ 1 2)"
        (fn ()
          (test-lisp
           '(clj/try
             (/ 1 2)
             (catch Exception e
               "there was an error")
             (finally
               (display "finally")))
           0.5)))
    (it "(clj/try (/ 1 3) (/ 1 2) ...)"
        (fn ()
          (test-lisp
           '(clj/try
             (/ 1 3)
             (/ 1 2)
             (catch Exception e
               "there was an error")
             (finally
               (display "finally")))
           0.5)))
    (xit "(throw (new Error \"an error\"))"
         (fn ()
           (test-lisp
            '(clj/try
              (throw (new Error "an error"))
              (catch Error e
                "there was an error")
              (finally
                (display "finally")))
            "there was an error")))))

(describe "unwind-protect"
  (fn ()
    (it "(unwind-protect 1 2 3)"
        (fn ()
          (test-lisp
           '(unwind-protect 1 2 3)
           1)))))

(describe "define-values"
  (fn ()
    (it "(begin (define-values (x y) ...) ...)"
        (fn ()
          (test-lisp
           '(begin
              (define-values (x y) (values 1 2))
              (list x y))
           '(1 2))))
    (it "(begin (define-values (x y) ...) x)"
        (fn ()
          (test-lisp
           '(begin
              (define-values (x y) (values 1 2))
              x)
           1)))
    (it "> (define-values (x y) (values 1 2))"
        (fn ()
          (test-repl
           '(roselisp
             > (define-values (x y)
                 (values 1 2))
             undefined
             > x
             1))))))

(describe "set!-values"
  (fn ()
    (it "> (set!-values (x y) (values 1 2))"
        (fn ()
          (test-repl
           '(roselisp
             > (set!-values (x y) (values 1 2))
             '(1 2)
             > x
             1)
           (js-obj "compile" #f))))))

(describe "let-values"
  (fn ()
    (it "(let-values ...)"
        (fn ()
          (test-lisp
           '(let-values (((x y) (values 1 2)))
              (list x y))
           '(1 2))))
    (it "(let-values (((x . y) ...))...)"
        (fn ()
          (test-lisp
           '(let-values (((x . y) (values 1 2)))
              (list x y))
           '(1 (2)))))))

(describe "let*-values"
  (fn ()
    (it "(let*-values ...)"
        (fn ()
          (test-lisp
           '(let-values (((x y) (values 1 2))
                         ((w z) (values 3 4)))
              (list x y w z))
           '(1 2 3 4))))))

(describe "set!-values"
  (fn ()
    (it "(let ... (set!-values (x y) ...) ...)"
        (fn ()
          (test-lisp
           '(let (x y)
              (set!-values (x y) (values 1 2))
              (list x y))
           '(1 2))))))

(describe "define-js-obj"
  (fn ()
    (it "> (define-js-obj (x) (js-obj \"x\" 1))"
        (fn ()
          (test-repl
           '(roselisp
             > (define-js-obj (x)
                 (js-obj "x" 1))
             undefined
             > x
             1))))))

(describe "set!-js-obj"
  (fn ()
    (it "> (set!-js-obj (x) (js-obj \"x\" 1))"
        (fn ()
          (test-repl
           '(roselisp
             > (set!-js-obj (x) (js-obj "x" 1))
             (js-obj "x" 1)
             > x
             1)
           (js-obj "compile" #f))))))

(describe "destructuring-bind"
  (fn ()
    (it "(destructuring-bind ...)"
        (fn ()
          (test-lisp
           '(destructuring-bind (x y)
                                '(1 2)
                                (list x y))
           '(1 2))))
    (it "(destructuring-bind (x . y) ...)"
        (fn ()
          (test-lisp
           '(destructuring-bind (x . y)
                                '(1 2)
                                (list x y))
           '(1 (2)))))))

(describe "multiple-values-bind"
  (fn ()
    (it "(multiple-values-bind ...)"
        (fn ()
          (test-lisp
           '(multiple-values-bind (x y)
                                  (values 1 2)
                                  (list x y))
           '(1 2))))))

(describe "hash"
  (fn ()
    (it "(hash)"
        (fn ()
          (test-lisp
           '(hash)
           (new Map))))
    (it "(hash '((\"foo\" . \"bar\")))"
        (fn ()
          (test-lisp
           '(hash '(("foo" . "bar")))
           (new Map
                '(("foo" "bar"))))))))

(describe "make-hash"
  (fn ()
    (it "(make-hash)"
        (fn ()
          (test-lisp
           '(make-hash)
           (new Map))))
    (it "(make-hash '((\"foo\" . \"bar\")))"
        (fn ()
          (test-lisp
           '(make-hash '(("foo" . "bar")))
           (new Map '(("foo" "bar"))))))))

(describe "hash?"
  (fn ()
    (it "(hash? (make-hash))"
        (fn ()
          (test-lisp
           '(hash? (make-hash))
           #t)))
    (it "(hash? 0)"
        (fn ()
          (test-lisp
           '(hash? 0)
           #f)))))

(describe "hash-clear"
  (fn ()
    (xit "(hash-clear (make-hash '((\"foo\" . \"bar\"))))"
         (fn ()
           (test-lisp
            '(hash-clear
              (make-hash
               '(("foo" . "bar"))))
            (new Map))))))

(describe "hash-clear!"
  (fn ()
    (it "(let ... (hash-clear! ...) ...)"
        (fn ()
          (test-lisp
           '(let ((ht (make-hash '(("foo" . "bar")))))
              (hash-clear! ht)
              ht)
           (new Map))))))

(describe "hash-copy"
  (fn ()
    (it "(hash-copy (make-hash '((\"foo\" . \"bar\"))))"
        (fn ()
          (test-lisp
           '(hash-copy
             (make-hash
              '(("foo" . "bar"))))
           (new Map '(("foo" "bar"))))))))

(describe "hash-keys"
  (fn ()
    (it "(hash-keys (make-hash '((\"foo\" . \"bar\"))))"
        (fn ()
          (test-lisp
           '(hash-keys
             (make-hash
              '(("foo" . "bar"))))
           '("foo"))))))

(describe "hash-values"
  (fn ()
    (it "(hash-values (make-hash '((\"foo\" . \"bar\"))))"
        (fn ()
          (test-lisp
           '(hash-values
             (make-hash
              '(("foo" . "bar"))))
           '("bar"))))))

(describe "hash->list"
  (fn ()
    (it "(hash->list (make-hash '((\"foo\" . \"bar\"))))"
        (fn ()
          (test-lisp
           '(hash->list
             (make-hash
              '(("foo" . "bar"))))
           '(("foo" . "bar")))))))

(describe "hash-set"
  (fn ()
    (it "(hash-set (make-hash) \"foo\" \"bar\")"
        (fn ()
          (test-lisp
           '(hash-set
             (make-hash)
             "foo"
             "bar")
           (new Map
                '(("foo" "bar"))))))))

(describe "hash-set!"
  (fn ()
    (it "(let ... (hash-set! ...) ...)"
        (fn ()
          (test-lisp
           '(let ((ht (make-hash)))
              (hash-set! ht "foo" "bar")
              ht)
           (new Map
                '(("foo" "bar"))))))))

(describe "hash-ref"
  (fn ()
    (it "(hash-ref (make-hash '((\"foo\" . \"bar\"))) \"foo\")"
        (fn ()
          (test-lisp
           '(hash-ref
             (make-hash
              '(("foo" . "bar")))
             "foo")
           "bar")))
    (it "(hash-ref (make-hash) \"quux\" #f)"
        (fn ()
          (test-lisp
           '(hash-ref (make-hash) "quux" #f)
           #f)))))

(describe "hash-has-key?"
  (fn ()
    (it "(hash-has-key? (make-hash '((\"foo\" . \"bar\"))) \"foo\")"
        (fn ()
          (test-lisp
           '(hash-has-key?
             (make-hash
              '(("foo" . "bar")))
             "foo")
           #t)))
    (it "(hash-has-key? (make-hash) \"quux\")"
        (fn ()
          (test-lisp
           '(hash-has-key? (make-hash) "quux")
           #f)))))

(describe "Map"
  (fn ()
    (it "(new Map)"
        (fn ()
          (test-lisp
           '(new Map)
           (new Map))))
    (it "(new Map), Map bound in environment"
        (fn ()
          (assert-equal
           (~> (interpret
                '(new Map)
                (new LispEnvironment
                     `((Map ,Map "function"))))
               (instance-of? Map))
           #t)))
    (it "(new Map (list (list 1 2))), Map bound in environment"
        (fn ()
          (assert-equal
           (~> (interpret
                '(new Map '((1 2)))
                (new LispEnvironment
                     `((Map ,Map "function"))))
               (send entries)
               (send Array from _))
           '((1 2)))))
    (xit "(new Map (list (list 1 2))), new, Map bound in environment"
         (fn ()
           (assert-equal
            (~> (interpret
                 '(new Map '((1 2)))
                 (new LispEnvironment
                      `((Map ,Map "function"))))
                (send entries)
                (send Array from _))
            '((1 2)))))))

(describe "+"
  (fn ()
    (it "(+)"
        (fn ()
          (test-lisp
           '(+)
           0)))
    (it "(+ 1)"
        (fn ()
          (test-lisp
           '(+ 1)
           1)))
    (it "(+ 1 2)"
        (fn ()
          (test-lisp
           '(+ 1 2)
           3)))
    (it "(+ 1 2 3)"
        (fn ()
          (test-lisp
           '(+ 1 2 3)
           6)))
    (it "(+ 1 1), custom function"
        (fn ()
          (test-lisp
           '(+ 1 1)
           2
           (js-obj "env"
                   (new LispEnvironment
                        `((+
                           ,(lambda (x y)
                              (+ x y))
                           "function")))))))
    (it "(+ 1 1), custom function"
        (fn ()
          (test-lisp
           '(+ 1 1)
           2
           (js-obj "env"
                   (new LispEnvironment
                        `((+
                           ,(lambda (x y)
                              (+ x y))
                           "function")))))))
    (it "(+ x x)"
        (fn ()
          (test-lisp
           '(+ x x)
           4
           (js-obj "env"
                   (new LispEnvironment
                        `((x
                           2
                           "variable")
                          (+
                           ,(lambda (x y)
                              (+ x y))
                           "function")))))))
    ;; (it "(+ x x), custom function and variable"
    ;;     (fn ()
    ;;       (test-lisp
    ;;        '(+ x x)
    ;;        2
    ;;        (js-obj "env"
    ;;                (new LispEnvironment
    ;;                     `((x
    ;;                        1
    ;;                        "variable")
    ;;                       (+
    ;;                        ,(lambda (x y)
    ;;                           (+ x y))
    ;;                        "function")))))))
    (it "(+ x x), custom function and variable"
        (fn ()
          (test-lisp
           '(+ x x)
           2
           (js-obj "env"
                   (new LispEnvironment
                        `((x
                           1
                           "variable")
                          (+
                           ,(lambda (x y)
                              (+ x y))
                           "function")))))))
    (it "(+ x x), custom function and variable"
        (fn ()
          (test-lisp
           '(+ x x)
           2
           (js-obj "env"
                   (new LispEnvironment
                        `((x
                           1
                           "variable")
                          (+
                           ,(lambda (x y)
                              (+ x y))
                           "function")))))))
    (it "(+ (+ 1 1) (+ 1 1)), custom function"
        (fn ()
          (test-lisp
           '(+ (+ 1 1) (+ 1 1))
           4
           (js-obj "env"
                   (new LispEnvironment
                        `((+
                           ,(lambda (x y)
                              (+ x y))
                           "function")))))))
    (it "(apply + '(1 2))"
        (fn ()
          (test-lisp
           '(apply + '(1 2))
           3)))))

(describe "-"
  (fn ()
    (it "(-)"
        (fn ()
          (test-lisp
           '(-)
           0)))
    (it "(- 1)"
        (fn ()
          (test-lisp
           '(- 1)
           -1)))
    (it "(- 1 2)"
        (fn ()
          (test-lisp
           '(- 1 2)
           -1)))
    (it "(- 1 2 3)"
        (fn ()
          (test-lisp
           '(- 1 2 3)
           -4)))))

(describe "*"
  (fn ()
    (it "(*)"
        (fn ()
          (test-lisp
           '(*)
           1)))
    (it "(* 1)"
        (fn ()
          (test-lisp
           '(* 1)
           1)))
    (it "(* 1 2)"
        (fn ()
          (test-lisp
           '(* 1 2)
           2)))
    (it "(* 1 2 3)"
        (fn ()
          (test-lisp
           '(* 1 2 3)
           6)))))

(describe "/"
  (fn ()
    (xit "(/)"
         (fn ()
           (test-lisp
            '(/)
            1)))
    (it "(/ 1)"
        (fn ()
          (test-lisp
           '(/ 1)
           1)))
    (it "(/ 2)"
        (fn ()
          (test-lisp
           '(/ 2)
           0.5)))
    (it "(/ 1 2)"
        (fn ()
          (test-lisp
           '(/ 1 2)
           0.5)))
    (it "(/ 1 2 3)"
        (fn ()
          (test-lisp
           '(/ 1 2 3)
           (/ 1 2 3))))))

(describe "range"
  (fn ()
    (it "(range 1 2)"
        (fn ()
          (test-lisp
           '(range 1 2)
           '(1))))
    (it "(range 10)"
        (fn ()
          (test-lisp
           '(range 10)
           '(0 1 2 3 4 5 6 7 8 9))))
    (it "(range 10 20)"
        (fn ()
          (test-lisp
           '(range 10 20)
           '(10 11 12 13 14 15 16 17 18 19))))
    (it "(range 20 40 2)"
        (fn ()
          (test-lisp
           '(range 20 40 2)
           '(20 22 24 26 28 30 32 34 36 38))))
    (it "(range 20 10 -1)"
        (fn ()
          (test-lisp
           '(range 20 10 -1)
           '(20 19 18 17 16 15 14 13 12 11))))
    (it "(range 10 15 1.5)"
        (fn ()
          (test-lisp
           '(range 10 15 1.5)
           '(10 11.5 13.0 14.5))))))

(describe "member"
  (fn ()
    (it "(member 2 (list 1 2 3 4))"
        (fn ()
          (test-lisp
           '(member 2 (list 1 2 3 4))
           '(2 3 4))))
    (it "(member 9 (list 1 2 3 4))"
        (fn ()
          (test-lisp
           '(member 9 (list 1 2 3 4))
           #f)))
    (it "(member 5 '(3 5 1 7 2 9) (lambda (x y) (< x y)))"
        (fn ()
          (test-lisp
           '(member 5
                    '(3 5 1 7 2 9)
                    (lambda (x y)
                      (< x y)))
           '(7 2 9))))))

(describe "member?"
  (fn ()
    (it "(member? 2 (list 1 2 3 4))"
        (fn ()
          (test-lisp
           '(member? 2 (list 1 2 3 4))
           #t)))
    (it "(member? 9 (list 1 2 3 4))"
        (fn ()
          (test-lisp
           '(member? 9 (list 1 2 3 4))
           #f)))))

(describe "take"
  (fn ()
    (it "(take '(1 2 3 4) 0)"
        (fn ()
          (test-lisp
           '(take '(1 2 3 4) 0)
           '())))
    (it "(take '(1 2 3 4) 1)"
        (fn ()
          (test-lisp
           '(take '(1 2 3 4) 1)
           '(1))))
    (it "(take '(1 2 3 4) 2)"
        (fn ()
          (test-lisp
           '(take '(1 2 3 4) 2)
           '(1 2))))))

(describe "drop"
  (fn ()
    (it "(drop '(1 2 3 4) 0)"
        (fn ()
          (test-lisp
           '(drop '(1 2 3 4) 0)
           '(1 2 3 4))))
    (it "(drop '(1 2 3 4) 1)"
        (fn ()
          (test-lisp
           '(drop '(1 2 3 4) 1)
           '(2 3 4))))))

(describe "drop-right"
  (fn ()
    (it "(drop-right '(1 2 3 4) 0)"
        (fn ()
          (test-lisp
           '(drop-right '(1 2 3 4) 0)
           '(1 2 3 4))))
    (it "(drop-right '(1 2 3 4) 1)"
        (fn ()
          (test-lisp
           '(drop-right '(1 2 3 4) 1)
           '(1 2 3))))))

(describe "map"
  (fn ()
    (it "(map list '(1 2))"
        (fn ()
          (test-lisp
           '(map list '(1 2))
           '((1) (2)))))
    (it "(map fact '(1 2 3 4 5 6))"
        (fn ()
          (test-lisp
           '(begin
              (define (fact n)
                (if (< n 2)
                    1
                    (* n (fact (- n 1)))))
              (map fact '(1 2 3 4 5 6)))
           '(1 2 6 24 120 720))))))

(describe "foldl"
  (fn ()
    (it "(foldr cons '() '(1 2 3 4))"
        (fn ()
          (test-lisp
           '(foldl cons '() '(1 2 3 4))
           '(4 3 2 1))))))

(describe "foldr"
  (fn ()
    (it "(foldr cons '() '(1 2 3 4))"
        (fn ()
          (test-lisp
           '(foldr cons '() '(1 2 3 4))
           '(1 2 3 4))))
    (it "(foldr (lambda (v l) (cons (add1 v) l)) ...)"
        (fn ()
          (test-lisp
           '(foldr (lambda (v l)
                     (cons (add1 v) l))
                   '()
                   '(1 2 3 4))
           '(2 3 4 5))))))

(describe "filter"
  (fn ()
    (it "(filter string? '(\"foo\" 1 2 3))"
        (fn ()
          (test-lisp
           '(filter string? '("foo" 1 2 3))
           '("foo"))))))

(describe "string functions"
  (fn ()
    (describe "string-length"
      (fn ()
        (it "> (string-length \"foo\")"
            (fn ()
              (test-repl
               '(roselisp
                 > (string-length "foo")
                 3))))))
    (describe "string-append"
      (fn ()
        (it "(string-append)"
            (fn ()
              (test-lisp
               '(string-append)
               "")))
        (it "(string-append \"foo\")"
            (fn ()
              (test-lisp
               '(string-append "foo")
               "foo")))
        (it "(string-append \"foo\" \"bar\")"
            (fn ()
              (test-lisp
               '(string-append "foo" "bar")
               "foobar")))
        (it "(apply string-append '(\"foo\" \"bar\"))"
            (fn ()
              (test-lisp
               '(apply string-append '("foo" "bar"))
               "foobar")))))
    (describe "string-trim"
      (fn ()
        (it "> (string-trim \"  foo bar  baz  \")"
            (fn ()
              (test-repl
               '(roselisp
                 > (string-trim "  foo bar  baz  ")
                 "foo bar  baz"))))
        (it "> (string-trim \"  foo bar  baz \\r\\n\\t\")"
            (fn ()
              (test-repl
               '(roselisp
                 > (string-trim "  foo bar  baz \r\n\t")
                 "foo bar  baz"))))
        (it "> (string-trim \"_foo bar  baz_\" \"_\")"
            (fn ()
              (test-repl
               '(roselisp
                 > (string-trim "_foo bar  baz_" "_")
                 "foo bar  baz")
               (js-obj "compile" #f))))
        (it "> (string-trim \"__foo bar  baz__\" \"_\" :repeat? #t)"
            (fn ()
              (test-repl
               '(roselisp
                 > (string-trim "__foo bar  baz__" "_" :repeat? #t)
                 "foo bar  baz")
               (js-obj "compile" #f))))
        (it "> (string-trim \"  foo bar  baz \\r\\n\\t\" \" \" :repeat? #t)"
            (fn ()
              (test-repl
               '(roselisp
                 > (string-trim "  foo bar  baz \r\n\t" " " :repeat? #t)
                 "foo bar  baz \r\n\t")
               (js-obj "compile" #f))))))
    (describe "substring"
      (fn ()
        (it "(substring \"Apple\" 1 3)"
            (fn ()
              (test-lisp
               '(substring "Apple" 1 3)
               "pp")))
        (it "(substring \"Apple\" 1"
            (fn ()
              (test-lisp
               '(substring "Apple" 1)
               "pple")))))))

(describe "apply"
  (fn ()
    (it "(apply new make-hash '())"
        (fn ()
          (test-lisp
           '(apply new make-hash '())
           (new Map)
           (js-obj "compile" #f))))
    (xit "(apply new make-hash '())"
         (fn ()
           (test-lisp
            '(apply new make-hash '())
            (new Map)
            (js-obj "compile" #f))))
    (xit "(apply send (make-hash) 'has '(\"foo\"))"
         (fn ()
           (test-lisp
            '(apply send (make-hash) 'has '("foo"))
            #f)))
    (xit "(apply send (make-hash) '(has \"foo\"))"
         (fn ()
           (test-lisp
            '(apply send (make-hash) '(has "foo"))
            #f
            (js-obj "compile" #f))))))

(describe "as~>"
  (fn ()
    (it "(as~> 0 _ (+ _ 1) (+ _ 1))"
        (fn ()
          (test-lisp
           '(as~> 0 _
              (+ _ 1)
              (+ _ 1))
           2)))))

(describe "ann"
  (fn ()
    (it "(ann undefined Any)"
        (fn ()
          (test-lisp
           '(ann undefined Any)
           undefined)))
    (it "((ann undefined Any))"
        (fn ()
          (test-lisp
           '((ann undefined Any))
           undefined
           (js-obj "compile" #f))))))

(describe "interpret"
  (fn ()
    (it "default environment"
        (fn ()
          (assert-equal
           (interpret (js/tag sexp "t"))
           #t)
          (assert-equal
           (interpret (js/tag sexp "t")
                      (new LispEnvironment))
           #t)))
    (it "currying"
        (fn ()
          (assert-equal
           ((interpret (js/tag sexp "t")
                       __)
            (new LispEnvironment))
           #t)
          (assert-equal
           ((interpret __
                       (new LispEnvironment))
            (js/tag sexp "t"))
           #t)
          (assert-equal
           (((interpret __ __)
             (js/tag sexp "t"))
            (new LispEnvironment))
           #t)))))

(describe "lisp"
  (fn ()
    (it "(quote foo)"
        (fn ()
          (assert-equal
           (lisp "(quote foo)")
           (js/tag s "foo"))))
    (it "(identity1 \"foo\")"
        (fn ()
          (assert-equal
           (lisp "(identity1 \"foo\")"
                 (new LispEnvironment
                      `((identity1
                         ,(lambda (x)
                            x)
                         "variable"))))
           "foo")))
    (it "(list 1 2)"
        (fn ()
          (assert-equal
           (lisp "(list 1 2)")
           '(1 2))))
    (describe "+"
      (fn ()
        (it "(+ 1 1)"
            (fn ()
              (assert-equal
               (lisp "(+ 1 1)"
                     (new LispEnvironment
                          `((+
                             ,(lambda (x y)
                                (+ x y))
                             "function"))))
               2)))
        (it "(+ foo foo)"
            (fn ()
              (assert-equal
               (lisp "(+ foo foo)"
                     (new LispEnvironment
                          `((foo
                             2
                             "variable")
                            (+
                             ,(lambda (x y)
                                (+ x y))
                             "function"))))
               4)))))))

(describe "current-environment"
  (fn ()
    (it "(send (current-environment) get 'x)"
        (fn ()
          (test-lisp
           '((lambda (x)
               (send (current-environment)
                     get
                     'x))
             1)
           1
           (js-obj "compile" #f))))))

(describe "set-car!"
  (fn ()
    (it "() -> ()"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo '())
             undefined
             > (set-car! foo 'bar)
             undefined
             > foo
             '()))))
    (it "(foo) -> (bar)"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo
                 '(foo))
             undefined
             > (set-car! foo 'bar)
             undefined
             > foo
             '(bar)))))))

(describe "set-cdr!"
  (fn ()
    (it "() -> ()"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo '())
             undefined
             > (set-cdr! foo '(bar))
             undefined
             > foo
             '()))))
    (it "(foo) -> (foo bar)"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo
                 '(foo))
             undefined
             > (set-cdr! foo '(bar))
             undefined
             > foo
             '(foo bar)))))
    (it "(foo bar) -> (foo baz)"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo
                 '(foo bar))
             undefined
             > (set-cdr! foo '(baz))
             undefined
             > foo
             '(foo baz)))))
    (it "(foo bar) -> (foo baz . quux)"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo
                 '(foo bar))
             undefined
             > (set-cdr! foo '(baz . quux))
             undefined
             > foo
             '(foo baz . quux)))))
    (it "(foo . bar) -> (foo baz)"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo
                 '(foo . bar))
             undefined
             > (set-cdr! foo '(baz))
             undefined
             > foo
             '(foo baz)))))
    (it "(foo . bar) -> (foo baz . quux)"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo
                 '(foo . bar))
             undefined
             > (set-cdr! foo '(baz . quux))
             undefined
             > foo
             '(foo baz . quux)))))
    (it "(foo) -> (foo . bar)"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo
                 '(foo))
             undefined
             > (set-cdr! foo 'bar)
             undefined
             > foo
             '(foo . bar)))))
    (it "(foo bar . baz) -> (foo quux)"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo
                 '(foo bar . baz))
             undefined
             > (set-cdr! foo '(quux))
             undefined
             > foo
             '(foo quux)))))
    (it "(foo bar . baz) -> (foo . quux)"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo
                 '(foo bar . baz))
             undefined
             > (set-cdr! foo 'quux)
             undefined
             > foo
             '(foo . quux)))))))

(describe "dotted-list?"
  (fn ()
    (it "(foo . bar)"
        (fn ()
          (test-repl
           '(roselisp
             > (dotted-list? '(foo . bar))
             #t))))
    (it "(foo bar)"
        (fn ()
          (test-repl
           '(roselisp
             > (dotted-list? '(foo bar))
             #f))))))

(describe "dotted-list-head"
  (fn ()
    (it "(foo . bar)"
        (fn ()
          (test-repl
           '(roselisp
             > (dotted-list-head '(foo . bar))
             '(foo)))))
    (it "(foo bar . baz)"
        (fn ()
          (test-repl
           '(roselisp
             > (dotted-list-head '(foo bar . baz))
             '(foo bar)))))))

(describe "dotted-list-tail"
  (fn ()
    (it "(foo . bar)"
        (fn ()
          (test-repl
           '(roselisp
             > (dotted-list-tail '(foo . bar))
             'bar))))
    (it "(foo bar . baz)"
        (fn ()
          (test-repl
           '(roselisp
             > (dotted-list-tail '(foo bar . baz))
             'baz))))))

(describe "dotted-list->proper-list"
  (fn ()
    (it "(foo . bar)"
        (fn ()
          (test-repl
           '(roselisp
             > (dotted-list->proper-list '(foo . bar))
             '(foo bar)))))
    (it "(foo bar . baz)"
        (fn ()
          (test-repl
           '(roselisp
             > (dotted-list->proper-list '(foo bar . baz))
             '(foo bar baz)))))))

(describe "proper-list?"
  (fn ()
    (it "(foo bar)"
        (fn ()
          (test-repl
           '(roselisp
             > (proper-list? '(foo bar))
             #t))))
    (it "(foo . bar)"
        (fn ()
          (test-repl
           '(roselisp
             > (proper-list? '(foo . bar))
             #f))))))

(describe "circular-list?"
  (fn ()
    (it "()"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo '())
             undefined
             > (circular-list? foo)
             #f
             > (set-cdr! foo foo)
             undefined
             > (circular-list? foo)
             #f))))
    (it "(foo)"
        (fn ()
          (test-repl
           '(roselisp
             > (circular-list? '(foo))
             #f))))
    (it "(foo . bar)"
        (fn ()
          (test-repl
           '(roselisp
             > (circular-list? '(foo . bar))
             #f))))
    (it "(foo . #0#)"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo
                 '(foo))
             undefined
             > (set-cdr! foo foo)
             undefined
             > (circular-list? foo)
             #t))))
    (it "(foo . #0#), from linked list"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo
                 '(foo . ()))
             undefined
             > (set-cdr! foo foo)
             undefined
             > (circular-list? foo)
             #t))))
    (it "(foo bar . #0#)"
        (fn ()
          (test-repl
           '(roselisp
             > (define foo
                 '(foo bar))
             undefined
             > (set-cdr! foo foo)
             undefined
             > (circular-list? foo)
             #t))))))

(describe "proper-list->dotted-list"
  (fn ()
    (it "(foo bar) -> (foo . bar)"
        (fn ()
          (test-repl
           '(roselisp
             > (proper-list->dotted-list '(foo bar))
             '(foo . bar)))))
    (it "(foo bar baz) -> (foo bar . baz)"
        (fn ()
          (test-repl
           '(roselisp
             > (proper-list->dotted-list '(foo bar baz))
             '(foo bar . baz)))))))

(describe "list*"
  (fn ()
    (it "(list*)"
        (fn ()
          (test-repl
           '(roselisp
             > (list*)
             undefined))))
    (it "(list* 1)"
        (fn ()
          (test-repl
           '(roselisp
             > (list* 1)
             1))))
    (it "(list* 1 2)"
        (fn ()
          (test-repl
           '(roselisp
             > (list* 1 2)
             '(1 . 2)))))
    (it "(list* 1 2 3)"
        (fn ()
          (test-repl
           '(roselisp
             > (list* 1 2 3)
             '(1 2 . 3)))))
    (it "(list* 1 2 3 4)"
        (fn ()
          (test-repl
           '(roselisp
             > (list* 1 2 3 4)
             '(1 2 3 . 4)))))
    (it "(list* 1 '())"
        (fn ()
          (test-repl
           '(roselisp
             > (list* 1 '())
             '(1)))))
    (it "(list* 1 '(2))"
        (fn ()
          (test-repl
           '(roselisp
             > (list* 1 '(2))
             '(1 2)))))
    (it "(list* 1 '(2 . 3))"
        (fn ()
          (test-repl
           '(roselisp
             > (list* 1 '(2 . 3))
             '(1 2 . 3)))))))

(describe "eval"
  (fn ()
    ;; (it "1 + 1, eval true"
    ;;     (fn ()
    ;;       (assert-equal
    ;;        (interpret '(+ 1 1) undefined (js-obj "eval" #t))
    ;;        2)))
    ;; (it "1 + 1, eval false"
    ;;     (fn ()
    ;;       (assert-equal
    ;;        (interpret '(+ 1 1) undefined (js-obj "eval" #f))
    ;;        2)))

    ;; (send it only "js/eval, eval true"
    ;;       (fn ()
    ;;         (assert-equal
    ;;          (interpret 'js/eval undefined (js-obj "eval" #t))
    ;;          js/eval)))
    (it "js/eval, eval true"
        (fn ()
          (assert-equal
           (interpret '(js/eval "1") undefined (js-obj "eval" #t))
           1)))
    ;; (send it only "js/eval, eval true"
    ;;       (fn ()
    ;;         (assert-equal
    ;;          (interpret '(js/eval "1") undefined (js-obj "eval" #f))
    ;;          undefined)))
    (it "js/eval, eval false"
        (fn ()
          (assert-equal
           (interpret 'js/eval undefined (js-obj "eval" #f))
           undefined)))
    ))
