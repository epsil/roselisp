;;; # Interpreter tests

(require (only-in "../../src/ts/language"
                  __
                  LispEnvironment
                  interpret
                  lisp))
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
           #t)))))

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

(describe "variables"
  (fn ()
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
          ))))

(describe "define-macro"
  (fn ()
    (it "(define-macro (foo x) x)"
        (fn ()
          (test-repl
           '(roselisp
             > (define-macro (foo x)
                 x)
             _
             > (foo '(foo 1))
             '(foo 1))
           (js-obj "compile" #f))))
    (it "(define-macro (foo x) `(+ ,x ,x))"
        (fn ()
          (test-repl
           '(roselisp
             > (define-macro (foo x)
                 `(+ ,x ,x))
             _
             > (foo 1)
             2)
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

(describe "defmacro"
  (fn ()
    (it "(defmacro foo (x) x)"
        (fn ()
          (test-repl
           '(roselisp
             > (defmacro foo (x)
                 x)
             _
             > (foo '(foo 1))
             '(foo 1))
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

(describe "lambda"
  (fn ()
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

(describe "if"
  (fn ()
    (xit "(if \"\" 1 2)"
         (fn ()
           (test-lisp
            '(if ""
                 1
                 2)
            1)))))

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
           '_)))))

(describe "for"
  (fn ()
    (xit "(let ... (for ((x ...) (y ...)) ...) ...)"
         (fn ()
           (test-lisp
            '(let ((result '()))
               (for ((x '(1 2 3))
                     (y '(4 5 6)))
                 (set! result (cons x result))
                 (set! result (cons y result)))
               result)
            '(6 3 5 2 4 1))))))

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
    (it "(send (make-hash '((\"foo\" . \"foo\"))) has '(\"foo\"))"
        (fn ()
          (test-lisp
           '(send (make-hash
                   '(("foo" . "foo")))
                  has
                  '("foo"))
           #f)))))

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
            (js-obj "compile" #f))))))

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

(describe "Map"
  (fn ()
    (it "(new Map), Map bound in environment"
        (fn ()
          (assert-equal
           (~> (interpret
                '(new Map)
                (new LispEnvironment
                     `((Map ,Map "function"))))
               (instance-of? Map))
           #t)))
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
                           "function")))))))))

(describe "string functions"
  (fn ()
    (describe "string-split"
      (fn ()
        (xit "(string-split \"  foo bar  baz \\r\\n\\t\")"
             (fn ()
               (test-repl
                '(roselisp
                  > (string-split "  foo bar  baz \r\n\t")
                  '("foo" "bar" "baz")))))))
    (describe "string-trim"
      (fn ()
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
               (js-obj "compile" #f))))))))

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

(describe "Y combinator"
  (fn ()
    (it "6!"
        (fn ()
          (test-repl
           '(roselisp
             > (define (Y f)
                 ((lambda (future)
                    (f (lambda (arg)
                         ((future future) arg))))
                  (lambda (future)
                    (f (lambda (arg)
                         ((future future) arg))))))
             #u
             > ((Y (lambda (f)
                     (lambda (x)
                       (if (zero? x)
                           1
                           (* x (f (- x 1)))))))
                6)
             720))))))

(describe "ann"
  (fn ()
    (xit "((ann #u Any))"
         (fn ()
           (test-lisp
            '((ann #u Any))
            #u
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

(describe "eval"
  (fn ()
    ;; (it "1 + 1, eval true"
    ;;     (fn ()
    ;;       (assert-equal
    ;;        (interpret '(+ 1 1) #u (js-obj "eval" #t))
    ;;        2)))
    ;; (it "1 + 1, eval false"
    ;;     (fn ()
    ;;       (assert-equal
    ;;        (interpret '(+ 1 1) #u (js-obj "eval" #f))
    ;;        2)))
    ;; (send it only "js/eval, eval true"
    ;;       (fn ()
    ;;         (assert-equal
    ;;          (interpret 'js/eval #u (js-obj "eval" #t))
    ;;          js/eval)))
    (it "js/eval, eval true"
        (fn ()
          (assert-equal
           (interpret '(js/eval "1") #u (js-obj "eval" #t))
           1)))
    ;; (send it only "js/eval, eval true"
    ;;       (fn ()
    ;;         (assert-equal
    ;;          (interpret '(js/eval "1") #u (js-obj "eval" #f))
    ;;          #u)))
    (it "js/eval, eval false"
        (fn ()
          (assert-equal
           (interpret 'js/eval #u (js-obj "eval" #f))
           #u)))))
