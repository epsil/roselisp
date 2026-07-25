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
                  test-repl
                  test-macro))

(test-macro
 :repl #t

 ;; Boolean values
 > (describe "boolean values")
 _
 > true
 #t
 > t
 #t
 > #t
 #t
 > false
 #f
 > #f
 #f

 ;; `truep`
 > (describe "truep")
 _
 > (truep true)
 #t
 > (truep false)
 #f
 > (truep undefined)
 #f
 > (truep true)
 #t

 ;; `falsep`
 > (describe "falsep")
 _
 > (falsep true)
 #f
 > (falsep false)
 #t
 > (falsep undefined)
 #t

 ;; Variables
 > (describe "Variables")
 _
 xit> '(begin
         (setq a 1 b 2 c 3)
         (list a b c))
 '(1 2 3)

 ;; Function calls
 > (describe "Function calls")
 _
 xit> (begin
        (define ((((my-add) x) y) z)
          (+ x y z))
        ((my-add _ 2 3) 1))
 6
 xit> (begin
        (define ((((my-add) x) y) z)
          (+ x y z))
        ((my-add _ _ _) 1 2 3))
 6
 xit> (begin
        (define ((((my-add) x) y) z)
          (+ x y z))
        (((my-add _ _ 3) 1) 2))
 6
 xit> (begin
        (define ((((my-add) x) y) z)
          (+ x y z))
        ((my-add _ _ 3) 1 2))
 6

 ;; `define`
 > (describe "define")
 _
 > (begin
     (define ((my-add x) y)
       (+ x y))
     (my-add 2 3))
 5
 xit> (begin
        (define ((my-add x) y)
          (+ x y))
        ((my-add 2) 3))
 5
 xit> (begin
        (define (((my-add) x) y)
          (+ x y))
        (((my-add) 2) 3))
 5
 xit> (begin
        (define ((my-add x y) z)
          (+ x y z))
        (my-add 1 2 3))
 6

 ;; `define-macro`
 > (describe "define-macro")
 _
 xit> ((lambda ()
         (define-macro (foo x)
           x)
         (foo '(foo 1))))
 '(foo 1)
 xit> ((lambda ()
         (define-macro (foo x)
           `(+ ,x ,x))
         (foo 1)))
 2
 xit> ((lambda ()
         (define-macro my-macro (x)
           `(begin ,x))
         (my-macro 1)))
 1

 ;; `defmacro`
 > (describe "defmacro")
 _
 xit> ((lambda ()
         (defmacro foo (x)
           x)
         (foo '(foo 1))))
 '(foo 1)
 xit> ((lambda ()
         (defmacro my-macro (&environment env)
           (send env has '+))
         (my-macro)))
 #t
 xit> ((lambda ()
         (defmacro my-macro (&environment env-arg)
           (send env-arg has '+))
         (my-macro)))
 #t
 xit> ((lambda ()
         (defmacro (my-macro x)
           `(begin ,x))
         (my-macro 1)))
 1

 ;; `lambda`
 > (describe "lambda")
 _
 > ((lambda (x y)
      (+ x y))
    1
    1)
 2

 ;; `nlambda`
 > (describe "nlambda")
 _
 xit> (begin
        (setq a 1 b 2 c 3)
        (define f
          (nlambda (x y z)
                   (list x y z)))
        (f a b c))
 '(a b c)
 xit> (begin
        (setq a 1 b 2 c 3)
        ((nlambda (x y z) (list x y z)) a b c))
 '(a b c)

 ;; `if`
 > (describe "if")
 _
 xit> (if "" 1 2)
 1

 ;; `eq?`
 > (describe "eq?")
 _
 > (begin
     (define (my-unit x)
       x)
     (my-unit 'foo))
 'foo
 xit> (begin
        (define ((my-curried-unit) x) x)
        (my-curried-unit '_))
 '_

 ;; `for`
 > (describe "for")
 _
 xit> (let ((result '()))
        (for ((x '(1 2 3))
              (y '(4 5 6)))
          (set! result (cons x result))
          (set! result (cons y result)))
        result)
 '(6 3 5 2 4 1)

 ;; `send`
 > (describe "send")
 _
 xit> (send obj 'add 1 1)
 2
 > (send (make-hash
          '(("foo" . "foo")))
         has
         '("foo"))
 #f

 ;; `dot`
 > (describe "dot")
 _
 > (let ((obj (js-obj "add1" (lambda (x) (+ x 1)))))
     (. obj add1 1))
 2
 > (let ((obj (js-obj "add1" (lambda (x) (+ x 1)))))
     (.add1 obj 1))
 2
 > (let ((obj (js-obj "add" (lambda (x y) (+ x y)))))
     (.add obj 1 1))
 2
 > (let ((obj (js-obj)))
     (set! (.-prop obj) "bar")
     (.-prop obj))
 "bar"
 > (let ((obj (js-obj "prop" "foo")))
     (.-prop obj))
 "foo"

 ;; `new`
 > (describe "new")
 _
 > (let (quux)
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

 ;; `class`
 > (describe "class")
 _
 > (it "(defclass Foo ...) with constructor and new"
       (let (quux)
         (defclass Foo ()
           (define x)
           (define (constructor x)
             (set! (.-x this) x))
           (define (bar)
             (.-x this)))
         (set! quux (new Foo "xyzzy"))
         (.bar quux)))
 "xyzzy"
 xit> (it "(defclass Foo ...) with no arguments"
          (let (quux)
            (defclass Foo ()
              (define x "wobble")
              (define (constructor x)
                (set! (.-x this) x))
              (define (bar)
                (.-x this)))
            (set! quux (new Foo))
            (.bar quux)))
 "wobble"

 ;; `clj/try`
 > (describe "clj/try")
 _
 > (clj/try
    (throw (new Error "an error"))
    (catch Error e
      "there was an error")
    (finally
      (display "finally")))
 "there was an error"

 ;; `+`
 > (describe "+")
 _
 > (+ 1 1)
 2
 > (let ((x 1))
     (+ x x))
 2
 > (+ (+ 1 1) (+ 1 1))
 4

 ;; String functions
 > (describe "String functions")
 _
 > (string-split "foo bar baz" " ")
 '("foo" "bar" "baz")
 xit> (string-split "  foo bar  baz \r\n\t")
 '("foo" "bar" "baz")
 > (string-trim "_foo bar  baz_" "_")
 "foo bar  baz"
 > (string-trim "__foo bar  baz__" "_" :repeat? #t)
 "foo bar  baz"
 > (string-trim "  foo bar  baz \r\n\t" " " :repeat? #t)
 "foo bar  baz \r\n\t"

 ;; `apply`
 > (describe "apply")
 _
 > (apply new make-hash '())
 (new Map)
 xit> (apply new make-hash '())
 (new Map)
 xit> (apply send (make-hash) 'has '("foo"))
 #f
 xit> (apply send (make-hash) '(has "foo"))
 #f

 ;; Y combinator
 > (describe "Y combinator")
 _
 > (let ((Y
          (lambda (f)
            ((lambda (future)
               (f (lambda (arg)
                    ((future future) arg))))
             (lambda (future)
               (f (lambda (arg)
                    ((future future) arg))))))))
     ((Y (lambda (f)
           (lambda (x)
             (if (zero? x)
                 1
                 (* x (f (- x 1)))))))
      6))
 720

 ;; `ann`
 > (describe "ann")
 _
 xit> ((ann #u Any))
 #u

 ;; `current-environment`
 > (describe "current-environment")
 _
 > ((lambda (x)
      (send (current-environment) get 'x))
    1)
 1

 ;; `js/eval`
 > (describe "js/eval")
 _
 > (js/eval "1")
 1
 > (interpret '(js/eval "1") #u (js-obj "eval" #t))
 1
 xit> (interpret 'js/eval #u (js-obj "eval" #f))
 #u

 ;; `interpret`
 > (describe "interpret")
 _
 > (interpret 't)
 #t)

(test-macro
 ;; `interpret`
 > (describe "interpret")
 _
 > (interpret 't)
 #t
 > (interpret 't
              (new LispEnvironment))
 #t
 xit> ((interpret 't
                  __)
       (new LispEnvironment))
 #t
 xit> ((interpret __
                  (new LispEnvironment))
       't)
 #t
 xit> (((interpret __ __)
        't)
       (new LispEnvironment))
 #t

 ;; `lisp`
 > (describe "lisp")
 _
 > (lisp "(quote foo)")
 'foo
 > (lisp "(identity1 \"foo\")"
         (new LispEnvironment
              `((identity1
                 ,(lambda (x)
                    x)
                 "variable"))))
 "foo"
 > (lisp "(list 1 2)")
 '(1 2)
 > (lisp "(+ 1 1)"
         (new LispEnvironment
              `((+
                 ,(lambda (x y)
                    (+ x y))
                 "function"))))
 2
 > (lisp "(+ foo foo)"
         (new LispEnvironment
              `((foo
                 2
                 "variable")
                (+
                 ,(lambda (x y)
                    (+ x y))
                 "function"))))
 4

 ;; `Map`
 > (describe "Map")
 _
 > (~> (interpret
        '(new Map)
        (new LispEnvironment
             `((Map ,Map "function"))))
       (instance-of? Map))
 #t
 xit> (~> (interpret
           '(new Map '((1 2)))
           (new LispEnvironment
                `((Map ,Map "function"))))
          (send entries)
          (send Array from _))
 '((1 2))

 ;; `error`
 > (describe "error")
 _
 > (it "(error)"
       (assert-throws
        (lambda ()
          (interpret
           '(error)
           (new LispEnvironment)))))
 _
 > (it "(error \"foo\")"
       (assert-throws
        (lambda ()
          (interpret
           '(error "foo")
           (new LispEnvironment)))))
 _)
