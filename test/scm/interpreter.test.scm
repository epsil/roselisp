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

 :describe "boolean values"
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

 :describe "truep"
 > (truep true)
 #t
 > (truep false)
 #f
 > (truep undefined)
 #f
 > (truep true)
 #t

 :describe "falsep"
 > (falsep true)
 #f
 > (falsep false)
 #t
 > (falsep undefined)
 #t

 :describe "define"
 > (begin
     (define ((my-add x) y)
       (+ x y))
     (my-add 2 3))
 5

 :describe "lambda"
 > ((lambda (x y)
      (+ x y))
    1
    1)
 2

 :describe "eq?"
 > (begin
     (define (my-unit x)
       x)
     (my-unit 'foo))
 'foo

 :describe "send"
 > (send (make-hash
          '(("foo" . "foo")))
         has
         '("foo"))
 #f

 :describe "dot"
 > (let ((obj (js/obj "add1" (lambda (x) (+ x 1)))))
     (. obj add1 1))
 2
 > (let ((obj (js/obj "add1" (lambda (x) (+ x 1)))))
     (.add1 obj 1))
 2
 > (let ((obj (js/obj "add" (lambda (x y) (+ x y)))))
     (.add obj 1 1))
 2
 > (let ((obj (js/obj)))
     (set! (.-prop obj) "bar")
     (.-prop obj))
 "bar"
 > (let ((obj (js/obj "prop" "foo")))
     (.-prop obj))
 "foo"

 :describe "new"
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

 :describe "class"
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

 :describe "clj/try"
 > (clj/try
    (throw (new Error "an error"))
    (catch Error e
      "there was an error")
    (finally
      (display "finally")))
 "there was an error"

 :describe "+"
 > (+ 1 1)
 2
 > (let ((x 1))
     (+ x x))
 2
 > (+ (+ 1 1) (+ 1 1))
 4

 :describe "String functions"
 > (string-split "foo bar baz" " ")
 '("foo" "bar" "baz")
 > (string-trim "_foo bar  baz_" "_")
 "foo bar  baz"
 > (string-trim "__foo bar  baz__" "_" :repeat? #t)
 "foo bar  baz"
 > (string-trim "  foo bar  baz \r\n\t" " " :repeat? #t)
 "foo bar  baz \r\n\t"

 :describe "apply"
 > (apply new make-hash '())
 (new Map)

 :describe "Y combinator"
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

 :describe "current-environment"
 > ((lambda (x)
      (send (current-environment) get 'x))
    1)
 1

 :describe "js/eval"
 > (js/eval "1")
 1
 > (interpret '(js/eval "1") #u (js/obj :eval #t))
 1

 :describe "interpret"
 > (interpret 't)
 #t

 :repl #f
 :describe "interpret"
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

 :describe "lisp"
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

 :describe "Map"
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

 :describe "error"
 > (it "(error)"
       (assert-throws
        (lambda ()
          (interpret
           '(error)
           (new LispEnvironment)))))
 > (it "(error \"foo\")"
       (assert-throws
        (lambda ()
          (interpret
           '(error "foo")
           (new LispEnvironment)))))
 _)
