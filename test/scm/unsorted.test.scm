;;; # Unsorted tests
;;;
;;; Tests that have not been sorted yet.

(require chai "chai")
(require (only-in "../../src/ts/env"
                  EnvironmentStack
                  LispEnvironment))
(require (only-in "../../src/ts/language"
                  compile
                  compilation-environment
                  write-to-string))
(require (only-in "./test-util"
                  assert-equal
                  test-lisp
                  test-repl))

;;; Test inbox
(describe "Unsorted tests"
  (fn ()
    (it "unsorted"
        (fn ()
          (test-repl
           ;; Move test code from here into their own tests.
           '(roselisp
             > (flatten '((((4)))))
             '(4)))))
    (it "2 + 2"
        (fn ()
          (test-repl
           '(roselisp
             > (+ 2 2)
             4))))
    (it "((define (foo . args) args))"
        (fn ()
          (test-repl
           '(roselisp
             > (define (foo . args)
                 args)
             undefined
             > (foo)
             '()))))
    (describe "equal?"
      (fn ()
        (it "(equal? 1 1)"
            (fn ()
              (test-repl
               '(roselisp
                 > (equal? 1 1)
                 #t))))
        (it "(equal? '() '())"
            (fn ()
              (test-repl
               '(roselisp
                 > (equal? '() '())
                 #t))))))
    (describe "define-js-obj"
      (fn ()
        (it "(define-js-obj (foo) ...)"
            (fn ()
              (test-repl
               '(roselisp
                 > (define-js-obj (foo)
                     (js-obj "foo" "bar"))
                 undefined
                 > foo
                 "bar"))))
        (it "(define-js-obj ((foo bar)) ...)"
            (fn ()
              (test-repl
               '(roselisp
                 > (define-js-obj ((foo bar))
                     (js-obj "foo" "bar"))
                 undefined
                 > bar
                 "bar"))))))
    (describe "call/cc"
      (fn ()
        (it "(+ 5 (call/cc (lambda (x) (* 10 3))))"
            (fn ()
              (test-repl
               '(roselisp
                 > (+ 5
                      (call/cc
                       (lambda (x)
                         (* 10 3))))
                 35))))
        (it "(+ 5 (call/cc (lambda (x) (* 10 (x 3)))))"
            (fn ()
              (test-repl
               '(roselisp
                 > (+ 5
                      (call/cc
                       (lambda (x)
                         (* 10 (x 3)))))
                 8))))
        (it "(+ 5 (call/cc (lambda (x) (x 10) 3)))"
            (fn ()
              (test-repl
               '(roselisp
                 > (+ 5 (call/cc
                         (lambda (x)
                           (x 10)
                           3)))
                 15))))
        (it "(+ 5 (call/cc (lambda (x) (x 10) (error \"error\"))))"
            (fn ()
              (test-repl
               '(roselisp
                 > (+ 5
                      (call/cc
                       (lambda (x)
                         (x 10)
                         (error "error"))))
                 15))))
        (it "(try ... (+ 5 (call/cc (lambda (x) (error \"error\")))) ...)"
            (fn ()
              (define result 0)
              (try
                (set! result
                      (+ 5 (call/cc
                            (lambda (x)
                              (error "error")))))
                (catch Object e))
              (assert-equal result 0)))))
    (describe "string-split"
      (fn ()
        (it "(string-split \"foo bar  baz\")"
            (fn ()
              (test-repl
               '(roselisp
                 > (string-split "foo bar  baz")
                 '("foo" "bar" "baz")))))
        (it "(string-split \"foo,bar,baz\" \",\")"
            (fn ()
              (test-repl
               '(roselisp
                 > (string-split "foo,bar,baz" ",")
                 '("foo" "bar" "baz")))))
        (it "(string-split \"foo, bar, baz\" \", \")"
            (fn ()
              (test-repl
               '(roselisp
                 > (string-split "foo, bar, baz" ", ")
                 '("foo" "bar" "baz")))))
        (it "(string-split \"foo\\nbar\\nbaz\" \"\\n\")"
            (fn ()
              (test-repl
               '(roselisp
                 > (string-split "foo\nbar\nbaz" "\n")
                 '("foo" "bar" "baz")))))
        (xit "(string-split \"  foo bar  baz \\r\\n\\t\")"
             (fn ()
               (test-repl
                '(roselisp
                  > (string-split "  foo bar  baz \r\n\t")
                  '("foo" "bar" "baz")))))))
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
                 undefined
                 > ((Y (lambda (f)
                         (lambda (x)
                           (if (zero? x)
                               1
                               (* x (f (- x 1)))))))
                    6)
                 720))))))
    (describe "cons dot"
      (fn ()
        (it "*cons-dot*"
            (fn ()
              (test-repl
               '(roselisp
                 > *cons-dot*
                 '.))))
        (it "cons-dot"
            (fn ()
              (test-repl
               '(roselisp
                 > (cons-dot)
                 '.))))
        (it "cons-dot?"
            (fn ()
              (test-repl
               '(roselisp
                 > (cons-dot? *cons-dot*)
                 #t))))))
    (describe "define-class"
      (fn ()
        (it "(define-class Foo () ...)"
            (fn ()
              (test-repl
               '(roselisp
                 > (define-class Foo ()
                     (define/public (bar)
                       "bar"))
                 undefined
                 > (define foo
                     (new Foo))
                 undefined
                 > (send foo bar)
                 "bar"))))))
    (describe "defclass"
      (fn ()
        (it "(defclass Foo () ...)"
            (fn ()
              (test-repl
               '(roselisp
                 > (defclass Foo ()
                     (define/public (bar)
                       "bar"))
                 undefined
                 > (define foo
                     (new Foo))
                 undefined
                 > (send foo bar)
                 "bar"))))))
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
               (js-obj "compile" #f))))))
    (describe "compiled environment"
      (fn ()
        (it "compiledEnv"
            (fn ()
              (define options
                (js-obj))
              (compile 'foo undefined options)
              (define compiled-env
                (oget options "compiledEnv"))
              (assert-equal
               (instance-of? compiled-env LispEnvironment)
               #t)))))
    (describe "continuation environment"
      (fn ()
        (it "EnvironmentStack"
            (fn ()
              (define options
                (js-obj))
              (compile 'foo
                       undefined
                       options)
              (define continuation-env
                (oget options "continuationEnv"))
              (assert-equal
               (instance-of? continuation-env EnvironmentStack)
               #t)))
        (it "has"
            (fn ()
              (define options
                (js-obj))
              (compile '(define foo 1)
                       undefined
                       options)
              (define continuation-env
                (oget options "continuationEnv"))
              (assert-equal
               (send continuation-env has 'foo)
               #t)))))
    (describe "dotted lists"
      (fn ()
        (it "equal?"
            (fn ()
              (test-repl
               '(roselisp
                 > (equal? '(1 2) '(1 . (2 . ())))
                 #t)
               (js-obj "compile" #f))))))
    (describe "empty list"
      (fn ()
        (it "cons?"
            (fn ()
              (test-repl
               '(roselisp
                 > (cons? '())
                 #f)
               (js-obj "compile" #f))))
        (it "list?"
            (fn ()
              (test-repl
               '(roselisp
                 > (list? '())
                 #t)
               (js-obj "compile" #f))))))
    (describe "list?"
      (fn ()
        (it "empty list"
            (fn ()
              (test-repl
               '(roselisp
                 > (list? '())
                 #t)
               (js-obj "compile" #f))))
        (it "dotted pair"
            (fn ()
              (test-repl
               '(roselisp
                 > (list? '(1 . 2))
                 #f)
               (js-obj "compile" #f))))
        (it "dotted list"
            (fn ()
              (test-repl
               '(roselisp
                 > (list? '(1 2 . 3))
                 #f)
               (js-obj "compile" #f))))
        (it "dotted list '(1 . ())"
            (fn ()
              (test-repl
               '(roselisp
                 > (list? '(1 . ()))
                 #t)
               (js-obj "compile" #f))))
        (it "dotted list '(1 . (2 . ()))"
            (fn ()
              (test-repl
               '(roselisp
                 > (list? '(1 . (2 . ())))
                 #t)
               (js-obj "compile" #f))))))
    (describe "vector?"
      (fn ()
        (it "empty list"
            (fn ()
              (test-repl
               '(roselisp
                 > (vector? '())
                 #t)
               (js-obj "compile" #f))))
        (it "dotted pair"
            (fn ()
              (test-repl
               '(roselisp
                 > (vector? '(1 . 2))
                 #t)
               (js-obj "compile" #f))))
        (it "dotted list"
            (fn ()
              (test-repl
               '(roselisp
                 > (vector? '(1 2 . 3))
                 #t)
               (js-obj "compile" #f))))
        (it "dotted list '(1 . ())"
            (fn ()
              (test-repl
               '(roselisp
                 > (vector? '(1 . ()))
                 #t)
               (js-obj "compile" #f))))
        (it "dotted list '(1 . (2 . ()))"
            (fn ()
              (test-repl
               '(roselisp
                 > (vector? '(1 . (2 . ())))
                 #t)
               (js-obj "compile" #f))))))
    (describe "dotted-list-length"
      (fn ()
        (it "()"
            (fn ()
              (test-repl
               '(roselisp
                 > (dotted-list-length '())
                 0)
               (js-obj "compile" #f))))
        (it "'(1 . ())"
            (fn ()
              (test-repl
               '(roselisp
                 > (dotted-list-length '(1 . ()))
                 1)
               (js-obj "compile" #f))))
        (it "'(1 . (2 . ()))"
            (fn ()
              (test-repl
               '(roselisp
                 > (dotted-list-length '(1 . (2 . ())))
                 2)
               (js-obj "compile" #f))))))
    (describe "dotted-list-last"
      (fn ()
        (it "()"
            (fn ()
              (test-repl
               '(roselisp
                 > (dotted-list-last '())
                 undefined)
               (js-obj "compile" #f))))
        (it "'(1 . ())"
            (fn ()
              (test-repl
               '(roselisp
                 > (dotted-list-last '(1 . ()))
                 1)
               (js-obj "compile" #f))))
        (it "'(1 . (2 . ()))"
            (fn ()
              (test-repl
               '(roselisp
                 > (dotted-list-last '(1 . (2 . ())))
                 2)
               (js-obj "compile" #f))))))
    (describe "dotted-list-last-cdr"
      (fn ()
        (it "()"
            (fn ()
              (test-repl
               '(roselisp
                 > (dotted-list-last-cdr '())
                 '())
               (js-obj "compile" #f))))
        (it "'(1 . ())"
            (fn ()
              (test-repl
               '(roselisp
                 > (dotted-list-last-cdr '(1 . ()))
                 '())
               (js-obj "compile" #f))))
        (it "'(1 . (2 . ()))"
            (fn ()
              (test-repl
               '(roselisp
                 > (dotted-list-last-cdr '(1 . (2 . ())))
                 '())
               (js-obj "compile" #f))))))
    (describe "nth"
      (fn ()
        (it "(nth 1 '(1 . (2 . ())))"
            (fn ()
              (test-repl
               '(roselisp
                 > (nth 1 '(1 . (2 . ())))
                 2)
               (js-obj "compile" #f))))
        (it "(nth 1 '(1 . (2 . ())))"
            (fn ()
              (test-repl
               '(roselisp
                 > (nth 1 '(1 2 . (3 . ())))
                 2)
               (js-obj "compile" #f))))))
    (describe "cdr"
      (fn ()
        (it "(cdr '(1 . (2 . ())))"
            (fn ()
              (test-repl
               '(roselisp
                 > (cdr '(1 . (2 . ())))
                 '(2 . ()))
               (js-obj "compile" #f))))
        (it "(cdr '(1 . (2 . ())))"
            (fn ()
              (test-repl
               '(roselisp
                 > (cdr '(1 2 . (3 . ())))
                 '(2 . (3 . ())))
               (js-obj "compile" #f))))))
    (describe "array-list?"
      (fn ()
        (it "(array-list? '())"
            (fn ()
              (test-repl
               '(roselisp
                 > (array-list? '())
                 #t)
               (js-obj "compile" #f))))
        (it "(array-list? '(1 . 2))"
            (fn ()
              (test-repl
               '(roselisp
                 > (array-list? '(1 . 2))
                 #t)
               (js-obj "compile" #f))))
        (it "(array-list? '(1 2))"
            (fn ()
              (test-repl
               '(roselisp
                 > (array-list? '(1 2))
                 #t)
               (js-obj "compile" #f))))
        (it "(array-list? '(1 2 3))"
            (fn ()
              (test-repl
               '(roselisp
                 > (array-list? '(1 2 3))
                 #t)
               (js-obj "compile" #f))))))
    (describe "linked-list?"
      (fn ()
        (it "(linked-list? '())"
            (fn ()
              (test-repl
               '(roselisp
                 > (linked-list? '())
                 #f)
               (js-obj "compile" #f))))
        (it "(linked-list? '(1 . 2))"
            (fn ()
              (test-repl
               '(roselisp
                 > (linked-list? '(1 . 2))
                 #f)
               (js-obj "compile" #f))))
        (it "(linked-list? '(1 . ()))"
            (fn ()
              (test-repl
               '(roselisp
                 > (linked-list? '(1 . ()))
                 #t)
               (js-obj "compile" #f))))
        (it "(linked-list? '(1 . (2 . ())))"
            (fn ()
              (test-repl
               '(roselisp
                 > (linked-list? '(1 . (2 . ())))
                 #t)
               (js-obj "compile" #f))))
        (it "(linked-list? '(1 2 . (3 . ())))"
            (fn ()
              (test-repl
               '(roselisp
                 > (linked-list? '(1 2 . (3 . ())))
                 #t)
               (js-obj "compile" #f))))))
    (describe "linked-list-link?"
      (fn ()
        (it "(linked-list-link? '())"
            (fn ()
              (test-repl
               '(roselisp
                 > (linked-list-link? '())
                 #f)
               (js-obj "compile" #f))))
        (it "(linked-list-link? '(1 . 2))"
            (fn ()
              (test-repl
               '(roselisp
                 > (linked-list-link? '(1 . 2))
                 #t)
               (js-obj "compile" #f))))
        (it "(linked-list-link? '(1 . ()))"
            (fn ()
              (test-repl
               '(roselisp
                 > (linked-list-link? '(1 . ()))
                 #t)
               (js-obj "compile" #f))))
        (it "(linked-list-link? '(1 . (2 . ())))"
            (fn ()
              (test-repl
               '(roselisp
                 > (linked-list-link? '(1 . (2 . ())))
                 #t)
               (js-obj "compile" #f))))
        (it "(linked-list-link? '(1 2 . (3 . ())))"
            (fn ()
              (test-repl
               '(roselisp
                 > (linked-list-link? '(1 2 . (3 . ())))
                 #t)
               (js-obj "compile" #f))))))
    (describe "dotted-list?"
      (fn ()
        (it "(dotted-list? '())"
            (fn ()
              (test-repl
               '(roselisp
                 > (dotted-list? '())
                 #f)
               (js-obj "compile" #f))))
        (it "(dotted-list? '(1 . 2))"
            (fn ()
              (test-repl
               '(roselisp
                 > (dotted-list? '(1 . 2))
                 #t)
               (js-obj "compile" #f))))
        (it "(dotted-list? '(1 . ()))"
            (fn ()
              (test-repl
               '(roselisp
                 > (dotted-list? '(1 . ()))
                 #f)
               (js-obj "compile" #f))))
        (it "(dotted-list? '(1 . (2 . ())))"
            (fn ()
              (test-repl
               '(roselisp
                 > (dotted-list? '(1 . (2 . ())))
                 #f)
               (js-obj "compile" #f))))
        (it "(dotted-list? '(1 . (2 . 3)))"
            (fn ()
              (test-repl
               '(roselisp
                 > (dotted-list? '(1 . (2 . 3)))
                 #t)
               (js-obj "compile" #f))))))
    (describe "length"
      (fn ()
        (it "(length '(1 . ()))"
            (fn ()
              (test-repl
               '(roselisp
                 > (length '(1 . ()))
                 1)
               (js-obj "compile" #f))))
        (it "(length '(1 . (2 . ())))"
            (fn ()
              (test-repl
               '(roselisp
                 > (length '(1 . (2 . ())))
                 2)
               (js-obj "compile" #f))))
        (it "(length '(1 2 . ()))"
            (fn ()
              (test-repl
               '(roselisp
                 > (length '(1 2 . ()))
                 2)
               (js-obj "compile" #f))))))
    (describe "last"
      (fn ()
        (it "(last '(1 . ()))"
            (fn ()
              (test-repl
               '(roselisp
                 > (last '(1 . ()))
                 1)
               (js-obj "compile" #f))))
        (it "(last '(1 . (2 . ())))"
            (fn ()
              (test-repl
               '(roselisp
                 > (last '(1 . (2 . ())))
                 2)
               (js-obj "compile" #f))))
        (it "(last '(1 2 . ()))"
            (fn ()
              (test-repl
               '(roselisp
                 > (last '(1 2 . ()))
                 2)
               (js-obj "compile" #f))))))
    (describe "js-keys"
      (fn ()
        (it "(js-keys (js-obj))"
            (fn ()
              (test-repl
               '(roselisp
                 > (js-keys (js-obj))
                 '())
               (js-obj "compile" #f))))
        (it "(js-keys (js-obj \"foo\" \"bar\"))"
            (fn ()
              (test-repl
               '(roselisp
                 > (js-keys (js-obj "foo" "bar"))
                 '("foo"))
               (js-obj "compile" #f))))
        (it "(js-keys (js-obj \"foo\" \"bar\" \"baz\" \"quux\"))"
            (fn ()
              (test-repl
               '(roselisp
                 > (js-keys (js-obj "foo" "bar"
                                    "baz" "quux"))
                 '("foo" "baz"))
               (js-obj "compile" #f))))))
    (describe "string-join"
      (fn ()
        (it "(string-join '(\"foo\" \"bar\"))"
            (fn ()
              (test-repl
               '(roselisp
                 > (string-join '("foo" "bar"))
                 "foo bar")
               (js-obj "compile" #f))))
        (it "(string-join '(\"foo\" \"bar\"))"
            (fn ()
              (test-repl
               '(roselisp
                 > (string-join '("foo" "bar") ",")
                 "foo,bar")
               (js-obj "compile" #f))))))
    (describe "string-upcase"
      (fn ()
        (it "(string-upcase \"foo\")"
            (fn ()
              (test-repl
               '(roselisp
                 > (string-upcase "foo")
                 "FOO")
               (js-obj "compile" #f))))))
    (describe "string-downcase"
      (fn ()
        (it "(string-downcase \"FOO\")"
            (fn ()
              (test-repl
               '(roselisp
                 > (string-downcase "FOO")
                 "foo")
               (js-obj "compile" #f))))))
    (describe "plist->alist"
      (fn ()
        (it "(plist->alist '())"
            (fn ()
              (test-repl
               '(roselisp
                 > (plist->alist '())
                 '()))))
        (it "(plist->alist '(foo bar))"
            (fn ()
              (test-repl
               '(roselisp
                 > (plist->alist '(foo bar))
                 '((foo . bar))))))
        (it "(plist->alist '(foo bar baz quux))"
            (fn ()
              (test-repl
               '(roselisp
                 > (plist->alist '(foo bar baz quux))
                 '((foo . bar) (baz . quux))))))))
    (describe "js/try"
      (fn ()
        (it "compile (js/try ... (catch ...) (finally ...))"
            (fn ()
              (assert-equal
               (compile
                '(js/try
                  (set! x (/ 2 1))
                  (catch e
                      (display "there was an error"))
                  (finally
                    (display "cleanup")))
                compilation-environment
                (js-obj "language" "JavaScript"))
               "try {
  x = 2 / 1;
} catch (e) {
  console.log('there was an error');
} finally {
  console.log('cleanup');
}")))
        (it "(js/try (/ 1 2) ...)"
            (fn ()
              (test-repl
               '(roselisp
                 > (js/try
                    (/ 1 2)
                    (catch e
                        (display "there was an error"))
                    (finally
                      (display "finally")))
                 0.5))))
        (it "(js/try (/ 1 3) (/ 1 2) ...)"
            (fn ()
              (test-repl
               '(roselisp
                 > (js/try
                    (/ 1 3)
                    (/ 1 2)
                    (catch e
                        (display "there was an error"))
                    (finally
                      (display "finally")))
                 0.5))))))
    (describe "instance-of?"
      (fn ()
        (it "(instance-of? (new Map) Map)"
            (fn ()
              (test-repl
               '(roselisp
                 > (instance-of? (new Map) Map)
                 #t))))))
    (describe "is-a?"
      (fn ()
        (it "(is-a? (new Map) Map)"
            (fn ()
              (test-repl
               '(roselisp
                 > (is-a? (new Map) Map)
                 #t))))))))
