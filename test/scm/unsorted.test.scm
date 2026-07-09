;;; # Unsorted tests
;;;
;;; Tests that have not been sorted yet.

(require (only-in "../../src/ts/env"
                  EnvironmentStack
                  LispEnvironment))
(require (only-in "../../src/ts/language"
                  compile
                  compilation-environment))
(require (only-in "./test-util"
                  assert-equal
                  test-repl))

;;; Test inbox
(describe "Unsorted tests"
  (fn ()
    (describe "call/cc"
      (fn ()
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
                 #u
                 > ((Y (lambda (f)
                         (lambda (x)
                           (if (zero? x)
                               1
                               (* x (f (- x 1)))))))
                    6)
                 720))))))
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
              (compile 'foo #u options)
              (define compiled-env
                (oget options "compiledEnv"))
              (assert-equal
               (instance-of? compiled-env LispEnvironment)
               #t)))))
    (describe "continuation environment"
      (fn ()
        (it "has"
            (fn ()
              (define options
                (js-obj))
              (compile '(define foo 1)
                       #u
                       options)
              (define continuation-env
                (oget options "continuationEnv"))
              (assert-equal
               (send continuation-env has 'foo)
               #t)))
        (xit "EnvironmentStack"
             (fn ()
               (define options
                 (js-obj))
               (compile 'foo
                        #u
                        options)
               (define continuation-env
                 (oget options "continuationEnv"))
               (assert-equal
                (instance-of? continuation-env EnvironmentStack)
                #t)))))
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
}")))))))
