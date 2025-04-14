(require chai "chai")
(require (only-in "../../src/ts/language"
                  lisp-environment
                  map-rose))
(require (only-in "./test-util"
                  assert-equal))

(describe "map-rose"
  (fn ()
    (describe "empty list"
      (fn ()
        (it "()"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '()
                        lisp-environment)
              (assert-equal
               expressions
               '(()))))))
    (describe "function application"
      (fn ()
        (it "(f x)"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(f x)
                        lisp-environment)
              (assert-equal
               expressions
               '(f
                 x
                 (f x)))))
        (it "(f (g x) y)"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(f (g x) y)
                        lisp-environment)
              (assert-equal
               expressions
               '(f
                 g
                 x
                 (g x)
                 y
                 (f (g x) y)))))))
    (describe "begin"
      (fn ()
        (it "(begin x y)"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(begin
                           x
                           y)
                        lisp-environment)
              (assert-equal
               expressions
               '(x
                 y
                 (begin
                   x
                   y)))))))
    (describe "begin0"
      (fn ()
        (it "(begin0 x y)"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(begin0 x
                           y)
                        lisp-environment)
              (assert-equal
               expressions
               '(x
                 y
                 (begin0 x
                   y)))))))
    (describe "let"
      (fn ()
        (it "(let ((x 1)) x)"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(let ((x 1))
                           x)
                        lisp-environment)
              (assert-equal
               expressions
               '(x
                 1
                 x
                 (let ((x 1))
                   x)))))))
    (describe "let-values"
      (fn ()
        (it "(let-values (((x) (foo))) x)"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(let-values (((x) (foo)))
                           x)
                        lisp-environment)
              (assert-equal
               expressions
               '(x
                 foo
                 (foo)
                 x
                 (let-values (((x) (foo)))
                   x)))))))
    (describe "cond"
      (fn ()
        (it "(cond ((foo bar) (baz quux)))"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(cond
                          ((foo bar)
                           (baz quux)))
                        lisp-environment)
              (assert-equal
               expressions
               '(foo
                 bar
                 (foo bar)
                 baz
                 quux
                 (baz quux)
                 (cond
                  ((foo bar)
                   (baz quux)))))))))
    (describe "cond"
      (fn ()
        (it "(cond ((foo bar) (baz quux)))"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(cond
                          ((foo bar)
                           (baz quux)))
                        lisp-environment)
              (assert-equal
               expressions
               '(foo
                 bar
                 (foo bar)
                 baz
                 quux
                 (baz quux)
                 (cond
                  ((foo bar)
                   (baz quux)))))))))
    (describe "lambda"
      (fn ()
        (it "(lambda (x) x)"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(lambda (x)
                           x)
                        lisp-environment)
              (assert-equal
               expressions
               '(x
                 x
                 (lambda (x)
                   x)))))
        (it "(lambda (x (y 1)) x)"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(lambda (x (y 1))
                           x)
                        lisp-environment)
              (assert-equal
               expressions
               '(x
                 y
                 1
                 x
                 (lambda (x (y 1))
                   x)))))))
    (describe "define"
      (fn ()
        (it "(define (I x) x)"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(define (I x)
                           x)
                        lisp-environment)
              (assert-equal
               expressions
               '(I
                 x
                 x
                 (define (I x)
                   x)))))
        (it "(define I (lambda (x) x))"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(define I
                           (lambda (x)
                             x))
                        lisp-environment)
              (assert-equal
               expressions
               '(I
                 x
                 x
                 (lambda (x)
                   x)
                 (define I
                   (lambda (x)
                     x))))))))
    (describe "quasiquote"
      (fn ()
        (it "(quasiquote x)"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(quasiquote x)
                        lisp-environment)
              (assert-equal
               expressions
               '((quasiquote x)))))
        (it "(quasiquote (x))"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(quasiquote (x))
                        lisp-environment)
              (assert-equal
               expressions
               '((quasiquote (x))))))
        (it "(quasiquote (unquote x))"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(quasiquote (unquote x))
                        lisp-environment)
              (assert-equal
               expressions
               '(x
                 (quasiquote (unquote x))))))
        (it "(quasiquote (x (unquote y) (unquote-splicing z)))"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(quasiquote
                          (x
                           (unquote y)
                           (unquote-splicing z)))
                        lisp-environment)
              (assert-equal
               expressions
               '(y
                 z
                 (quasiquote
                  (x
                   (unquote y)
                   (unquote-splicing z)))))))))
    (describe "defmacro"
      (fn ()
        (it "(defmacro f (x) x)"
            (fn ()
              (define expressions
                (quote ()))
              (map-rose (fn (x)
                          (push-right! expressions x)
                          x)
                        '(defmacro f (x)
                           x)
                        lisp-environment)
              (assert-equal
               expressions
               '(f
                 x
                 x
                 (defmacro f (x)
                   x)))))))))
