(require (only-in "../../src/ts/language"
                  lisp-environment
                  map-rose))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; `map-rose`
 > (describe "map-rose")
 _
 > (it "()"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '()
                   lisp-environment)
         expressions))
 '(())
 > (it "(f x)"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(f x)
                   lisp-environment)
         expressions))
 '(f
   x
   (f x))
 > (it "(f (g x) y)"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(f (g x) y)
                   lisp-environment)
         expressions))
 '(f
   g
   x
   (g x)
   y
   (f (g x) y))
 > (it "(begin x y)"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(begin
                      x
                      y)
                   lisp-environment)
         expressions))
 '(x
   y
   (begin
     x
     y))
 > (it "(begin0 x y)"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(begin0 x
                      y)
                   lisp-environment)
         expressions))
 '(x
   y
   (begin0 x
     y))
 > (it "(let ((x 1)) x)"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(let ((x 1))
                      x)
                   lisp-environment)
         expressions))
 '(x
   1
   x
   (let ((x 1))
     x))
 > (it "(let-values (((x) (foo))) x)"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(let-values (((x) (foo)))
                      x)
                   lisp-environment)
         expressions))
 '(x
   foo
   (foo)
   x
   (let-values (((x) (foo)))
     x))
 > (it "(cond ((foo bar) (baz quux)))"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(cond
                     ((foo bar)
                      (baz quux)))
                   lisp-environment)
         expressions))
 '(foo
   bar
   (foo bar)
   baz
   quux
   (baz quux)
   (cond
    ((foo bar)
     (baz quux))))
 > (it "(cond ((foo bar) (baz quux)))"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(cond
                     ((foo bar)
                      (baz quux)))
                   lisp-environment)
         expressions))
 '(foo
   bar
   (foo bar)
   baz
   quux
   (baz quux)
   (cond
    ((foo bar)
     (baz quux))))
 > (it "(lambda (x) x)"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(lambda (x)
                      x)
                   lisp-environment)
         expressions))
 '(x
   x
   (lambda (x)
     x))
 > (it "(lambda (x (y 1)) x)"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(lambda (x (y 1))
                      x)
                   lisp-environment)
         expressions))
 '(x
   y
   1
   x
   (lambda (x (y 1))
     x))
 > (it "(define (I x) x)"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(define (I x)
                      x)
                   lisp-environment)
         expressions))
 '(I
   x
   x
   (define (I x)
     x))
 > (it "(define I (lambda (x) x))"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(define I
                      (lambda (x)
                        x))
                   lisp-environment)
         expressions))
 '(I
   x
   x
   (lambda (x)
     x)
   (define I
     (lambda (x)
       x)))
 > (it "(quasiquote x)"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(quasiquote x)
                   lisp-environment)
         expressions))
 '((quasiquote x))
 > (it "(quasiquote (x))"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(quasiquote (x))
                   lisp-environment)
         expressions))
 '((quasiquote (x)))
 > (it "(quasiquote (unquote x))"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(quasiquote (unquote x))
                   lisp-environment)
         expressions))
 '(x
   (quasiquote (unquote x)))
 > (it "(quasiquote (x (unquote y) (unquote-splicing z)))"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(quasiquote
                     (x
                      (unquote y)
                      (unquote-splicing z)))
                   lisp-environment)
         expressions))
 '(y
   z
   (quasiquote
    (x
     (unquote y)
     (unquote-splicing z))))
 > (it "(defmacro f (x) x)"
       (let ((expressions '()))
         (map-rose (fn (x)
                     (push-right! expressions x)
                     x)
                   '(defmacro f (x)
                      x)
                   lisp-environment)
         expressions))
 '(f
   x
   x
   (defmacro f (x)
     x)))
