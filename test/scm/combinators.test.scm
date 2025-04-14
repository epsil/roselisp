(require chai "chai")
(require (only-in "../../src/ts/combinators"
                  __
                  curried
                  variadic))
(require (only-in "./test-util"
                  assert-equal))

(describe "curried"
  (fn ()
    (define-js-obj (C Y)
      curried)
    (describe "C"
      (fn ()
        (it "C (-) 1 2"
            (fn ()
              (define (subtraction x y)
                (- x y))
              (assert-equal
               (subtraction 1 2)
               -1)
              (assert-equal
               (C subtraction 1 2)
               1)
              (assert-equal
               ((C subtraction) 1 2)
               1)
              (assert-equal
               (((C subtraction) 1) 2)
               1)))))
    (describe "Y"
      (fn ()
        (it "6!"
            (fn ()
              (define (factorial x)
                (if (eq? x 0)
                    1
                    (* x (factorial (- x 1)))))
              (define factorialY
                (Y (fn (factorial)
                     (fn (x)
                       (if (eq? x 0)
                           1
                           (* x (factorial (- x 1))))))))
              (assert-equal
               (factorial 6)
               (* 1 2 3 4 5 6))
              (assert-equal
               (factorialY 6)
               (* 1 2 3 4 5 6))))))))

(describe "variadic"
  (fn ()
    (define-js-obj (A B I Q T)
      variadic)
    (describe "A"
      (fn ()
        (it "A I 1"
            (fn ()
              (assert-equal
               (A I 1)
               1)))
        (it "A (λx. x + 4) 1"
            (fn ()
              (assert-equal
               (A (fn (x) (+ x 4)) 1)
               5)))
        (it "A (+) 1 1"
            (fn ()
              (assert-equal
               (A (fn (x y) (+ x y)) 1 1)
               2)))
        (it "((A _ 1 1) +)"
            (fn ()
              (assert-equal
               ((A __ 1 1)
                (fn (x y) (+ x y)))
               2)))
        (it "((A + _ 1) 1)"
            (fn ()
              (assert-equal
               ((A (fn (x y) (+ x y))
                   __
                   1)
                1)
               2)))
        (it "((A + 1 _) 1)"
            (fn ()
              (assert-equal
               ((A (fn (x y) (+ x y)) 1 __)
                1)
               2)))))
    (describe "B"
      (fn ()
        (it "B"
            (fn ()
              (assert-equal
               (eq? (B) undefined)
               #t)))
        (it "B 1"
            (fn ()
              (assert-equal
               (B 1)
               1)))
        (it "B I 1"
            (fn ()
              (assert-equal
               (B I 1)
               1)))
        (it "B I I 1"
            (fn ()
              (assert-equal
               (B I I 1)
               1)))
        (it "B I I I 1"
            (fn ()
              (assert-equal
               (B I I I 1)
               1)))
        (it "B (λx. -x) (λx. x + 4) 5"
            (fn ()
              (assert-equal
               (B (fn (x) (- x))
                  (fn (x) (+ x 4))
                  5)
               -9)))
        (it "((B (λx. -x) (λx. x + 4) _) 5)"
            (fn ()
              (assert-equal
               ((B (fn (x) (- x))
                   (fn (x) (+ x 4))
                   __)
                5)
               -9)))))
    (describe "I"
      (fn ()
        (it "I"
            (fn ()
              (assert-equal
               (eq? (I) undefined)
               #t)))
        (it "I I"
            (fn ()
              (assert-equal
               (I I)
               I)))
        (it "I 1"
            (fn ()
              (assert-equal
               (I 1)
               1)))
        (it "I I 1"
            (fn ()
              (assert-equal
               (I I I 1)
               I)))
        (it "I I I 1"
            (fn ()
              (assert-equal
               (I I I I 1)
               I)))
        (it "I I I I 1"
            (fn ()
              (assert-equal
               (I I I I I 1)
               I)))
        (it "(I _) 1"
            (fn ()
              (assert-equal
               ((I __) 1)
               1)))
        (it "(I _) I"
            (fn ()
              (assert-equal
               ((I __) I)
               I)))))
    (describe "Q"
      (fn ()
        (it "Q"
            (fn ()
              (assert-equal
               (eq? (Q) undefined)
               #t)))
        (it "Q 1"
            (fn ()
              (assert-equal
               (Q 1)
               1)))
        (it "Q I 1"
            (fn ()
              (assert-equal
               (Q I 1)
               1)))
        (it "Q I I 1"
            (fn ()
              (assert-equal
               (Q I I 1)
               1)))
        (it "Q I I I 1"
            (fn ()
              (assert-equal
               (Q I I I 1)
               1)))
        (it "Q (λx. x + 4) (λx. -x) 5"
            (fn ()
              (assert-equal
               (Q (fn (x) (+ x 4))
                  (fn (x) (- x))
                  5)
               -9)))
        (it "((Q (λx. x + 4) (λx. -x) _) 5)"
            (fn ()
              (assert-equal
               ((Q (fn (x) (+ x 4))
                   (fn (x) (- x))
                   __)
                5)
               -9)))))
    (describe "T"
      (fn ()
        (it "T"
            (fn ()
              (assert-equal
               (eq? (T) undefined)
               #t)))
        (it "T 1"
            (fn ()
              (assert-equal
               (T 1)
               1)))
        (it "T 1 I"
            (fn ()
              (assert-equal
               (T 1 I)
               1)))
        (it "T 1 I I"
            (fn ()
              (assert-equal
               (T 1 I I)
               1)))
        (it "T 1 I I I"
            (fn ()
              (assert-equal
               (T 1 I I I)
               1)))
        (it "T 5 (λx. x + 4) (λx. -x)"
            (fn ()
              (assert-equal
               (T 5
                  (fn (x) (+ x 4))
                  (fn (x) (- x)))
               -9)))
        (it "((T _ (λx. x + 4) (λx. -x)) 5)"
            (fn ()
              (assert-equal
               ((T __
                   (fn (x) (+ x 4))
                   (fn (x) (- x)))
                5)
               -9)))))))
