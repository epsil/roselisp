(require (only-in "../../src/ts/combinators"
                  __
                  curried
                  variadic))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; `curried`
 > (describe "curried")
 _
 > (define-fields (C Y)
     curried)
 _
 > (define (subtraction x y)
     (- x y))
 _
 > (subtraction 1 2)
 -1
 > (C subtraction 1 2)
 1
 > ((C subtraction) 1 2)
 1
 > (((C subtraction) 1) 2)
 1
 > (define (factorial x)
     (if (eq? x 0)
         1
         (* x (factorial (- x 1)))))
 _
 > (define factorialY
     (Y (fn (factorial)
          (fn (x)
            (if (eq? x 0)
                1
                (* x (factorial (- x 1))))))))
 _
 > (factorial 6)
 (* 1 2 3 4 5 6)
 > (factorialY 6)
 (* 1 2 3 4 5 6)

 ;; `variadic`
 > (describe "variadic")
 _
 > (define-fields (A B I Q T)
     variadic)
 _
 > (A I 1)
 1
 > (A (fn (x) (+ x 4)) 1)
 5
 > (A (fn (x y) (+ x y)) 1 1)
 2
 > ((A __ 1 1)
    (fn (x y) (+ x y)))
 2
 > ((A (fn (x y) (+ x y))
       __
       1)
    1)
 2
 > ((A (fn (x y) (+ x y)) 1 __)
    1)
 2
 > (eq? (B) #u)
 #t
 > (B 1)
 1
 > (B I 1)
 1
 > (B I I 1)
 1
 > (B I I I 1)
 1
 > (B (fn (x) (- x))
      (fn (x) (+ x 4))
      5)
 -9
 > ((B (fn (x) (- x))
       (fn (x) (+ x 4))
       __)
    5)
 -9
 > (eq? (I) #u)
 #t
 > (I I)
 I
 > (I 1)
 1
 > (I I I 1)
 I
 > (I I I I 1)
 I
 > (I I I I I 1)
 I
 > ((I __) 1)
 1
 > ((I __) I)
 I
 > (eq? (Q) #u)
 #t
 > (Q 1)
 1
 > (Q I 1)
 1
 > (Q I I 1)
 1
 > (Q I I I 1)
 1
 > (Q (fn (x) (+ x 4))
      (fn (x) (- x))
      5)
 -9
 > ((Q (fn (x) (+ x 4))
       (fn (x) (- x))
       __)
    5)
 -9
 > (eq? (T) #u)
 #t
 > (T 1)
 1
 > (T 1 I)
 1
 > (T 1 I I)
 1
 > (T 1 I I I)
 1
 > (T 5
      (fn (x) (+ x 4))
      (fn (x) (- x)))
 -9
 > ((T __
       (fn (x) (+ x 4))
       (fn (x) (- x)))
    5)
 -9)
