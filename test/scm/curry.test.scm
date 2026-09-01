(require (only-in "../../src/ts/curry"
                  __
                  curry
                  dashify))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 :describe "curry"
 > (define (a x)
     (list x))
 > (define a-c
     (curry a))
 > (a-c 1)
 '(1)
 > ((a-c) 1)
 '(1)
 > (define (ab x y)
     (list x y))
 > (define ab-c
     (curry ab))
 > (ab-c 1 2)
 '(1 2)
 > ((ab-c 1) 2)
 '(1 2)
 > (((ab-c) 1) 2)
 '(1 2)
 > (define (abc x y z)
     (list x y z))
 > (define abc-c
     (curry abc))
 > (abc-c 1 2 3)
 '(1 2 3)
 > ((abc-c 1 2) 3)
 '(1 2 3)
 > (((abc-c 1) 2) 3)
 '(1 2 3)
 > ((((abc-c) 1) 2) 3)
 '(1 2 3)
 > (define abc-c1
     (curry abc 1))
 > (abc-c1 1)
 '(1 #u #u)
 > (abc-c1 1 2)
 '(1 2 #u)
 > (abc-c 1 2 3)
 '(1 2 3)
 > ((abc-c __ __ __) 1 2 3)
 '(1 2 3)
 > (not (eq? __ '_))
 #t

 :describe "dashify"
 > (define (I x)
     x)
 > ((dashify I) I)
 I
 > (((dashify I) __) I)
 I
 > (define (add x y)
     (+ x y))
 > ((dashify add) 1 2)
 3
 > (((dashify add) __ 2) 1)
 3
 > (define (sub x y)
     (- x y))
 > ((dashify sub) 1 2)
 -1
 > (((dashify sub) __ 2) 1)
 -1
 > ((((dashify sub) __ 2) __) 1)
 -1
 > ((((dashify sub) __ 2) __ __) 1)
 -1
 > (((((dashify sub) __ 2) __) __) 1)
 -1
 > (((dashify sub) 1 __) 2)
 -1)
