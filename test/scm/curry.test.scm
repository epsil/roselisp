(require chai "chai")
(require (only-in "../../src/ts/curry"
                  __
                  curry))
(require (only-in "./test-util"
                  assert-equal
                  assert-not-equal))

(describe "curry"
  (fn ()
    (it "a"
        (fn ()
          (define (a b)
            (list b))
          (define curriedA
            (curry a))
          (assert-equal
           (curriedA 1)
           '(1))
          (assert-equal
           ((curriedA) 1)
           '(1))))
    (it "ab"
        (fn ()
          (define (ab a b)
            (list a b))
          (define curriedAB
            (curry ab))
          (assert-equal
           (curriedAB 1 2)
           '(1 2))
          (assert-equal
           ((curriedAB 1) 2)
           '(1 2))
          (assert-equal
           (((curriedAB) 1) 2)
           '(1 2))))
    (it "abc"
        (fn ()
          (define (abc a b c)
            (list a b c))
          (define curriedABC
            (curry abc))
          (assert-equal
           (curriedABC 1 2 3)
           '(1 2 3))
          (assert-equal
           ((curriedABC 1 2) 3)
           '(1 2 3))
          (assert-equal
           (((curriedABC 1) 2) 3)
           '(1 2 3))
          (assert-equal
           ((((curriedABC) 1) 2) 3)
           '(1 2 3))))
    (it "arity"
        (fn ()
          (define (abc a b c)
            (list a b c))
          (define curriedABC1
            (curry abc 1))
          (assert-equal
           (curriedABC1 1)
           (list 1 undefined undefined))
          (assert-equal
           (curriedABC1 1 2)
           (list 1 2 undefined))
          (assert-equal
           (curriedABC1 1 2 3)
           '(1 2 3))))
    (it "wildcards"
        (fn ()
          (define (abc a b c)
            (list a b c))
          (define curriedABC1
            (curry abc))
          (assert-equal
           ((curriedABC1 __ __ __) 1 2 3)
           '(1 2 3))))
    (it "__ !== '_"
        (fn ()
          (assert-not-equal
           __
           '_)))))
