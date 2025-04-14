(require chai "chai")
(require (only-in "../../src/ts/combinators"
                  I
                  K))
(require (only-in "../../src/ts/memo"
                  eof
                  memoize))
(require (only-in "./test-util"
                  assert-equal))

(describe "memoize"
  (fn ()
    (it "cache property"
        (fn ()
          (define memoized-f
            (memoize I))
          (assert-equal
           (instance-of? (get-field cache memoized-f)
                         Map)
           #t)))
    (it "cache I()"
        (fn ()
          (define memoized-f
            (memoize I))
          (assert-equal
           (eq? (memoized-f) undefined)
           #t)
          (assert-equal
           (get-field cache memoized-f)
           (new Map `((,eof ,undefined))))))
    (it "cache I(1)"
        (fn ()
          (define memoized-f
            (memoize I))
          (assert-equal
           (memoized-f 1)
           1)
          (assert-equal
           (get-field cache memoized-f)
           (new Map
                `((1 ,(new Map
                           `((,eof 1)))))))
          ;; Change cached value and verify that
          ;; the cached value is returned.
          (set-field! cache memoized-f
                      (new Map
                           `((1 ,(new Map
                                      `((,eof 500)))))))
          (assert-equal
           (memoized-f 1)
           500)))
    (it "cache K(1, 2)"
        (fn ()
          (define memoized-f
            (memoize K))
          (assert-equal
           (memoized-f 1 2)
           1)
          (assert-equal
           (get-field cache memoized-f)
           (new Map
                `((1 ,(new Map
                           `((2 ,(new Map
                                      `((,eof 1))))))))))))))
