(require chai "chai")
(require (only-in "../../src/ts/index"
                  add
                  div
                  license
                  mul
                  sub))
(require (only-in "./test-util"
                  assert-equal))

(describe "license"
  (fn ()
    (it "MPL-2.0"
        (fn ()
          (assert-equal
           license
           'MPL-2.0)))))

(describe "add"
  (fn ()
    (it "(add)"
        (fn ()
          (assert-equal
           (add)
           0)))
    (it "(add 1)"
        (fn ()
          (assert-equal
           (add 1)
           1)))
    (it "(add 1 2)"
        (fn ()
          (assert-equal
           (add 1 2)
           3)))
    (it "(add 1 2 4)"
        (fn ()
          (assert-equal
           (add 1 2 4)
           7)))))

(describe "sub"
  (fn ()
    (it "(sub)"
        (fn ()
          (assert-equal
           (sub)
           0)))
    (it "(sub 1)"
        (fn ()
          (assert-equal
           (sub 1)
           -1)))
    (it "(sub 1 2)"
        (fn ()
          (assert-equal
           (sub 1 2)
           -1)))
    (it "(sub 1 2 4)"
        (fn ()
          (assert-equal
           (sub 1 2 4)
           -5)))))

(describe "mul"
  (fn ()
    (it "(mul)"
        (fn ()
          (assert-equal
           (mul)
           1)))
    (it "(mul 1)"
        (fn ()
          (assert-equal
           (mul 1)
           1)))
    (it "(mul 1 2)"
        (fn ()
          (assert-equal
           (mul 1 2)
           2)))
    (it "(mul 1 2 4)"
        (fn ()
          (assert-equal
           (mul 1 2 4)
           8)))))

(describe "div"
  (fn ()
    (xit "(div)"
         (fn ()
           (assert-equal
            (div)
            undefined)))
    (it "(div 1)"
        (fn ()
          (assert-equal
           (div 1)
           1)))
    (it "(div 1 2)"
        (fn ()
          (assert-equal
           (div 1 2)
           0.5)))
    (it "(div 1 2 4)"
        (fn ()
          (assert-equal
           (div 1 2 4)
           0.125)))))
