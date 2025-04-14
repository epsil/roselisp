(require chai "chai")
(require (only-in "../../src/ts/trampoline"
                  tcall
                  trampoline))
(require (only-in "./test-util"
                  assert-equal))

(describe "trampoline"
  (fn ()
    (it "1"
        (fn ()
          (assert-equal
           (trampoline (fn (x) x) 1)
           1)))
    (describe "fibonacci"
      (fn ()
        (define (add x y)
          (+ x y))
        (define (fibonacci n)
          (if (< n 2)
              n
              (tcall add
                     (tcall fibonacci (- n 1))
                     (tcall fibonacci (- n 2)))))
        (it "0"
            (fn ()
              (assert-equal
               (trampoline fibonacci 0)
               0)))
        (it "1"
            (fn ()
              (assert-equal
               (trampoline fibonacci 1)
               1)))
        (it "2"
            (fn ()
              (assert-equal
               (trampoline fibonacci 2)
               1)))
        (it "3"
            (fn ()
              (assert-equal
               (trampoline fibonacci 3)
               2)))
        (it "4"
            (fn ()
              (assert-equal
               (trampoline fibonacci 4)
               3)))
        (it "5"
            (fn ()
              (assert-equal
               (trampoline fibonacci 5)
               5)))
        (it "6"
            (fn ()
              (assert-equal
               (trampoline fibonacci 6)
               8)))
        (it "7"
            (fn ()
              (assert-equal
               (trampoline fibonacci 7)
               13)))
        (it "8"
            (fn ()
              (assert-equal
               (trampoline fibonacci 8)
               21)))
        (it "9"
            (fn ()
              (assert-equal
               (trampoline fibonacci 9)
               34)))
        (it "10"
            (fn ()
              (assert-equal
               (trampoline fibonacci 10)
               55)))))
    (describe "sequence"
      (fn ()
        (define (sub x y)
          (- x y))
        (define (sequence n)
          (if (< n 2)
              n
              (tcall sub
                     (tcall sequence (- n 1))
                     (tcall sequence (- n 2)))))
        (it "0"
            (fn ()
              (assert-equal
               (trampoline sequence 0)
               0)))
        (it "1"
            (fn ()
              (assert-equal
               (trampoline sequence 1)
               1)))
        (it "2"
            (fn ()
              (assert-equal
               (trampoline sequence 2)
               1)))
        (it "3"
            (fn ()
              (assert-equal
               (trampoline sequence 3)
               0)))
        (it "4"
            (fn ()
              (assert-equal
               (trampoline sequence 4)
               -1)))
        (it "5"
            (fn ()
              (assert-equal
               (trampoline sequence 5)
               -1)))
        (it "6"
            (fn ()
              (assert-equal
               (trampoline sequence 6)
               0)))
        (it "7"
            (fn ()
              (assert-equal
               (trampoline sequence 7)
               1)))
        (it "8"
            (fn ()
              (assert-equal
               (trampoline sequence 8)
               1)))
        (it "9"
            (fn ()
              (assert-equal
               (trampoline sequence 9)
               0)))
        (it "10"
            (fn ()
              (assert-equal
               (trampoline sequence 10)
               -1)))))))
