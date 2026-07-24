(require (only-in "../../src/ts/trampoline"
                  tcall
                  trampoline))
(require (only-in "./test-util"
                  assert-equal
                  test-macro))

(declare-macro test-macro)

(test-macro
 ;; `trampoline`
 > (describe "trampoline")
 _
 > (trampoline (fn (x) x) 1)
 1
 > (define (add x y)
     (+ x y))
 _
 > (define (fibonacci n)
     (if (< n 2)
         n
         (tcall add
                (tcall fibonacci (- n 1))
                (tcall fibonacci (- n 2)))))
 _
 > (trampoline fibonacci 0)
 0
 > (trampoline fibonacci 1)
 1
 > (trampoline fibonacci 2)
 1
 > (trampoline fibonacci 3)
 2
 > (trampoline fibonacci 4)
 3
 > (trampoline fibonacci 5)
 5
 > (trampoline fibonacci 6)
 8
 > (trampoline fibonacci 7)
 13
 > (trampoline fibonacci 8)
 21
 > (trampoline fibonacci 9)
 34
 > (trampoline fibonacci 10)
 55
 > (define (sub x y)
     (- x y))
 _
 > (define (sequence n)
     (if (< n 2)
         n
         (tcall sub
                (tcall sequence (- n 1))
                (tcall sequence (- n 2)))))
 _
 > (trampoline sequence 0)
 0
 > (trampoline sequence 1)
 1
 > (trampoline sequence 2)
 1
 > (trampoline sequence 3)
 0
 > (trampoline sequence 4)
 -1
 > (trampoline sequence 5)
 -1
 > (trampoline sequence 6)
 0
 > (trampoline sequence 7)
 1
 > (trampoline sequence 8)
 1
 > (trampoline sequence 9)
 0
 > (trampoline sequence 10)
 -1)
