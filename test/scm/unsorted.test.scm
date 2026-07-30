;;; # Various unsorted tests
;;;
;;; This file functions as an "inbox" for incoming tests.

(require (only-in "./test-util"
                  assert-equal
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 :repl #t

 > (describe "To do")
 _
 xit> (compile '(js/< x y))
 "x < y;"
 xit> (compile '(js/> x y))
 "x > y;"
 xit> (compile '(js/% x y))
 "x % y;"
 xit> (compile '(js/and x y))
 "x and y;"
 xit> (compile '(js/or x y))
 "x or y;"
 xit> (compile '(js/! x))
 "!x;"

 > (describe "Bitwise operators")
 _
 xit> (compile '(bitwise-and x y))
 "x & y;"
 xit> (compile '(bit-and x y))
 "x & y;"
 xit> (compile '(js/& x y))
 "x & y;"
 xit> (compile '(bitwise-or x y))
 "x | y;"
 xit> (compile '(bit-or x y))
 "x | y;"
 xit> (compile '(js/| x y))
 "x | y;"
 xit> (compile '(bitwise-xor x y))
 "x ^ y;"
 xit> (compile '(bit-xor x y))
 "x ^ y;"
 xit> (compile '(js/^ x y))
 "x ^ y;"
 xit> (compile '(bitwise-negation x))
 "~x;"
 xit> (compile '(bitwise-not x))
 "~x;"
 xit> (compile '(bit-not x))
 "~x;"
 xit> (compile '(js/~ x))
 "~x;"
 xit> (compile '(bitwise-shift-left x y))
 "x << y;"
 xit> (compile '(bit-shift-left x y))
 "x << y;"
 xit> (compile '(js/<< x y))
 "x << y;"
 xit> (compile '(bitwise-shift-right x y))
 "x >> y;"
 xit> (compile '(bit-shift-right x y))
 "x >> y;"
 xit> (compile '(js/>> x y))
 "x >> y;"
 xit> (compile '(unsigned-bit-shift-right x y))
 "x >> y;"
 xit> (compile '(js/>>> x y))
 "x >>> y;"
 xit> (compile '(js/<<= x y))
 "x <<= y;"
 xit> (compile '(js/>>= x y))
 "x >>= y;"
 xit> (compile '(js/>>>= x y))
 "x >>>= y;"
 xit> (compile '(js/&= x y))
 "x &= y;"
 xit> (compile '(js/|= x y))
 "x |= y;"
 xit> (compile '(js/^= x y))
 "x ^= y;"
 xit> (compile '(js/= x y))
 "x = y;")
