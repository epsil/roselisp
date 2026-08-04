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

 ;; `gensym`
 > (describe "gensym")
 _
 xit> (compile `(begin
                  (define x 1)
                  (define ,(gensym "x") 2)
                  (define x1 3)))
 "let x = 1;

let x2 = 2;

let x1 = 3;"
 xit> (compile `(begin
                  (define x 1)
                  (define ,(gensym "x") 2)
                  (define-values (x1)
                    (list 3))))
 "let x = 1;

let x2 = 2;

let [x1] = [3];"
 xit> (compile `(begin
                  (define x 1)
                  (define ,(gensym "x") 2)
                  (define ,(gensym "x") 3)
                  (define x1 4)
                  (define x2 5)))
 "let x = 1;

let x3 = 2;

let x4 = 3;

let x1 = 4;

let x2 = 5;"

 > (describe "Fundamental operators")
 _
 xit> (compile '(js/= x y))
 "x = y;"
 xit> (compile '(js/, x y))
 "x, y;"
 xit> (compile '(js/\; x y))
 "x;
y;"

 > (describe "Assignment operators")
 _
 xit> (compile '(js/+= x y))
 "x += y;"
 xit> (compile '(js/-= x y))
 "x -= y;"
 xit> (compile '(js/*= x y))
 "x *= y;"
 xit> (compile '(js//= x y))
 "x /= y;"
 xit> (compile '(js/^= x y))
 "x ^= y;"
 xit> (compile '(js/&= x y))
 "x &= y;"
 xit> (compile '(js/\|= x y))
 "x |= y;"
 xit> (compile '(js/<<= x y))
 "x <<= y;"
 xit> (compile '(js/>>= x y))
 "x >>= y;"
 xit> (compile '(js/>>>= x y))
 "x >>>= y;")
