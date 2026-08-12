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

 ;; `js/=`
 > (describe "js/=")
 _
 > (compile '(js/= x y))
 "x = y;"
 > (compile '(js/= (aget x i) y))
 "x[i] = y;"
 > (compile '(js/= (list x y) z))
 "[x, y] = z;"
 > (compile '(js/= (list #f y) z))
 "[, y] = z;"
 > (compile '(js/= (list* x) y))
 "x = y;"
 > (compile '(js/= (list* x y) z))
 "[x, ...y] = z;"
 > (compile '(js/= (list* #f x y) z))
 "[, x, ...y] = z;"
 > (compile '(js/= (values x y) z))
 "[x, y] = z;"
 > (compile '(js/= (js/obj x x) y))
 "({x} = y);"
 > (compile '(js/= (js/obj x y) z))
 "({x: y} = z);"
 > (compile '(js/= (set! x) y))
 "x = y;"
 > (compile '(js/= (aset! x i) y))
 "x[i] = y;"
 > (compile '(js/= (oset! x "y") z))
 "x['y'] = z;"
 > (compile '(js/= (set!-values (x)) y))
 "[x] = y;"
 > (compile '(js/= (set!-fields (x)) y))
 "({x} = y);"
 > (compile '(js/= (define x) y))
 "let x = y;"
 > (compile '(js/= (define-values (x)) y))
 "let [x] = y;"
 > (compile '(js/= (define-values (_ x)) y))
 "let [, x] = y;"
 > (compile '(js/= (define-values (_ __ x)
                     :hole-marker __)
                   y))
 "let [_, , x] = y;"
 > (compile '(js/= (define-fields (x)) y))
 "let {x} = y;"

 > (describe "Fundamental operators")
 _
 xit> (compile '(js/, x y))
 "x, y;"
 xit> (compile '(js/\; x y))
 "x;
y;"
 xit> (compile '(js/\(\) x))
 "x()"
 xit> (compile '(js/\(\) x y))
 "x(y)"
 xit> (compile '(js/\(\) x y z))
 "x(y, z)"
 xit> (compile '(js/\[\] x y))
 "x[y]"
 xit> (compile '(js/\{\} x))
 "{
  x;
}"

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
 "x >>>= y;"

 ;; `define-syntax`
 > (describe "define-syntax")
 _
 xit> (compile '(module m scheme
                  (define x 1)
                  (define-syntax (foo x)
                    (syntax
                     (begin
                       (define x 2)
                       x)))
                  (foo)))
 "import {
  datumToSyntax
} from 'roselisp';

let x = 1;

function foo(x) {
  return datumToSyntax(false, [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('x'), 2], Symbol.for('x')]);
}

foo.ftype = [Symbol.for('macro->'), Symbol.for('Syntax'), Symbol.for('Syntax')];

let x1 = 2;

x1;")
