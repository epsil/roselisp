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

 > (describe "define-syntax")
 _
 > (compile '(module m scheme
               (define-syntax (foo x)
                 (syntax test))
               (foo 1)))
 "import {
  datumToSyntax
} from 'roselisp';

function foo(x) {
  return datumToSyntax(false, Symbol.for('test'));
}

foo.ftype = [Symbol.for('macro->'), Symbol.for('Syntax'), Symbol.for('Syntax')];

test;"

 > (compile '(module m scheme
               (define-syntax (foo x)
                 (js/second (syntax-e x)))
               (foo 1)))
 "import {
  syntaxE
} from 'roselisp';

function foo(x) {
  return syntaxE(x)[1];
}

foo.ftype = [Symbol.for('macro->'), Symbol.for('Syntax'), Symbol.for('Syntax')];

1;"

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
