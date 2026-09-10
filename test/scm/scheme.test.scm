;;; # Scheme procedures
;;;
;;; Scheme procedures and constructs from Scheme and various
;;; Scheme implementations.

(require (only-in "./test-util"
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 :repl #t

 :describe "#t"
 > #t
 #t
 > '#t
 #t
 > (compile #t)
 "true;"
 > (compile #t :as "statement")
 "true;"
 > (compile #t :as "expression")
 "true"
 > (compile #t :as "return")
 "return true;"

 :describe "#f"
 > #f
 #f
 > '#f
 #f
 > (compile #f)
 "false;"
 > (compile #f :as "statement")
 "false;"
 > (compile #f :as "expression")
 "false"
 > (compile #f :as "return")
 "return false;"

 :describe "null"
 > null
 '()
 > (listp null)
 #t
 > (length null)
 0
 > (compile 'null)
 "[];"

 :describe "Numbers"
 > 0
 0
 > 1
 1
 > 2
 2
 > (compile 0)
 "0;"
 > (compile 1)
 "1;"
 > (compile 2)
 "2;"

 :describe "Strings"
 > ""
 ""
 > "foo"
 "foo"
 > "\"foo\""
 "\"foo\""
 > (eq? "\t" "	")
 #t
 > (compile "")
 "'';"
 > (compile "foo")
 "'foo';"
 > (compile "don't")
 "'don\\'t';"
 > (compile "newline
test")
 "'newline\\n' +
  'test';"
 > (compile "newline\ntest")
 "'newline\\n' +
  'test';"
 > (compile "newline
test
three")
 "'newline\\n' +
  'test\\n' +
  'three';"
 > (compile "\\s")
 "'\\\\s';"

 :describe "Symbols"
 > 'foo
 'foo
 > (compile 'foo)
 "foo;"
 > (compile 'foo-bar)
 "fooBar;"
 > (compile ''foo)
 "Symbol.for('foo');"
 > (compile ''foo-bar)
 "Symbol.for('foo-bar');"
 > (compile 'js/undefined)
 "undefined;"
 > (compile 'js/null)
 "null;"
 > (compile 'foo-bar
            :case "none")
 "foo-bar;"
 > (compile 'foo-bar)
 "fooBar;"
 > (compile 'foo/bar)
 "fooBar;"
 > (compile 'foo!)
 "foox;"
 > (compile 'foo-bar!)
 "fooBarX;"
 > (compile 'foo?)
 "foop;"
 > (compile 'foo-bar?)
 "fooBarP;"
 > (compile '*foo-bar*)
 "starFooBarStar;"
 > (compile ''*foo-bar*)
 "Symbol.for('*foo-bar*');"
 > (compile 'A)
 "A;"
 > (compile '(module m scheme
               (define lst
                 (map symbol? '(a b c)))))
 "let lst = [Symbol.for('a'), Symbol.for('b'), Symbol.for('c')].map(function (x) {
  return typeof x === 'symbol';
});"

 :describe "symbol?"
 > (symbol? 'foo)
 #t
 > (symbol? 1)
 #f
 > (symbol? "foo")
 #f
 > (symbol? (js/obj))
 #f
 > (symbol? '())
 #f

 :describe "symbol->string"
 > (symbol->string 'foo)
 "foo"

 :describe "Cons cells"
 > (cons 1 2)
 '(1 . 2)
 > (cons 1 (cons 2 3))
 '(1 2 . 3)
 > (cons 1 '())
 '(1)
 > (cons 1 '(2))
 '(1 2)
 > (car '(1 . 2))
 1
 > (cdr '(1 . 2))
 2
 > (car (cons 1 2))
 1
 > (cdr (cons 1 2))
 2
 > (compile ''(1 . ()))
 "[1, Symbol.for('.'), []];"
 > (compile ''(1 . 2))
 "[1, Symbol.for('.'), 2];"
 > (compile ''(1 2 . 3))
 "[1, 2, Symbol.for('.'), 3];"

 :describe "Lists"
 > (list 1 2)
 '(1 2)
 > (aget '(1 2) 0)
 1
 > (aget '((1 2) (3 4)) 0 1)
 2
 > (aref '(1 2) 0)
 1
 > (aset! '(1 2) 0 3)
 3
 > (let ((lst '(1 2)))
     (aset! lst 0 3)
     lst)
 '(3 2)
 > (let ((lst '(1 2))
         (i 0))
     (aget lst i))
 1
 > (let ((lst '(1 2))
         (i 0))
     (aget lst (+ i 1)))
 2
 > (let ((lst '(1 2)))
     (set! (aref lst 0) 3)
     lst)
 '(3 2)
 > (compile '())
 "[];"
 > (compile ''())
 "[];"
 > (compile ''(1))
 "[1];"
 > (compile ''(1 2))
 "[1, 2];"
 > (compile '(aget x 0))
 "x[0];"
 > (compile '(let ((length 0))
               (aget x length)))
 "let length = 0;

x[length];"
 > (compile '(aget x 'length))
 "x['length'];"
 > (compile '(aget x :length))
 "x['length'];"
 > (compile '(aget (js/?. x) 0))
 "x?.[0];"

 :describe "pair?"
 > (pair? 0)
 #f
 > (pair? 'x)
 #f
 > (pair? "x")
 #f
 > (pair? '())
 #f
 > (pair? '(1))
 #t
 > (pair? '(1 2))
 #t
 > (pair? '(1 2 3))
 #t
 > (pair? '(1 . 2))
 #t
 > (pair? '(1 2 . 3))
 #t
 > (pair? '(()))
 #t
 > (pair? '(.))
 #t
 > (pair? '(. 1))
 #t
 > (pair? '(. 1 2))
 #t

 :describe "cons"
 > (cons 1 2)
 '(1 . 2)
 > (cons 1 (cons 2 3))
 '(1 2 . 3)
 > (cons 1 '())
 '(1)
 > (cons 1 '(2))
 '(1 2)
 > (compile '(cons 1 2))
 "[1, Symbol.for('.'), 2];"
 > (compile '(cons "1" "2"))
 "['1', Symbol.for('.'), '2'];"
 > (compile '(cons x (list y)))
 "[x, y];"
 > (compile '(cons x '(y)))
 "[x, Symbol.for('y')];"
 > (compile '(cons x `(y)))
 "[x, Symbol.for('y')];"
 > (compile '(cons x `(,y)))
 "[x, y];"
 > (compile '(cons x `(,@y)))
 "[x, ...y];"
 > (compile '(cons x y))
 "[x, ...(Array.isArray(y) ? y : [Symbol.for('.'), y])];"
 > (compile '(cons (x) y))
 "[x(), ...(Array.isArray(y) ? y : [Symbol.for('.'), y])];"
 > (compile '(cons x (y)))
 "[x, ...((x) => {
  return Array.isArray(x) ? x : [Symbol.for('.'), x];
})(y())];"
 > (compile '(cons (x) (y)))
 "[x(), ...((x) => {
  return Array.isArray(x) ? x : [Symbol.for('.'), x];
})(y())];"

 :describe "cons?"
 > (cons? 0)
 #f
 > (cons? 'x)
 #f
 > (cons? "x")
 #f
 > (cons? '())
 #f
 > (cons? '(1))
 #t
 > (cons? '(1 2))
 #t
 > (cons? '(1 2 3))
 #t
 > (cons? '(1 . 2))
 #t
 > (cons? '(1 2 . 3))
 #t
 > (cons? '(()))
 #t
 > (cons? '(.))
 #t
 > (cons? '(. 1))
 #t
 > (cons? '(. 1 2))
 #t

 :describe "list?"
 > (list? '())
 #t
 > (list? '(1))
 #t
 > (list? '(1 2))
 #t
 > (list? '(1 2 3))
 #t
 > (list? '(1 . 2))
 #f
 > (list? '(1 2 . 3))
 #f
 > (compile '(list? x))
 "Array.isArray(x) && !((x.length >= 3) && (x.at(-2) === Symbol.for('.')) && !Array.isArray(x.at(-1)));"
 > (compile '(module m scheme
               (list? x))
            :fdottedlists #f)
 "Array.isArray(x) && !((x.length >= 3) && (x.at(-2) === Symbol.for('.')) && !Array.isArray(x.at(-1)));"
 > (compile '(module m scheme
               (list? x))
            :fdottedlists #t)
 "import {
  listp
} from 'roselisp';

listp(x);"

 :describe "list-ref"
 > (compile '(list-ref lst i))
 "lst[i];"
 > (compile '(list-ref lst i j))
 "lst[i][j];"
 > (compile '(module m scheme
               (list-ref lst i))
            :fdottedlists #f)
 "lst[i];"
 > (compile '(module m scheme
               (list-ref lst i))
            :fdottedlists #t)
 "import {
  listRef
} from 'roselisp';

listRef(lst, i);"

 :describe "list-set"
 > (list-set '(1 2 3) 0 4)
 '(4 2 3)
 > (list-set '((1) 2 3) 0 0 4)
 '((4) 2 3)
 > (funcall list-set '(1 . ()) 0 2)
 '(2 . ())
 > (funcall list-set '(1 . (2 . ())) 1 3)
 '(1 . (3 . ()))
 > (funcall list-set '((1 . 2) . (3 . ())) 0 0 4)
 '((4 . 2) . (3 . ()))
 > (funcall list-set '(1 2 . (3 . ())) 1 4)
 '(1 4 . (3 . ()))

 :describe "list-set!"
 > (let ((lst '(1 2 3)))
     (list-set! lst 0 4)
     lst)
 '(4 2 3)
 > (let ((lst '((1) 2 3)))
     (list-set! lst 0 0 4)
     lst)
 '((4) 2 3)
 > (let ((lst '(1 . ())))
     (funcall list-set! lst 0 2)
     lst)
 '(2 . ())
 > (let ((lst '(1 . (2 . ()))))
     (funcall list-set! lst 1 3)
     lst)
 '(1 . (3 . ()))
 > (let ((lst '((1 . 2) . (3 . ()))))
     (funcall list-set! lst 0 0 4)
     lst)
 '((4 . 2) . (3 . ()))
 > (let ((lst '(1 2 . (3 . ()))))
     (funcall list-set! lst 1 4)
     lst)
 '(1 4 . (3 . ()))
 > (compile '(list-set! lst i x))
 "lst[i] = x;"
 > (compile '(list-set! lst i j x))
 "lst[i][j] = x;"
 > (compile '(module m scheme
               (list-set! lst i x))
            :fdottedlists #f)
 "lst[i] = x;"
 > (compile '(module m scheme
               (list-set! lst i x))
            :fdottedlists #t)
 "import {
  listSetX
} from 'roselisp';

listSetX(lst, i, x);"

 :describe "length"
 > (length '())
 0
 > (length '(1))
 1
 > (length '(1 2))
 2
 > (length '(1 2 3))
 3
 > (funcall length '())
 0
 > (funcall length '(1))
 1
 > (funcall length '(1 2))
 2
 > (funcall length '(1 2 3))
 3
 > (funcall length '(1 . ()))
 1
 > (funcall length '(1 . (2 . ())))
 2
 > (funcall length '(1 2 . ()))
 2
 > (compile '(length x))
 "x.length;"
 > (compile '(module m scheme
               (length x))
            :fdottedlists #f)
 "x.length;"
 > (compile '(module m scheme
               (length x))
            :fdottedlists #t)
 "import {
  length
} from 'roselisp';

length(x);"

 :describe "first"
 > (compile '(first x))
 "x[0];"
 > (compile '(module m scheme
               (first x))
            :fdottedlists #f)
 "x[0];"
 > (compile '(module m scheme
               (first x))
            :fdottedlists #t)
 "import {
  first
} from 'roselisp';

first(x);"

 :describe "second"
 > (compile '(second x))
 "x[1];"
 > (compile '(module m scheme
               (second x))
            :fdottedlists #f)
 "x[1];"
 > (compile '(module m scheme
               (second x))
            :fdottedlists #t)
 "import {
  second
} from 'roselisp';

second(x);"

 :describe "third"
 > (compile '(third x))
 "x[2];"
 > (compile '(module m scheme
               (third x))
            :fdottedlists #f)
 "x[2];"
 > (compile '(module m scheme
               (third x))
            :fdottedlists #t)
 "import {
  third
} from 'roselisp';

third(x);"

 :describe "fourth"
 > (compile '(fourth x))
 "x[3];"
 > (compile '(module m scheme
               (fourth x))
            :fdottedlists #f)
 "x[3];"
 > (compile '(module m scheme
               (fourth x))
            :fdottedlists #t)
 "import {
  fourth
} from 'roselisp';

fourth(x);"

 :describe "fifth"
 > (compile '(fifth x))
 "x[4];"
 > (compile '(module m scheme
               (fifth x))
            :fdottedlists #f)
 "x[4];"
 > (compile '(module m scheme
               (fifth x))
            :fdottedlists #t)
 "import {
  fifth
} from 'roselisp';

fifth(x);"

 :describe "sixth"
 > (compile '(sixth x))
 "x[5];"
 > (compile '(module m scheme
               (sixth x))
            :fdottedlists #f)
 "x[5];"
 > (compile '(module m scheme
               (sixth x))
            :fdottedlists #t)
 "import {
  sixth
} from 'roselisp';

sixth(x);"

 :describe "seventh"
 > (compile '(seventh x))
 "x[6];"
 > (compile '(module m scheme
               (seventh x))
            :fdottedlists #f)
 "x[6];"
 > (compile '(module m scheme
               (seventh x))
            :fdottedlists #t)
 "import {
  seventh
} from 'roselisp';

seventh(x);"

 :describe "eighth"
 > (compile '(eighth x))
 "x[7];"
 > (compile '(module m scheme
               (eighth x))
            :fdottedlists #f)
 "x[7];"
 > (compile '(module m scheme
               (eighth x))
            :fdottedlists #t)
 "import {
  eighth
} from 'roselisp';

eighth(x);"

 :describe "ninth"
 > (compile '(ninth x))
 "x[8];"
 > (compile '(module m scheme
               (ninth x))
            :fdottedlists #f)
 "x[8];"
 > (compile '(module m scheme
               (ninth x))
            :fdottedlists #t)
 "import {
  ninth
} from 'roselisp';

ninth(x);"

 :describe "tenth"
 > (compile '(tenth x))
 "x[9];"
 > (compile '(module m scheme
               (tenth x))
            :fdottedlists #f)
 "x[9];"
 > (compile '(module m scheme
               (tenth x))
            :fdottedlists #t)
 "import {
  tenth
} from 'roselisp';

tenth(x);"

 :describe "last"
 > (last '(1))
 1
 > (last '(1 2))
 2
 > (last '(1 2 3))
 3
 > (funcall last '(1 . ()))
 1
 > (funcall last '(1 . (2 . ())))
 2
 > (funcall last '(1 2 . ()))
 2
 > (compile '(last lst))
 "lst.at(-1);"
 > (compile '(module m scheme
               (last lst))
            :fdottedlists #f)
 "lst.at(-1);"
 > (compile '(module m scheme
               (last lst))
            :fdottedlists #t)
 "import {
  last
} from 'roselisp';

last(lst);"

 :describe "list-tail"
 > (list-tail '(1 2 3) 0)
 '(1 2 3)
 > (list-tail '(1 2 3) 1)
 '(2 3)
 > (list-tail '(1 2 3) 2)
 '(3)
 > (list-tail '(1 2 3) 3)
 '()
 > (list-tail '(1 . 2) 1)
 2
 > (compile '(module m scheme
               (list-tail x n))
            :fdottedlists #f)
 "import {
  listTail
} from 'roselisp';

listTail(x, n);"
 > (compile '(module m scheme
               (list-tail x n))
            :fdottedlists #t)
 "import {
  listTail
} from 'roselisp';

listTail(x, n);"

 :describe "cdr"
 > (cdr '(1))
 '()
 > (cdr '(1 2))
 '(2)
 > (cdr '(1 . 2))
 2
 > (cdr '(1 . ()))
 '()
 > (cdr '(1 2 . ()))
 '(2 . ())
 > (cdr '(1 . (2 . ())))
 '(2 . ())
 > (cdr '(1 2 . (3 . ())))
 '(2 . (3 . ()))
 > (funcall cdr '(1))
 '()
 > (funcall cdr '(1 2))
 '(2)
 > (funcall cdr '(1 . 2))
 2
 > (funcall cdr '(1 . ()))
 '()
 > (funcall cdr '(1 2 . ()))
 '(2 . ())
 > (funcall cdr '(1 . (2 . ())))
 '(2 . ())
 > (funcall cdr '(1 2 . (3 . ())))
 '(2 . (3 . ()))
 > (compile '(cdr x))
 "((x.length === 3) && (x[1] === Symbol.for('.'))) ? x[2] : x.slice(1);"
 > (compile '(module m scheme
               (cdr x))
            :fdottedlists #f)
 "((x.length === 3) && (x[1] === Symbol.for('.'))) ? x[2] : x.slice(1);"
 > (compile '(module m scheme
               (cdr x))
            :fdottedlists #t)
 "import {
  cdr
} from 'roselisp';

cdr(x);"

 :describe "rest"
 > (rest '(1))
 '()
 > (rest '(1 2))
 '(2)
 > (rest '(1 2 3))
 '(2 3)
 > (funcall rest '(1))
 '()
 > (funcall rest '(1 2))
 '(2)
 > (funcall rest '(1 2 3))
 '(2 3)
 > (funcall rest '(1 . 2))
 2
 > (funcall rest '(1 . ()))
 '()
 > (funcall rest '(1 2 . ()))
 '(2 . ())
 > (funcall rest '(1 . (2 . ())))
 '(2 . ())
 > (funcall rest '(1 2 . (3 . ())))
 '(2 . (3 . ()))
 > (compile '(rest x))
 "x.slice(1);"
 > (compile '(module m scheme
               (rest x))
            :fdottedlists #f)
 "x.slice(1);"
 > (compile '(module m scheme
               (rest x))
            :fdottedlists #t)
 "import {
  rest
} from 'roselisp';

rest(x);"

 :describe "set-car!"
 > ((lambda ()
      (define foo '())
      (set-car! foo 'bar)
      foo))
 '()
 > ((lambda ()
      (define foo
        '(foo))
      (set-car! foo 'bar)
      foo))
 '(bar)

 :describe "set-cdr!"
 > (let ((foo '()))
     (set-cdr! foo '(bar))
     foo)
 '()
 > (let ((foo '(foo)))
     (set-cdr! foo '(bar))
     foo)
 '(foo bar)
 > (let ((foo '(foo bar)))
     (set-cdr! foo '(baz))
     foo)
 '(foo baz)
 > (let ((foo '(foo bar)))
     (set-cdr! foo '(baz . quux))
     foo)
 '(foo baz . quux)
 > (let ((foo '(foo . bar)))
     (set-cdr! foo '(baz))
     foo)
 '(foo baz)
 > (let ((foo '(foo . bar)))
     (set-cdr! foo '(baz . quux))
     foo)
 '(foo baz . quux)
 > (let ((foo '(foo)))
     (set-cdr! foo 'bar)
     foo)
 '(foo . bar)
 > (let ((foo '(foo bar . baz)))
     (set-cdr! foo '(quux))
     foo)
 '(foo quux)
 > (let ((foo '(foo bar . baz)))
     (set-cdr! foo 'quux)
     foo)
 '(foo . quux)

 :describe "list*"
 > (list*)
 #u
 > (list* 1)
 1
 > (list* 1 2)
 '(1 . 2)
 > (list* 1 2 3)
 '(1 2 . 3)
 > (list* 1 2 3 4)
 '(1 2 3 . 4)
 > (list* 1 '())
 '(1)
 > (list* 1 '(2))
 '(1 2)
 > (list* 1 '(2 . 3))
 '(1 2 . 3)

 :describe "flatten"
 > (flatten '(1 2 3 4))
 '(1 2 3 4)
 > (flatten '(1 . 2))
 '(1 2)
 > (flatten '((a) b (c (d) . e) ()))
 '(a b c d e)
 > (flatten '((((4)))))
 '(4)

 :describe "list"
 > (compile '(list))
 "[];"
 > (compile '(list 1))
 "[1];"
 > (compile '(list 1 2))
 "[1, 2];"
 > (compile '(list (list 1)))
 "[[1]];"

 :describe "append"
 > (compile '(append))
 "[];"
 > (compile '(append foo))
 "[...foo];"
 > (compile '(append foo bar))
 "[...foo, ...bar];"
 > (compile '(append (list)))
 "[];"
 > (compile '(append (list x)))
 "[x];"
 > (compile '(append '("foo") '("bar")))
 "['foo', 'bar'];"

 :describe "quote"
 > (quote foo)
 'foo
 > (quote (1))
 '(1)
 > (quote (1 2))
 '(1 2)
 > (quote ((1 2) (3 4)))
 '((1 2) (3 4))
 > (compile '(quote foo))
 "Symbol.for('foo');"
 > (compile '(quote ()))
 "[];"
 > (compile '(quote (1)))
 "[1];"
 > (compile '(quote (1 2)))
 "[1, 2];"
 > (compile '(quote ((1))))
 "[[1]];"
 > (compile '(quote ((1 2) (3 4))))
 "[[1, 2], [3, 4]];"
 > (compile '(quote (1 . 2)))
 "[1, Symbol.for('.'), 2];"
 > (compile '(quote (#t #f)))
 "[true, false];"
 > (compile '(quote (x y z)))
 "[Symbol.for('x'), Symbol.for('y'), Symbol.for('z')];"

 :describe "quasiquote"
 > `foo
 'foo
 > (quasiquote foo)
 'foo
 > (quasiquote (,1))
 '(1)
 > (quasiquote ((,1)))
 '((1))
 > (quasiquote (,@(list 1 2 3)))
 '(1 2 3)
 > `(`(,,1))
 '(`(,1))
 > (let ((x 1))
     `(`(,,x)))
 '(`(,1))
 > `(1 `,(+ 1 ,(+ 2 3)) 4)
 '(1 `,(+ 1 5) 4)
 > `(1 ```,,@,,@(list (+ 1 2)) 4)
 '(1 ```,,@,3 4)
 > ``(,,@(list 1 2 3))
 '`((unquote 1) (unquote 2) (unquote 3))
 > (let ((lst '(foo bar baz)))
     ``(,,@lst))
 '`((unquote foo bar baz))
 > (compile '(quasiquote foo))
 "Symbol.for('foo');"
 > (compile '(quasiquote ()))
 "[];"
 > (compile '(quasiquote (1)))
 "[1];"
 > (compile '(quasiquote ((1))))
 "[[1]];"
 > (compile '(quasiquote (1 (unquote 2))))
 "[1, 2];"
 > (compile '(quasiquote (1 (unquote 2 3))))
 "[1, 2, 3];"
 > (compile '(quasiquote (1 . 2)))
 "[1, Symbol.for('.'), 2];"
 > (compile '(quasiquote ((1 . 2))))
 "[[1, Symbol.for('.'), 2]];"
 > (compile '(quasiquote (x y z)))
 "[Symbol.for('x'), Symbol.for('y'), Symbol.for('z')];"
 > (compile '(quasiquote (,1)))
 "[1];"
 > (compile '(quasiquote ((,1))))
 "[[1]];"
 > (compile '(quasiquote (1 (unquote-splicing (list 2)))))
 "[1, 2];"
 > (compile '(quasiquote (,@(list 1 2 3))))
 "[1, 2, 3];"
 > (compile '(quasiquote ((1 . (unquote 2)))))
 "[[1, Symbol.for('.'), 2]];"
 > (compile '(quasiquote ((1 . (unquote 2))
                          (3 . (unquote 4)))))
 "[[1, Symbol.for('.'), 2], [3, Symbol.for('.'), 4]];"
 > (compile '(quasiquote (1 (unquote-splicing (list 2) (list 3)))))
 "[1, 2, 3];"
 > (compile '(quasiquote ((unquote-splicing x))))
 "[...x];"
 > (compile '(quasiquote ((unquote-splicing x)
                          (unquote-splicing y))))
 "[...x, ...y];"
 > (compile '(quasiquote (quasiquote (1 (unquote (unquote-splicing (list 2)))))))
 "[Symbol.for('quasiquote'), [1, [Symbol.for('unquote'), 2]]];"
 > (compile '(quasiquote (quasiquote (1 (unquote (unquote-splicing (list 2 3)))))))
 "[Symbol.for('quasiquote'), [1, [Symbol.for('unquote'), 2], [Symbol.for('unquote'), 3]]];"
 > (compile '(quasiquote (x y (unquote z))))
 "[Symbol.for('x'), Symbol.for('y'), z];"
 > (compile '(quasiquote (x y (unquote-splicing z))))
 "[Symbol.for('x'), Symbol.for('y'), ...z];"
 > (compile '(quasiquote (x y (quasiquote z))))
 "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), Symbol.for('z')]];"
 > (compile '(quasiquote (x y (quasiquote (z)))))
 "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [Symbol.for('z')]]];"
 > (compile '(quasiquote (x y (quasiquote ((unquote z))))))
 "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [[Symbol.for('unquote'), Symbol.for('z')]]]];"
 > (compile '(quasiquote (x y (quasiquote ((unquote-splicing z))))))
 "[Symbol.for('x'), Symbol.for('y'), [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('z')]]]];"
 > (compile '(define test-map-1
               `(("foo" . ,test-fn)
                 ("bar" . ,test-fn))))
 "let testMap1 = [['foo', Symbol.for('.'), testFn], ['bar', Symbol.for('.'), testFn]];"
 > (compile '(set! let-exp
                   (quasiquote
                    (let (((unquote arg-list)
                           (quote (unquote args))))
                      (unquote-splicing body)))))
 "letExp = [Symbol.for('let'), [[argList, [Symbol.for('quote'), args]]], ...body];"

 :describe "Variables"
 > (let ((x 2))
     x)
 2
 > (let ((x 2)
         y)
     y)
 #u
 > (let (x)
     (set! x 2)
     x)
 2
 > ((lambda ()
      (define x 2)
      x))
 2
 > ((lambda ()
      (define x)
      (set! x 2)
      x))
 2
 > (let (x y)
     (set! x 2)
     (set! y 3)
     (+ x y))
 5

 :describe "Function calls"
 > (let ((identity (lambda (x) x)))
     (identity "foo"))
 "foo"
 > (let ((my-add (lambda (x y) (+ x y))))
     (my-add 1 2))
 3
 > (let ((my-add (lambda (x y z) (+ x y z))))
     (my-add 1 2 3))
 6

 :describe "define"
 > ((lambda ()
      (define x 1)
      1))
 1
 > ((lambda ()
      (define (foo . args)
        args)
      (foo)))
 '()
 > ((lambda ()
      (define (my-add x y)
        (+ x y))
      (my-add 2 3)))
 5
 > (let ((my-add (lambda (x y) (+ x y))))
     ((lambda ()
        (define (my-add-2 x y)
          (my-add x y))
        (my-add-2 2 3))))
 5
 > (let ((my-add (lambda (x y z) (+ x y z))))
     ((lambda ()
        (define (my-add-2 x y z)
          (my-add x y z))
        (my-add-2 1 2 3))))
 6
 > (compile '(define x))
 "let x;"
 > (compile '(define x 1))
 "let x = 1;"
 > (compile '(define (foo x)
               x))
 "function foo(x) {
  return x;
}"
 > (compile '(define foo
               (lambda (x)
                 x)))
 "let foo = function (x) {
  return x;
};"
 > (compile '(define x)
            :to "typescript")
 "let x: any;"
 > (compile '(define x 1))
 "let x = 1;"
 > (compile '(define x 1)
            :to "typescript")
 "let x: any = 1;"
 > (compile '(define I
               (lambda (x)
                 x)))
 "let I = function (x) {
  return x;
};"
 > (compile '(define I
               (memoize
                (lambda (x)
                  x))))
 "let I = memoize(function (x) {
  return x;
});"
 > (compile '(define (identity-function x)
               x))
 "function identityFunction(x) {
  return x;
}"
 > (compile '(define (I x)
               x))
 "function I(x) {
  return x;
}"
 > (compile '(define (K x y)
               x))
 "function K(x, y) {
  return x;
}"
 > (compile '(define (S f g x)
               (f x (g x))))
 "function S(f, g, x) {
  return f(x, g(x));
}"
 > (compile '(define (S f g x)
               ((f x) (g x))))
 "function S(f, g, x) {
  return f(x)(g(x));
}"
 > (compile '(define (C f x y)
               (f y x)))
 "function C(f, x, y) {
  return f(y, x);
}"
 > (compile '(define (U f)
               (f f)))
 "function U(f) {
  return f(f);
}"
 > (compile '(define (A f . args)
               (apply f args)))
 "function A(f, ...args) {
  return f(...args);
}"
 > (compile '(define (A f . args)
               (apply f args))
            :to "typescript")
 "function A(f: any, ...args: any[]): any {
  return f(...args);
}"
 > (compile
    '(define (Q . args)
       (cond
        ((= (.-length args) 0)
         #u)
        ((= (.-length args) 1)
         (aref args 0))
        (else
         (let ((fs (.slice args 0 -1))
               (x (aref args (- (.-length args) 1))))
           (.reduce fs (lambda (acc f) (f acc)) x))))))
 "function Q(...args) {
  if (args.length === 0) {
    return undefined;
  } else if (args.length === 1) {
    return args[0];
  } else {
    let fs = args.slice(0, -1);
    let x = args[args.length - 1];
    return fs.reduce(function (acc, f) {
      return f(acc);
    }, x);
  }
}"
 > (compile
    '(define (T . args)
       (cond
        ((= (.-length args) 0)
         #u)
        ((= (.-length args) 1)
         (aref args 0))
        (else
         (let-values (((x . fs) args))
           (.reduce fs (lambda (acc f) (f acc)) x))))))
 "function T(...args) {
  if (args.length === 0) {
    return undefined;
  } else if (args.length === 1) {
    return args[0];
  } else {
    let [x, ...fs] = args;
    return fs.reduce(function (acc, f) {
      return f(acc);
    }, x);
  }
}"
 > (compile
    '(define (Y f)
       ((lambda (future)
          (f (lambda (arg)
               ((future future) arg))))
        (lambda (future)
          (f (lambda (arg)
               ((future future) arg)))))))
 "function Y(f) {
  return (function (future) {
    return f(function (arg) {
      return future(future)(arg);
    });
  })(function (future) {
    return f(function (arg) {
      return future(future)(arg);
    });
  });
}"
 > (compile '(define (compose f g)
               (lambda (x)
                 (f (g x)))))
 "function compose(f, g) {
  return function (x) {
    return f(g(x));
  };
}"
 > (compile '(define (foo)
               (set! x (+ x 1))
               (set! y (+ y 1))))
 "function foo() {
  x++;
  return ++y;
}"
 > (compile
    '(define (map-get map path)
       (let-values (((value) (map-get-2 map path)))
         value)))
 "function mapGet(map, path) {
  let [value] = mapGet2(map, path);
  return value;
}"
 > (compile
    '(define (add-matrix m1 m2)
       (let ((l1 (length m1))
             (l2 (length m2)))
         (let ((matrix (make-matrix l1 l2)))
           (for ((i (range 0 l1)))
             (for ((j (range 0 l2)))
               (set! (aget matrix i j)
                     (+ (aget m1 i j)
                        (aget m2 i j)))))
           matrix))))
 "function addMatrix(m1, m2) {
  let l1 = m1.length;
  let l2 = m2.length;
  let matrix = makeMatrix(l1, l2);
  for (let i = 0; i < l1; i++) {
    for (let j = 0; j < l2; j++) {
      matrix[i][j] = m1[i][j] + m2[i][j];
    }
  }
  return matrix;
}"
 > (compile
    '(define _ (js/obj "dash" #t)))
 "let _ = {
  dash: true
};"
 > (compile
    '(define __ (js/obj "dash" #t)))
 "let __ = {
  dash: true
};"
 > (compile
    '(module m scheme
       (define I
         (curry-n
          1
          (lambda (x)
            x)))
       (define K
         (curry-n
          2
          (lambda (x y)
            x)))))
 "import {
  curryN
} from 'roselisp';

let I = curryN(1, function (x) {
  return x;
});

let K = curryN(2, function (x, y) {
  return x;
});"
 > (compile '(define Foo
               (class object%)))
 "class Foo {
}"
 > (compile '(define Foo
               (class Bar)))
 "class Foo extends Bar {
}"

 :describe "define-syntax"
 > (compile '(module m scheme
               (define-syntax foo
                 (lambda (x)
                   (syntax test)))
               (foo 1)))
 "import {
  datumToSyntax
} from 'roselisp';

let foo = function (x) {
  return datumToSyntax(false, Symbol.for('test'));
};

foo.ftype = [Symbol.for('macro->'), Symbol.for('Syntax'), Symbol.for('Syntax')];

test;"
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
                 (second (syntax-e x)))
               (foo 1)))
 "import {
  syntaxE
} from 'roselisp';

function foo(x) {
  return syntaxE(x)[1];
}

foo.ftype = [Symbol.for('macro->'), Symbol.for('Syntax'), Symbol.for('Syntax')];

1;"

 :describe "syntax->list"
 > (syntax->list (syntax ()))
 '()
 > (syntax->list (syntax (1 . 2)))
 #f

 :describe "syntax-e"
 > (syntax-e (syntax ()))
 '()
 > (dotted-list? (syntax-e (syntax (1 . 2))))
 #t
 > (dotted-list? (cdr (syntax-e (syntax (1 . (2 . 3))))))
 #t

 :describe "let"
 > (let ((x 0))
     x)
 0
 > (let ((x 1))
     (let ((y 2))
       x))
 1
 > (let ((x '((1 2) (3 4))))
     x)
 '((1 2) (3 4))
 > (let (x)
     (set! x 1)
     x)
 1
 > (let (x)
     (set! x 1)
     (set! x 2)
     x)
 2
 > (let (x))
 #u
 > (let ((a 1))
     (+ (let ((a 2))
          a)
        a))
 3
 > (let ((compose (lambda (f g)
                    (lambda (x)
                      (f (g x)))))
         (square (lambda (x) (* x x)))
         (add1 (lambda (x) (+ x 1))))
     ((compose square add1) (add1 4)))
 36
 > (compile '(let (x)))
 "let x;"
 > (compile '(let (x)
               x)
            :as "return")
 "let x;

return x;"
 > (compile '(let (x)
               x)
            :as "expression")
 "(() => {
  let x;
  return x;
})()"
 > (compile '(let (x)
               x)
            :as "return"
            :to "typescript")
 "let x: any;

return x;"
 > (compile '(let ((x 1))
               x)
            :as "return")
 "let x = 1;

return x;"
 > (compile '(let ((x 1))
               x)
            :as "return"
            :to "typescript")
 "let x: any = 1;

return x;"
 > (compile
    '(let ((compose (lambda (f g)
                      (lambda (x)
                        (f (g x)))))
           (square (lambda (x) (* x x)))
           (add1 (lambda (x) (+ x 1))))
       (display ((compose square add1) (add1 4)))))
 "let compose = function (f, g) {
  return function (x) {
    return f(g(x));
  };
};

let square = function (x) {
  return x * x;
};

let add1 = function (x) {
  return x + 1;
};

console.log(compose(square, add1)(add1(4)));"
 > (compile
    '(let ((and (lambda (x y)
                  (if x (if y #t #f) #f))))
       (and x y)))
 "let and = function (x, y) {
  if (x) {
    if (y) {
      return true;
    } else {
      return false;
    }
  } else {
    return false;
  }
};

and(x, y);"
 > (compile '(begin
               x
               (let ((x 1))
                 x)))
 "x;

let x = 1;

x;"
 > (compile '(begin
               (let ((x 1))
                 (display x))
               (let ((x 1))
                 (display x))))
 "let x = 1;

console.log(x);

{
  let x = 1;
  console.log(x);
}"
 > (compile '(cond
              (foo
               bar)
              (else
               x
               (let ((x 1))
                 x)))
            :as "return"
            :to "typescript")
 "if (foo) {
  return bar;
} else {
  x;
  let x: any = 1;
  return x;
}"
 > (compile
    '(define make-compilation-evaluator
       (memoize
        (lambda (env (options (js/obj)))
          (let ((language (oget options "language")))
            (set! language (or language default-language))
            (let ((compilation-env (or (.get compilation-map
                                             language)
                                       javascript-env)))
              (new CompilationEvaluator
                   env
                   compilation-env
                   options))))))
    :to "typescript")
 "let makeCompilationEvaluator: any = memoize(function (env: any, options: any = {}): any {
  let language: any = options['language'];
  language = language || defaultLanguage;
  let compilationEnv: any = compilationMap.get(language) || javascriptEnv;
  return new CompilationEvaluator(env, compilationEnv, options);
});"
 > (compile '(cond
              (foo
               (let ((x #t))
                 x))
              (else
               #f))
            :as "return")
 "if (foo) {
  let x = true;
  return x;
} else {
  return false;
}"

 :describe "let*"
 > (let* ((x 1))
     x)
 1

 :describe "let-values"
 > (let-values (((x y) (values 1 2)))
     (list x y))
 '(1 2)
 > (let-values (((x . y) (values 1 2)))
     (list x y))
 '(1 (2))
 > (compile '(let-values (((x y) (values 1 2)))
               (define z
                 (+ x y))))
 "let [x, y] = [1, 2];

let z = x + y;"
 > (compile '(let-values ((value (foo bar baz)))
               value)
            :as "return")
 "let value = foo(bar, baz);

return value;"
 > (compile '(let-values (((value) (foo bar baz)))
               value)
            :as "return")
 "let [value] = foo(bar, baz);

return value;"
 > (compile '(let-values (((value) (foo bar baz)))
               value)
            :as "return"
            :to "typescript")
 "let [value]: any[] = foo(bar, baz);

return value;"
 > (compile '(let-values (((x . fs) args))
               (.reduce fs (lambda (acc f) (f acc)) x)))
 "let [x, ...fs] = args;

fs.reduce(function (acc, f) {
  return f(acc);
}, x);"
 > (compile '(let-values (((x . fs) args))
               (.reduce fs (lambda (acc f) (f acc)) x))
            :to "typescript")
 "let [x, ...fs]: any[] = args;

fs.reduce(function (acc: any, f: any): any {
  return f(acc);
}, x);"
 > (compile '(let-values (((value1) (foo bar))
                          ((value2) (bar baz)))
               (list value1 value2))
            :as "return")
 "let [value1] = foo(bar);

let [value2] = bar(baz);

return [value1, value2];"
 > (compile '(begin
               value
               (let-values ((value (foo bar baz)))
                 value))
            :as "return")
 "value;

let value = foo(bar, baz);

return value;"

 :describe "let*-values"
 > (let*-values (((x y) (values 1 2))
                 ((w z) (values 3 4)))
     (list x y w z))
 '(1 2 3 4)
 > (compile '(let*-values (((x y) (values 1 2))
                           ((w z) (values 3 4)))
               (define z
                 (+ x y w z))))
 "let [x, y] = [1, 2];

let [w, z] = [3, 4];

let z = x + y + w + z;"

 :describe "lambda"
 > ((lambda (x) x) 1)
 1
 > ((lambda (x)
      x)
    "Lisp")
 "Lisp"
 > ((lambda ((x "Lisp"))
      x))
 "Lisp"
 > ((lambda ((x "Lisp"))
      x) "Scheme")
 "Scheme"
 > ((lambda x
      x)
    "Lisp")
 '("Lisp")
 > ((fn (x)
      x)
    1)
 1
 > ((λ (x)
      x)
    1)
 1
 > (compile '(lambda (x)
               x))
 "function (x) {
  return x;
};"
 > (compile '(lambda (x)
               x)
            :to "typescript")
 "function (x: any): any {
  return x;
};"
 > (compile '(lambda args
               args))
 "function (...args) {
  return args;
};"
 > (compile '(lambda (x . args)
               args))
 "function (x, ...args) {
  return args;
};"
 > (compile '(lambda (x y . args)
               args))
 "function (x, y, ...args) {
  return args;
};"
 > (compile '(lambda (x)
               (let ((x 1))
                 x)))
 "function (x) {
  {
    let x = 1;
    return x;
  }
};"
 > (compile '(lambda (x)
               (let ((y 1))
                 y)))
 "function (x) {
  let y = 1;
  return y;
};"
 > (compile '(lambda (given (surname "Smith"))
               (string-append
                "Hello, "
                given
                " "
                surname)))
 "function (given, surname = 'Smith') {
  return 'Hello, ' + given + ' ' + surname;
};"
 > (compile '(lambda (given (surname "Smith"))
               (string-append
                "Hello, "
                given
                " "
                surname))
            :to "typescript")
 "function (given: any, surname: any = 'Smith'): any {
  return 'Hello, ' + given + ' ' + surname;
};"
 > (compile '(lambda (arg (options (js/obj)))
               arg)
            :to "typescript")
 "function (arg: any, options: any = {}): any {
  return arg;
};"

 :describe "thunk"
 > (procedure? (thunk 1))
 #t
 > (thunk? (thunk 1))
 #t
 > (arity (thunk 1))
 0
 > ((thunk 1))
 1
 > ((thunk 1 2))
 2
 > ((thunk 1 2 3))
 3

 :describe "delay"
 > (force (delay 1))
 1
 > (force (delay 1 2))
 2
 > (force (delay 1 2 3))
 3

 :describe "lazy"
 > (force (lazy 1))
 1
 > (force (lazy 1 2))
 2
 > (force (lazy 1 2 3))
 3
 > (force (lazy (delay 1)))
 1
 > (force (lazy (lazy 1)))
 1
 > (force (lazy (lazy (lazy 1))))
 1

 :describe "promise?"
 > (promise? (delay 1))
 #t
 > (promise? (lazy 1))
 #t

 :describe "promise-forced?"
 > (promise-forced? (delay 1))
 #f
 > (let ((p (delay 1)))
     (force p)
     (promise-forced? p))
 #t

 :describe "apply"
 > (compile '(apply f args))
 "f(...args);"
 > (compile '(apply f x args))
 "f(x, ...args);"
 > (compile '(apply new Foo args))
 "new Foo(...args);"
 > (compile '(apply new Foo x y args))
 "new Foo(x, y, ...args);"
 > (compile '(apply (get-field method obj) args))
 "obj.method(...args);"
 > (compile '(apply (.-method obj) args))
 "obj.method(...args);"

 :describe "lexical scope"
 > ((lambda ()
      (define (K x)
        (lambda () x))
      ((K 42))))
 42
 > ((lambda ()
      (define incrementer #u)
      (let ((x 1))
        (set! incrementer
              (lambda ()
                (set! x (+ x 1))
                x)))
      (incrementer)))
 2
 > (let ((x 100)
         incrementer)
     (let ((x 1))
       (set! incrementer
             (lambda ()
               (set! x (+ x 1))
               x)))
     (incrementer)
     x)
 100

 :describe "begin"
 > (begin)
 #u
 > (compile '(begin x y z))
 "x;

y;

z;"
 > (compile '(begin x (begin y z)))
 "x;

y;

z;"
 > (compile `(begin x y)
            :as "expression")
 "x, y"
 > (compile '(begin x y z)
            :as "expression")
 "x, y, z"
 > (compile
    '(begin
       ;; Redefine core functions (nonsensically).
       (define (and x y)
         (or x y))
       (define (or x y) x)
       (and x (or y z))))
 "function and(x, y) {
  return or(x, y);
}

function or(x, y) {
  return x;
}

and(x, or(y, z));"

 :describe "begin0"
 > (begin0 1
     2)
 1

 :describe "if"
 > (if #t 1 2)
 1
 > (if #f 1 2)
 2
 > (if (< 1 2) 1 2)
 1
 > (if (> 2 1) 1 2)
 1
 > (compile '(if x
                 y
                 z))
 "if (x) {
  y;
} else {
  z;
}"
 > (compile '(if #t (foo) (bar)))
 "if (true) {
  foo();
} else {
  bar();
}"
 > (compile '(if #t (foo) (bar))
            :as "statement")
 "if (true) {
  foo();
} else {
  bar();
}"
 > (compile '(if #t (foo) (bar))
            :as "return")
 "if (true) {
  return foo();
} else {
  return bar();
}"
 > (compile '(if #t (foo) (bar)) :as "expression")
 "true ? foo() : bar()"
 > (compile '(if #t (foo) (bar) (baz)))
 "if (true) {
  foo();
} else {
  bar();
}"
 > (compile '(if x
                 y)
            :as "expression")
 "x ? y : undefined"
 > (compile '(if x y z)
            :as "expression")
 "x ? y : z"
 > (compile '(if x
                 y
                 z)
            :as "return")
 "if (x) {
  return y;
} else {
  return z;
}"
 > (compile '(if "foo"
                 "bar"
                 "baz")
            :as "expression")
 "'foo' ? 'bar' : 'baz'"
 > (compile '(if x
                 (begin
                   y
                   z)
                 w)
            :as "return")
 "if (x) {
  y;
  return z;
} else {
  return w;
}"
 > (compile '(if (set! x y)
                 z
                 w))
 "if ((x = y)) {
  z;
} else {
  w;
}"
 > (compile '(if (set! x y)
                 z
                 w)
            :as "return")
 "if ((x = y)) {
  return z;
} else {
  return w;
}"
 > (compile '(if (set!-values (x) y)
                 z
                 w))
 "if (([x] = y)) {
  z;
} else {
  w;
}"

 > (compile '(if (set!-fields (x) y)
                 z
                 w))
 "if (({x} = y)) {
  z;
} else {
  w;
}"

 :describe "when"
 > (when (< 1 2)
     1 2)
 2
 > (when (> 1 2)
     1 2)
 #u
 > (compile '(when x
               y
               z))
 "if (x) {
  y;
  z;
}"
 > (compile '(when (< 1 2)
               (foo)
               (bar)))
 "if (1 < 2) {
  foo();
  bar();
}"
 > (compile
    '(when (> (length args) 0)
       (set! args (.concat (.slice args 0 (- (length args) 1))
                           (aref args (- (length args) 1))))))
 "if (args.length > 0) {
  args = args.slice(0, args.length - 1).concat(args[args.length - 1]);
}"

 :describe "unless"
 > (unless (< 1 2)
     1 2)
 #u
 > (unless (> 1 2)
     1 2)
 2
 > (compile '(unless x
               y z))
 "if (!x) {
  y;
  z;
}"
 > (compile '(unless (> 1 2)
               (foo)
               (bar)))
 "if (!(1 > 2)) {
  foo();
  bar();
}"

 :describe "cond"
 > (cond
    (#f
     1)
    (else
     2))
 2
 > (cond
    (#t
     1)
    (#f
     2))
 1
 > (cond
    (#f
     1)
    (#t
     2))
 2
 > (cond
    (#f
     1)
    (#t
     2))
 2
 > (cond
    (1 => add1)
    (else
     3))
 2
 > (macroexpand-1 '(cond
                    (#f
                     (foo))
                    (else
                     (bar))))
 '(if #f
      (foo)
      (bar))
 > (macroexpand-1 '(cond
                    (#f
                     (foo)
                     (bar))
                    (else
                     (baz))))
 '(if #f
      (begin
        (foo)
        (bar))
      (baz))
 > (macroexpand-1 '(cond
                    (#f
                     (foo)
                     (bar))
                    (else
                     (baz)
                     (quux))))
 '(if #f
      (begin
        (foo)
        (bar))
      (begin
        (baz)
        (quux)))
 > (macroexpand-1 '(cond
                    (x
                     (foo))
                    (y
                     (bar))
                    (else
                     (baz))))
 '(if x
      (foo)
      (if y
          (bar)
          (baz)))
 > (compile '(cond
              (#f
               (foo))
              (else
               (bar))))
 "if (false) {
  foo();
} else {
  bar();
}"
 > (compile '(cond
              (x
               (foo))
              (y
               (bar))
              (else
               (baz))))
 "if (x) {
  foo();
} else if (y) {
  bar();
} else {
  baz();
}"
 > (compile '(cond
              (#f
               (foo))
              (else
               (bar)))
            :as "statement")
 "if (false) {
  foo();
} else {
  bar();
}"
 > (compile '(cond
              (x
               y))
            :as "return")
 "if (x) {
  return y;
}"
 > (compile '(cond
              (#f
               (foo))
              (else
               (bar)))
            :as "return")
 "if (false) {
  return foo();
} else {
  return bar();
}"
 > (compile '(cond
              (x
               y)
              (else
               z))
            :as "return")
 "if (x) {
  return y;
} else {
  return z;
}"
 > (compile '(cond
              ((set! x y)
               z)
              (else
               w))
            :as "return")
 "if ((x = y)) {
  return z;
} else {
  return w;
}"
 > (compile '(cond
              (x
               y))
            :as "expression")
 "x ? y : undefined"
 > (compile '(cond
              (x
               y)
              (else
               z))
            :as "expression")
 "x ? y : z"
 > (compile '(cond
              (#f
               (foo))
              (else
               (bar)))
            :as "expression")
 "false ? foo() : bar()"
 > (compile '(cond
              (x
               y)
              (else
               w
               z))
            :as "expression")
 "x ? y : (w, z)"
 > (compile '(cond
              (#t => y)
              (else
               #f)))
 "let _condVar;

if ((_condVar = true)) {
  y(_condVar);
} else {
  false;
}"
 > (compile '(begin
               (cond
                (#t => y)
                (else
                 #f))
               (cond
                (#t => y)
                (else
                 #f))))
 "let _condVar;

if ((_condVar = true)) {
  y(_condVar);
} else {
  false;
}

let _condVar1;

if ((_condVar1 = true)) {
  y(_condVar1);
} else {
  false;
}"

 :describe "="
 > (compile '(= 1 1))
 "1 === 1;"
 > (compile '(= x y))
 "x === y;"

 :describe "eq?"
 > (eq? #t #t)
 #t
 > (eq? #f #f)
 #t
 > (eq? #t #f)
 #f
 > (eq '_ '_)
 #t
 > (eq _ '_)
 #f
 > (compile '(eq? #t #t))
 "true === true;"

 :describe "equal?"
 > (equal? #t #t)
 #t
 > (equal? #f #f)
 #t
 > (equal? #t #f)
 #f
 > (equal? _ '_)
 #f
 > (equal? 1 1)
 #t
 > (equal? '() '())
 #t

 :describe "not"
 > (not #f)
 #t
 > (not #t)
 #f
 > (compile '(not x))
 "!x;"
 > (compile '(not (f x)))
 "!f(x);"
 > (compile '(not (= 1 2)))
 "1 !== 2;"
 > (compile '(not (> 1 2)))
 "!(1 > 2);"
 > (compile '(not (and x y)))
 "!(x && y);"

 :describe "and"
 > (and)
 #t
 > (and #t)
 #t
 > (and #t #t)
 #t
 > (and #f #f)
 #f
 > (and #f #t)
 #f
 > (and #t #f)
 #f
 > (compile '(and))
 "true;"
 > (compile '(and x))
 "x;"
 > (compile '(and #t #t))
 "true && true;"
 > (compile '(and x y))
 "x && y;"
 > (compile '(and x y z))
 "x && y && z;"
 > (compile '(and x y (or w z)))
 "x && y && (w || z);"
 > (compile '(and x y (w z)))
 "x && y && w(z);"
 > (compile '(and (not (f x)) (not (g y))))
 "!f(x) && !g(y);"

 :describe "or"
 > (or)
 #f
 > (or #t)
 #t
 > (or #t #t)
 #t
 > (or #f #f)
 #f
 > (or #f #t)
 #t
 > (or #t #f)
 #t
 > (or 1 2)
 1
 > (or #u 2)
 2
 > (compile '(or))
 "false;"
 > (compile '(or x))
 "x;"
 > (compile '(or #t #t))
 "true || true;"
 > (compile '(or x y))
 "x || y;"
 > (compile '(or x y z))
 "x || y || z;"

 :describe "bitwise-and"
 > (compile '(bitwise-and x y))
 "x & y;"
 > (compile '(bit-and x y))
 "x & y;"
 > (compile '(js/& x y))
 "x & y;"
 > (compile '(js/& x y z))
 "x & y & z;"

 :describe "bitwise-or"
 > (compile '(bitwise-or x y))
 "x | y;"
 > (compile '(bit-or x y))
 "x | y;"
 > (compile '(js/\| x y))
 "x | y;"
 > (compile '(js/\| x y z))
 "x | y | z;"

 :describe "bitwise-xor"
 > (compile '(bitwise-xor x y))
 "x ^ y;"
 > (compile '(bit-xor x y))
 "x ^ y;"
 > (compile '(js/^ x y))
 "x ^ y;"

 :describe "bitwise-not"
 > (compile '(bitwise-negation x))
 "~x;"
 > (compile '(bitwise-not x))
 "~x;"
 > (compile '(bit-not x))
 "~x;"
 > (compile '(js/~ x))
 "~x;"

 :describe "bitwise-shift-left"
 > (compile '(bitwise-shift-left x y))
 "x << y;"
 > (compile '(bit-shift-left x y))
 "x << y;"
 > (compile '(js/<< x y))
 "x << y;"

 :describe "bitwise-shift-right"
 > (compile '(bitwise-shift-right x y))
 "x >> y;"
 > (compile '(bit-shift-right x y))
 "x >> y;"
 > (compile '(js/>> x y))
 "x >> y;"

 :describe "unsigned-bitwise-shift-right"
 > (compile '(unsigned-bitwise-shift-right x y))
 "x >>> y;"
 > (compile '(unsigned-bit-shift-right x y))
 "x >>> y;"
 > (compile '(js/>>> x y))
 "x >>> y;"

 :describe "for"
 > (let ((result '()))
     (for ((x '(1 2 3)))
       (set! result (cons x result)))
     result)
 '(3 2 1)
 > ((lambda ()
      (define foo
        '(1 2 3 4))
      (define len
        (length foo))
      (for ((i (range 0 len)))
        (pop-right! foo))
      foo))
 '()
 > ((lambda ()
      (define foo
        '(1 2 3 4))
      (for ((i (range 0 (length foo))))
        (pop-right! foo))
      foo))
 '()
 > (compile '(for ((i (range 0 10)))
               (foo)))
 "for (let i = 0; i < 10; i++) {
  foo();
}"
 > (compile '(for ((i (range 0 10 2)))
               (foo)))
 "for (let i = 0; i < 10; i = i + 2) {
  foo();
}"
 > (compile '(for ((x lst))
               (foo)))
 "for (let x of lst) {
  foo();
}"
 > (compile '(for ((x '(1 2 3)))
               (foo)))
 "for (let x of [1, 2, 3]) {
  foo();
}"
 > (compile '(for ((x '(1 2 3)))
               (break)))
 "for (let x of [1, 2, 3]) {
  break;
}"
 > (compile '(for ((x '(1 2 3)))
               (continue)))
 "for (let x of [1, 2, 3]) {
  continue;
}"
 > (compile '(for ((x '(1 2 3)))
               (let ((x 1))
                 (display x))))
 "for (let x of [1, 2, 3]) {
  {
    let x = 1;
    console.log(x);
  }
}"
 > (compile '(for ((x '(1 2 3)))
               (let ((y 1))
                 (display x y))))
 "for (let x of [1, 2, 3]) {
  let y = 1;
  console.log(x, y);
}"
 > (compile '(for ((x '(1 2 3)))
               (let ((y 1))
                 (display y))
               (display x)))
 "for (let x of [1, 2, 3]) {
  let y = 1;
  console.log(y);
  console.log(x);
}"
 > (compile '(for ((i (range 0 len)))
               (foo)))
 "for (let i = 0; i < len; i++) {
  foo();
}"
 > (compile '(for ((i (range 0 (js/length foo))))
               (foo)))
 "let _end = foo.length;

for (let i = 0; i < _end; i++) {
  foo();
}"
 > (compile '(for ((x '(1 2 3)))
               (display x)))
 "for (let x of [1, 2, 3]) {
  console.log(x);
}"
 > (compile '(for ((i (range 0 10)))
               (display x)))
 "for (let i = 0; i < 10; i++) {
  console.log(x);
}"
 > (compile '(for ((i (range 0 10)))
               (display x))
            :to "typescript")
 "for (let i: any = 0; i < 10; i++) {
  console.log(x);
}"
 > (compile '(for ((i (range 1 10 2)))
               (display x)))
 "for (let i = 1; i < 10; i = i + 2) {
  console.log(x);
}"
 > (compile '(for ((i (range 10 1 -1)))
               (display x)))
 "for (let i = 10; i > 1; i--) {
  console.log(x);
}"
 > (compile '(for ((i (range 10 1 -2)))
               (display x)))
 "for (let i = 10; i > 1; i = i - 2) {
  console.log(x);
}"
 > (compile '(for ((i (range 0 (+ 1 1))))
               (display i)))
 "let _end = 1 + 1;

for (let i = 0; i < _end; i++) {
  console.log(i);
}"
 > (compile '(begin
               (for ((i (range 0 (+ 1 1))))
                 (display i))
               (for ((j (range 0 (+ 2 2))))
                 (display j))))
 "let _end = 1 + 1;

for (let i = 0; i < _end; i++) {
  console.log(i);
}

let _end1 = 2 + 2;

for (let j = 0; j < _end1; j++) {
  console.log(j);
}"
 > (compile '(for ((i (range (+ 1 1) (+ 2 2))))
               (display i)))
 "let _start = 1 + 1;

let _end = 2 + 2;

for (let i = _start; i < _end; i++) {
  console.log(i);
}"
 > (compile '(for ((i (range (+ 1 1) (+ 2 2))))
               (display i))
            :to "typescript")
 "let _start: any = 1 + 1;

let _end: any = 2 + 2;

for (let i: any = _start; i < _end; i++) {
  console.log(i);
}"
 > (compile '(let ((_start 0)
                   (_end 0))
               (for ((i (range (+ 1 1) (+ 2 2))))
                 (display i)))
            :to "typescript")
 "let _start: any = 0;

let _end: any = 0;

let _start1: any = 1 + 1;

let _end1: any = 2 + 2;

for (let i: any = _start1; i < _end1; i++) {
  console.log(i);
}"
 > (compile '(for ((i (range (+ 1 1) (+ 2 2))))
               (for ((j (range (+ 3 3) (+ 4 4))))
                 (display j)))
            :to "typescript")
 "let _start: any = 1 + 1;

let _end: any = 2 + 2;

for (let i: any = _start; i < _end; i++) {
  let _start1: any = 3 + 3;
  let _end1: any = 4 + 4;
  for (let j: any = _start1; j < _end1; j++) {
    console.log(j);
  }
}"
 > (compile '(define (foo)
               (for ((x '(1 2 3)))
                 (display x))))
 "function foo() {
  for (let x of [1, 2, 3]) {
    console.log(x);
  }
}"
 > (compile '(for ((i (range 0 10))
                   (j (range 0 10)))
               (foo)))
 "for (let i = 0, j = 0; (i < 10) && (j < 10); i++, j++) {
  foo();
}"
 > (compile '(for ((x foo)
                   (y bar))
               (foo)))
 "let _end = foo.length;

let _end1 = bar.length;

for (let i = 0, j = 0; (i < _end) && (j < _end1); i++, j++) {
  let x = foo[i];
  let y = bar[j];
  foo();
}"
 > (compile '(for ((x (foo))
                   (y (bar)))
               (foo)))
 "let _val = foo();

let _end = _val.length;

let _val1 = bar();

let _end1 = _val1.length;

for (let i = 0, j = 0; (i < _end) && (j < _end1); i++, j++) {
  let x = _val[i];
  let y = _val1[j];
  foo();
}"

 :describe "for-each"
 > (compile '(for-each (lambda (x)
                         x)
                       lst))
 "lst.forEach(function (x) {
  return x;
});"

 :describe "do"
 > (compile '(do ()
                 ((not (< (length result) 3)))
               (display result)))
 "while (result.length < 3) {
  console.log(result);
}"

 :describe "get-field"
 > (let ((obj (js/obj "foo" "bar")))
     (get-field foo obj))
 "bar"
 > (compile '(get-field foo obj))
 "obj.foo;"
 > (compile '(get-field foo-bar obj))
 "obj.fooBar;"
 > (compile '(get-field "foo" obj))
 "obj['foo'];"
 > (compile '(get-field "foo-bar" obj))
 "obj['foo-bar'];"
 > (compile '(get-field (foo-bar) obj))
 "obj[fooBar()];"
 > (compile '(get-field length arr))
 "arr.length;"

 :describe "set-field!"
 > (let ((obj (js/obj)))
     (set-field! foo-bar obj "baz")
     (get-field foo-bar obj))
 "baz"
 > (compile '(set-field! foo-bar obj "baz"))
 "obj.fooBar = 'baz';"
 > (compile '(set-field! 'foo-bar obj "baz"))
 "obj.fooBar = 'baz';"
 > (compile '(set-field! :foo-bar obj "baz"))
 "obj.fooBar = 'baz';"
 > (compile '(set-field! "foo-bar" obj "baz"))
 "obj['foo-bar'] = 'baz';"
 > (compile '(set-field! (foo-bar) obj "baz"))
 "obj[fooBar()] = 'baz';"
 > (compile
    '(set-field! def-method
                 generic-function
                 (lambda (arglist function-definition)
                   (let ((entry (list arglist function-definition)))
                     (push! (get-field methods generic-function) entry)
                     generic-function))))
 "genericFunction.defMethod = function (arglist, functionDefinition) {
  let entry = [arglist, functionDefinition];
  genericFunction.methods.unshift(entry);
  return genericFunction;
};"

 :describe "field-bound?"
 > (let ((obj (js/obj "foo" "bar")))
     (field-bound? foo obj))
 #t
 > (compile '(begin
               (define foo
                 (js/obj))
               (define bar
                 (field-bound? baz foo))))
 "let foo = {};

let bar = foo && ('baz' in foo);"
 > (compile '(begin
               (define foo
                 (js/obj))
               (define bar
                 (field-bound? baz-baz foo))))
 "let foo = {};

let bar = foo && ('bazBaz' in foo);"

 :describe "send"
 > (let ((obj (js/obj "add" (lambda (x y) (+ x y)))))
     (send obj add 1 1))
 2
 > (let ((obj (make-hash '(("foo" . "foo")))))
     (send obj has "foo"))
 #t
 > (let ((obj (make-hash '(("foo" . "foo")))))
     (send obj has "bar"))
 #f
 > (compile '(send obj m arg))
 "obj.m(arg);"
 > (compile '(send map get "foo"))
 "map.get('foo');"

 :describe "send/apply"
 > (let ((obj (make-hash '(("foo" . "foo")))))
     (send/apply obj has '("foo")))
 #t
 > (compile '(send/apply obj m args))
 "obj.m(...args);"
 > (compile '(send/apply map get foo))
 "map.get(...foo);"
 > (compile '(send/apply map get '("foo")))
 "map.get('foo');"

 :describe "is-a?"
 > (is-a? (new Map) Map)
 #t
 > (compile '(is-a? x Foo))
 "x instanceof Foo;"

 :describe "module"
 > (module foo bar
     (+ 1 1))
 2
 > (compile '(module m scheme
               (define (I x)
                 x)
               (define (K x y)
                 x)))
 "function I(x) {
  return x;
}

function K(x, y) {
  return x;
}"
 > (compile '(module m scheme
               (define I
                 (lambda (x)
                   x))
               (define K
                 (lambda (x y)
                   x))))
 "let I = function (x) {
  return x;
};

let K = function (x, y) {
  return x;
};"
 > (compile '(module m scheme
               (define (foo length)
                 length))
            :to "typescript")
 "function foo(length: any): any {
  return length;
}"
 > (compile '(module m scheme
               (define (foo (length : Number)) : Number
                 length))
            :to "typescript")
 "function foo(length: number): number {
  return length;
}"
 > (compile '(module m scheme
               (define truish #t)
               (define falsy (not truish))))
 "let truish = true;

let falsy = !truish;"
 > (compile
    '(module m scheme
       (require (only-in "foo"
                         and
                         or))
       (and x (or y z))))
 "import {
  and,
  or
} from 'foo';

and(x, or(y, z));"
 > (compile '(module m lisp
               (define x 1)
               (define y 2)))
 "let x = 1;

let y = 2;"
 > (compile '(module m lisp
               (define (js_ str)
                 (js/eval str))))
 "function js_(str) {
  return eval(str);
}"
 > (compile '(module m lisp
               (define (my-fn foldl f v l)
                 (foldl f v l))))
 "function myFn(foldl, f, v, l) {
  return foldl(f, v, l);
}"
 > (compile '(module m lisp
               (define (my-foldl-obj obj f v l)
                 (.foldl obj f v l))))
 "function myFoldlObj(obj, f, v, l) {
  return obj.foldl(f, v, l);
}"
 > (compile '(module m lisp
               (define-class Foo ()
                 (define/public (foldl f v l)
                   l))))
 "class Foo {
  foldl(f, v, l) {
    return l;
  }
}"
 > (compile '(module m lisp
               (define (my-pop lst x)
                 (pop! lst x))))
 "function myPop(lst, x) {
  return lst.shift();
}"
 > (compile '(module m lisp
               (define (my-pop-2 lst x)
                 (pop! (append lst) x))))
 "function myPop2(lst, x) {
  return [...lst].shift();
}"
 > (compile '(module m lisp
               (define (my-pop-right lst x)
                 (pop-right! lst x))))
 "function myPopRight(lst, x) {
  return lst.pop();
}"
 > (compile '(module m lisp
               (define (my-pop-right-2 lst x)
                 (pop-right! (append lst) x))))
 "function myPopRight2(lst, x) {
  return [...lst].pop();
}"
 > (compile '(module m lisp
               (define (my-push lst x)
                 (push! lst x))))
 "function myPush(lst, x) {
  lst.unshift(x);
  return lst;
}"
 > (compile '(module m lisp
               (define (my-push-2 lst x)
                 (push! (append lst) x))))
 "function myPush2(lst, x) {
  let arr = [...lst];
  arr.unshift(x);
  return arr;
}"
 > (compile '(module m lisp
               (define (my-push-3 lst x)
                 (push! lst x)
                 lst)))
 "function myPush3(lst, x) {
  lst.unshift(x);
  return lst;
}"
 > (compile '(module m lisp
               (define (my-push-right lst x)
                 (push-right! lst x))))
 "function myPushRight(lst, x) {
  lst.push(x);
  return lst;
}"
 > (compile '(module m lisp
               (define (my-push-right-2 lst x)
                 (push-right! (append lst) x))))
 "function myPushRight2(lst, x) {
  let arr = [...lst];
  arr.push(x);
  return arr;
}"
 > (compile '(module m lisp
               (define (my-push-right-3 lst x)
                 (push-right! lst x)
                 lst)))
 "function myPushRight3(lst, x) {
  lst.push(x);
  return lst;
}"

 :describe "call/cc"
 > (+ 5
      (call/cc
       (lambda (x)
         (* 10 3))))
 35
 > (+ 5
      (call/cc
       (lambda (x)
         (* 10 (x 3)))))
 8
 > (+ 5 (call/cc
         (lambda (x)
           (x 10)
           3)))
 15
 > (+ 5
      (call/cc
       (lambda (x)
         (x 10)
         (error "error"))))
 15
 > (it "(try ... (+ 5 (call/cc (lambda (x) (error ...)))) ...)"
       (let ((result 0))
         (try
           (set! result
                 (+ 5 (call/cc
                       (lambda (x)
                         (error "error")))))
           (catch Object e))
         result))
 0

 :describe "define-values"
 > ((lambda ()
      (define-values (x y)
        (values 1 2))))
 #u
 > ((lambda ()
      (define-values (x y)
        (values 1 2))
      (list x y)))
 '(1 2)
 > ((lambda ()
      (define-values (x y)
        (values 1 2))
      x))
 1
 > (compile
    '(define-values value
       (foo bar baz)))
 "let value = foo(bar, baz);"
 > (compile
    '(define-values (value)
       (foo bar baz)))
 "let [value] = foo(bar, baz);"
 > (compile
    '(define-values (value)
       (foo bar baz))
    :to "typescript")
 "let [value]: any[] = foo(bar, baz);"
 > (compile
    '(define-values (#f #f value)
       (foo bar baz))
    :to "typescript")
 "let [, , value]: any[] = foo(bar, baz);"
 > (compile
    '(define-values (_ _ value)
       (foo bar baz))
    :to "typescript")
 "let [, , value]: any[] = foo(bar, baz);"
 > (compile
    '(define-values (_ __ value)
       :hole-marker __
       (foo bar baz))
    :to "typescript")
 "let [_, , value]: any[] = foo(bar, baz);"
 > (compile
    '(module m scheme
       (define (foo)
         (define xs
           '(1 2 3 4))
         (define-values (x . rest)
           xs)
         (append rest '(5))))
    :to "typescript")
 "function foo(): any {
  let xs: any = [1, 2, 3, 4];
  let [x, ...rest]: any[] = xs;
  return [...rest, 5];
}"

 :describe "set!"
 > (compile '(set! x 1))
 "x = 1;"
 > (compile '(set! (aref args 0) 1))
 "args[0] = 1;"
 > (compile '(set! x (add1 x))
            :as "expression")
 "++x"
 > (compile '(set! x (sub1 x))
            :as "expression")
 "--x"
 > (compile '(set! x (+ x 1))
            :as "expression")
 "++x"
 > (compile '(set! x (+ x 1)))
 "x++;"
 > (compile '(set! x (+ x 1))
            :as "return")
 "return ++x;"

 :describe "set!-values"
 > (let (x y)
     (set!-values (x y) (values 1 2))
     x)
 1
 > (let (x y)
     (set!-values (x y) (values 1 2))
     (list x y))
 '(1 2)
 > (compile '(set!-values (x y) (values 1 2)))
 "[x, y] = [1, 2];"
 > (compile
    '(set!-values (value) (foo bar baz)))
 "[value] = foo(bar, baz);"
 > (compile
    '(set!-values (_ value) (foo bar baz)))
 "[, value] = foo(bar, baz);"
 > (compile
    '(set!-values (_ __ value)
                  :hole-marker __
                  (foo bar baz)))
 "[_, , value] = foo(bar, baz);"

 :describe "hash"
 > (hash)
 (new Map)
 > (hash '(("foo" . "bar")))
 (new Map '(("foo" "bar")))
 > (compile '(hash))
 "new Map();"
 > (compile '(hash '(("foo" . "bar"))))
 "new Map([['foo', 'bar']]);"

 :describe "make-hash"
 > (make-hash)
 (new Map)
 > (make-hash '(("foo" . "bar")))
 (new Map '(("foo" "bar")))
 > (apply new make-hash '())
 (new Map)
 > (compile '(make-hash))
 "new Map();"
 > (compile '(make-hash '(("foo" . "bar"))))
 "new Map([['foo', 'bar']]);"
 > (compile '(make-hash
              '(("foo" . "bar")
                ("baz" . "quux"))))
 "new Map([['foo', 'bar'], ['baz', 'quux']]);"
 > (compile '(make-hash
              '(("foo" "bar")
                ("baz" "quux"))))
 "new Map([['foo', ['bar']], ['baz', ['quux']]]);"
 > (compile '(make-hash
              `(("foo" . "bar")
                ("baz" . "quux"))))
 "new Map([['foo', 'bar'], ['baz', 'quux']]);"
 > (compile '(make-hash
              `(("foo" . "bar")
                ("baz" . "quux")
                (unquote-splicing
                 (hash->list xyzzy)))))
 "new Map([['foo', 'bar'], ['baz', 'quux'], ...xyzzy.entries()]);"
 > (compile '(make-hash
              `(("foo" "bar")
                ("baz" "quux"))))
 "new Map([['foo', ['bar']], ['baz', ['quux']]]);"

 :describe "hash?"
 > (hash? (make-hash))
 #t
 > (hash? 0)
 #f
 > (compile '(hash? x))
 "x instanceof Map;"

 :describe "hash-clear"
 > (hash-clear
    (make-hash
     '(("foo" . "bar"))))
 (new Map)

 :describe "hash-clear!"
 > (let ((ht (make-hash '(("foo" . "bar")))))
     (hash-clear! ht)
     ht)
 (new Map)
 > (compile '(hash-clear! x))
 "x.clear();"

 :describe "hash-copy"
 > (hash-copy
    (make-hash
     '(("foo" . "bar"))))
 (new Map '(("foo" "bar")))
 > (compile '(hash-copy x))
 "new Map(x);"

 :describe "hash-keys"
 > (hash-keys
    (make-hash
     '(("foo" . "bar"))))
 '("foo")
 > (compile '(hash-keys x))
 "[...x.keys()];"

 :describe "hash-values"
 > (hash-values
    (make-hash
     '(("foo" . "bar"))))
 '("bar")
 > (compile '(hash-values x))
 "[...x.values()];"

 :describe "hash->list"
 > (hash->list
    (make-hash
     '(("foo" . "bar"))))
 '(("foo" . "bar"))

 :describe "hash-set"
 > (hash-set
    (make-hash)
    "foo"
    "bar")
 (new Map
      '(("foo" "bar")))

 :describe "hash-set!"
 > (let ((ht (make-hash)))
     (hash-set! ht "foo" "bar")
     ht)
 (new Map
      '(("foo" "bar")))
 > (compile '(hash-set! ht key val))
 "ht.set(key, val);"

 :describe "hash-ref"
 > (hash-ref
    (make-hash
     '(("foo" . "bar")))
    "foo")
 "bar"
 > (hash-ref (make-hash) "quux" #f)
 #f
 > (compile '(hash-ref ht "foo"))
 "ht.get('foo');"

 :describe "hash-has-key?"
 > (hash-has-key?
    (make-hash
     '(("foo" . "bar")))
    "foo")
 #t
 > (hash-has-key? (make-hash) "quux")
 #f
 > (compile '(hash-has-key? ht "quux"))
 "ht.has('quux');"

 :describe "+"
 > (+)
 0
 > (+ 1)
 1
 > (+ 1 2)
 3
 > (+ 2 2)
 4
 > (+ 1 2 3)
 6
 > (+ 1 2 4)
 7
 > (+ (+ 1 1) (+ 1 1))
 4
 > (let ((x 2))
     (+ x x))
 4
 > (apply + '(1 2))
 3
 > (compile '(+ 1))
 "1;"
 > (compile '(+ 1 1))
 "1 + 1;"
 > (compile '(+ 1 1 1))
 "1 + 1 + 1;"
 > (compile '(+ x 1))
 "x + 1;"
 > (compile '(+ x 1 2))
 "x + 1 + 2;"

 :describe "-"
 > (-)
 0
 > (- 1)
 -1
 > (- 1 2)
 -1
 > (- 1 2 3)
 -4
 > (- 1 2 4)
 -5
 > (compile '(- 1))
 "-1;"
 > (compile '(- 1 1))
 "1 - 1;"
 > (compile '(- 1 1 1))
 "1 - 1 - 1;"
 > (compile '(- x))
 "-x;"
 > (compile '(- x 1))
 "x - 1;"
 > (compile '(- x 1 2))
 "x - 1 - 2;"

 :describe "*"
 > (*)
 1
 > (* 1)
 1
 > (* 1 2)
 2
 > (* 1 2 3)
 6
 > (* 1 2 4)
 8
 > (compile '(* 1 1))
 "1 * 1;"
 > (compile '(* 1 1 1))
 "1 * 1 * 1;"

 :describe "/"
 > (/)
 #u
 > (/ 1)
 1
 > (/ 1 2)
 0.5
 > (/ 1 2 3)
 (/ 1 2 3)
 > (/ 1 2 4)
 0.125
 > (compile '(/ 1 2))
 "1 / 2;"
 > (compile '(/ 1 2 4))
 "1 / 2 / 4;"

 :describe "<"
 > (< 1)
 #t
 > (< 1 2)
 #t
 > (< 2 1)
 #f
 > (< 1 2 3)
 #t
 > (< 2 1 3)
 #f
 > (< 1 3 2)
 #f
 > (funcall < 1 2)
 #t
 > (funcall < 2 1)
 #f
 > (funcall < 1 2 3)
 #t
 > (funcall < 2 1 3)
 #f
 > (funcall < 1 3 2)
 #f
 > (compile '(< 1))
 "true;"
 > (compile '(< 1 2))
 "1 < 2;"
 > (compile '(< x y))
 "x < y;"
 > (compile '(< 1 2 3))
 "(1 < 2) && (2 < 3);"
 > (compile '(< x y z))
 "(x < y) && (y < z);"

 :describe "<="
 > (<= 1 2)
 #t
 > (<= 2 1)
 #f
 > (<= 1 2 3)
 #t
 > (<= 1 1 2)
 #t
 > (<= 2 1 3)
 #f
 > (<= 1 3 2)
 #f
 > (funcall <= 1 2)
 #t
 > (funcall <= 2 1)
 #f
 > (funcall <= 1 2 3)
 #t
 > (funcall <= 1 1 2)
 #t
 > (funcall <= 2 1 3)
 #f
 > (funcall <= 1 3 2)
 #f
 > (compile '(<= x y))
 "x <= y;"
 > (compile '(<= x y z))
 "(x <= y) && (y <= z);"

 :describe ">"
 > (> 1)
 #t
 > (> 2 1)
 #t
 > (> 1 2)
 #f
 > (> 3 2 1)
 #t
 > (> 1 2 3)
 #f
 > (> 2 1 3)
 #f
 > (> 1 3 2)
 #f
 > (funcall > 2 1)
 #t
 > (funcall > 1 2)
 #f
 > (funcall > 3 2 1)
 #t
 > (funcall > 1 2 3)
 #f
 > (funcall > 2 1 3)
 #f
 > (funcall > 1 3 2)
 #f
 > (compile '(> 1))
 "true;"
 > (compile '(> 2 1))
 "2 > 1;"
 > (compile '(> x y))
 "x > y;"
 > (compile '(> 3 2 1))
 "(3 > 2) && (2 > 1);"
 > (compile '(> x y z))
 "(x > y) && (y > z);"

 :describe ">="
 > (>= 1)
 #t
 > (>= 2 1)
 #t
 > (>= 2 2)
 #t
 > (>= 1 2)
 #f
 > (>= 3 2 1)
 #t
 > (>= 3 2 2)
 #t
 > (>= 1 2 3)
 #f
 > (>= 2 1 3)
 #f
 > (>= 1 3 2)
 #f
 > (funcall >= 2 1)
 #t
 > (funcall >= 2 2)
 #t
 > (funcall >= 1 2)
 #f
 > (funcall >= 3 2 1)
 #t
 > (funcall >= 3 2 2)
 #t
 > (funcall >= 1 2 3)
 #f
 > (funcall >= 2 1 3)
 #f
 > (funcall >= 1 3 2)
 #f
 > (compile '(>= x y))
 "x >= y;"
 > (compile '(>= x y z))
 "(x >= y) && (y >= z);"

 :describe "mod"
 > (compile '(mod x y))
 "x % y;"

 :describe "abs"
 > (abs 1)
 1
 > (abs -1)
 1
 > (compile '(abs x))
 "Math.abs(x);"

 :describe "range"
 > (range 1 2)
 '(1)
 > (range 10)
 '(0 1 2 3 4 5 6 7 8 9)
 > (range 10 20)
 '(10 11 12 13 14 15 16 17 18 19)
 > (range 20 40 2)
 '(20 22 24 26 28 30 32 34 36 38)
 > (range 20 10 -1)
 '(20 19 18 17 16 15 14 13 12 11)
 > (range 10 15 1.5)
 '(10 11.5 13.0 14.5)

 :describe "member"
 > (member 2 '(1 2 3 4))
 '(2 3 4)
 > (member 9 '(1 2 3 4))
 #f
 > (member 5
           '(3 5 1 7 2 9)
           (lambda (x y)
             (< x y)))
 '(7 2 9)

 :describe "take"
 > (take '(1 2 3) 0)
 '()
 > (take '(1 2 3) 1)
 '(1)
 > (take '(1 2 3) 2)
 '(1 2)
 > (take '(1 2 3) 3)
 '(1 2 3)
 > (funcall take '(1 2 3) 0)
 '()
 > (funcall take '(1 2 3) 1)
 '(1)
 > (funcall take '(1 2 3) 2)
 '(1 2)
 > (funcall take '(1 2 3) 3)
 '(1 2 3)
 > (compile '(take lst 0))
 "[];"
 > (compile '(take lst 1))
 "lst.slice(0, -(lst.length - 1) || undefined);"
 > (compile '(define x
               (take lst 1)))
 "let x = lst.slice(0, -(lst.length - 1) || undefined);"
 > (compile '(let ((n 1))
               (take lst n)))
 "let n = 1;

lst.slice(0, -(lst.length - n) || undefined);"
 > (compile '(module m scheme
               (let ((n 1))
                 (take lst n)))
            :fdottedlists #f)
 "let n = 1;

lst.slice(0, -(lst.length - n) || undefined);"
 > (compile '(module m scheme
               (let ((n 1))
                 (take lst n)))
            :fdottedlists #t)
 "import {
  take
} from 'roselisp';

let n = 1;

take(lst, n);"

 :describe "drop"
 > (drop '(1 2 3 4) 0)
 '(1 2 3 4)
 > (drop '(1 2 3 4) 1)
 '(2 3 4)
 > (compile '(drop lst 0))
 "lst;"
 > (compile '(drop lst 1))
 "lst.slice(1);"
 > (compile '(drop lst n))
 "lst.slice(n);"
 > (compile '(module m scheme
               (drop lst n))
            :fdottedlists #f)
 "lst.slice(n);"
 > (compile '(module m scheme
               (drop lst n))
            :fdottedlists #t)
 "import {
  drop
} from 'roselisp';

drop(lst, n);"

 :describe "drop-right"
 > (drop-right '(1 2 3 4) 0)
 '(1 2 3 4)
 > (drop-right '(1 2 3 4) 1)
 '(1 2 3)
 > (compile '(drop-right lst 0))
 "lst;"
 > (compile '(drop-right lst 1))
 "lst.slice(0, -1);"
 > (compile '(drop-right lst n))
 "lst.slice(0, -n || undefined);"
 > (compile '(module m scheme
               (drop-right lst n))
            :fdottedlists #f)
 "lst.slice(0, -n || undefined);"
 > (compile '(module m scheme
               (drop-right lst n))
            :fdottedlists #t)
 "import {
  dropRight
} from 'roselisp';

dropRight(lst, n);"

 :describe "reverse"
 > (reverse '())
 '()
 > (reverse '(1))
 '(1)
 > (reverse '(1 2))
 '(2 1)
 > (reverse '(1 2 3))
 '(3 2 1)
 > (compile '(reverse lst))
 "[...lst].reverse();"
 > (compile '(module m scheme
               (reverse lst))
            :fdottedlists #f)
 "[...lst].reverse();"
 > (compile '(module m scheme
               (reverse lst))
            :fdottedlists #t)
 "import {
  reverse
} from 'roselisp';

reverse(lst);"

 :describe "map"
 > (map list '(1 2))
 '((1) (2))
 > ((lambda ()
      (define (fact n)
        (if (< n 2)
            1
            (* n (fact (- n 1)))))
      (map fact '(1 2 3 4 5 6))))
 '(1 2 6 24 120 720)
 > (compile '(map f lst))
 "lst.map(function (x) {
  return f(x);
});"
 > (compile '(map (lambda (x) x) lst))
 "lst.map(function (x) {
  return x;
});"
 > (compile '(map f x))
 "x.map(function (x) {
  return f(x);
});"
 > (compile '(map (lambda (x) x) x))
 "x.map(function (x) {
  return x;
});"
 ;; We can't compile this to `x.map(g(y))` because
 ;; we need to ensure that the function is only
 ;; called with a single argument, and JavaScript's
 ;; `.map()` method passes multiple arguments.
 > (compile '(map (g y) x))
 "x.map((function (f) {
  return function (x) {
    return f(x);
  };
})(g(y)));"

 :describe "foldl"
 > (foldl cons '() '(1 2 3 4))
 '(4 3 2 1)
 > (compile '(foldl (lambda (x acc) x) v lst))
 "lst.reduce(function (acc, x) {
  return x;
}, v);"
 > (compile '(foldl f v lst))
 "lst.reduce(function (acc, x) {
  return f(x, acc);
}, v);"
 > (compile '(foldl f v l))
 "l.reduce(function (acc, x) {
  return f(x, acc);
}, v);"
 > (compile '(foldl (lambda (x acc)
                      (f x acc))
                    v
                    l))
 "l.reduce(function (acc, x) {
  return f(x, acc);
}, v);"
 > (compile '(foldl + 0 '(1 2 3 4)))
 "[1, 2, 3, 4].reduce(function (acc, x) {
  return x + acc;
}, 0);"

 :describe "foldr"
 > (foldr cons '() '(1 2 3 4))
 '(1 2 3 4)
 > (foldr (lambda (v l)
            (cons (add1 v) l))
          '()
          '(1 2 3 4))
 '(2 3 4 5)
 > (compile '(foldr (lambda (x acc) x) v lst))
 "lst.reduceRight(function (acc, x) {
  return x;
}, v);"
 > (compile '(foldr f v lst))
 "lst.reduceRight(function (acc, x) {
  return f(x, acc);
}, v);"
 > (compile '(foldr (f g) v lst))
 "lst.reduceRight((function (f) {
  return function (x, y) {
    return f(y, x);
  };
})(f(g)), v);"

 :describe "filter"
 > (filter string? '("foo" 1 2 3))
 '("foo")
 > (compile '(filter f lst))
 "lst.filter(f);"

 :describe "string?"
 > (string? "foo")
 #t
 > (string? 1)
 #f
 > (string? (js/obj))
 #f
 > (string? (list "foo"))
 #f
 > (string? (js/obj "foo" ""))
 #f
 > (string? (js/obj "foo" (quote ())))
 #f
 > (string? (js/obj "foo" (js/obj)))
 #f
 > (string? (js/obj "foo" "foo"))
 #f
 > (string? (quote ()))
 #f
 > (compile '(string? x))
 "typeof x === 'string';"

 :describe "string-length"
 > (string-length "foo")
 3
 > (compile '(string-length x))
 "x.length;"

 :describe "string-append"
 > (string-append)
 ""
 > (string-append "foo")
 "foo"
 > (string-append "foo" "bar")
 "foobar"
 > (apply string-append '("foo" "bar"))
 "foobar"
 > (compile '(string-append "foo"))
 "'foo';"
 > (compile '(string-append "foo" "bar"))
 "'foo' + 'bar';"
 > (compile '(string-append "foo" "bar" "baz"))
 "'foo' + 'bar' + 'baz';"

 :describe "string-join"
 > (string-join '("foo" "bar"))
 "foo bar"
 > (string-join '("foo" "bar") ",")
 "foo,bar"
 > (compile '(string-join '("foo" "bar") ","))
 "['foo', 'bar'].join(',');"

 :describe "string-split"
 > (string-split "foo bar  baz")
 '("foo" "bar" "baz")
 > (string-split "foo,bar,baz" ",")
 '("foo" "bar" "baz")
 > (string-split "foo, bar, baz" ", ")
 '("foo" "bar" "baz")
 > (string-split "foo\nbar\nbaz" "\n")
 '("foo" "bar" "baz")
 > (compile '(string-split "foo,bar,baz" ","))
 "'foo,bar,baz'.split(',');"

 :describe "string-trim"
 > (string-trim "  foo bar  baz  ")
 "foo bar  baz"
 > (string-trim "  foo bar  baz \r\n\t")
 "foo bar  baz"
 > (compile '(string-trim x))
 "x.trim();"

 :describe "string-upcase"
 > (string-upcase "foo")
 "FOO"
 > (compile '(string-upcase x))
 "x.toUpperCase();"

 :describe "string-downcase"
 > (string-downcase "FOO")
 "foo"
 > (compile '(string-downcase x))
 "x.toLowerCase();"

 :describe "substring"
 > (substring "Apple" 1 3)
 "pp"
 > (substring "Apple" 1)
 "pple"
 > (compile '(substring str i))
 "str.substring(i);"
 > (compile '(substring str i j))
 "str.substring(i, j);"

 :describe "match"
 > (match 1
     (x
      x))
 1
 > (match 1
     ((var x)
      x))
 1
 > (match "foo"
     ("foo"
      1))
 1
 > (match "foo"
     ((not "bar")
      1))
 1
 > (match 'a
     ('a
      1))
 1
 > (match '(1 2 3)
     ((list a b c)
      (list a b c)))
 '(1 2 3)
 > (match '(1 2 3)
     ((list a b c) a))
 1
 > (match '(1 2 3)
     ((list _ _ a) a))
 3
 > (match '(1 2 3)
     ((list x y ...) y))
 '(2 3)
 > (compile '(match "foo"
               ("foo"
                1)))
 "if ('foo' === 'foo') {
  1;
}"
 > (compile '(match "foo"
               ("foo"
                1)
               (_
                2)))
 "if ('foo' === 'foo') {
  1;
} else {
  2;
}"
 > (match '((1) 2 3)
     ((list (list a) b c)
      (list a b c)))
 '(1 2 3)
 > (compile '(match "foo"
               ((not "bar")
                1)))
 "if ('foo' !== 'bar') {
  1;
}"
 > (compile '(match 1
               (x
                x)))
 "let x = 1;

x;"
 > (compile '(match 1
               ((var x)
                x)))
 "let x = 1;

x;"
 > (compile '(match 'a
               ('a
                1)))
 "let matchVal = Symbol.for('a');

if (matchVal === Symbol.for('a')) {
  1;
}"
 > (compile '(match '(1 2 3)
               ((list a b c) a)))
 "let matchVal = [1, 2, 3];

if (Array.isArray(matchVal) && (matchVal.length === 3)) {
  let [a, b, c] = matchVal;
  a;
}"
 > (compile '(match '(1 2 3)
               ((list _ _ a) a)))
 "let matchVal = [1, 2, 3];

if (Array.isArray(matchVal) && (matchVal.length === 3)) {
  let [, , a] = matchVal;
  a;
}"
 > (compile '(match '(1 2 3)
               ((list x ...) x)))
 "let matchVal = [1, 2, 3];

if (Array.isArray(matchVal) && (matchVal.length >= 0)) {
  let x = matchVal;
  x;
}"
 > (compile '(match '(1 2 3)
               ((list x y ...) y)))
 "let matchVal = [1, 2, 3];

if (Array.isArray(matchVal) && (matchVal.length >= 1)) {
  let [x, ...y] = matchVal;
  y;
}"
 > (compile '(match '(1 2 3)
               ((list* x) x)))
 "let matchVal = [1, 2, 3];

if (Array.isArray(matchVal) && (matchVal.length >= 0)) {
  let x = matchVal;
  x;
}"
 > (compile '(match '(1 2 3)
               ((list* x y) y)))
 "let matchVal = [1, 2, 3];

if (Array.isArray(matchVal) && (matchVal.length >= 1)) {
  let [x, ...y] = matchVal;
  y;
}"
 > (compile
    '(match '(1 2 3)
       ((cons x y)
        (list x y))))
 "let matchVal = [1, 2, 3];

if (Array.isArray(matchVal) && (matchVal.length >= 1)) {
  let [x, ...y] = matchVal;
  [x, y];
}"
 > (compile '(match '(1 2 3)
               ((list a b c)
                (list a b c))))
 "let matchVal = [1, 2, 3];

if (Array.isArray(matchVal) && (matchVal.length === 3)) {
  let [a, b, c] = matchVal;
  [a, b, c];
}"
 > (compile '(match '((1) 2 3)
               ((list (list a) b c)
                (list a b c))))
 "let matchVal = [[1], 2, 3];

if (Array.isArray(matchVal) && (matchVal.length === 3) && Array.isArray(matchVal[0]) && (matchVal[0].length === 1)) {
  let [[a], b, c] = matchVal;
  [a, b, c];
}"
 > (compile
    '(match exp
       ((list (list 'foo x) y ...)
        (list x y))
       (_
        exp)))
 "if (Array.isArray(exp) && (exp.length >= 1) && Array.isArray(exp[0]) && (exp[0].length === 2) && (exp[0][0] === Symbol.for('foo'))) {
  let [[, x], ...y] = exp;
  [x, y];
} else {
  exp;
}"
 > (compile
    '(match exp
       ((and _ ())
        #t)))
 "if (Array.isArray(exp) && (exp.length === 0)) {
  true;
}"
 > (compile
    '(match exp
       ((or _ ())
        #t)))
 "true;"
 > (compile
    '(match "foo"
       ((regexp "foo")
        #t)))
 "if ('foo'.match(new RegExp('foo'))) {
  true;
}"
 > (compile
    '(match "foo"
       ((? string?)
        #t)))
 "if (typeof 'foo' === 'string') {
  true;
}"
 > (compile
    '(match "foo"
       ((? string? "foo")
        #t)))
 "if ((typeof 'foo' === 'string') && ('foo' === 'foo')) {
  true;
}"
 > (compile
    '(match "foo"
       ((app string-length 3)
        #t)))
 "if ('foo'.length === 3) {
  true;
}"
 > (compile
    '(match "foo"
       ((app string-length (? number?) 3)
        #t)))
 "if ((() => {
  let patternMatchVal = 'foo'.length;
  return Number.isFinite(patternMatchVal) && (patternMatchVal === 3);
})()) {
  true;
}"

 :describe "assert"
 > (compile '(assert #t))
 "console.assert(true);"
 > (compile '(assert #t "test"))
 "console.assert(true, 'test');"

 :describe "display"
 > (compile '(display #t))
 "console.log(true);"
 > (compile '(display #t "test"))
 "console.log(true, 'test');")
