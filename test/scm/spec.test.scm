;;; # Test specification
;;;
;;; Tests expressed as a Roselisp REPL session.

(require (only-in "./test-util"
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 :repl #t

 ;; `#t`
 > (describe "#t")
 _
 > #t
 #t
 > '#t
 #t
 > (compile #t)
 "true;"

 ;; `#f`
 > (describe "#f")
 _
 > #f
 #f
 > '#f
 #f
 > (compile #f)
 "false;"

 ;; `#u`
 > (describe "#u")
 _
 > #u
 #u
 > '#u
 #u
 > undefined
 #u
 > (compile #u)
 "undefined;"

 ;; `#n`
 > (describe "#n")
 _
 > #n
 #n
 > '#n
 #n
 > js-null
 #n
 > js/null
 #n
 > (compile #n)
 "null;"

 ;; `true?`
 > (describe "true?")
 _
 > (true? #t)
 #t
 > (true? #f)
 #f
 > (true? #u)
 #f
 > (true? #n)
 #f
 > (true? '())
 #t

 ;; `false?`
 > (describe "false?")
 _
 > (false? #t)
 #f
 > (false? #f)
 #t
 > (false? #u)
 #t
 > (false? #n)
 #t
 > (false? '())
 #f

 ;; `nil`
 > (describe "nil")
 _
 > nil
 '()
 > (list? nil)
 #t
 > (length nil)
 0

 ;; `null`
 > (describe "null")
 _
 > null
 '()
 > (listp null)
 #t
 > (length null)
 0

 ;; Numbers
 > (describe "Numbers")
 _
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

 ;; Strings
 > (describe "Strings")
 _
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

 ;; Symbols
 > (describe "Symbols")
 _
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

 ;; Keywords
 > (describe "Keywords")
 _
 > :foo
 ':foo
 > ':foo
 ':foo
 > (keyword? ':foo)
 #t
 > (keyword? 'foo)
 #f
 > (compile ':foo)
 "Symbol.for(':foo');"

 ;; `symbol?`
 > (describe "symbol?")
 _
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

 ;; `symbol->string`
 > (describe "symbol->string")
 _
 > (symbol->string 'foo)
 "foo"

 ;; `intern`
 > (describe "intern")
 _
 > (intern "foo")
 'foo

 ;; `gensym`
 > (describe "gensym")
 _
 > (symbol? (gensym "foo"))
 #t
 > (compile '(gensym "foo"))
 "Symbol('foo');"

 ;; Cons cells
 > (describe "Cons cells")
 _
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

 ;; Lists
 > (describe "Lists")
 _
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
 > (compile ''())
 "[];"
 > (compile '(list))
 "[];"
 > (compile ''(1))
 "[1];"
 > (compile '(list 1))
 "[1];"
 > (compile ''(1 2))
 "[1, 2];"
 > (compile '(list 1 2))
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

 ;; `quote`
 > (describe "quote")
 _
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
 > (compile '(quote (1)))
 "[1];"
 > (compile '(quote (1 2)))
 "[1, 2];"
 > (compile '(quote ((1 2) (3 4))))
 "[[1, 2], [3, 4]];"

 ;; `quasiquote`
 > (describe "quasiquote")
 _
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
 > (compile '(quasiquote foo))
 "Symbol.for('foo');"
 > (compile '(quasiquote (,1)))
 "[1];"
 > (compile '(quasiquote ((,1))))
 "[[1]];"
 > (compile '(quasiquote (,@(list 1 2 3))))
 "[...[1, 2, 3]];"

 ;; Variables
 > (describe "Variables")
 _
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

 ;; Function calls
 > (describe "Function calls")
 _
 > (let ((identity (lambda (x) x)))
     (identity "foo"))
 "foo"
 > (let ((my-add (lambda (x y) (+ x y))))
     (my-add 1 2))
 3
 > (let ((my-add (lambda (x y z) (+ x y z))))
     (my-add 1 2 3))
 6

 ;; `define`
 > (describe "define")
 _
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

 ;; `defun`
 > (describe "defun")
 _
 > ((lambda ()
      (defun my-add (x y)
        (+ x y))
      (my-add 2 3)))
 5
 > (let ((my-add (lambda (x y) (+ x y))))
     ((lambda ()
        (defun my-add-2 (x y)
          (my-add x y))
        (my-add-2 2 3))))
 5
 > (let ((my-add (lambda (x y z) (+ x y z))))
     ((lambda ()
        (defun my-add-2 (x y z)
          (my-add x y z))
        (my-add-2 1 2 3))))
 6

 ;; `define-macro`
 > (describe "define-macro")
 _
 > ((lambda ()
      (define-macro (my-macro x)
        x)
      (my-macro 1)))
 1

 ;; `defmacro`
 > (describe "defmacro")
 _
 > ((lambda ()
      (defmacro my-macro (x)
        x)
      (my-macro 1)))
 1

 ;; `let`
 > (describe "let")
 _
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

 ;; `let*`
 > (describe "let*")
 _
 > (let* ((x 1))
     x)
 1

 ;; `lambda`
 > (describe "lambda")
 _
 > ((lambda (x) x) 1)
 1
 > ((lambda (x)
      x)
    "Lisp")
 "Lisp"
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

 ;; Lexical scope
 > (describe "lexical scope")
 _
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

 ;; `begin`
 > (describe "begin")
 _
 > (begin)
 #u

 ;; `begin0`
 > (describe "begin0")
 _
 > (begin0 1
     2)
 1

 ;; `if`
 > (describe "if")
 _
 > (if #t 1 2)
 1
 > (if #f 1 2)
 2
 > (if (< 1 2) 1 2)
 1
 > (if (> 2 1) 1 2)
 1
 > (compile '(if #t (foo) (bar)))
 "if (true) {
  foo();
} else {
  bar();
}"
 > (compile '(if #t (foo) (bar))
            :as 'statement)
 "if (true) {
  foo();
} else {
  bar();
}"
 > (compile '(if #t (foo) (bar))
            :as 'return)
 "if (true) {
  return foo();
} else {
  return bar();
}"
 > (compile '(if #t (foo) (bar)) :as 'expression)
 "true ? foo() : bar()"
 > (compile '(if #t (foo) (bar) (baz)))
 "if (true) {
  foo();
} else {
  bar();
}"

 ;; `when`
 > (describe "when")
 _
 > (when (< 1 2)
     1 2)
 2
 > (when (> 1 2)
     1 2)
 #u
 > (compile '(when (< 1 2)
               (foo)
               (bar)))
 "if (1 < 2) {
  foo();
  bar();
}"

 ;; `unless`
 > (describe "unless")
 _
 > (unless (< 1 2)
     1 2)
 #u
 > (unless (> 1 2)
     1 2)
 2
 > (compile '(unless (> 1 2)
               (foo)
               (bar)))
 "if (!(1 > 2)) {
  foo();
  bar();
}"

 ;; `cond`
 > (describe "cond")
 _
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
 xit> (macroexpand '(cond
                     (#f
                      (foo))
                     (else
                      (bar))))
 '(if #f
      (foo)
      (bar))
 xit> (macroexpand '(cond
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
              (#f
               (foo))
              (else
               (bar)))
            :as 'statement)
 "if (false) {
  foo();
} else {
  bar();
}"
 > (compile '(cond
              (#f
               (foo))
              (else
               (bar)))
            :as 'return)
 "if (false) {
  return foo();
} else {
  return bar();
}"
 > (compile '(cond
              (#f
               (foo))
              (else
               (bar)))
            :as 'expression)
 "false ? foo() : bar()"

 ;; `js/?`
 > (describe "js/?")
 _
 > (compile '(js/? x y))
 "x ? y : undefined;"
 > (compile '(js/? x y z))
 "x ? y : z;"
 > (compile '(js/? x y (js/? z w)))
 "x ? y : (z ? w : undefined);"
 > (compile '(js/? x y z)
            :as 'statement)
 "x ? y : z;"
 > (compile '(js/? x y z)
            :as 'return)
 "return x ? y : z;"
 > (compile '(js/? x y z)
            :as 'expression)
 "x ? y : z"

 ;; `js/if`
 > (describe "js/if")
 _
 > (compile '(js/if x y))
 "if (x) {
  y;
}"
 > (compile '(js/if x y z))
 "if (x) {
  y;
} else {
  z;
}"
 > (compile '(js/if x y (js/if z w)))
 "if (x) {
  y;
} else if (z) {
  w;
}"
 > (compile '(js/if x y z)
            :as 'statement)
 "if (x) {
  y;
} else {
  z;
}"
 > (compile '(js/if x y z)
            :as 'return)
 "if (x) {
  return y;
} else {
  return z;
}"
 it> (compile '(js/if x y z)
              :as 'expression)
 "(() => {
  if (x) {
    return y;
  } else {
    return z;
  }
})()"

 ;; `js/switch`
 > (describe "js/switch")
 _
 > (let* ((x "foo")
          (y "bar"))
     (js/switch x
                (case "foo"
                  (set! y "baz")
                  (break))
                (default
                  (set! y "quux")))
     y)
 "baz"
 > (compile
    '(js/switch x
                (case "foo"
                  (display "foo")
                  (break))
                (default
                  (display "bar"))))
 "switch (x) {
  case 'foo': {
    console.log('foo');
    break;
  }
  default: {
    console.log('bar');
  }
}"
 > (compile
    '(js/switch x
                (case "foo"
                  (display "foo")
                  (break))
                (default
                  (display "bar")))
    :as 'return)
 "switch (x) {
  case 'foo': {
    return console.log('foo');
    break;
  }
  default: {
    return console.log('bar');
  }
}"
 > (compile
    '(js/switch x
                (case "foo"
                  (display "foo"))
                (default
                  (display "bar")))
    :as 'return)
 "switch (x) {
  case 'foo': {
    console.log('foo');
  }
  default: {
    return console.log('bar');
  }
}"
 > (compile
    '(js/switch x
                (case "foo"
                  (display "foo")
                  (break))
                (default
                  (display "bar")))
    :as 'expression)
 "(() => {
  switch (x) {
    case 'foo': {
      return console.log('foo');
      break;
    }
    default: {
      return console.log('bar');
    }
  }
})()"

 ;; `eq?`
 > (describe "eq?")
 _
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

 ;; `equal?`
 > (describe "equal?")
 _
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

 ;; `not`
 > (describe "not")
 _
 > (not #f)
 #t
 > (not #t)
 #f
 > (compile '(not x))
 "!x;"

 ;; `js/!`
 > (describe "js/!")
 _
 > (js/! #f)
 #t
 > (js/! #t)
 #f
 > (compile '(js/! x))
 "!x;"

 ;; `and`
 > (describe "and")
 _
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
 > (compile '(and #t #t))
 "true && true;"

 ;; `js/&&`
 > (describe "js/&&")
 _
 > (js/&&)
 #t
 > (js/&& #t)
 #t
 > (js/&& #t #t)
 #t
 > (js/&& #t #f)
 #f
 > (funcall js/&&)
 #t
 > (funcall js/&& #t)
 #t
 > (funcall js/&& #t #t)
 #t
 > (funcall js/&& #t #f)
 #f
 > (compile '(js/&& x y))
 "x && y;"
 > (compile '(js/&& x y z))
 "x && y && z;"

 ;; `or`
 > (describe "or")
 _
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
 > (compile '(or #t #t))
 "true || true;"

 ;; `js/\|\|`
 > (describe "js/\|\|")
 _
 > (js/\|\|)
 #f
 > (js/\|\| #t)
 #t
 > (js/\|\| #t #t)
 #t
 > (js/\|\| #t #f)
 #t
 > (funcall js/\|\|)
 #f
 > (funcall js/\|\| #t)
 #t
 > (funcall js/\|\| #t #t)
 #t
 > (funcall js/\|\| #t #f)
 #t
 > (compile '(js/\|\| x y))
 "x || y;"
 > (compile '(js/\|\| x y z))
 "x || y || z;"

 ;; `bitwise-and`
 > (describe "bitwise-and")
 _
 > (compile '(bitwise-and x y))
 "x & y;"
 > (compile '(bit-and x y))
 "x & y;"
 > (compile '(js/& x y))
 "x & y;"
 > (compile '(js/& x y z))
 "x & y & z;"

 ;; `bitwise-or`
 > (describe "bitwise-or")
 _
 > (compile '(bitwise-or x y))
 "x | y;"
 > (compile '(bit-or x y))
 "x | y;"
 > (compile '(js/\| x y))
 "x | y;"
 > (compile '(js/\| x y z))
 "x | y | z;"

 ;; `bitwise-xor`
 > (describe "bitwise-xor")
 _
 > (compile '(bitwise-xor x y))
 "x ^ y;"
 > (compile '(bit-xor x y))
 "x ^ y;"
 > (compile '(js/^ x y))
 "x ^ y;"

 ;; `bitwise-not`
 > (describe "bitwise-not")
 _
 > (compile '(bitwise-negation x))
 "~x;"
 > (compile '(bitwise-not x))
 "~x;"
 > (compile '(bit-not x))
 "~x;"
 > (compile '(js/~ x))
 "~x;"

 ;; `bitwise-shift-left`
 > (describe "bitwise-shift-left")
 _
 > (compile '(bitwise-shift-left x y))
 "x << y;"
 > (compile '(bit-shift-left x y))
 "x << y;"
 > (compile '(js/<< x y))
 "x << y;"

 ;; `bitwise-shift-right`
 > (describe "bitwise-shift-right")
 _
 > (compile '(bitwise-shift-right x y))
 "x >> y;"
 > (compile '(bit-shift-right x y))
 "x >> y;"
 > (compile '(js/>> x y))
 "x >> y;"

 ;; `unsigned-bitwise-shift-right`
 > (describe "unsigned-bitwise-shift-right")
 _
 > (compile '(unsigned-bitwise-shift-right x y))
 "x >>> y;"
 > (compile '(unsigned-bit-shift-right x y))
 "x >>> y;"
 > (compile '(js/>>> x y))
 "x >>> y;"

 ;; `js/op`
 > (describe "js/op")
 _
 > (js/op ! #t)
 #f
 > (js/op && #t #f)
 #f
 > (js/op \|\| #t #f)
 #t
 > (compile '(js/op ! x))
 "!x;"
 > (compile '(js/op ~ x))
 "~x;"
 > (compile '(js/op & x y))
 "x & y;"
 > (compile '(js/op && x y))
 "x && y;"
 > (compile '(js/op \|\| x y))
 "x || y;"

 ;; `while`
 > (describe "while")
 _
 > (let ((result '()))
     (while (< (length result) 3)
       (set! result (cons 1 result)))
     result)
 '(1 1 1)
 > (compile '(while (> x 0)
               (set! x (- x 1))))
 "while (x > 0) {
  x--;
}"

 ;; `for`
 > (describe "for")
 _
 > (let (result)
     (js/for (() () ())
             (set! result 1)
             (break))
     result)
 1
 > (let (result)
     (js/for (#u #u #u)
             (set! result 1)
             (break))
     result)
 1
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
 > (compile '(js/for (() () ())
                     (break)))
 "for (;;) {
  break;
}"
 > (compile '(js/for (#u #u #u)
                     (break)))
 "for (;;) {
  break;
}"
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
 xit> (compile '(for ((i (range 0 10))
                      (j (range 0 10)))
                  (foo)))
 "for (i = 0, j = 0; (i < 10) && (j < 10); i++, j++) {
  foo();
}"

 ;; `js/for`
 > (describe "js/for")
 _
 > (let ((result 0))
     (js/for ((i 0) (< i 10) (+ i 1))
             (set! result (+ result 2)))
     result)
 20
 > (compile '(js/for ((i 0) (< i 10) (+ i 1))
                     (foo)))
 "for (let i = 0; i < 10; i++) {
  foo();
}"
 > (compile '(js/for ((set! i 0) (< i 10) (+ i 1))
                     (foo)))
 "for (i = 0; i < 10; i++) {
  foo();
}"
 > (compile '(js/for ((define i 0) (< i 10) (+ i 1))
                     (foo)))
 "for (let i = 0; i < 10; i++) {
  foo();
}"
 > (compile '(js/for ((begin (set! i 0) (set! j 0))
                      (and (< i 10) (< j 10))
                      (begin (set! i (+ i 1)) (set! j (+ j 1))))
                     (foo)))
 "for (i = 0, j = 0; (i < 10) && (j < 10); i++, j++) {
  foo();
}"

 ;; `js/for-in`
 > (describe "js/for-in")
 _
 > (compile '(js/for-in ((i obj))
                        (foo)))
 "for (let i in obj) {
  foo();
}"

 ;; `js/for-of`
 > (describe "js/for-of")
 _
 > (compile '(js/for-of ((i lst))
                        (foo)))
 "for (let i of lst) {
  foo();
}"

 ;; `break`
 > (describe "break")
 _
 > ((lambda ()
      (while #t
        (break))
      1))
 1
 > (let ((result (list)))
     (for ((i (range 0 10)))
       (break)
       (push-right! result i))
     result)
 '()
 > (compile '(while #t
               (break)))
 "while (true) {
  break;
}"

 ;; `continue`
 > (describe "continue")
 _
 > (let ((result (list))
         (i 0))
     (while (< i 10)
       (set! i (+ i 1))
       (when (< i 5)
         (continue))
       (push-right! result i))
     result)
 '(5 6 7 8 9 10)
 > (let ((result (list)))
     (for ((i (range 0 11)))
       (when (< i 5)
         (continue))
       (push-right! result i))
     result)
 '(5 6 7 8 9 10)
 > (compile '(while #f
               (continue)))
 "while (false) {
  continue;
}"

 ;; `return`
 > (describe "return")
 _
 > ((lambda ()
      (return 1)
      2))
 1
 > ((js/function ()
      (return 1)
      2))
 1
 > ((js/arrow ()
      (return 1)
      2))
 1
 > (compile '(while #t
               (return 0)))
 "while (true) {
  return 0;
}"

 ;; `js/.`
 > (describe "js/.")
 _
 > (let ((obj (js/obj "foo" "bar")))
     (js/. obj foo))
 "bar"
 > (let ((obj (js/obj "foo" "bar")))
     (js/. obj "foo"))
 "bar"
 > (let ((obj (js/obj "foo" (js/obj "bar" "baz"))))
     (js/. obj foo bar))
 "baz"
 > (let ((obj (js/obj "foo" (js/obj "bar" "baz"))))
     (js/. (js/. obj foo) bar))
 "baz"
 > (compile '(js/. obj prop))
 "obj.prop;"
 > (compile '(js/. obj "prop"))
 "obj['prop'];"
 > (compile '(js/. obj :foo))
 "obj.foo;"
 > (compile '(js/. obj :foo-bar))
 "obj.fooBar;"
 > (compile '(js/. obj 'foo))
 "obj.foo;"
 > (compile '(js/. obj 'foo-bar))
 "obj.fooBar;"
 > (compile '(js/. obj prop1 prop2))
 "obj.prop1.prop2;"
 > (compile '(js/. (js/. obj prop1) prop2))
 "obj.prop1.prop2;"

 ;; `js/?.`
 > (describe "js/?.")
 _
 > (let ((obj (js/obj "foo" "bar")))
     (js/?. obj foo))
 "bar"
 > (let ((obj (js/obj "foo" "bar")))
     (js/?. obj "foo"))
 "bar"
 > (let ((obj (js/obj "foo" "bar")))
     (js/?. obj quux))
 #u
 > (let ((obj (js/obj "foo" "bar")))
     ((js/?. obj quux)))
 #u
 > (let ((obj (js/obj "foo" "bar")))
     (js/?. obj quux wobble))
 #u
 > (let ((obj (js/obj "foo" "bar")))
     (js/?. (js/?. obj quux) wobble))
 #u
 > (compile '(js/?. obj prop))
 "obj?.prop;"
 > (compile '(js/?. obj prop1 prop2))
 "obj?.prop1?.prop2;"
 > (compile '(js/?. (js/?. obj prop1) prop2))
 "obj?.prop1?.prop2;"
 > (compile '(js/?. (js/?. (js/?. obj) prop1) prop2))
 "obj?.prop1?.prop2;"

 ;; `get-field`
 > (describe "get-field")
 _
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

 ;; `set-field!`
 > (describe "set-field!")
 _
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

 ;; `field-bound?`
 > (describe "field-bound?")
 _
 > (let ((obj (js/obj "foo" "bar")))
     (field-bound? foo obj))
 #t

 ;; `oget`
 > (describe "oget")
 _
 > (let ((obj (js/obj "prop" "foo")))
     (oget obj "prop"))
 "foo"
 > (oget _ "@@functional/placeholder")
 #t
 > (compile '(oget obj foo-bar))
 "obj[fooBar];"
 > (compile '(oget obj 'foo-bar))
 "obj['fooBar'];"
 > (compile '(oget obj :foo-bar))
 "obj['fooBar'];"
 > (compile '(oget obj "foo-bar"))
 "obj['foo-bar'];"
 > (compile '(oget obj (foo-bar)))
 "obj[fooBar()];"

 ;; `oset!`
 > (describe "oset!")
 _
 > (let ((obj (js/obj)))
     (oset! obj 'foo-bar "baz")
     (oget obj 'foo-bar))
 "baz"
 > (let ((obj (js/obj)))
     (oset! obj :foo-bar "baz")
     (oget obj :foo-bar))
 "baz"
 > (let ((obj (js/obj)))
     (oset! obj "foo-bar" "baz")
     (oget obj "foo-bar"))
 "baz"
 > (compile '(oset! obj foo-bar "baz"))
 "obj[fooBar] = 'baz';"
 > (compile '(oset! obj 'foo-bar "baz"))
 "obj['fooBar'] = 'baz';"
 > (compile '(oset! obj :foo-bar "baz"))
 "obj['fooBar'] = 'baz';"
 > (compile '(oset! obj "foo-bar" "baz"))
 "obj['foo-bar'] = 'baz';"
 > (compile '(oset! obj (foo-bar) "baz"))
 "obj[fooBar()] = 'baz';"

 ;; `send`
 > (describe "send")
 _
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

 ;; `send/apply`
 > (describe "send/apply")
 _
 > (let ((obj (make-hash '(("foo" . "foo")))))
     (send/apply obj has '("foo")))
 #t
 > (compile '(send/apply obj m args))
 "obj.m(...args);"

 ;; `new`
 > (describe "new")
 _
 > (let (quux)
     (set! quux
           (new (class ()
                  (define/public (bar)
                    "baz"))))
     (send quux bar))
 "baz"
 > (let (quux)
     (set! quux
           (new (class ()
                  (define/public val 1)
                  (define (constructor x)
                    (set-field! val this x))
                  (define/public (bar)
                    (get-field val this)))
                2))
     (send quux bar))
 2
 > (compile '(new Foo))
 "new Foo();"
 > (compile '(new Foo x))
 "new Foo(x);"

 ;; `new/apply`
 > (describe "new/apply")
 _
 > (compile '(new/apply Foo args))
 "new Foo(...args);"

 ;; `class`
 > (describe "class")
 _
 > ((lambda ()
      (define Foo
        (class object%
          (define/public (bar)
            "baz")))
      (define quux
        (new Foo))
      (send quux bar)))
 "baz"
 > ((lambda ()
      (defclass Foo ()
        (define/public (bar)
          "baz"))
      (define quux
        (new Foo))
      (send quux bar)))
 "baz"
 > ((lambda ()
      (defclass Foo ()
        (define bar "baz"))
      (define quux
        (new Foo))
      (get-field bar quux)))
 "baz"
 > ((lambda ()
      (defclass Foo ()
        (define x)
        (define (constructor x)
          (set-field! x this x))
        (define (bar)
          (get-field x this)))
      (define quux
        (new Foo "xyzzy"))
      (send quux bar)))
 "xyzzy"
 > ((lambda ()
      (defclass Foo (Object)
        (define (bar)
          "baz"))
      (define quux
        (new Foo))
      (send quux bar)))
 "baz"
 > (compile '(class ()
               (define/public (bar)
                 "bar")))
 "class {
  bar() {
    return 'bar';
  }
}"

 ;; `define-class`
 > (describe "define-class")
 _
 > ((lambda ()
      (define-class Foo ()
        (define/public (bar)
          "bar"))
      (define foo
        (new Foo))
      (send foo bar)))
 "bar"
 > (compile '(define-class Foo ()
               (define/public (bar)
                 "bar")))
 "class Foo {
  bar() {
    return 'bar';
  }
}"

 ;; `defclass`
 > (describe "defclass")
 _
 > ((lambda ()
      (defclass Foo ()
        (define/public (bar)
          "bar"))
      (define foo
        (new Foo))
      (send foo bar)))
 "bar"
 > (compile '(defclass Foo ()
               (define/public (bar)
                 "bar")))
 "class Foo {
  bar() {
    return 'bar';
  }
}"

 ;; `instance-of?`
 > (describe "instance-of?")
 _
 > (instance-of? (new Map) Map)
 #t
 > (compile '(instance-of? x Foo))
 "x instanceof Foo;"

 ;; `is-a?`
 > (describe "is-a?")
 _
 > (is-a? (new Map) Map)
 #t
 > (compile '(is-a? x Foo))
 "x instanceof Foo;"

 ;; `js/obj`
 > (describe "js/obj")
 _
 > (js/obj)
 (js/obj)
 > (js/obj "foo" "bar")
 (js/obj "foo" "bar")
 > (js/obj "foo" 1 "bar" 2)
 (js/obj "foo" 1 "bar" 2)
 > (compile '(js/obj))
 "({});"
 > (compile '(js/obj "foo" foo))
 "({
  foo
});"
 > (compile '(js/obj "foo" "bar"))
 "({
  foo: 'bar'
});"
 > (compile '(js/obj foo foo))
 "({
  [foo]: foo
});"
 > (compile '(js/obj foo "bar"))
 "({
  [foo]: 'bar'
});"
 > (compile '(js/obj 'foo "bar"))
 "({
  foo: 'bar'
});"
 > (compile '(js/obj :foo "bar"))
 "({
  foo: 'bar'
});"
 > (compile '(js/obj "foo-bar" "baz"))
 "({
  'foo-bar': 'baz'
});"
 > (compile '(js/obj foo-bar "baz"))
 "({
  [fooBar]: 'baz'
});"
 > (compile '(js/obj 'foo-bar "baz"))
 "({
  fooBar: 'baz'
});"
 > (compile '(js/obj :foo-bar "baz"))
 "({
  fooBar: 'baz'
});"
 > (compile '(js/obj "foo bar" "baz"))
 "({
  'foo bar': 'baz'
});"
 > (compile '(js/obj "foo bar" baz))
 "({
  'foo bar': baz
});"
 > (compile '(js/obj "foo bar" 'baz))
 "({
  'foo bar': Symbol.for('baz')
});"
 > (compile '(js/obj "foo bar" :baz))
 "({
  'foo bar': Symbol.for(':baz')
});"
 > (compile '(js/obj "foo" 1 "bar" 2))
 "({
  foo: 1,
  bar: 2
});"
 > (compile '(js/obj "foo" foo "bar" bar))
 "({
  foo,
  bar
});"
 > (compile '(js/obj "foo" (js/obj "bar" "baz")))
 "({
  foo: {
    bar: 'baz'
  }
});"
 > (compile '(js/obj "foo" (js/obj "foo" "foo")
                     "bar" (js/obj "bar" "bar")))
 "({
  foo: {
    foo: 'foo'
  },
  bar: {
    bar: 'bar'
  }
});"
 > (compile '(js/obj "foo" (js/obj)
                     "bar" (js/obj "bar" "bar")
                     "baz" (js/obj "baz" "baz")))
 "({
  foo: {},
  bar: {
    bar: 'bar'
  },
  baz: {
    baz: 'baz'
  }
});"
 > (compile '(js/obj)
            :as 'expression)
 "{}"
 > (compile '(js/obj "foo" "bar")
            :as 'expression)
 "{
  foo: 'bar'
}"
 > (compile '(js/obj "foo" 1 "bar" 2)
            :as 'expression)
 "{
  foo: 1,
  bar: 2
}"
 > (compile '(js/obj)
            :as 'return)
 "return {};"
 > (compile '(js/obj "foo" "bar")
            :as 'return)
 "return {
  foo: 'bar'
};"
 > (compile '(js/obj "foo" 1 "bar" 2)
            :as 'return)
 "return {
  foo: 1,
  bar: 2
};"

 ;; `js/obj?`
 > (describe "js/obj?")
 _
 > (compile '(js/obj? x))
 "(x !== null) && (typeof x === 'object');"

 ;; `js/obj-append`
 > (describe "js/obj-append")
 _
 > (compile '(js/obj-append
              obj
              (js/obj "foo" "bar")))
 "({
  ...obj,
  foo: 'bar'
});"

 ;; `js/keys`
 > (describe "js/keys")
 _
 > (js/keys (js/obj))
 '()
 > (js/keys (js/obj "foo" "bar"))
 '("foo")
 > (js/keys (js/obj "foo" "bar"
                    "baz" "quux"))
 '("foo" "baz")
 > (compile '(js/keys x))
 "Object.keys(x);"

 ;; `js/in`
 > (describe "js/in")
 _
 > (let ((obj (js/obj "foo" "bar")))
     (js/in "foo" obj))
 #t
 > (compile '(js/in "foo" obj))
 "'foo' in obj;"

 ;; `plist->alist`
 > (describe "plist->alist")
 _
 > (plist->alist '())
 '()
 > (plist->alist '(foo bar))
 '((foo . bar))
 > (plist->alist '(foo bar baz quux))
 '((foo . bar) (baz . quux))

 ;; `plist->object`
 > (describe "plist->object")
 _
 > (plist->object '())
 (js/obj)
 > (plist->object '(foo bar))
 (js/obj "foo" 'bar)
 > (plist->object '(foo bar baz quux))
 (js/obj "foo" 'bar
         "baz" 'quux)

 ;; `module`
 > (describe "module")
 _
 > (module foo bar
     (+ 1 1))
 2

 ;; `js/try`
 > (describe "js/try")
 _
 > (js/try
    (/ 1 2)
    (catch e
        (display "there was an error"))
    (finally
      (display "finally")))
 0.5
 > (js/try
    (/ 1 3)
    (/ 1 2)
    (catch e
        (display "there was an error"))
    (finally
      (display "finally")))
 0.5
 > (compile '(js/try
              (/ 1 2)
              (catch e
                  (display "there was an error"))
              (finally
                (display "finally"))))
 "try {
  1 / 2;
} catch (e) {
  console.log('there was an error');
} finally {
  console.log('finally');
}"

 ;; `clj/try`
 > (describe "clj/try")
 _
 > (clj/try
    (/ 1 2)
    (catch Exception e
      "there was an error")
    (finally
      (display "finally")))
 0.5
 > (clj/try
    (/ 1 3)
    (/ 1 2)
    (catch Exception e
      "there was an error")
    (finally
      (display "finally")))
 0.5
 > (compile '(clj/try
              (/ 1 2)
              (catch Object e
                (display "there was an error"))
              (finally
                (display "finally"))))
 "try {
  1 / 2;
} catch (e) {
  console.log('there was an error');
} finally {
  console.log('finally');
}"
 > (compile '(clj/try
              (/ 1 2)
              (catch Exception e
                (display "there was an error"))
              (finally
                (display "finally"))))
 "try {
  1 / 2;
} catch (e) {
  if (e instanceof Exception) {
    console.log('there was an error');
  } else {
    throw e;
  }
} finally {
  console.log('finally');
}"

 ;; `unwind-protect`
 > (describe "unwind-protect")
 _
 > (unwind-protect 1 2 3)
 1
 > (compile '(unwind-protect (foo) (bar)))
 "try {
  foo();
} finally {
  bar();
}"

 ;; `call/cc`
 > (describe "call/cc")
 _
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

 ;; `define-values`
 > (describe "define-values")
 _
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

 ;; `set!-values`
 > (describe "set!-values")
 _
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

 ;; `let-values`
 > (describe "let-values")
 _
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

 ;; `let*-values`
 > (describe "let*-values")
 _
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

 ;; `define-fields`
 > (describe "define-fields")
 _
 > ((lambda ()
      (define-fields (x)
        (js/obj "x" 1))
      x))
 1
 > ((lambda ()
      (define-fields (foo)
        (js/obj "foo" "bar"))
      foo))
 "bar"
 > ((lambda ()
      (define-fields ((foo bar))
        (js/obj "foo" "bar"))
      bar))
 "bar"
 > (compile '(define-fields (foo)
               (js/obj "foo" "bar")))
 "let {foo} = {
  foo: 'bar'
};"
 > (compile '(define-fields ((foo bar))
               (js/obj "foo" "bar")))
 "let {foo: bar} = {
  foo: 'bar'
};"

 ;; `set!-fields`
 > (describe "set!-fields")
 _
 > ((lambda ()
      (let (x)
        (set!-fields (x) (js/obj "x" 1))
        x)))
 1
 > (compile '(set!-fields (x) (js/obj "x" 1)))
 "({x} = {
  x: 1
});"

 ;; `destructuring-bind`
 > (describe "destructuring-bind")
 _
 > (destructuring-bind (x y)
                       '(1 2)
                       (list x y))
 '(1 2)
 > (destructuring-bind (x . y)
                       '(1 2)
                       (list x y))
 '(1 (2))
 > (compile '(destructuring-bind (x y)
                                 '(1 2)
                                 (list x y)))
 "let [x, y] = [1, 2];

[x, y];"
 > (compile '(destructuring-bind (x . y)
                                 '(1 2)
                                 (list x y)))
 "let [x, ...y] = [1, 2];

[x, y];"

 ;; `multiple-values-bind`
 > (describe "multiple-values-bind")
 _
 > (multiple-values-bind (x y)
                         (values 1 2)
                         (list x y))
 '(1 2)
 > (compile '(multiple-values-bind (x y)
                                   (values 1 2)
                                   (list x y)))
 "let [x, y] = [1, 2];

[x, y];"

 ;; `hash`
 > (describe "hash")
 _
 > (hash)
 (new Map)
 > (hash '(("foo" . "bar")))
 (new Map '(("foo" "bar")))
 > (compile '(hash))
 "new Map();"
 > (compile '(hash '(("foo" . "bar"))))
 "new Map([['foo', 'bar']]);"

 ;; `make-hash`
 > (describe "make-hash")
 _
 > (make-hash)
 (new Map)
 > (make-hash '(("foo" . "bar")))
 (new Map '(("foo" "bar")))
 > (compile '(make-hash))
 "new Map();"
 > (compile '(make-hash '(("foo" . "bar"))))
 "new Map([['foo', 'bar']]);"

 ;; `hash?`
 > (describe "hash?")
 _
 > (hash? (make-hash))
 #t
 > (hash? 0)
 #f
 > (compile '(hash? x))
 "x instanceof Map;"

 ;; `hash-clear`
 > (describe "hash-clear")
 _
 > (hash-clear
    (make-hash
     '(("foo" . "bar"))))
 (new Map)

 ;; `hash-clear!`
 > (describe "hash-clear!")
 _
 > (let ((ht (make-hash '(("foo" . "bar")))))
     (hash-clear! ht)
     ht)
 (new Map)
 > (compile '(hash-clear! x))
 "x.clear();"

 ;; `hash-copy`
 > (describe "hash-copy")
 _
 > (hash-copy
    (make-hash
     '(("foo" . "bar"))))
 (new Map '(("foo" "bar")))
 > (compile '(hash-copy x))
 "new Map(x);"

 ;; `hash-keys`
 > (describe "hash-keys")
 _
 > (hash-keys
    (make-hash
     '(("foo" . "bar"))))
 '("foo")
 > (compile '(hash-keys x))
 "[...x.keys()];"

 ;; `hash-values`
 > (describe "hash-values")
 _
 > (hash-values
    (make-hash
     '(("foo" . "bar"))))
 '("bar")
 > (compile '(hash-values x))
 "[...x.values()];"

 ;; `hash->list`
 > (describe "hash->list")
 _
 > (hash->list
    (make-hash
     '(("foo" . "bar"))))
 '(("foo" . "bar"))

 ;; `hash-set`
 > (describe "hash-set")
 _
 > (hash-set
    (make-hash)
    "foo"
    "bar")
 (new Map
      '(("foo" "bar")))

 ;; `hash-set!`
 > (describe "hash-set!")
 _
 > (let ((ht (make-hash)))
     (hash-set! ht "foo" "bar")
     ht)
 (new Map
      '(("foo" "bar")))
 > (compile '(hash-set! ht key val))
 "ht.set(key, val);"

 ;; `hash-ref`
 > (describe "hash-ref")
 _
 > (hash-ref
    (make-hash
     '(("foo" . "bar")))
    "foo")
 "bar"
 > (hash-ref (make-hash) "quux" #f)
 #f
 > (compile '(hash-ref ht "foo"))
 "ht.get('foo');"

 ;; `hash-has-key?`
 > (describe "hash-has-key?")
 _
 > (hash-has-key?
    (make-hash
     '(("foo" . "bar")))
    "foo")
 #t
 > (hash-has-key? (make-hash) "quux")
 #f
 > (compile '(hash-has-key? ht "quux"))
 "ht.has('quux');"

 ;; `Map`
 > (describe "Map")
 _
 > (new Map)
 (new Map)
 > (~> (new Map '((1 2)))
       (send _ entries)
       (send Array from _))
 '((1 2))

 ;; `+`
 > (describe "+")
 _
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

 ;; `js/+`
 > (describe "js/+")
 _
 > (js/+)
 0
 > (js/+ 1)
 1
 > (js/+ 1 2)
 3
 > (js/+ 2 2)
 4
 > (js/+ 1 2 3)
 6
 > (js/+ 1 2 4)
 7
 > (js/+ (js/+ 1 1) (js/+ 1 1))
 4
 > (let ((x 2))
     (js/+ x x))
 4
 > (js/+ 1 "")
 "1"
 > (compile '(js/+ 1 1))
 "1 + 1;"
 > (compile '(js/+ 1 1 1))
 "1 + 1 + 1;"

 ;; `-`
 > (describe "-")
 _
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

 ;; `js/-`
 > (describe "js/-")
 _
 > (js/-)
 0
 > (js/- 1)
 -1
 > (js/- 1 2)
 -1
 > (js/- 1 2 3)
 -4
 > (js/- 1 2 4)
 -5
 > (compile '(js/- 1))
 "-1;"
 > (compile '(js/- 1 1))
 "1 - 1;"
 > (compile '(js/- 1 1 1))
 "1 - 1 - 1;"

 ;; `*`
 > (describe "*")
 _
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

 ;; `js/*`
 > (describe "js/*")
 _
 > (js/*)
 1
 > (js/* 1)
 1
 > (js/* 1 2)
 2
 > (js/* 1 2 3)
 6
 > (js/* 1 2 4)
 8
 > (compile '(js/* 1 1))
 "1 * 1;"
 > (compile '(js/* 1 1 1))
 "1 * 1 * 1;"

 ;; `/`
 > (describe "/")
 _
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

 ;; `js//`
 > (describe "js//")
 _
 > (js//)
 #u
 > (js// 1)
 1
 > (js// 1 2)
 0.5
 > (js// 1 2 3)
 (js// 1 2 3)
 > (/ 1 2 4)
 0.125
 > (compile '(js// 1 2))
 "1 / 2;"
 > (compile '(js// 1 2 4))
 "1 / 2 / 4;"

 ;; `<`
 > (describe "<")
 _
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
 > (compile '(< x y))
 "x < y;"
 > (compile '(< x y z))
 "(x < y) && (y < z);"

 ;; `js/<`
 > (describe "js/<")
 _
 > (js/< 1 2)
 #t
 > (js/< 2 1)
 #f
 > (js/< 1 2 3)
 #t
 > (js/< 2 1 3)
 #f
 > (js/< 1 3 2)
 #f
 > (funcall js/< 1 2)
 #t
 > (funcall js/< 2 1)
 #f
 > (funcall js/< 1 2 3)
 #t
 > (funcall js/< 2 1 3)
 #f
 > (funcall js/< 1 3 2)
 #f
 > (compile '(js/< x y))
 "x < y;"
 > (compile '(js/< x y z))
 "(x < y) && (y < z);"

 ;; `<=`
 > (describe "<=")
 _
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

 ;; `js/<=`
 > (describe "js/<=")
 _
 > (js/<= 1 2)
 #t
 > (js/<= 2 1)
 #f
 > (js/<= 1 2 3)
 #t
 > (js/<= 1 1 2)
 #t
 > (js/<= 2 1 3)
 #f
 > (js/<= 1 3 2)
 #f
 > (funcall js/<= 1 2)
 #t
 > (funcall js/<= 2 1)
 #f
 > (funcall js/<= 1 2 3)
 #t
 > (funcall js/<= 1 1 2)
 #t
 > (funcall js/<= 2 1 3)
 #f
 > (funcall js/<= 1 3 2)
 #f
 > (compile '(js/<= x y))
 "x <= y;"
 > (compile '(js/<= x y z))
 "(x <= y) && (y <= z);"

 ;; `>`
 > (describe ">")
 _
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
 > (compile '(> x y))
 "x > y;"
 > (compile '(> x y z))
 "(x > y) && (y > z);"

 ;; `js/>`
 > (describe "js/>")
 _
 > (js/> 2 1)
 #t
 > (js/> 1 2)
 #f
 > (js/> 3 2 1)
 #t
 > (js/> 1 2 3)
 #f
 > (js/> 2 1 3)
 #f
 > (js/> 1 3 2)
 #f
 > (funcall js/> 2 1)
 #t
 > (funcall js/> 1 2)
 #f
 > (funcall js/> 3 2 1)
 #t
 > (funcall js/> 1 2 3)
 #f
 > (funcall js/> 2 1 3)
 #f
 > (funcall js/> 1 3 2)
 #f
 > (compile '(js/> x y))
 "x > y;"
 > (compile '(js/> x y z))
 "(x > y) && (y > z);"

 ;; `>=`
 > (describe ">=")
 _
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

 ;; `js/>=`
 > (describe "js/>=")
 _
 > (js/>= 2 1)
 #t
 > (js/>= 2 2)
 #t
 > (js/>= 1 2)
 #f
 > (js/>= 3 2 1)
 #t
 > (js/>= 3 2 2)
 #t
 > (js/>= 1 2 3)
 #f
 > (js/>= 2 1 3)
 #f
 > (js/>= 1 3 2)
 #f
 > (funcall js/>= 2 1)
 #t
 > (funcall js/>= 2 2)
 #t
 > (funcall js/>= 1 2)
 #f
 > (funcall js/>= 3 2 1)
 #t
 > (funcall js/>= 3 2 2)
 #t
 > (funcall js/>= 1 2 3)
 #f
 > (funcall js/>= 2 1 3)
 #f
 > (funcall js/>= 1 3 2)
 #f
 > (compile '(js/>= x y))
 "x >= y;"
 > (compile '(js/>= x y z))
 "(x >= y) && (y >= z);"

 > (compile '(js/% x y))
 "x % y;"

 ;; `abs`
 > (describe "abs")
 _
 > (abs 1)
 1
 > (abs -1)
 1
 > (compile '(abs x))
 "Math.abs(x);"
 > (compile '(js/abs x))
 "Math.abs(x);"

 ;; `range`
 > (describe "range")
 _
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

 ;; `member`
 > (describe "member")
 _
 > (member 2 '(1 2 3 4))
 '(2 3 4)
 > (member 9 '(1 2 3 4))
 #f
 > (member 5
           '(3 5 1 7 2 9)
           (lambda (x y)
             (< x y)))
 '(7 2 9)

 ;; `member?`
 > (describe "member?")
 _
 > (member? 2 '(1 2 3 4))
 #t
 > (member? 9 '(1 2 3 4))
 #f

 ;; `memq?`
 > (describe "memq?")
 _
 > (memq? 2 '(1 2 3 4))
 #t
 > (memq? 9 '(1 2 3 4))
 #f
 > (compile '(memq? x lst))
 "lst.includes(x);"

 ;; `take`
 > (describe "take")
 _
 > (take '(1 2 3 4) 0)
 '()
 > (take '(1 2 3 4) 1)
 '(1)
 > (take '(1 2 3 4) 2)
 '(1 2)

 ;; `drop`
 > (describe "drop")
 _
 > (drop '(1 2 3 4) 0)
 '(1 2 3 4)
 > (drop '(1 2 3 4) 1)
 '(2 3 4)

 ;; `drop-right`
 > (describe "drop-right")
 _
 > (drop-right '(1 2 3 4) 0)
 '(1 2 3 4)
 > (drop-right '(1 2 3 4) 1)
 '(1 2 3)

 ;; `map`
 > (describe "map")
 _
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

 ;; `foldl`
 > (describe "foldl")
 _
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

 ;; `foldr`
 > (describe "foldr")
 _
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

 ;; `filter`
 > (describe "filter")
 _
 > (filter string? '("foo" 1 2 3))
 '("foo")
 > (compile '(filter f lst))
 "lst.filter(f);"

 ;; `string?`
 > (describe "string?")
 _
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

 ;; `string-length`
 > (describe "string-length")
 _
 > (string-length "foo")
 3
 > (compile '(string-length x))
 "x.length;"

 ;; `string-append`
 > (describe "string-append")
 _
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

 ;; `string-join`
 > (describe "string-join")
 _
 > (string-join '("foo" "bar"))
 "foo bar"
 > (string-join '("foo" "bar") ",")
 "foo,bar"
 > (compile '(string-join '("foo" "bar") ","))
 "['foo', 'bar'].join(',');"

 ;; `string-split`
 > (describe "string-split")
 _
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

 ;; `string-trim`
 > (describe "string-trim")
 _
 > (string-trim "  foo bar  baz  ")
 "foo bar  baz"
 > (string-trim "  foo bar  baz \r\n\t")
 "foo bar  baz"
 > (compile '(string-trim x))
 "x.trim();"

 ;; `string-upcase`
 > (describe "string-upcase")
 _
 > (string-upcase "foo")
 "FOO"
 > (compile '(string-upcase x))
 "x.toUpperCase();"

 ;; `string-downcase`
 > (describe "string-downcase")
 _
 > (string-downcase "FOO")
 "foo"
 > (compile '(string-downcase x))
 "x.toLowerCase();"

 ;; `substring`
 > (describe "substring")
 _
 > (substring "Apple" 1 3)
 "pp"
 > (substring "Apple" 1)
 "pple"
 > (compile '(substring str i))
 "str.substring(i);"
 > (compile '(substring str i j))
 "str.substring(i, j);"

 ;; `as~>`
 > (describe "as~>")
 _
 > (as~> 0 _
     (+ _ 1)
     (+ _ 1))
 2
 > (macroexpand '(as~> 0 _
                   (+ _ 1)
                   (+ _ 1)))
 '(+ (+ 0 1) 1)
 > (compile '(as~> 0 _
               (+ _ 1)
               (+ _ 1)))
 "0 + 1 + 1;"

 ;; `ann`
 > (describe "ann")
 _
 > (ann #u Any)
 #u
 > (compile '(ann #t Any))
 "true;"
 > (compile '(ann #t Any) :to 'typescript)
 "true as any;"

 ;; `cons?`
 > (describe "cons?")
 _
 > (cons? '())
 #f

 ;; `list?`
 > (describe "list?")
 _
 > (list? '())
 #t
 > (list? '(1 . 2))
 #f
 > (list? '(1 2 . 3))
 #f
 > (list? '(1 . ()))
 #t
 > (list? '(1 . (2 . ())))
 #t

 ;; `vector?`
 > (describe "vector?")
 _
 > (vector? '())
 #t
 > (vector? '(1 . 2))
 #t
 > (vector? '(1 2 . 3))
 #t
 > (vector? '(1 . ()))
 #t
 > (vector? '(1 . (2 . ())))
 #t

 ;; `array-list?`
 > (describe "array-list?")
 _
 > (array-list? '())
 #t
 > (array-list? '(1 . 2))
 #t
 > (array-list? '(1 2))
 #t
 > (array-list? '(1 2 3))
 #t

 ;; `linked-list?`
 > (describe "linked-list?")
 _
 > (linked-list? '())
 #f
 > (linked-list? '(1 . 2))
 #f
 > (linked-list? '(1 . ()))
 #t
 > (linked-list? '(1 . (2 . ())))
 #t
 > (linked-list? '(1 2 . (3 . ())))
 #t

 ;; `linked-list-link?`
 > (describe "linked-list-link?")
 _
 > (linked-list-link? '())
 #f
 > (linked-list-link? '(1 . 2))
 #t
 > (linked-list-link? '(1 . ()))
 #t
 > (linked-list-link? '(1 . (2 . ())))
 #t
 > (linked-list-link? '(1 2 . (3 . ())))
 #t

 ;; `length`
 > (describe "length")
 _
 > (length '(1 . ()))
 1
 > (length '(1 . (2 . ())))
 2
 > (length '(1 2 . ()))
 2

 ;; `last`
 > (describe "last")
 _
 > (last '(1 . ()))
 1
 > (last '(1 . (2 . ())))
 2
 > (last '(1 2 . ()))
 2

 ;; `nth`
 > (describe "nth")
 _
 > (nth 1 '(1 . (2 . ())))
 2
 > (nth 1 '(1 2 . (3 . ())))
 2

 ;; `cdr`
 > (describe "cdr")
 _
 > (cdr '(1 . (2 . ())))
 '(2 . ())
 > (cdr '(1 2 . (3 . ())))
 '(2 . (3 . ()))

 ;; `set-car!`
 > (describe "set-car!")
 _
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

 ;; `set-cdr!`
 > (describe "set-cdr!")
 _
 > ((lambda ()
      (define foo '())
      (set-cdr! foo '(bar))
      foo))
 '()
 > ((lambda ()
      (define foo
        '(foo))
      (set-cdr! foo '(bar))
      foo))
 '(foo bar)
 > ((lambda ()
      (define foo
        '(foo bar))
      (set-cdr! foo '(baz))
      foo))
 '(foo baz)
 > ((lambda ()
      (define foo
        '(foo bar))
      (set-cdr! foo '(baz . quux))
      foo))
 '(foo baz . quux)
 > ((lambda ()
      (define foo
        '(foo . bar))
      (set-cdr! foo '(baz))
      foo))
 '(foo baz)
 > ((lambda ()
      (define foo
        '(foo . bar))
      (set-cdr! foo '(baz . quux))
      foo))
 '(foo baz . quux)
 > ((lambda ()
      (define foo
        '(foo))
      (set-cdr! foo 'bar)
      foo))
 '(foo . bar)
 > ((lambda ()
      (define foo
        '(foo bar . baz))
      (set-cdr! foo '(quux))
      foo))
 '(foo quux)
 > ((lambda ()
      (define foo
        '(foo bar . baz))
      (set-cdr! foo 'quux)
      foo))
 '(foo . quux)

 ;; Dotted lists
 > (describe "Dotted lists")
 _
 > (equal? '(1 2) '(1 . (2 . ())))
 #t

 ;; `dotted-list?`
 > (describe "dotted-list?")
 _
 > (dotted-list? '())
 #f
 > (dotted-list? '(1 . 2))
 #t
 > (dotted-list? '(1 . ()))
 #f
 > (dotted-list? '(1 . (2 . ())))
 #f
 > (dotted-list? '(1 . (2 . 3)))
 #t
 > (dotted-list? '(foo . bar))
 #t
 > (dotted-list? '(foo bar))
 #f

 ;; `dotted-list-length`
 > (describe "dotted-list-length")
 _
 > (dotted-list-length '())
 0
 > (dotted-list-length '(1 . ()))
 1
 > (dotted-list-length '(1 . (2 . ())))
 2

 ;; `dotted-list-head`
 > (describe "dotted-list-head")
 _
 > (dotted-list-head '(foo . bar))
 '(foo)
 > (dotted-list-head '(foo bar . baz))
 '(foo bar)

 ;; `dotted-list-tail`
 > (describe "dotted-list-tail")
 _
 > (dotted-list-tail '(foo . bar))
 'bar
 > (dotted-list-tail '(foo bar . baz))
 'baz

 ;; `dotted-list-last`
 > (describe "dotted-list-last")
 _
 > (dotted-list-last '())
 #u
 > (dotted-list-last '(1 . ()))
 1
 > (dotted-list-last '(1 . (2 . ())))
 2

 ;; `dotted-list-last-cdr`
 > (describe "dotted-list-last-cdr")
 _
 > (dotted-list-last-cdr '())
 '()
 > (dotted-list-last-cdr '(1 . ()))
 '()
 > (dotted-list-last-cdr '(1 . (2 . ())))
 '()

 ;; `dotted-list->proper-list`
 > (describe "dotted-list->proper-list")
 _
 > (dotted-list->proper-list '(foo . bar))
 '(foo bar)
 > (dotted-list->proper-list '(foo bar . baz))
 '(foo bar baz)

 ;; `proper-list?`
 > (describe "proper-list?")
 _
 > (proper-list? '(foo bar))
 #t
 > (proper-list? '(foo . bar))
 #f

 ;; `circular-list?`
 > (describe "circular-list?")
 _
 > ((lambda ()
      (define foo '())
      (circular-list? foo)
      (set-cdr! foo foo)
      (circular-list? foo)))
 #f
 > (circular-list? '(foo))
 #f
 > (circular-list? '(foo . bar))
 #f
 > ((lambda ()
      (define foo
        '(foo))
      (set-cdr! foo foo)
      (circular-list? foo)))
 #t
 > ((lambda ()
      (define foo
        '(foo . ()))
      (set-cdr! foo foo)
      (circular-list? foo)))
 #t
 > ((lambda ()
      (define foo
        '(foo bar))
      (set-cdr! foo foo)
      (circular-list? foo)))
 #t

 ;; `proper-list->dotted-list`
 > (describe "proper-list->dotted-list")
 _
 > (proper-list->dotted-list '(foo bar))
 '(foo . bar)
 > (proper-list->dotted-list '(foo bar baz))
 '(foo bar . baz)

 ;; `list*`
 > (describe "list*")
 _
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

 ;; `flatten`
 > (describe "flatten")
 _
 > (flatten '(1 2 3 4))
 '(1 2 3 4)
 > (flatten '(1 . 2))
 '(1 2)
 > (flatten '((a) b (c (d) . e) ()))
 '(a b c d e)
 > (flatten '((((4)))))
 '(4)

 ;; Cons dot
 > (describe "Cons dot")
 _
 > *cons-dot*
 '.
 > (cons-dot)
 '.
 > (cons-dot? *cons-dot*)
 #t

 ;; `require`
 > (describe "require")
 _
 > (compile '(require "foo"))
 "import * as foo from 'foo';"
 > (compile '(require "foo-bar"))
 "import * as fooBar from 'foo-bar';"
 > (compile '(require foo "bar"))
 "import * as foo from 'bar';"
 > (compile '(require (only-in foo
                               bar)))
 "import {
  bar
} from 'foo';"
 > (compile '(require (only-in foo
                               (bar baz))))
 "import {
  bar as baz
} from 'foo';"
 > (compile '(require (only-in "foo"
                               (bar baz))))
 "import {
  bar as baz
} from 'foo';"
 > (compile '(require (only-in foo bar bar)))
 "import {
  bar
} from 'foo';"
 > (compile '(require (only-in foo
                               bar
                               (baz bar))))
 "import {
  bar
} from 'foo';"
 > (compile '(require "foo")
            :fes-module-interop #t)
 "import foo from 'foo';"
 > (compile '(require "foo-bar")
            :fes-module-interop #t)
 "import fooBar from 'foo-bar';"
 > (compile '(require foo "bar")
            :fes-module-interop #t)
 "import foo from 'bar';"
 > (compile '(require "foo" "bar")
            :fes-module-interop #t)
 "import foo from 'bar';"
 > (compile '(require "foo")
            :fcommonjs #t)
 "let foo = require('foo');"
 > (compile '(require foo "bar")
            :fcommonjs #t)
 "let foo = require('bar');"
 > (compile '(require "foo" "bar")
            :fcommonjs #t)
 "let foo = require('bar');"
 > (compile '(require (only-in "foo"
                               bar))
            :fcommonjs #t)
 "let {bar} = require('foo');"
 > (compile '(require (only-in "foo"
                               (bar baz)))
            :fcommonjs #t)
 "let {bar: baz} = require('foo');"
 xit> (compile '(require 'foo "bar"))
 "import foo from 'bar';"
 xit> (compile '(require foo :as bar))
 "import bar from 'foo';"
 xit> (compile '(require (foo :as bar)))
 "import bar from 'foo';"
 xit> (compile '(require ("foo" :as "bar")))
 "import bar from 'foo';"

 ;; `provide`
 > (describe "provide")
 _
 > (compile '(provide))
 ""
 > (compile '(provide x))
 "export {
  x
};"
 > (compile '(provide x y))
 "export {
  x,
  y
};"
 > (compile '(provide (rename-out (x y))))
 "export {
  x as y
};"
 > (compile '(provide (rename-out (x y) (w z))))
 "export {
  x as y,
  w as z
};"
 > (compile '(provide x (rename-out (y z))))
 "export {
  x,
  y as z
};"
 > (compile '(provide x x))
 "export {
  x
};"
 > (compile '(provide x (rename-out (y x))))
 "export {
  x
};"
 > (compile '(provide (rename-out (x js/undefined))))
 "export {
  x as jsUndefined
};"
 > (compile '(provide (all-from-out "foo")))
 "export * from 'foo';"
 > (compile '(provide
               (all-from-out "foo")
               bar))
 "export * from 'foo';

export {
  bar
};"
 > (compile '(provide x)
            :fcommonjs #t)
 "module.exports = {
  x
};"
 > (compile '(provide x y)
            :fcommonjs #t)
 "module.exports = {
  x,
  y
};"
 > (compile '(provide foo-bar)
            :fcommonjs #t)
 "module.exports = {
  fooBar
};"
 > (compile '(provide (rename-out (x y)))
            :fcommonjs #t)
 "module.exports = {
  x: y
};"
 > (compile '(provide (all-from-out "foo-bar") baz)
            :fcommonjs #t)
 "module.exports = {
  ...fooBar,
  baz
};"

 ;; `compile`
 > (describe "compile")
 _
 > (compile #t)
 "true;"
 > (compile #t :to 'javascript)
 "true;"
 > (compile #t :from 'roselisp :to 'javascript)
 "true;"
 > (compile '(ann #t Any) :from 'roselisp :to 'typescript)
 "true as any;"
 > (compile "true" :to 'roselisp)
 #t
 > (compile "true" :from 'javascript :to 'roselisp)
 #t
 > (compile "true as any" :from 'typescript :to 'roselisp)
 '(ann #t Any)

 ;; `decompile`
 > (describe "decompile")
 _
 > (decompile "true")
 #t
 > (decompile "true" :from 'javascript)
 #t
 > (decompile "true" :from 'javascript :to 'roselisp)
 #t
 > (decompile "true as any" :from 'typescript)
 '(ann #t Any)
 > (decompile "true as any" :from 'typescript :to 'roselisp)
 '(ann #t Any)

 ;; `license`
 > (describe "license")
 _
 > license
 'MPL-2.0)
