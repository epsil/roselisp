;;; # Lisp tests
;;;
;;; Tests of some non-Scheme Lisp constructs. Intended to exercise
;;; the language's capability to implement other Lisp dialects.

(require (only-in "./test-util"
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 :repl #t

 :describe "nil"
 > nil
 '()
 > (list? nil)
 #t
 > (length nil)
 0
 > (compile 'nil)
 "[];"

 :describe "intern"
 > (intern "foo")
 'foo

 :describe "gensym"
 > (symbol? (gensym "foo"))
 #t
 > (eq? (gensym "foo") 'foo)
 #f
 > (eq? (gensym "foo") (gensym "foo"))
 #f
 > (compile '(gensym "foo"))
 "Symbol('foo');"
 > (compile `(begin ,(gensym "x")))
 "x;"
 > (compile `(define ,(gensym "x") 1))
 "let x = 1;"
 > (compile `(let ((x 0))
               (define ,(gensym "x") 1)))
 "let x = 0;

let x1 = 1;"
 > (compile `(let ((x 1))
               (define y
                 (quote ,(gensym "x")))))
 "let x = 1;

let y = Symbol.for('x1');"
 > (compile `(let ((x 0))
               (define ,(gensym "x") 1)
               (let ((x1 0)))))
 "let x = 0;

let x2 = 1;

let x1 = 0;"
 > (compile `(begin
               (define x 1)
               (define ,(gensym "x") 2)))
 "let x = 1;

let x1 = 2;"
 > (compile `(begin
               (define x 1)
               (define x1 2)
               (define ,(gensym "x") 3)))
 "let x = 1;

let x1 = 2;

let x2 = 3;"
 > (compile `(begin
               (define x 1)
               (define ,(gensym "x") 2)
               (define x1 3)))
 "let x = 1;

let x2 = 2;

let x1 = 3;"
 > (compile `(begin
               (define x 1)
               (define ,(gensym "x") 2)
               (define-values (x1)
                 (list 3))))
 "let x = 1;

let x2 = 2;

let [x1] = [3];"
 > (compile `(begin
               (define x 1)
               (define (,(gensym "x"))
                 2)
               (define-values (x1)
                 (list 3))))
 "let x = 1;

function x2() {
  return 2;
}

let [x1] = [3];"
 > (compile `(begin
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
 > (compile (let ((gensym-x (gensym "x")))
              `(let ((x 0))
                 (define ,gensym-x 1)
                 (let ((x1 0))
                   (define ,gensym-x 1)))))
 "let x = 0;

let x2 = 1;

let x1 = 0;

let x2 = 1;"
 > (compile `(begin
               (define foo
                 ,(gensym "test"))
               (define bar
                 ,(gensym "test"))))
 "let foo = test;

let bar = test1;"
 > (compile `(define x
               ,(gensym "x")))
 "let x = x1;"
 > (compile `(define x
               ',(gensym "x")))
 "let x = Symbol.for('x1');"
 > (compile `(let ((x ,(gensym "x")))
               (foo)))
 "let x = x1;

foo();"
 > (compile `(let ((x (quote ,(gensym "x"))))
               (foo)))
 "let x = Symbol.for('x1');

foo();"
 > (compile `(let ((x ',(gensym "x")))
               (foo)))
 "let x = Symbol.for('x1');

foo();"

 :describe "Keywords"
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

 :describe "nth"
 > (nth 0 '(1))
 1
 > (nth 1 '(1 2))
 2
 > (nth 2 '(1 2 3))
 3
 > (funcall nth 1 '(1 . (2 . ())))
 2
 > (funcall nth 1 '(1 2 . (3 . ())))
 2
 > (compile '(nth n x))
 "x[n];"
 > (compile '(module m scheme
               (nth n x))
            :fdottedlists #f)
 "x[n];"
 > (compile '(module m scheme
               (nth n x))
            :fdottedlists #t)
 "import {
  nth
} from 'roselisp';

nth(n, x);"

 :describe "aref"
 > (compile '(aref args 0))
 "args[0];"
 > (compile '(aref args 0 1))
 "args[0][1];"

 :describe "aget"
 > (compile '(aget args 0))
 "args[0];"
 > (compile '(aget args 0 1))
 "args[0][1];"

 :describe "aset!"
 > (compile '(aset! args 0 1))
 "args[0] = 1;"

 :describe "nthcdr"
 > (nthcdr 0 '(1 2 3))
 '(1 2 3)
 > (nthcdr 1 '(1 2 3))
 '(2 3)
 > (nthcdr 2 '(1 2 3))
 '(3)
 > (nthcdr 3 '(1 2 3))
 '()
 > (nthcdr 1 '(1 . 2))
 2
 > (compile '(module m scheme
               (nthcdr n x))
            :fdottedlists #f)
 "import {
  nthcdr
} from 'roselisp';

nthcdr(n, x);"
 > (compile '(module m scheme
               (nthcdr n x))
            :fdottedlists #t)
 "import {
  nthcdr
} from 'roselisp';

nthcdr(n, x);"

 :describe "funcall"
 > (compile '(funcall f))
 "f();"
 > (compile '(funcall f x))
 "f(x);"
 > (compile '(funcall f x y))
 "f(x, y);"
 > (compile '(module m scheme
               (funcall length x)))
 "import {
  length
} from 'roselisp';

length(x);"

 :describe "while"
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

 :describe "defclass"
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

 :describe "->"
 > (compile '(-> x
                 (.foo "bar")
                 (.baz)))
 "x.foo('bar').baz();"
 > (compile '(-> regular-args
                 (.map (lambda (arg)
                         (compile-expression
                          arg env inherited-options)))
                 (.join ", ")))
 "regularArgs.map(function (arg) {
  return compileExpression(arg, env, inheritedOptions);
}).join(', ');"

 :describe "set"
 > (compile '(set 'x 1))
 "x = 1;"

 :describe "setq"
 > (compile '(setq x 1))
 "x = 1;"
 > (compile '(setq x 1
                   y 2))
 "x = 1;

y = 2;"

 :describe "destructuring-bind"
 > (destructuring-bind (x)
                       '(1)
                       (list x))
 '(1)
 > (destructuring-bind (x y)
                       '(1 2)
                       (list x y))
 '(1 2)
 > (destructuring-bind (x y z)
                       '(1 2 3)
                       (list x y z))
 '(1 2 3)
 > (destructuring-bind ((x) y z)
                       '((1) 2 3)
                       (list x y z))
 '(1 2 3)
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

 :describe "multiple-values-bind"
 > (multiple-values-bind (x y)
                         (values 1 2)
                         (list x y))
 '(1 2)
 > (compile '(multiple-values-bind (x y)
                                   (values 1 2)
                                   (list x y)))
 "let [x, y] = [1, 2];

[x, y];"

 :describe "cl/listp"
 > (cl/listp #t)
 #f
 > (cl/listp '())
 #t
 > (cl/listp '(1 . 2))
 #t
 > (cl/listp '(1 2 3))
 #t

 :describe "el/listp"
 > (el/listp #t)
 #f
 > (el/listp '())
 #t
 > (el/listp '(1 . 2))
 #t
 > (el/listp '(1 2 3))
 #t

 :describe "el/if"
 > (el/if #t 1 2)
 1
 > (el/if #f 1 2)
 2
 > (el/if #f 1 2 3)
 3
 > (el/if #f 1 2 3 4)
 4

 :describe "defun"
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

 :describe "defmacro"
 > ((lambda ()
      (defmacro my-macro (x)
        x)
      (my-macro 1)))
 1
 > (compile
    '(module m scheme
       (defmacro foo ()
         '(begin))
       (foo)))
 "function foo(exp, env) {
  return [Symbol.for('begin')];
}

foo.ftype = 'macro';"
 > (compile
    '(module m scheme
       (defmacro foo (x)
         x)
       (define (bar x)
         (foo x))))
 "function foo(exp, env) {
  let [x] = exp.slice(1);
  return x;
}

foo.ftype = 'macro';

function bar(x) {
  return x;
}"
 > (compile
    '(module m scheme
       (defmacro foo (x)
         `(begin ,x))
       (define (bar x)
         (foo x))))
 "function foo(exp, env) {
  let [x] = exp.slice(1);
  return [Symbol.for('begin'), x];
}

foo.ftype = 'macro';

function bar(x) {
  return x;
}"
 > (compile
    '(module m scheme
       (defmacro foo (x . args)
         x)
       (define (bar x)
         (foo x))))
 "function foo(exp, env) {
  let [x, ...args] = exp.slice(1);
  return x;
}

foo.ftype = 'macro';

function bar(x) {
  return x;
}"
 > (compile
    '(module m scheme
       (defmacro foo (x . args)
         x)
       (define bar
         (foo 1))))
 "function foo(exp, env) {
  let [x, ...args] = exp.slice(1);
  return x;
}

foo.ftype = 'macro';

let bar = 1;"
 > (compile
    '(begin
       (defmacro foo (x . args)
         x)
       (define bar
         (foo 1))))
 "function foo(exp, env) {
  let [x, ...args] = exp.slice(1);
  return x;
}

foo.ftype = 'macro';

let bar = 1;"
 > (compile
    '(begin
       (define (foo x)
         x)
       (defmacro bar (x)
         (foo x))
       (define baz
         (bar 1))))
 "function foo(x) {
  return x;
}

function bar(exp, env) {
  let [x] = exp.slice(1);
  return foo(x);
}

bar.ftype = 'macro';

let baz = 1;"
 > (compile
    '(begin
       (define (foo-bar x)
         x)
       (defmacro bar (x)
         (foo-bar x))
       (define baz
         (bar 1))))
 "function fooBar(x) {
  return x;
}

function bar(exp, env) {
  let [x] = exp.slice(1);
  return fooBar(x);
}

bar.ftype = 'macro';

let baz = 1;"
 > (compile
    '(begin
       (define (foo-bar x)
         x)
       (defmacro bar (x)
         (foo-bar 'x))
       (define baz
         (bar 1))))
 "function fooBar(x) {
  return x;
}

function bar(exp, env) {
  let [x] = exp.slice(1);
  return fooBar(Symbol.for('x'));
}

bar.ftype = 'macro';

let baz = x;"
 > (compile
    '(begin
       (define (foo-bar x)
         'x)
       (defmacro bar (x)
         (foo-bar x))
       (define baz
         (bar 1))))
 "function fooBar(x) {
  return Symbol.for('x');
}

function bar(exp, env) {
  let [x] = exp.slice(1);
  return fooBar(x);
}

bar.ftype = 'macro';

let baz = x;"
 > (compile
    '(module m scheme
       (define (foo-bar x)
         (keyword? x))
       (defmacro bar (x)
         (foo-bar x))
       (define baz
         (bar 1)))
    :finline-functions #t)
 "let [keywordp] = (() => {
  function keywordp_(obj) {
    return (typeof obj === 'symbol') && (obj.description.match(new RegExp('^:')) ? true : false);
  }
  return [keywordp_];
})();

function fooBar(x) {
  return keywordp(x);
}

function bar(exp, env) {
  let [x] = exp.slice(1);
  return fooBar(x);
}

bar.ftype = 'macro';

let baz = false;"
 > (compile
    '(begin
       (define foo
         (lambda (x)
           x))
       (defmacro bar (x)
         (foo x))
       (define baz
         (bar 1))))
 "let foo = function (x) {
  return x;
};

function bar(exp, env) {
  let [x] = exp.slice(1);
  return foo(x);
}

bar.ftype = 'macro';

let baz = 1;"
 > (compile
    '(begin
       (define-values (foo)
         (list
          (lambda (x)
            x)))
       (defmacro bar (x)
         (foo x))
       (define baz
         (bar 1))))
 "let [foo] = [function (x) {
  return x;
}];

function bar(exp, env) {
  let [x] = exp.slice(1);
  return foo(x);
}

bar.ftype = 'macro';

let baz = 1;"
 > (compile
    '(begin
       (define-fields (foo)
         (js/obj "foo"
                 (lambda (x)
                   x)))
       (defmacro bar (x)
         (foo x))
       (define baz
         (bar 1))))
 "let {foo} = {
  foo: function (x) {
    return x;
  }
};

function bar(exp, env) {
  let [x] = exp.slice(1);
  return foo(x);
}

bar.ftype = 'macro';

let baz = 1;"
 > (compile
    '(begin
       (define-fields ((foo foo1))
         (js/obj "foo"
                 (lambda (x)
                   x)))
       (defmacro bar (x)
         (foo1 x))
       (define baz
         (bar 1))))
 "let {foo: foo1} = {
  foo: function (x) {
    return x;
  }
};

function bar(exp, env) {
  let [x] = exp.slice(1);
  return foo1(x);
}

bar.ftype = 'macro';

let baz = 1;"
 > (compile
    '(begin
       (define/async (foo x)
         x)
       (defmacro bar (x)
         (foo x))
       (define baz
         (bar 1))))
 "async function foo(x) {
  return x;
}

function bar(exp, env) {
  let [x] = exp.slice(1);
  return foo(x);
}

bar.ftype = 'macro';

let baz = 1;"
 > (compile
    '(begin
       (define foo
         (async
          (lambda (x)
            x)))
       (defmacro bar (x)
         (foo x))
       (define baz
         (bar 1))))
 "async function foo(x) {
  return x;
}

function bar(exp, env) {
  let [x] = exp.slice(1);
  return foo(x);
}

bar.ftype = 'macro';

let baz = 1;"
 > (compile
    '(begin
       (define-fexpr (foo x)
         x)
       (defmacro bar (x)
         (foo x))
       (define baz
         (bar 1))))
 "function foo(x) {
  return x;
}

foo.ftype = 'fexpr';

function bar(exp, env) {
  let [x] = exp.slice(1);
  return foo(Symbol.for('x'));
}

bar.ftype = 'macro';

let baz = x;"
 > (compile
    '(begin
       (define-class Foo ()
         (define/public (foo)
           "foo"))
       (define bar
         (new Foo))
       (defmacro baz (x)
         (send bar foo))
       (define quux
         (baz 1))))
 "class Foo {
  foo() {
    return 'foo';
  }
}

let bar = new Foo();

function baz(exp, env) {
  let [x] = exp.slice(1);
  return bar.foo();
}

baz.ftype = 'macro';

let quux = 'foo';"

 :describe "unwind-protect"
 > (unwind-protect 1 2 3)
 1
 > (compile '(unwind-protect (foo) (bar)))
 "try {
  foo();
} finally {
  bar();
}"

 :describe "clj/try"
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
 > (compile '(clj/try))
 "try {
}"
 > (compile '(clj/try
              (set! x (/ 2 1))))
 "try {
  x = 2 / 1;
}"
 > (compile '(clj/try
              (set! x (/ 2 1))
              (finally
                (display "cleanup"))))
 "try {
  x = 2 / 1;
} finally {
  console.log('cleanup');
}"
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
 > (compile '(clj/try
              (set! x (/ 2 1))
              (catch Object e
                (display "there was an error"))
              (finally
                (display "cleanup"))))
 "try {
  x = 2 / 1;
} catch (e) {
  console.log('there was an error');
} finally {
  console.log('cleanup');
}"
 > (compile '(clj/try
              (set! x (/ 2 1))
              (catch MyException e
                (display "there was an error")
                (return #f))
              (finally
                (display "cleanup"))))
 "try {
  x = 2 / 1;
} catch (e) {
  if (e instanceof MyException) {
    console.log('there was an error');
    return false;
  } else {
    throw e;
  }
} finally {
  console.log('cleanup');
}"
 > (compile '(clj/try
              (set! x (/ 2 1))
              (catch Object e
                (display "there was an error")
                (return #f))
              (finally
                (display "cleanup"))))
 "try {
  x = 2 / 1;
} catch (e) {
  console.log('there was an error');
  return false;
} finally {
  console.log('cleanup');
}"

 :describe "cl/loop"
 > (compile '(cl/loop
              for n in names
              collect (foo)))
 "let result = [];

for (let n of names) {
  result.push(foo());
}

result;"
 > (compile '(cl/loop
              for g in gensyms
              for n in names
              collect (list g n)))
 "let result = [];

let _end = gensyms.length;

let _end1 = names.length;

for (let i = 0, j = 0; (i < _end) && (j < _end1); i++, j++) {
  let g = gensyms[i];
  let n = names[j];
  result.push([g, n]);
}

result;"

 :describe "with-gensyms"
 > (compile '(with-gensyms (x)
                           x))
 "let x = Symbol('g');

x;"

 :describe "once-only"
 > (compile '(begin
               (define-macro (my-square x)
                 (once-only (x)
                            `(* ,x ,x)))
               (my-square (+ 1 1))))
 "function mySquare(exp, env) {
  let [x] = exp.slice(1);
  let g = Symbol('g');
  return [Symbol.for('let'), [[g, x]], (() => {
    let x = g;
    return [Symbol.for('*'), x, x];
  })()];
}

mySquare.ftype = 'macro';

let g = 1 + 1;

g * g;"
 > (compile '(begin
               (define-macro (my-plus x y)
                 (once-only (x y)
                            `(+ ,x ,y)))
               (my-plus (+ 1 1) (+ 2 2))))
 "function myPlus(exp, env) {
  let [x, y] = exp.slice(1);
  let g = Symbol('g');
  let g1 = Symbol('g');
  return [Symbol.for('let'), [[g, x], [g1, y]], (() => {
    let x = g;
    let y = g1;
    return [Symbol.for('+'), x, y];
  })()];
}

myPlus.ftype = 'macro';

let g = 1 + 1;

let g1 = 2 + 2;

g + g1;")
