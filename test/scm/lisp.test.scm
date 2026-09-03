;;; # Lisp tests
;;;
;;; Tests of some non-Scheme Lisp constructs. Intended to exercise the
;;; language's capability to implement other Lisp dialects.

(require (only-in "./test-util"
                  test-repl
                  test-macro))

(declare-macro test-macro)

(test-macro
 :repl #t

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

g * g;")
